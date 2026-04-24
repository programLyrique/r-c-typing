(** This is to type normal C functions, and functions that use the .C convention.
See https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Interface-functions-_002eC-and-_002eFortran 

We directly type from PAst. We only need to look at the parameters and return type.
*)

open Mlsem.Types
open PAst



(* Is it a C function with no SEXP type as parameter or return type?
And with simple types. If there are structs, we still want to infer it with its body 
because the struct could contain SEXPs...*)
let is_simple_c_function past =
  match past with 
  | Fundef(ret_ty, _, params, _) -> 
    let ret = match ret_ty with
      | Ast.SEXP -> false
      | Ast.Struct _ -> false 
      | _ -> true
    in
    let params = List.for_all (fun (ty, _) -> match ty with
        | Ast.SEXP -> false
        | Ast.Struct _ -> false 
        | _ -> true) params
  in
   ret && params
  | _ -> false

       

(* [typedef_map] carries the accumulated [TypeDecl]s so that typedef-aliased
   parameter/return types (e.g. [r_obj* = struct SEXPREC*]) are resolved to
   their canonical form before being translated to Sstt. Without it,
   [Typeref "r_obj"] degrades to [Ty.any] and a [SEXP] signature becomes
   [*any -> *any]. The result is wrapped with [Attr.mk_anyclass] to match
   the calling convention expected by [Ast.build_call] (which projects the
   callee through [pfun], whose domain requires an attribute-wrapped
   function); without this, any body applying such a binding fails with
   "untypeable projection". *)
let infer_cfun ?(typedef_map = Ast.DeclMap.empty) return_ty params =
  let resolve = PAst.resolve_ctype typedef_map in
  let arg_types = List.map (fun (ty, _) -> Ast.typeof_ctype (resolve ty)) params in
  let arg_tuple = Tuple.mk arg_types in
  let ret_ty = Ast.typeof_ctype (resolve return_ty) in
  Arrow.mk arg_tuple ret_ty |> Rstt.Attr.mk_anyclass


let dotC_typeof typedef_map ct =
  let rec resolve ty =
    match ty with
    | Ast.Typeref name ->
        (match Ast.DeclMap.find_opt name typedef_map with
         | Some t -> resolve t
         | None -> ty)
    | Ast.Ptr inner -> Ast.Ptr (resolve inner)
    | _ -> ty
  in
  let ct = resolve ct in
  let open Ast in
  let open Rstt in
  match ct with
  (* TODO: add also Rcomplex* *)
  | Ptr Int -> Ty.cup (Vec.AnyLength (Prim.mk Prim.Int.any) |> Vec.mk |> Attr.mk_anyclass)
    (Vec.AnyLength (Prim.mk Prim.Lgl.any) |> Vec.mk |> Attr.mk_anyclass) (* We allow logical vectors as well since they can be coerced to int *)
  | Ptr Float -> Vec.AnyLength (Prim.mk Prim.Dbl.any) |> Vec.mk |> Attr.mk_anyclass
  | Ptr Ptr Char -> Vec.AnyLength (Prim.mk Prim.Chr.any) |> Vec.mk |> Attr.mk_anyclass
  | Ptr Char -> Vec.AnyLength (Prim.mk Prim.Raw.any) |> Vec.mk |> Attr.mk_anyclass (* Actually unsigned char* *)
  | _ -> failwith (Printf.sprintf "Unsupported type for .C interface: %s. Only init*, double*, char** and unsigned char* are supported." (Ast.show_ctype ct))


let infer_dotC ?(typedef_map = Ast.DeclMap.empty) ret_ty params =
  let arg_types = List.map (fun (ty, _) -> dotC_typeof typedef_map ty) params in
  let arg_tuple = Tuple.mk arg_types in
  if ret_ty != Ast.Void then
    failwith "Return type of .C functions must be void.";
  (* Same attribute wrapping as [infer_cfun] — see comment there. *)
  Arrow.mk arg_tuple Rstt.Cenums.void |> Rstt.Attr.mk_anyclass


let infer_dotC_from_past ?(typedef_map = Ast.DeclMap.empty) = function
  | _, Fundef (ret_ty, _, params, _) -> infer_dotC ~typedef_map ret_ty params
  | _ -> failwith "Expected a function definition."


let mk_test_fundef ?(ret_ty=Ast.Void) ?(params=[]) name =
  let pos = Mlsem.Common.Position.dummy in
  let body = (pos, Seq []) in
  (pos, Fundef (ret_ty, name, params, body))


let infer_dotC_failure past =
  try
    ignore (infer_dotC_from_past past);
    None
  with Failure msg ->
    Some msg


let%test "infer_dotC infers supported .C parameters from a PAst function" =
  let past =
    mk_test_fundef
      ~params:
        [ Ast.Ptr Ast.Int, "ints";
          Ast.Ptr Ast.Float, "dbls";
          Ast.Ptr (Ast.Ptr Ast.Char), "strings";
          Ast.Ptr Ast.Char, "bytes" ]
      "native_entry"
  in
  let open Rstt in
  let expected =
    Arrow.mk
      (Tuple.mk
         [ Ty.cup
             (Vec.AnyLength (Prim.mk Prim.Int.any) |> Vec.mk |> Attr.mk_anyclass)
             (Vec.AnyLength (Prim.mk Prim.Lgl.any) |> Vec.mk |> Attr.mk_anyclass);
           Vec.AnyLength (Prim.mk Prim.Dbl.any) |> Vec.mk |> Attr.mk_anyclass;
           Vec.AnyLength (Prim.mk Prim.Chr.any) |> Vec.mk |> Attr.mk_anyclass;
           Vec.AnyLength (Prim.mk Prim.Raw.any) |> Vec.mk |> Attr.mk_anyclass ])
      Cenums.void
    |> Attr.mk_anyclass
  in
  Ty.equiv (infer_dotC_from_past past) expected


let%test "infer_dotC rejects non-void return type from a PAst function" =
  let past =
    mk_test_fundef ~ret_ty:Ast.Int ~params:[Ast.Ptr Ast.Int, "ints"] "native_entry"
  in
  infer_dotC_failure past = Some "Return type of .C functions must be void."


let%test "infer_dotC rejects unsupported .C parameter types from a PAst function" =
  let past =
    mk_test_fundef ~params:[Ast.Int, "value"] "native_entry"
  in
  let expected =
    Printf.sprintf
      "Unsupported type for .C interface: %s. Only init*, double*, char** and unsigned char* are supported."
      (Ast.show_ctype Ast.Int)
  in
  infer_dotC_failure past = Some expected