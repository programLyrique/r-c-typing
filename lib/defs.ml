open Rstt
open Mlsem.Types 
open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)


let any_sexp = let open Rstt.Prim in
   Ty.disj [Chr.any; Clx.any; Dbl.any; Int.any; Lgl.any; Raw.any ; Rstt.Env.any; Rstt.Vec.any]

let any_c = Ty.disj [Cint.any; Cenums.char; Cenums.double; Cptr.any]

let exprsxp = Rstt.Builder.(
  build TIdMap.empty (TList ([], [], TOption (TCup (TLang, TSym))))
)

(* Special type constructors for lists*)
let allocVector_vecsxp_ty n =
  let open Rstt.Builder in 
  let builder = TList (List.init n (fun _ -> TAny), [], TOption TEmpty) in
  build TIdMap.empty builder

let mkNamed_vecsxp_ty names = 
  let open Rstt.Builder in 
  let builder =
    TList
      ( [],
        List.map (fun name -> (name, TAny)) names,
        TOption TEmpty )
  in 
  build TIdMap.empty builder

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Cint.bool in
  (* We actually only care about this part for C*)
  let tt = Arrow.mk Cint.tt Cint.tt in
  let ff = Arrow.mk Cint.ff Cint.ff in
  let ty = Ty.conj [def;tt;ff] in
  v, ty


module BuiltinOp = struct
let eq = MVariable.create Immut (Some "==__2")
let neq = MVariable.create Immut (Some "!=__2")

let inf_strict = MVariable.create Immut (Some "<__2")
let sup_strict = MVariable.create Immut (Some ">__2")
let all = [ eq ; neq ; inf_strict ; sup_strict ]
let find_builtin str =
  let f v =
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  in
  List.find_opt f all
end



module BuiltinVar = struct
  let mk_builtin name ty =
  let v = MVariable.create Immut (Some name) in
  v, ty

  let all = List.map 
    (* Types that are not for vectors should get attributes. *)
    (fun (name, ty) -> mk_builtin name ty)
    [ ("INTSXP", Prim.Int.any |> Prim.mk);
      ("REALSXP", Prim.Dbl.any |> Prim.mk);
      ("RAWSXP", Prim.Raw.any |> Prim.mk);
      ("CPLXSXP", Prim.Clx.any |> Prim.mk);
      ("STRSXP", Prim.Chr.any |> Prim.mk);
      ("LGLSXP", Prim.Lgl.any |> Prim.mk);
      ("NILSXP", Null.any |> Attr.mk_anyclass);
      ("VECSXP", Rstt.Lst.any |> Attr.mk_anyclass);
      ("EXPRSXP", exprsxp);
      ("CLOSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_anyclass);
      ("BUILTINSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_anyclass);
      ("SPECIALSXP", Arrow.mk Ty.any Ty.any |> Attr.mk_anyclass);
      ("CHARSXP", Prim.Chr.any |> Prim.mk);
      ("SYMSXP", Sym.any |> Attr.mk_anyclass);
      ("LISTSXP", Lang.any |> Attr.mk_anyclass);
      ("LANGSXP", Lang.any |> Attr.mk_anyclass);
      ("ENVSXP", Rstt.Env.any |> Attr.mk_anyclass);
      ("ANYSXP", Attr.any);
      ("DOTSXP", Ty.any); (* TODO: replace by the actual type: a list with an any tail*)
      (* Booleans*)
      ("TRUE", Cint.tt);
      ("FALSE", Cint.ff);
      (* Special values *)
      ("R_NilValue", Null.any |> Attr.mk_anyclass);
    ]


let find_builtin str =
  List.assoc_opt str all

let find_builtin_var str = 
  List.find_opt (fun (v, _) -> 
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  ) all |> Option.map fst

let has_builtin str =
  List.mem_assoc str all
end


(* For the new type algebra *)
let filename = Type_parser.find_types_base_ty () 
let parsed_types = Type_parser.load_file filename 

let build_vars m = 
  let add_var name ty acc =
    let mVar =  BuiltinOp.find_builtin name |>
       Option.value ~default:(
        BuiltinVar.find_builtin_var name |>
        Option.value ~default:(MVariable.create Immut (Some name))
       ) in
    (mVar, ty) :: acc
  in
  Rstt.Builder.StrMap.fold add_var m []

let defs = (tobool, tobool_t) :: build_vars parsed_types

module StrMap = Map.Make(String)
let defs_map = List.fold_left 
  (fun acc (v, _) -> 
    match Variable.get_name v with 
    | Some name -> StrMap.add name v acc
    | None -> failwith "Definition variable must have a name."
  ) StrMap.empty defs

let initial_env =
  let add_def env (v,ty) =
    Env.add v (GTy.mk ty |> TyScheme.mk_poly) env
  in
  List.fold_left add_def Env.empty defs