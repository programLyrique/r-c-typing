open Rstt
open Mlsem.Types 
open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)

let any_sexp =
   Ty.disj [Rstt.Prim.any ; Rstt.Env.any; Rstt.Vec.any ]

let any_c = Ty.disj [Cint.any; Cenums.char; Cenums.double; Cptr.any]

let exprsxp = Rstt.Builder.(
  build TIdMap.empty (TList {bindings=[] ; sym=[] ; tl=TOption (TCup (TLang, TSym))})
)

(* Special type constructors for lists*)
let allocVector_vecsxp_ty n =
  let open Rstt.Builder in 
  let builder = TList ({ bindings = List.init n (fun i -> ("_" ^ (string_of_int i), TNull)) ; sym = [] ; tl = TOption TEmpty }) in
  build TIdMap.empty builder

let mkNamed_vecsxp_ty names = 
  let open Rstt.Builder in 
  let builder =
    TList
      { bindings = List.map (fun name -> (name, TAny)) names;
        sym = [];
        tl = TOption TEmpty }
  in 
  build TIdMap.empty builder


let set_vector_elt_ty name = 
  let open Rstt.Builder in 
  let _, r' = rvar empty_env "r" in
  let _, a = tvar empty_env "a" in
  (* t({;`r}, 'a) -> {name: 'a; `r} ; actually, maybe 'a & any_sexp for the 1st argument... *)
  let builder =
    TArrow
      ( TTuple [TList {bindings = []; sym = []; tl = TRowVar r'}; TVar a],
        TList ({bindings = [(name, TVar a)]; sym = []; tl = TRowVar r'}) )
  in
  build TIdMap.empty builder

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let non_int_or_ptr = Ty.diff Ty.any (Ty.cup Cint.any_na Cptr.any) in
  (* In rstt, c_true is defined as c(1), not as c(1..) | c(..-1) i.e. not as an interval*)
  let nonzero = Ty.diff Cint.any_na Cint.ff in
  let nonnull = Ty.diff Cptr.any Cptr.null in
  let def = Arrow.mk non_int_or_ptr Cint.bool in
  (* C conditions follow C truthiness: 0 is false, any other integer is true. *)
  let tt_int = Arrow.mk nonzero Cint.tt in
  let ff_int = Arrow.mk Cint.ff Cint.ff in
  (* Pointers *)
  let tt_ptr = Arrow.mk nonnull Cint.tt in
  let ff_ptr = Arrow.mk Cptr.null Cint.ff in
  let ty = Ty.conj [def;tt_int;ff_int;tt_ptr;ff_ptr] in
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
      ("EXTPTRSXP", ExternalPtr.any |> Attr.mk_anyclass);
      ("ANYSXP", Attr.any);
      ("DOTSXP", Ty.any); (* TODO: replace by the actual type: a list with an any tail*)
      (* Booleans*)
      ("TRUE", Cint.tt);
      ("FALSE", Cint.ff);
      (* Special values *)
      ("R_NilValue", Null.any |> Attr.mk_anyclass);
    ]


let dynamic : (string, Variable.t * Ty.t) Hashtbl.t = Hashtbl.create 64

(* Register [name] with declared type [ty]. If [name] is already present,
   the variable is reused (so call sites alias) and the stored type is
   refined when [ty] is a subtype of the previously-stored type. This lets
   a later, more precise declaration upgrade a fallback binding (e.g. a
   [Ty.any] placeholder installed by [PAst.var]'s unknown-name path gets
   replaced when the real [extern SEXP foo;] declaration is seen). *)
let register_dynamic_binding ?(mut = false) name ty : Variable.t * Ty.t =
  match Hashtbl.find_opt dynamic name with
  | Some (v, existing) ->
      let ty =
        if Ty.leq ty existing && not (Ty.equiv ty existing) then ty
        else existing
      in
      let needs_mutable_rebind =
        if not mut then false
        else
          match MVariable.kind v with
          | MVariable.AnnotMut existing -> not (GTy.equiv existing (GTy.mk ty))
          | _ -> true
      in
      if needs_mutable_rebind then begin
        let v = MVariable.create (MVariable.AnnotMut (GTy.mk ty)) (Some name) in
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end else begin
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end
  | None ->
      let kind = if mut then MVariable.AnnotMut (GTy.mk ty) else Immut in
      let v = MVariable.create kind (Some name) in
      Hashtbl.add dynamic name (v, ty);
      v, ty

let register_dynamic ?mut name ty : Variable.t =
  fst (register_dynamic_binding ?mut name ty)

let find_builtin (v : Variable.t) =
  let name = Variable.get_name v in
  let from_all = List.find_opt (fun (v2, _) ->
    match Variable.get_name v2 with
    | None -> false
    | Some n -> Some n = name
  ) all |> Option.map snd
  in
  match from_all with
  | Some _ -> from_all
  | None ->
      (match name with
       | None -> None
       | Some n -> Hashtbl.find_opt dynamic n |> Option.map snd)

let find_builtin_var str =
  let from_all =
    List.find_opt (fun (v, _) ->
      match Variable.get_name v with
      | None -> false
      | Some name -> String.equal name str
    ) all |> Option.map fst
  in
  match from_all with
  | Some _ -> from_all
  | None -> Hashtbl.find_opt dynamic str |> Option.map fst

let has_builtin str =
  (List.exists (fun (v, _) ->
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  ) all) || Hashtbl.mem dynamic str
end


(* SEXPTYPE numeric values from R's [Rinternals.h], mapped to the
   [BuiltinVar] binding name we register in [BuiltinVar.all] above. Used
   by [PAst.process_call] to rewrite an integer SEXPTYPE argument back to
   its symbolic name (e.g. [Rf_allocVector(13, n)] is rewritten to
   [Rf_allocVector(INTSXP, n)]) so the call matches the [prim] type-tag
   domain of the [allocVector] signature instead of failing as an
   untypeable application. Values outside this table are left alone; the
   call will then fail downstream, which mirrors the runtime check
   [allocVector] does on the type argument. *)
let sexptype_name_of_int = function
  | 0 -> Some "NILSXP"
  | 1 -> Some "SYMSXP"
  | 2 -> Some "LISTSXP"
  | 3 -> Some "CLOSXP"
  | 4 -> Some "ENVSXP"
  | 6 -> Some "LANGSXP"
  | 7 -> Some "SPECIALSXP"
  | 8 -> Some "BUILTINSXP"
  | 9 -> Some "CHARSXP"
  | 10 -> Some "LGLSXP"
  | 13 -> Some "INTSXP"
  | 14 -> Some "REALSXP"
  | 15 -> Some "CPLXSXP"
  | 16 -> Some "STRSXP"
  | 17 -> Some "DOTSXP"
  | 18 -> Some "ANYSXP"
  | 19 -> Some "VECSXP"
  | 20 -> Some "EXPRSXP"
  | 22 -> Some "EXTPTRSXP"
  | 24 -> Some "RAWSXP"
  | _ -> None


(* For the new type algebra *)
let load_parsed_types () =
  let types_dir = Type_parser.find_types_dir () in
  let ty_files =
    Sys.readdir types_dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".ty")
    |> List.sort String.compare
    |> List.map (Filename.concat types_dir)
  in
  List.fold_left (fun acc f ->
    let rf_aliases = Filename.basename f = "base.ty" in
    let types = Type_parser.load_file ~rf_aliases f in
    Rstt.Builder.StrMap.union (fun _k _v1 v2 -> Some v2) acc types
  ) Rstt.Builder.StrMap.empty ty_files

let parsed_types, parsed_types_penv =
  Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty
    (fun () -> load_parsed_types ()) ()

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
