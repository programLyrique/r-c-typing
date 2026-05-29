open Rstt
open Mlsem.Types 
open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)

(* The set of values that can flow as a real C SEXP (anything reachable via
   [r_obj*]). Of the [Prim] family only [Chr] (CHARSXP) is itself a passable
   SEXP — the others ([Int], [Dbl], [Lgl], [Clx], [Raw]) only exist as the
   element type of an enclosing [Vec]. Everything else can carry attributes,
   so [Attr.any] (any attr-tagged value, regardless of class) covers env,
   null, sym, list, lang, closure, externalptr, and vec uniformly.
   This must match the SEXP-param model in [ast.ml]'s [Function] handler so
   parameters and casts on the same SEXP value agree on its type. *)
let any_sexp = Ty.cup Rstt.Prim.(Chr.any |> mk) Rstt.Attr.any

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

(* Domain of the [getAttrib(_, R_ClassSymbol)] projection: any R SEXP. *)
let getAttrib_class_pdom _result_ty = any_sexp

(* Type of [getAttrib(v, R_ClassSymbol)] given v's inferred type. The class
   attribute is a chr vector listing v's classes. When v's [classes]
   component (from its [attr(content, classes)] encoding) is a concrete,
   positive list we refine to [v[N](class-name singletons)]; otherwise we
   fall back to [v(chr)]. The result is [Attr.mk_anyclass]-wrapped to match
   the encoding R values use in [.ty] (Builder.build does this implicitly
   for Vec; we have to be explicit here). *)
let getAttrib_class_ty v_ty =
  let open Rstt in
  let fallback =
    Attr.mk_anyclass (Vec.mk (Vec.AnyLength (Prim.mk Prim.Chr.any)))
  in
  try
    let classes_ty = Attr.proj_classes v_ty in
    match Classes.destruct classes_ty with
    | { pos; neg = []; unk = []; tail = NoOther } when pos <> [] ->
      let names = List.map (fun (Classes.L (s, _)) -> s) pos in
      let elem_ty =
        names |> List.map Prim.Chr.str |> Ty.disj |> Prim.mk
      in
      Attr.mk_anyclass (Vec.mk (Vec.CstLength (List.length names, elem_ty)))
    | _ -> fallback
  with _ -> fallback

(* [inherits(x, "name")] is lowered (in [ast.ml]) to a type-case testing [x]
   directly against [inherits_test_ty name]: the set of attr-tagged values whose
   class set includes [name] (other class labels left [Unknown], i.e. they may
   or may not be present -- [inherits] only asserts membership, not the exact
   class set; [AllOthers] would be wrong as it asserts every other class is
   present, a near-empty type). mlsem's occurrence typing then refines [x] in
   both branches: the then-branch to [x & <name, ...>], the else-branch to
   [x \ <name, ...>] (which keeps e.g. the [p(chr)] CHARSXP half of [any_sexp],
   soundly: a CHARSXP carries no class so the test is false for it).

   Testing [x] directly (rather than projecting its class component) keeps the
   type-case total over all SEXPs -- a projection would be partial (undefined on
   the classless [p(chr)]), making [inherits] on a possibly-CHARSXP value
   untypeable. *)
let inherits_test_ty name =
  let open Rstt in
  let classes =
    Classes.mk { pos = [Classes.L (name, [])]; neg = []; unk = []; tail = Classes.Unknown }
  in
  Attr.mk { content = Attr.proj_content Attr.any; classes }

(* setAttrib(vec, R_ClassSymbol, val): the result is vec's value re-tagged with
   the class attribute carried by [val]. This is the dual of
   [getAttrib_class_ty]: there we read class names out of [v]'s [classes]
   component, here we write them in from [val]'s type.

   [setAttrib_class_classes] derives the class component from [val]'s type. When
   [val] is a chr vector whose element type is a positive, finite set of string
   singletons (e.g. [mkString "foo"], typed [t(c_string('a)) -> v[1](chr('a))]
   in base.ty, or a variable holding such a value — the type flows through let
   bindings) we set exactly those classes ([tail = NoOther]). Otherwise the
   concrete names are unknown and we fall back to [Classes.any], i.e. arbitrary
   classes. The [allocVector(STRSXP,n)] + [SET_STRING_ELT] build-up idiom also
   lands here: [allocVector] seeds elements to "" and [SET_STRING_ELT] unions in
   each assigned string (see base.ty), so the element type is the set of strings
   written, plus "".

   The empty string is dropped from the recovered names: it is the STRSXP
   allocation default ("") standing in for any slot not provably overwritten, not
   a class the program asked for, and "" is not a meaningful S3/S4 class name.
   (Soundness note: this means a degenerate genuine "" class would be missed, but
   no real code dispatches on it.) If nothing but "" remains, the names are
   treated as unknown ([Classes.any]). *)
let setAttrib_class_classes val_ty =
  let open Rstt in
  let names_opt =
    try
      let content = try Attr.proj_content val_ty with _ -> val_ty in
      match Vec.destruct content with
      | [ (atom, []) ] ->
        let elem = match atom with
          | Vec.AnyLength e | Vec.CstLength (_, e) | Vec.VarLength (_, e) -> e
        in
        (match Prim.Chr.destruct (Prim.destruct elem) with
         | _, { Prim.Chr.positive = true; content } ->
           (match List.filter (fun s -> s <> "") content with
            | [] -> None
            | names -> Some names)
         | _ -> None)
      | _ -> None
    with _ -> None
  in
  match names_opt with
  | Some names ->
    Classes.mk
      { pos = List.map (fun n -> Classes.L (n, [])) names;
        neg = []; unk = []; tail = Classes.NoOther }
  | None -> Classes.any

(* Domain of the [setAttrib(_, R_ClassSymbol, _)] constructor: the value being
   tagged and the class value are both arbitrary R SEXPs. The checker calls this
   with [Ty.any], so the argument is ignored. The two entries match the two
   constructor arguments ([vec], [val]). *)
let setAttrib_class_cdom _result_ty = [ [ any_sexp; any_sexp ] ]

(* Result type of [setAttrib(vec, R_ClassSymbol, val)] from the inferred types
   of [vec] and [val]: keep vec's content, install val's classes. *)
let setAttrib_class_cons tys =
  let open Rstt in
  match tys with
  | [ vec_ty; val_ty ] ->
    let content = try Attr.proj_content vec_ty with _ -> vec_ty in
    let classes = setAttrib_class_classes val_ty in
    Attr.mk { content; classes }
  | _ -> assert false

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
      (* Predefined preprocessor macros (see
         https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html).
         [__FILE__], [__DATE__], [__TIME__] expand to string literals,
         [__LINE__] to an integer constant. We use the non-singleton C
         types since the concrete value is never load-bearing for typing. *)
      ("__FILE__", Cptr.string);
      ("__LINE__", Cint.any);
      ("__DATE__", Cptr.string);
      ("__TIME__", Cptr.string);
    ]


let dynamic : (string, Variable.t * Ty.t) Hashtbl.t = Hashtbl.create 64

(* Register [name] with declared type [ty]. If [name] is already present,
   the variable is reused (so call sites alias) and the stored type is
   refined when [ty] is a subtype of the previously-stored type. This lets
   a later, more precise declaration upgrade a fallback binding (e.g. a
   [Ty.any] placeholder installed by [PAst.var]'s unknown-name path gets
   replaced when the real [extern SEXP foo;] declaration is seen). *)
let register_dynamic_binding ?(mut = false) ?annot name ty : Variable.t * Ty.t =
  (* [annot] (when given) is the gradual [GTy.t] to use for the [AnnotMut]
     kind, overriding the default pinned [GTy.mk ty]. Used by [infer_def]
     for SEXP globals: there we want [mk_gradual empty any_sexp] so a read
     can refine to whichever SEXP sub-family (sym / env / lang / ...) the
     use-site actually requires. Non-SEXP globals stay pinned. *)
  let annot_of ty = match annot with Some a -> a | None -> GTy.mk ty in
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
          | MVariable.AnnotMut existing -> not (GTy.equiv existing (annot_of ty))
          | _ -> true
      in
      if needs_mutable_rebind then begin
        let v = MVariable.create (MVariable.AnnotMut (annot_of ty)) (Some name) in
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end else begin
        Hashtbl.replace dynamic name (v, ty);
        v, ty
      end
  | None ->
      let kind = if mut then MVariable.AnnotMut (annot_of ty) else Immut in
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

(* Wall-clock time of [load_parsed_types ()] at module-init. Captured here and
   exposed so [bin/main.ml] can emit a [Phase: load_ty …] line under
   [--log-times] without restructuring the [let] binding. *)
let ty_load_time = ref 0.0

let parsed_types, parsed_types_penv =
  let t0 = Unix.gettimeofday () in
  let r =
    Mlsem.Types.PEnv.sequential_handler Mlsem.Types.PEnv.empty
      (fun () -> load_parsed_types ()) ()
  in
  ty_load_time := Unix.gettimeofday () -. t0;
  r

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
