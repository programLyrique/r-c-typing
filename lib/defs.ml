open Rstt
open Mlsem.Types 
open Mlsem.Common
open Vectors
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)


let any_sexp = let open Rstt.Prim in
   Ty.disj [Chr.any; Clx.any; Dbl.any; Int.any; Lgl.any; Raw.any ; Rstt.Env.any; Rstt.Vec.any]

let any_c = Ty.disj [Cint.any; Cenums.char; Cenums.double; Cptr.any]

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Ty.bool in
  (* We actually only care about this part for C*)
  let tt = Arrow.mk (Ty.cup Ty.tt Cint.tt) Ty.tt in 
  let ff = Arrow.mk (Ty.cup Ty.ff Cint.ff) Ty.ff in
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
    (fun (name, ty) -> mk_builtin name ty)
    [ ("INTSXP", Prim.int); (* Primitive types*)
      ("REALSXP", Prim.dbl);
      ("RAWSXP", Prim.raw);
      ("CPLXSXP", Prim.clx);
      ("STRSXP", Prim.chr);
      ("LGLSXP", Prim.lgl);
      ("NILSXP", Prim.nil);
      ("VECSXP", Prim.vlist);
      ("EXPRSXP", Prim.expr);
      ("CLOSXP", Prim.closure);
      ("BUILTINSXP", Prim.builtin);
      ("SPECIALSXP", Prim.special);
      ("CHARSXP", Prim.chr); (* We should actually differentiate between STRSXP and CHARSXP: STRSXP is a vector of CHARSXP?*)
      ("SYMSXP", Prim.sym);
      ("LISTSXP", Prim.pairlist);
      ("LANGSXP", Prim.lang);
      ("ENVSXP", Prim.env);
      ("ANYSXP", Ty.cup Vecs.any Prim.any);
      ("DOTSXP", Ty.any); (* TODO: replace by the actual type: a list with an any tail*)
      (* Booleans*)
      ("TRUE", Cint.tt);
      ("FALSE", Cint.ff);
      (* SPecial values *)
      ("R_NilValue", Prim.nil);
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