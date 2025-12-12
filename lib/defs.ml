open Mlsem.Common
open Mlsem.Types
open Vectors
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API *)

let error = 
  let v = MVariable.create Immut (Some "error") in
  let ty = Arrow.mk (Tuple.mk [Ty.empty]) Ty.any in 
  v, ty

let isInteger = 
  let v = MVariable.create Immut (Some "isInteger") in
  let tt = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int]) C.one in
  let ff = Arrow.mk (Tuple.mk [(Ty.neg (Vecs.mk_unsized Prim.int))]) C.zero in
  let ty = Ty.cap tt ff in
  v, ty

let isReal = 
  let v = MVariable.create Immut (Some "isReal") in
   let tt = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.dbl]) C.one in
  let ff = Arrow.mk (Tuple.mk [(Ty.neg (Vecs.mk_unsized Prim.dbl))]) C.zero in
  let ty = Ty.cap tt ff in
  v, ty

let is_scalar =
  let v = MVariable.create Immut (Some "IS_SCALAR") in
  let tt_int = Arrow.mk (Tuple.mk [Vecs.mk_singl Prim.int]) C.one in
  let tt_dbl = Arrow.mk (Tuple.mk [Vecs.mk_singl Prim.dbl]) C.one in
  let ff_int = Arrow.mk (Tuple.mk [Ty.diff (Vecs.mk_unsized Prim.int) (Vecs.mk_singl Prim.int)]) C.zero in
  let ff_dbl = Arrow.mk (Tuple.mk [Ty.diff (Vecs.mk_unsized Prim.dbl) (Vecs.mk_singl Prim.dbl)]) C.zero in
  let ty = Ty.conj [tt_int; tt_dbl; ff_int; ff_dbl] in
  v, ty

let integer = 
  let v = MVariable.create Immut (Some "INTEGER") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int]) C.int_ptr in
  v, ty

let real = 
  let v = MVariable.create Immut (Some "REAL") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.dbl]) C.double_ptr in
  v, ty

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Ty.bool in
  (* We actually only care about this part for C*)
  let tt = Arrow.mk (Ty.cup Ty.tt C.not_zero) Ty.tt in 
  let ff = Arrow.mk (Ty.cup Ty.ff C.zero) Ty.ff in
  let ty = Ty.conj [def;tt;ff] in
  v, ty

let logical_or =
  let v = MVariable.create Immut (Some "||__2") in

  (* Only handle "C" and mlsem booleans *)
  let tt = Arrow.mk (Ty.disj [Tuple.mk [Ty.cup Ty.tt C.not_zero; Ty.cup Ty.tt C.not_zero] ;
    Tuple.mk [Ty.cup Ty.tt C.not_zero; Ty.cup Ty.ff C.zero] ; 
    Tuple.mk [Ty.cup Ty.ff C.zero; Ty.cup Ty.tt C.not_zero]]) Ty.tt in
  
  let ff = Arrow.mk (Tuple.mk [Ty.cup Ty.ff C.zero; Ty.cup Ty.ff C.zero]) Ty.ff in
  let ty = Ty.conj [tt;ff] in
  v, ty

let logical_and =
  let v = MVariable.create Immut (Some "&&__2") in

  (* Only handle "C" and mlsem booleans *)
  let tt = Arrow.mk (Tuple.mk [Ty.cup Ty.tt C.not_zero; Ty.cup Ty.tt C.not_zero]) Ty.tt in
  let ff = Arrow.mk (Ty.disj [Tuple.mk [Ty.cup Ty.ff C.zero; Ty.cup Ty.ff C.zero] ;
    Tuple.mk [Ty.cup Ty.tt C.not_zero; Ty.cup Ty.ff C.zero] ; 
    Tuple.mk [Ty.cup Ty.ff C.zero; Ty.cup Ty.tt C.not_zero]]) Ty.ff in
  let ty = Ty.conj [tt;ff] in
  v, ty

let neg = 
  let v = MVariable.create Immut (Some "!__1") in
  let tt = Arrow.mk (Tuple.mk [Ty.cup C.not_zero Ty.tt]) Ty.ff in
  let ff = Arrow.mk (Tuple.mk [Ty.cup C.zero Ty.ff]) Ty.tt in

  let ty = Ty.conj  [tt;ff] in
  v, ty

  let inferior_strict = 
  let v = MVariable.create Immut (Some "<__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) Ty.bool in
  let dbl_ty = Arrow.mk (Tuple.mk [C.double; C.double]) Ty.bool in
  let num_ty = Arrow.mk (Tuple.mk [C.num; C.num]) Ty.bool in
  let ty = Ty.conj [int_ty; dbl_ty; num_ty] in
  v, ty

let superior_strict = 
  let v = MVariable.create Immut (Some ">__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) Ty.bool in
  let dbl_ty = Arrow.mk (Tuple.mk [C.double; C.double]) Ty.bool in
  let num_ty = Arrow.mk (Tuple.mk [C.num; C.num]) Ty.bool in
  let ty = Ty.conj [int_ty; dbl_ty; num_ty] in
  v, ty

let allocVector =
  let v = MVariable.create Immut (Some "allocVector") in
  let alpha = TVar.mk KInfer None |> TVar.typ in 
  let scalar = Arrow.mk (Tuple.mk [(Ty.cap alpha Prim.any_scalar); C.one]) (Vecs.mk_singl alpha) in
  let vec = Arrow.mk (Tuple.mk [(Ty.cap alpha Prim.any_scalar); C.not_one]) (Vecs.mk_unsized alpha)  in
  let ty = Ty.cap scalar vec in
  v, ty

let  length =
  let v = MVariable.create Immut (Some "LENGTH") in
  let scalar = Arrow.mk (Tuple.mk [(Vecs.mk_singl Prim.any_scalar)]) C.one in
  let vec = Arrow.mk (Ty.diff (Tuple.mk [(Vecs.mk_unsized Prim.any_scalar)]) (Tuple.mk [(Vecs.mk_singl Prim.any)])) C.not_one in
  let ty = Ty.cap scalar vec in
  v, ty

let xlength =
  let v = MVariable.create Immut (Some "XLENGTH") in
  let _,ty = length in 
  v, ty

let array_assignment =
  let v = MVariable.create Immut (Some "[]<-") in

  let alpha = TVar.mk KInfer None |> TVar.typ in
  let ty = Arrow.mk (Tuple.mk [C.mk_ptr alpha; C.int; alpha]) C.void in
  v, ty

let array_access =
  let v = MVariable.create Immut (Some "[]") in
  let alpha = TVar.mk KInfer None |> TVar.typ in

  let ty = Arrow.mk (Tuple.mk [C.mk_ptr alpha; C.int]) alpha in
  v, ty

let protect =
  let v = MVariable.create Immut (Some "PROTECT") in
  let alpha = TVar.mk KInfer None |> TVar.typ in 
  let ty = Arrow.mk (Tuple.mk [alpha]) alpha in
  v, ty

let unprotect =
  let v = MVariable.create Immut (Some "UNPROTECT") in
  let ty = Arrow.mk (Tuple.mk [C.int]) C.void in
  v, ty


let na_integer =
  let v = MVariable.create Immut (Some "NA_INTEGER") in
  let ty = C.int_na in
  v, ty

let plus = 
  let v = MVariable.create Immut (Some "+__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) C.int in
  let real_ty = Arrow.mk (Tuple.mk [C.double; C.double]) C.double in
  let num_ty = Arrow.mk (Tuple.mk [C.num; C.num]) C.num in
  let na_int_ty = Arrow.mk (Ty.disj [Tuple.mk [C.int_na; C.int_na]; Tuple.mk [C.int_na ; C.int]; Tuple.mk [C.int ; C.int_na]]) C.int_na in
  let ty = Ty.conj [int_ty; real_ty ; num_ty ; na_int_ty] in
  v, ty

let minus = 
  let v = MVariable.create Immut (Some "-__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) C.int in
  let real_ty = Arrow.mk (Tuple.mk [C.double; C.double]) C.double in
  let num_ty = Arrow.mk (Tuple.mk [C.num; C.num]) C.num in

  let na_int_ty = Arrow.mk (Ty.disj [Tuple.mk [C.int_na; C.int_na]; Tuple.mk [C.int_na ; C.int]; Tuple.mk [C.int ; C.int_na]]) C.int_na in
  let ty = Ty.conj [int_ty; real_ty ; num_ty ; na_int_ty] in
  v, ty

let modulo =
  let v = MVariable.create Immut (Some "%__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) C.int in
  let ty = int_ty in
  v, ty

let incr = 
  let v = MVariable.create Immut (Some "++__1") in
  let int_ty = Arrow.mk (Tuple.mk [C.int]) C.int in
  let real_ty = Arrow.mk (Tuple.mk [C.double]) C.double in
  let num_ty = Arrow.mk (Tuple.mk [C.num]) C.num in
  let ty = Ty.conj [int_ty; real_ty ; num_ty] in
  v, ty

let deref =
  let v = MVariable.create Immut (Some "*__1") in
  let alpha = TVar.mk KInfer None |> TVar.typ in
  let ty = Arrow.mk (Tuple.mk [C.mk_ptr alpha]) alpha in
  v, ty

let reference = 
  let v = MVariable.create Immut (Some "&__1") in
  let alpha = TVar.mk KInfer None |> TVar.typ in
  let ty = Arrow.mk (Tuple.mk [alpha]) (C.mk_ptr alpha) in
  v, ty

let r_int_min =
  let v = MVariable.create Immut (Some "R_INT_MIN") in
  let ty = C.int in
  v, ty

let r_int_max =
  let v = MVariable.create Immut (Some "R_INT_MAX") in
  let ty = C.int in
  v, ty

let envlength = 
  let v = MVariable.create Immut (Some "Rf_envlength") in
  let ty = Arrow.mk (Tuple.mk [Prim.env]) C.int in
  v, ty

let cdr = 
  let v = MVariable.create Immut (Some "CDR") in
  let pairlist_ty = Arrow.mk (Tuple.mk [Prim.pairlist]) (Prim.pairlist) in
  let lang_ty = Arrow.mk (Tuple.mk [Prim.lang]) (Prim.lang) in
  let dotsxp_ty = Arrow.mk (Tuple.mk [Ty.any]) Ty.any in
  let ty = Ty.conj [pairlist_ty; lang_ty; dotsxp_ty] in
  v, ty

let typeof = 
  let v = MVariable.create Immut (Some "TYPEOF") in
  let alpha = TVar.mk KInfer None |> TVar.typ in 
  let vec_ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized (Ty.cap alpha Prim.any_scalar)]) alpha in
  let other_ty = Arrow.mk (Tuple.mk [Ty.diff (Ty.cap alpha Prim.any) (Ty.cup Vecs.any Prim.any_scalar)]) alpha in
  (* primitive scalar types (Prim.any) cannot be arguments of TYPEOF*)
  let ty = Ty.conj [vec_ty; other_ty] in 
  v, ty



module BuiltinOp = struct
let eq = MVariable.create Immut (Some "==__2")
let neq = MVariable.create Immut (Some "!=__2")
let all = [ eq ; neq ]
let find_builtin str =
  let f v =
    match Variable.get_name v with
    | None -> false
    | Some name -> String.equal name str
  in
  List.find_opt f all
end


let eq = 
  let v = BuiltinOp.eq in
  let ty = Arrow.mk (Tuple.mk [Ty.any; Ty.any]) Ty.bool in
  v, ty

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
      ("CHARSXP", Prim.chr); (* We should actually differentiate between STRSXP and CHARSXP: STRSXP is a vector of CHARSXP?*)
      ("SYMSXP", Prim.sym);
      ("LISTSXP", Prim.pairlist);
      ("LANGSXP", Prim.lang);
      ("ENVSXP", Prim.env);
      ("ANYSXP", Ty.cup Vecs.any Prim.any);
      ("DOTSXP", Ty.any); (* TODO: replace by the actual type: a list with an any tail*)
      (* Booleans*)
      ("TRUE", C.one);
      ("FALSE", C.zero);
      (* SPecial values *)
      ("R_NilValue", Prim.nil);
    ]


let find_builtin str =
  List.assoc_opt str all


let has_builtin str =
  List.mem_assoc str all
end

let defs = [(tobool, tobool_t); error ; isInteger ; integer ; array_assignment ; 
            array_access ; logical_or ; length ; xlength ; allocVector ; protect ; 
            unprotect ; neg ; plus; minus ; real ; isReal ; is_scalar ;
            logical_and ; inferior_strict ; incr ; modulo ; superior_strict ;
            na_integer ; r_int_min ; r_int_max ; typeof ; eq ; deref ;
            reference; envlength ; cdr ] @ BuiltinVar.all


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