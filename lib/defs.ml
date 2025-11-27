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
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int]) C.not_zero in
  v, ty

let isReal = 
  let v = MVariable.create Immut (Some "isReal") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.dbl]) C.not_zero in
  v, ty

let integer = 
  let v = MVariable.create Immut (Some "INTEGER") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int]) Ty.any in
  v, ty

let real = 
  let v = MVariable.create Immut (Some "REAL") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.dbl]) Ty.any in
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

let allocVector =
  let v = MVariable.create Immut (Some "allocVector") in
  let alpha = TVar.mk TVar.KInfer None |> TVar.typ in 
  let scalar = Arrow.mk (Tuple.mk [(Ty.cap alpha Prim.any); C.one]) (Vecs.mk_singl alpha) in
  let vec = Arrow.mk (Tuple.mk [(Ty.cap alpha Prim.any); C.not_one]) (Vecs.mk_unsized alpha)  in
  let ty = Ty.cap scalar vec in
  v, ty

let  length =
  let v = MVariable.create Immut (Some "LENGTH") in
  let scalar = Arrow.mk (Tuple.mk [(Vecs.mk_singl Prim.any)]) C.one in
  let vec = Arrow.mk (Ty.diff (Tuple.mk [(Vecs.mk_unsized Prim.any)]) (Tuple.mk [(Vecs.mk_singl Prim.any)])) C.not_one in
  let ty = Ty.cap scalar vec in
  v, ty

let array_assignment =
  let v = MVariable.create Immut (Some "[]<-") in
  let ty = Arrow.mk (Tuple.mk [Ty.any; C.int; C.int]) C.void in
  v, ty

let array_access =
  let v = MVariable.create Immut (Some "[]") in
  let ty = Arrow.mk (Tuple.mk [Ty.any; C.int]) C.int in
  v, ty

let protect =
  let v = MVariable.create Immut (Some "PROTECT") in
  let alpha = TVar.mk TVar.KInfer None |> TVar.typ in 
  let ty = Arrow.mk (Tuple.mk [alpha]) alpha in
  v, ty

let unprotect =
  let v = MVariable.create Immut (Some "UNPROTECT") in
  let ty = Arrow.mk (Tuple.mk [C.int]) C.void in
  v, ty

let intsxp =
  let v = MVariable.create Immut (Some "INTSXP") in
  let ty = Prim.int in
  v, ty

let realsxp =
  let v = MVariable.create Immut (Some "REALSXP") in
  let ty = Prim.dbl in
  v, ty

let plus = 
  let v = MVariable.create Immut (Some "+__2") in
  let int_ty = Arrow.mk (Tuple.mk [C.int; C.int]) C.int in
  let real_ty = Arrow.mk (Tuple.mk [C.double; C.double]) C.double in
  let ty = Ty.cap int_ty real_ty in
  v, ty

let defs = [(tobool, tobool_t); error ; isInteger ; integer ; array_assignment ; 
            array_access ; logical_or ; length ; allocVector ; protect ; 
            unprotect ; neg ; intsxp ; plus; realsxp ; real ; isReal ;
            logical_and ]


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