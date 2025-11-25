open Mlsem.Common
open Mlsem.Types
open Vectors
module MVariable = Mlsem.Lang.MVariable

(* Add here any known type definitions, for instance, from the R C API
TODO for the example in small.c:
  isInteger 
  LENGTH
  INTEGER
  alloVector
  PROTECT
  error
*)

let error = 
  let v = MVariable.create Immut (Some "error") in
  let ty = Arrow.mk Ty.empty Ty.any in 
  v, ty

let isInteger = 
  let v = MVariable.create Immut (Some "isInteger") in
  let ty = Arrow.mk Prim.int C.int in
  v, ty

let integer = 
  let v = MVariable.create Immut (Some "INTEGER") in
  let ty = Arrow.mk Prim.int Ty.any in
  v, ty

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Ty.bool in
  let tt = Arrow.mk (Ty.disj [Prim.tt;Vecs.mk_singl Prim.tt]) Ty.tt in
  let ff = Arrow.mk (Ty.disj [Prim.ff;Vecs.mk_singl Prim.ff]) Ty.ff in
  let ty = Ty.conj [def;tt;ff] in
  v, ty


let defs = [(tobool, tobool_t); error ; isInteger ; integer ]
let initial_env =
  let add_def env (v,ty) =
    Env.add v (GTy.mk ty |> TyScheme.mk_poly) env
  in
  List.fold_left add_def Env.empty defs