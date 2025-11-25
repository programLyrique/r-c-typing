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
  let ty = Arrow.mk (Vecs.mk_unsized Prim.int) C.not_zero in
  v, ty

let integer = 
  let v = MVariable.create Immut (Some "INTEGER") in
  let ty = Arrow.mk Prim.int C.int in
  v, ty

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Ty.bool in
  let tt = Arrow.mk (Ty.disj [Prim.tt;Vecs.mk_singl Prim.tt]) Ty.tt in
  let ff = Arrow.mk (Ty.disj [Prim.ff;Vecs.mk_singl Prim.ff]) Ty.ff in
  let c_tt = Arrow.mk C.not_zero Ty.tt in 
  let c_ff = Arrow.mk C.zero Ty.ff in
  let ty = Ty.conj [def;tt;ff;c_tt;c_ff] in
  v, ty

let logical_or =
  let v = MVariable.create Immut (Some "||") in
  let tt = Arrow.mk (Ty.disj [Tuple.mk [Ty.tt; Ty.tt] ;Tuple.mk [Ty.tt; Ty.ff] ; Tuple.mk [Ty.ff;Ty.tt]]) Ty.tt in
  let ff = Arrow.mk (Tuple.mk [Ty.ff; Ty.ff]) Ty.ff in
  let ty = Ty.conj [tt;ff] in
  v, ty

let neg = 
  let v = MVariable.create Immut (Some "!") in
  let tt = Arrow.mk Ty.tt Ty.ff in
  let ff = Arrow.mk Ty.ff Ty.tt in
  let ty = Ty.conj [tt;ff] in
  v, ty

let allocVector =
  let v = MVariable.create Immut (Some "alloVector") in
  let alpha = TVar.mk TVar.KInfer None |> TVar.typ in 
  let ty = Arrow.mk (Tuple.mk [(Ty.cap alpha Prim.any); C.int]) (Vecs.mk_unsized alpha)  in
  v, ty

let  length =
  let v = MVariable.create Immut (Some "LENGTH") in
  let ty = Arrow.mk (Vecs.mk_unsized Prim.any) C.int in
  v, ty

let array_assignment =
  let v = MVariable.create Immut (Some "[]<-") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int; C.int; C.int]) C.void in
  v, ty

let array_access =
  let v = MVariable.create Immut (Some "[]") in
  let ty = Arrow.mk (Tuple.mk [Vecs.mk_unsized Prim.int; C.int]) C.int in
  v, ty

let protect =
  let v = MVariable.create Immut (Some "PROTECT") in
  let alpha = TVar.mk TVar.KInfer None |> TVar.typ in 
  let ty = Arrow.mk alpha alpha in
  v, ty

let defs = [(tobool, tobool_t); error ; isInteger ; integer ; array_assignment ; array_access ; logical_or ; length ; allocVector ; protect]


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