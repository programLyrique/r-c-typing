(** This is to type functions that use the .C convention.
See https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Interface-functions-_002eC-and-_002eFortran 

We directly type from PAst. We only need to look at the parameters and return type.
*)

open Mlsem.Types
open PAst

let mk_C_ty fundef = 
  match fundef with
  | _pos, Fundef (return_ty, _name, params, _body) ->
      let args = List.map (fun (ty, _) -> Ast.typeof_ctype ty) params in
      let arg_tuple = Tuple.mk args in
      let ret_ty = Ast.typeof_ctype return_ty in
      Arrow.mk arg_tuple ret_ty
  | _pos, Struct _ | _pos, Define _ -> failwith "mk_C_ty expects a function definition"

