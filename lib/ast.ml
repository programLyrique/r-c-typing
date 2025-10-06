open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

type label = string
[@@deriving show]

type const =
| CChr of string
| CDbl of string
| CLgl of bool
| CNull
[@@deriving show]

type e' =
| Const of const
| Id of Variable.t
| Declare of Variable.t * e
| Let of Variable.t * e * e
| VarAssign of Variable.t * e
| Unop of Variable.t * e
| Binop of Variable.t * e * e
| Call of e * arg list
| Ite of e * e * e
| While of e * e
(*| TyCheck of e * Types.Ty.t*) (* Test between an expression and a constant *)
| Function of param list * e
| Seq of e * e
| Return of e option | Break | Next
[@@deriving show]
and arg = arg_label * e
[@@deriving show]
and arg_label = Positional | Named of label
[@@deriving show]
and param = NoDefault of Variable.t | Default of Variable.t * e | Ellipsis
[@@deriving show]
and e = Eid.t * e'
[@@deriving show]