open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

type label = string
[@@deriving show]

type const =
| CStr of string
| CDbl of string
| CInt of int
| CBool of bool
| CNull
| CNa
[@@deriving show]

type e' =
| Const of const
| Id of Variable.t
| Declare of Variable.t * e
| Let of Variable.t * e * e
| VarAssign of Variable.t * e
| Unop of Variable.t * e
| Binop of Variable.t * e * e
| Call of e * e list
| Ite of e * e * e
| While of e * e
(*| TyCheck of e * Types.Ty.t*) (* Test between an expression and a constant *)
| Function of Variable.t list * e
| Seq of e * e
| Return of e option | Break | Next
[@@deriving show]
and e = Eid.t * e'
[@@deriving show]

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