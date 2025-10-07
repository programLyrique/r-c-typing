open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

module Position = struct
  type t = Position.t
  let pp fmt pos = if pos = Position.dummy then Format.fprintf fmt "dummy"
        else  Format.fprintf fmt "(%d,%d)" 
    (Position.start_of_position pos).pos_lnum (Position.end_of_position pos).pos_lnum
end

type const =
 | CStr of string 
 | CFloat of string 
 | CInt of int 
 | CNull
 | CNa
 [@@deriving show]

type ctype = 
 | Void
 | Int
 | Float 
 | Char
 | Ptr of ctype
 | Array of ctype * int option
 | Struct of string * (ctype * string) list
 | Union of string * (ctype * string) list
 | Enum of string * (string * int option) list
 | Typedef of string
 (* SEXPs *)
 | SEXP
 | Any
 [@@deriving show]


 type top_level_unit' = 
  | Fundef of ctype * string * param list * e
  [@@deriving show]
 and top_level_unit = Position.t * top_level_unit'
  [@@deriving show]
 and e' = 
  | Const of const 
  | Id of string
  | Unop of string * e
  | Binop of string * (e * e) 
  | Call of e * e list 
  | Ite of e * e * e option
  | Return of e option
  | Break
  | Next
  | Seq of e list
  | Comma of e * e
  [@@deriving show]
 and param = ctype * string
  [@@deriving show]
 and e = Position.t * e'
  [@@deriving show]


type definitions = top_level_unit list
[@@deriving show]

module StrMap = Map.Make(String)
type env = { id: Variable.t StrMap.t }

let var env str = 
  match StrMap.find_opt str env.id with 
 | None ->
    begin match Ast.BuiltinOp.find_builtin str with
    | None -> MVariable.create Immut (Some str)
    | Some v -> v
    end
  | Some v -> v

let add_var env str =
  let v = MVariable.create MVariable.Mut (Some str) in
  StrMap.add str v env

let aux_const c = 
  match c with 
  | CStr s -> Ast.CStr s 
  | CFloat s -> Ast.CDbl s
  | CInt i -> Ast.CInt i
  | CNull -> Ast.CNull
  | CNa -> Ast.CNa


