open Mlsem.Common

module Position = struct
  type t = Position.t
  let pp fmt _ = Format.fprintf fmt "_"
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
  | Seq of e list
  | Comma of e * e
  [@@deriving show]
 and param = ctype * string
  [@@deriving show]
 and e = Position.t * e'
  [@@deriving show]


type definitions = top_level_unit list
[@@deriving show]