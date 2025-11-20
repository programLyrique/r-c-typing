open Mlsem.Common
open Mlsem.Types
open Vectors
module MVariable = Mlsem.Lang.MVariable
module A = Mlsem.Lang.Ast
module SA = Mlsem.System.Ast

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

type e' =
| Const of const
| Id of Variable.t
| Declare of Variable.t * e
| Let of Variable.t * e * e
| VarAssign of Variable.t * e
| Unop of Variable.t * e
| Binop of Variable.t * e * e
| Call of e * e list
| If of e * e * e option
| While of e * e
(*| TyCheck of e * Types.Ty.t (* Test between an expression and a constant *)*)
| Function of ctype * (ctype * Variable.t) list * e
| Seq of e * e
| Noop (* Hack for the declarations*)
| Return of e option | Break | Next
[@@deriving show]
and e = Eid.t * e'
[@@deriving show]

type funcs = e list (* list of function definitions *)
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

let tobool, tobool_t =
  let v = MVariable.create Immut (Some "tobool") in
  let def = Arrow.mk Ty.any Ty.bool in
  let tt = Arrow.mk (Ty.disj [Prim.tt;Vecs.mk_singl Prim.tt]) Ty.tt in
  let ff = Arrow.mk (Ty.disj [Prim.ff;Vecs.mk_singl Prim.ff]) Ty.ff in
  let ty = Ty.conj [def;tt;ff] in
  v, ty

let typeof_const c = 
  match c with 
  | CStr _ -> C.str
  | CDbl _ -> C.double
  | CInt _ -> C.int
  | CBool _ -> C.int
  | CNull -> Null.null
  | CNa -> Prim.na

(* Transformation to MLsem ast
  GTy is used for gradual types, Ty for "normal" types
*)
let rec aux_e (eid, e) =
  let rec aux e = 
    match e with 
    | Const c -> A.Value (typeof_const c |> GTy.mk )
    | Id v -> A.Var v 
    | Declare (v,e) -> A.Declare (v, aux_e e)
    | Let (v,e1,e2) -> A.Let ([], v, aux_e e1, aux_e e2)
    | VarAssign (v,e2) -> A.VarAssign (v, aux_e e2)
    | Unop (op,e) -> aux (Call ((Eid.unique (), Id op), [e]))
    | Binop (op,e1,e2) -> aux (Call ((Eid.unique (), Id op), [e1; e2]))
    | Call (f,args) -> 
        let es = List.map aux_e args in 
        (* State how arguments of a function are encoded: here, they are in a tuple*)
        let args = (Eid.unique (), A.Constructor 
          (SA.Tuple (List.length es), es)) in
        A.App (aux_e f, args)
    | If (cond, then_, else_) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var tobool), cond))) in
        A.If (cond, Ty.tt, aux_e then_, Option.map aux_e else_)
    | While (_cond, _body) -> failwith "While loops not supported yet"
    | Seq (e1,e2) -> A.Seq (aux_e e1, aux_e e2)
    | Return e -> A.Return (match e with 
        | None -> (Eid.unique (), A.Void)
        | Some e -> aux_e e)
    | Function (_ret_type, params, body) ->
      (*TODO: add projection for the tuples representing the arguments*)

      (* Suggested type decomposition, domain, type variable, body*)
      A.Lambda ([], GTy.mk @@ Tuple.mk (List.map (function (typ, _) -> match typ with SEXP -> TVar.typ (TVar.mk TVar.KInfer None) | _ -> TVar.typ (TVar.mk TVar.KNoInfer None)) params), 
      MVariable.create Immut None, aux_e body) 
    | Break -> A.Break
    | Next -> A.Break
    (* Lambda *)
    | _ -> A.Value (GTy.mk C.void)  (* Placeholder for other expressions *)
    
  in
  (eid, aux e)

  let to_mlsem e = 
    e |> aux_e |> Mlsem.Lang.Transform.transform