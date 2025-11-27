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
 | Any (* Do we actually need that?*)
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
| TyCheck of e * Ty.t (* Test between an expression and a constant *)
| Function of string * ctype * (ctype * Variable.t) list * e
| Seq of e * e
| Noop (* Hack for the declarations. We should rather use the declarations directly rather than doing the imprecise analysis of used variables in bv_e*)
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



let typeof_const c = 
  match c with 
  | CStr _ -> C.str
  | CDbl _ -> C.double
  | CInt v -> Ty.interval (Some (Z.of_int v)) (Some (Z.of_int v))
  | CBool v -> if v then C.not_zero else C.zero
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
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.If (cond, Ty.tt, aux_e then_, Option.map aux_e else_)
    | While (_cond, _body) -> failwith "While loops not supported yet"
    | Seq (e1,e2) -> A.Seq (aux_e e1, aux_e e2)
    | Return e -> A.Return (match e with 
        | None -> (Eid.unique (), A.Void)
        | Some e -> aux_e e)
    | TyCheck (e, ty) -> 
        let e = aux_e e in 
        let tt = Eid.unique (), A.Value (GTy.mk Ty.tt) in
        let ff = Eid.unique (), A.Value (GTy.mk Ty.ff) in
        A.Ite (e, ty, tt, ff)
    | Function (_name, _ret_type, params, body) ->
      (* Create lets in the body for each argument: match parameter names with
       type variable in the domain *)
      let arg_types = List.map 
        (function (typ, _) -> match typ with 
          SEXP -> TVar.typ (TVar.mk TVar.KInfer None) 
          | _ -> TVar.typ (TVar.mk TVar.KNoInfer None))
          params
      in  
      let add_let body (p, ty) = 
        Eid.unique(), A.Let ([], p, (Eid.unique (), A.Value (GTy.mk ty)), body)
      in
      let body = List.fold_left add_let (aux_e body) (List.combine (List.map snd params) arg_types) in
      (* Suggested type decomposition, domain, type variable, body*)
      A.Lambda ([], GTy.mk @@ Tuple.mk arg_types, 
      MVariable.create Immut None, body) 
    | Break -> A.Break
    | Next -> A.Break
    (* Lambda *)
    | _ -> A.Value (GTy.mk C.void)  (* Placeholder for other expressions *)
    
  in
  (eid, aux e)



  let map f e = 
    let rec aux (eid, e) = 
      let e = match e with 
      | Const _ | Id _ -> e
      | Declare (v,e) -> Declare (v, aux e)
      | Let (v,e1,e2) -> Let (v, aux e1, aux e2)
      | VarAssign (v,e2) -> VarAssign (v, aux e2)
      | Unop (op,e) -> Unop (op, aux e)
      | Binop (op,e1,e2) -> Binop (op, aux e1, aux e2)
      | Call (f,args) -> Call (aux f, List.map aux args)
      | If (cond, then_, else_) -> 
          If (aux cond, aux then_, Option.map aux else_)
      | While (cond, body) -> While (aux cond, aux body)
      | Seq (e1,e2) -> Seq (aux e1, aux e2)
      | Return e -> Return (Option.map aux e)
      | TyCheck (e, ty) -> TyCheck (aux e, ty)
      | Function (name, ret_type, params, body) ->
          Function (name, ret_type, params, aux body)
      | Break -> Break
      | Next -> Next
      | Noop -> Noop
    in
    f (eid, e)
  in aux e

  let recognize_const_comparison e = 
    let f = function 
    | id, (Binop (v, e, (_, Const c)) | Binop (v, (_, Const c), e))
    when Variable.equals v BuiltinOp.eq -> id, TyCheck (e, typeof_const c)
    | id, (Binop (v, e, (_, Const c)) | Binop (v, (_, Const c), e))
    when Variable.equals v BuiltinOp.neq -> id, TyCheck (e, Ty.neg (typeof_const c))
    | e -> e
    in
    map f e


  let to_mlsem e = 
    e |> recognize_const_comparison |> aux_e |> Mlsem.Lang.Transform.transform