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
| CDbl of float
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
 | Bool
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
| Ite of e * e * e (* For ternary conditions *)
| While of e * e
| TyCheck of e * Ty.t (* Test between an expression and a constant *)
| Cast of ctype * e (* Type cast expression *)
| Function of string * ctype * (ctype * Variable.t) list * e
| Switch of e * (e * e * bool) list (* expression on which to switch, and then case/default (Noop), their bodies and whether it had a break*)
| Seq of e * e
| Noop (* Hack for the declarations. We should rather use the declarations directly rather than doing the imprecise analysis of used variables in bv_e*)
| Return of e option | Break | Next
[@@deriving show]
and e = Eid.t * e'
[@@deriving show]

type funcs = e list (* list of function definitions *)
[@@deriving show]



let typeof_const c = 
  match c with 
  | CStr _ -> C.str
  | CDbl _ -> C.double
  | CInt v -> Ty.interval (Some (Z.of_int v)) (Some (Z.of_int v))
  | CBool v -> if v then C.one else C.zero
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
        A.If (cond, GTy.mk Ty.tt, aux_e then_, Option.map aux_e else_)
    | Ite (cond, then_, else_) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.Ite (cond, GTy.mk Ty.tt, aux_e then_, aux_e else_)
    | While (cond, body) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.While (cond, GTy.mk Ty.tt, aux_e body)
    | Seq (e1,e2) -> A.Seq (aux_e e1, aux_e e2)
    | Return e -> A.Return (match e with 
        | None -> (Eid.unique (), A.Void)
        | Some e -> aux_e e)
    | TyCheck (e, ty) -> 
        let e = aux_e e in 
        let tt = Eid.unique (), A.Value (GTy.mk Ty.tt) in
        let ff = Eid.unique (), A.Value (GTy.mk Ty.ff) in
        A.Ite (e, GTy.mk ty, tt, ff)
    | Cast (_ty, e) -> aux_e e |> snd (* For now, just return the expression without the cast
                               TODO: Handle actual type casting in MLsem *)
    | Function (_name, _ret_type, params, body) ->
      (* Create lets in the body for each argument: match parameter names with
       type variable in the domain *)
      let arg_types = List.map 
        (function _ -> 
          TVar.typ (TVar.mk KInfer None)) 
          params
      in  
      let add_let body (p, ty) = 
        Eid.unique(), A.Let ([], p, (Eid.unique (), A.Value (GTy.mk ty)), body)
      in
      let body = List.fold_left add_let (aux_e body) (List.combine (List.map snd params) arg_types) in
      (* Suggested type decomposition, domain, type variable, body*)
      A.Lambda ([], GTy.mk @@ Tuple.mk arg_types, 
      MVariable.create Immut None, body) 
    | Switch (e, cases) ->
        let e = aux_e e in
        let make_pattern_case acc (case_e, body_e, has_break) =
          let case_ty = match case_e with 
            | _,Const c -> typeof_const c 
            | _,Id v -> (match Defs.BuiltinVar.find_builtin v with 
                | Some ty -> ty
                | None -> failwith "Invalid switch case expression: unknown define")
            | _,Noop -> Ty.any (* Default case *)
            | _ -> failwith "Invalid switch case expression"
          in
          let body = aux_e body_e in
          if has_break || List.is_empty acc then 
            (A.PType case_ty, body) :: acc
          else 
            let _last_case, last_body = List.hd acc in
            (* if there are no break in the current case, then the current case will also execute the 
              body of the next case(s) until reaching a break, so we merge those bodies. *)
            let new_body = Eid.unique (), A.Seq (last_body, body) in
            (A.PType case_ty, new_body) :: acc
        in
        (* We start from the reverse list because we want to merge bodies of cases with the one 
        before when there is a break. So we start with the default case, then the one before
         and so on *)
        A.PatMatch (e, List.fold_left make_pattern_case [] (List.rev cases))
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
      | Ite (cond, then_, else_) ->
          Ite (aux cond, aux then_, aux else_)
      | While (cond, body) -> While (aux cond, aux body)
      | Seq (e1,e2) -> Seq (aux e1, aux e2)
      | Return e -> Return (Option.map aux e)
      | TyCheck (e, ty) -> TyCheck (aux e, ty)
      | Cast (ty, e) -> Cast (ty, aux e)
      | Function (name, ret_type, params, body) ->
          Function (name, ret_type, params, aux body)
      | Switch (e, cases) ->
          let cases = List.map (fun (case_e, body_e, has_break) -> (aux case_e, aux body_e, has_break)) cases in
          Switch (aux e, cases)
      | Break -> Break
      | Next -> Next
      | Noop -> Noop
    in
    f (eid, e)
  in aux e

  let recognize_const_comparison e = 
    let f expr = match expr with 
    | id, (Binop (op, e, (_, Const c)) | Binop (op, (_, Const c), e))
    when Variable.equal op Defs.BuiltinOp.eq -> id, TyCheck (e, typeof_const c)
    | id, (Binop (op, e, (_, Id v)) | Binop (op, (_, Id v), e))
    when Variable.equal op Defs.BuiltinOp.eq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, TyCheck (e, built)
      | None -> expr)
    | id, (Binop (op, e, (_, Const c)) | Binop (op, (_, Const c), e))
    when Variable.equal op Defs.BuiltinOp.neq -> id, TyCheck (e, Ty.neg (typeof_const c))
    | id, (Binop (op, e, (_, Id v)) | Binop (op, (_, Id v), e))
    when Variable.equal op Defs.BuiltinOp.neq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, TyCheck (e, Ty.neg built)
      | None -> expr)
    | e -> e
    in
    map f e


  let to_mlsem e = 
    e |> recognize_const_comparison |> aux_e |> Mlsem.Lang.Transform.transform