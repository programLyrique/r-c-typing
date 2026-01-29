open Mlsem.Common
open Mlsem.Types
module MVariable = Mlsem.Lang.MVariable
module A = Mlsem.Lang.Ast
module SA = Mlsem.System.Ast

type label = string
[@@deriving show]

type const =
| CChar of char
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

  (*level is the number of pointer indirections*)
  let rec build_ptr level ty =
    if level <= 0 then ty
    else build_ptr (level - 1) (Ptr ty)

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
  let open Rstt in 
  match c with 
  | CChar _ ->  Cenums.char
  | CStr _ -> Cenums.str
  | CDbl _ -> Cenums.double
  | CInt v -> Cint.singl v
  | CBool v -> if v then Cint.tt else Cint.ff
  | CNull -> Null.any
  | CNa -> Cint.na

let rec typeof_ctype ct = 
  let open Rstt in 
  match ct with 
  | Void -> Cenums.void
  | Int -> Cint.any_na
  | Float -> Cenums.double
  | Char -> Cenums.char
  | Bool -> Cint.bool
  | Ptr ty -> Cptr.mk (typeof_ctype ty)
  | SEXP -> Defs.any_sexp
  | Any -> Defs.any_c
  | _ -> failwith ("Type not supported yet in typeof_ctype: " ^ show_ctype ct)


module AttrProj = struct
  let pdom ty = Rstt.Attr.mk_anyclass ty
  let proj ty = Rstt.Attr.proj_content ty
end

module AttrConstr = struct
  open Rstt
  let cons classes tys =
    match tys with
    | [ty] -> Attr.mk { content=ty ; classes }
    | _ -> assert false
  let cdom classes ty =
    try
      Attr.destruct ty
      |> List.filter_map (fun (ps,_) ->
        let content = ps |> List.map (fun a -> a.Attr.content) |> Ty.conj in
        let ty' = Attr.mk { content ; classes } in
        if Ty.leq ty' ty then Some [content] else None
      )
    with Invalid_argument _ -> []
end


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
        (* Handle the attributes *)
        let f = Eid.unique (), A.Projection
        (PCustom { pname="pfun" ; pgen=true ; pdom=AttrProj.pdom ; proj=AttrProj.proj }, aux_e f) in
        A.App (f, args)
    | If (cond, then_, else_) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.If (cond, GTy.mk Rstt.Cint.tt, aux_e then_, Option.map aux_e else_)
    | Ite (cond, then_, else_) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.Ite (cond, GTy.mk Rstt.Cint.tt, aux_e then_, aux_e else_)
    | While (cond, body) -> 
        let cond = aux_e cond in
        let cond = (Eid.unique (), (A.App ((Eid.unique (), A.Var Defs.tobool), cond))) in
        A.While (cond, GTy.mk Rstt.Cint.tt, aux_e body)
    | Seq (e1,e2) -> A.Seq (aux_e e1, aux_e e2)
    | Return e -> A.Return (match e with 
        | None -> (Eid.unique (), A.Void)
        | Some e -> aux_e e)
    | TyCheck (e, ty) -> 
        let e = aux_e e in 
        let tt = Eid.unique (), A.Value (GTy.mk Rstt.Cint.tt) in
        let ff = Eid.unique (), A.Value (GTy.mk Rstt.Cint.ff) in
        A.Ite (e, GTy.mk ty, tt, ff)
    | Cast (ty, e) -> let e = aux_e e in 
        A.TypeCoerce (e, typeof_ctype ty |> GTy.mk , SA.NoCheck)
    | Function (_name, _ret_type, params, body) ->
      (* Create lets in the body for each argument: match parameter names with
       type variable in the domain *)
      let arg_types = List.map 
        (function (cty, _) -> 
          (* Special case of SEXPs. we coul restrict it to actual SEXP but
           typeof_ctype currently does not return all possible sexp types for SEXP. *)
          match cty with 
          | SEXP -> Ty.cap Rstt.Attr.any (TVar.typ (TVar.mk KInfer None))
          | _ -> Ty.cap (typeof_ctype cty) (TVar.typ (TVar.mk KInfer None))
        )  params
      in  
      let add_let body (p, ty) = 
        Eid.unique(), A.Let ([], p, (Eid.unique (), A.Value (GTy.mk ty)), body)
      in
      let body = List.fold_left add_let (aux_e body) (List.combine (List.map snd params) arg_types) in
      (* Suggested type decomposition, domain, type variable, body*)
      let lambda = A.Lambda ([], GTy.mk @@ Tuple.mk arg_types, 
        MVariable.create Immut None, body) in
        (* we could give a more precise type for the attributes: noclass But then it displays <> after the closures, which is
          too much noise! *)
        A.Constructor
          (CCustom { cname="cattr" ; cgen=true ; cdom=AttrConstr.cdom Rstt.Classes.any ; cons=AttrConstr.cons Rstt.Classes.any },
           [Eid.unique(), lambda])
    | Switch (e, cases) ->
        let e = aux_e e in
        let make_pattern_case acc (case_e, body_e, has_break) =
          let case_ty = match case_e with 
            | _,Const c -> typeof_const c 
            | _,Id v -> (match Defs.BuiltinVar.find_builtin v with 
                | Some ty -> ty
                | None -> failwith ("Invalid switch case expression: unknown define " ^ (Variable.get_unique_name v)))
            | _,Noop -> Ty.any (* Default case *)
            | _ -> failwith "Invalid switch case expression"
          in
          let body = aux_e body_e in
          if has_break || List.is_empty acc then 
            (A.PType case_ty, body) :: acc
          else 
            let last_case, last_body = List.hd acc in
            (* 2  when there are no breaks:
              - current body is empty (i.e. a const NULL in the body) : we can merge ( = or) the patterns  
              - current body is not empty: current case will also execute the 
              body of the next case(s) until reaching a break, so we merge those bodies.
              Note: the empty body optimization does not seem to bring much in terms of perf. 
              A better optimization might be to encode it as a series of if, else if else. *)
             match body_e with 
            | _,Const CNull -> 
              (A.POr (A.PType case_ty, last_case), last_body) :: (List.tl acc)
            | _ -> let new_body = Eid.unique (), A.Seq (last_body, body) in 
              (A.PType case_ty, new_body) :: acc
        in
        (* We start from the reverse list because we want to merge bodies of cases with the one 
        before when there is a break. So we start with the default case, then the one before
         and so on *)
        A.PatMatch (e, List.fold_left make_pattern_case [] (List.rev cases))
    | Break -> A.Break
    | Next -> A.Break
    (* Lambda *)
    | _ -> A.Value (GTy.mk Rstt.Cenums.void)  (* Placeholder for other expressions *)
    
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
    | id, (Binop (op, e, (_, Const (CInt c))) | Binop (op, (_, Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.inf_strict -> 
      id, TyCheck (e, Rstt.Cint.interval (None, Some (c-1))) (* interval is closed by < is open on the right*)
    | id, (Binop (op, e, (_, Const (CInt c))) | Binop (op, (_, Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.sup_strict -> 
      id, TyCheck (e, Rstt.Cint.interval (Some (c+1), None)) (* interval is closed by > is open on the left*)
    | e -> e
    in
    map f e


  let to_mlsem e = 
    e |> recognize_const_comparison |> aux_e |> Mlsem.Lang.Transform.transform