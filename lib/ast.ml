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
| CArray of const list
[@@deriving show]


(* Names to build names for a list with mkNamed look like {"a", "b", ""}*)
let rec extract_names l = 
  match l with 
  | [CStr ""] -> []
  | (CStr s)::s2::l -> s :: (extract_names (s2::l))
  | [CStr _ ] -> failwith "expected \"\" at the end "
  | _ -> failwith "expected string constant"

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

module VarMap = struct
  include Map.Make(Variable)
  let pp(pp_v : Format.formatter -> 'v -> unit)
      (fmt : Format.formatter)
      (m : 'v t) : unit =
    let pp_binding fmt (k, v) =
      Format.fprintf fmt "%a -> %a" Variable.pp k pp_v v
    in
    Format.fprintf fmt "{@[<hov 1>%a@]}"
      (Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
        pp_binding)
      (bindings m)
  end


type kind = 
  | C of const 
  | L of string list (* list of possible labels *)
[@@deriving show]


let add_const m v c = 
  VarMap.add v (C c) m

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
and e = Eid.t * (kind VarMap.t) * e'
[@@deriving show]

type funcs = e list (* list of function definitions *)
[@@deriving show]


let rec typeof_const c = 
  let open Rstt in 
  match c with 
  | CChar _ ->  Cenums.char
  | CStr _ -> Cenums.str
  | CDbl _ -> Cenums.double
  | CInt v -> Cint.singl v
  | CBool v -> if v then Cint.tt else Cint.ff
  | CNull -> Null.any
  | CNa -> Cint.na
  | CArray [] -> Cptr.mk Defs.any_c
  | CArray (elem::_) -> Cptr.mk (typeof_const elem)


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

let build_call f args =
  (* State how arguments of a function are encoded: here, they are in a tuple*)
   let args = (Eid.unique (), A.Constructor 
      (SA.Tuple (List.length args), args)) in
      (* Handle the attributes *)
      let f = Eid.unique (), A.Projection
      (PCustom { pname="pfun" ; pgen=true ; pdom=AttrProj.pdom ; proj=AttrProj.proj }, f) in
      A.App (f, args)

(* Transformation to MLsem ast
  GTy is used for gradual types, Ty for "normal" types
*)
let rec aux_e (eid, vars, e) =
  let rec aux e = 
    match e with 
    | Const c -> A.Value (typeof_const c |> GTy.mk )
    | Id v -> A.Var v
    | Declare (v,e) -> A.Declare (v, aux_e e)
    | Let (v,e1,e2) -> A.Let ([], v, aux_e e1, aux_e e2)
    | VarAssign (v,e2) -> A.VarAssign (v, aux_e e2)
    | Unop (op,e) -> aux (Call ((Eid.unique (), vars, Id op), [e]))
    | Binop (op,e1,e2) -> aux (Call ((Eid.unique (), vars, Id op), [e1; e2]))
    (* Some special cases for some calls*)
    | Call ((_,_, Id v), [(_, _,Id vty); ( _ ,_, Const (CInt n)) ]) 
      when (Variable.get_name v = Some "allocVector") && 
           (Variable.get_name vty = Some "VECSXP") ->
        let ty = Defs.allocVector_vecsxp_ty n in
        A.Value (GTy.mk ty)
    | Call ((_,_,Id v1), [(_, _,Id vty); ( _ ,_, Id v2) ]) 
      when (Variable.get_name v1 = Some "mkNamed") && 
           (Variable.get_name vty = Some "VECSXP") ->
        Format.eprintf "%a@." (VarMap.pp pp_kind) vars;
        let kind = VarMap.find_opt v2 vars in 
        (* print the content of vars *)
        let names = match kind with 
            | None -> failwith (Format.asprintf "No variable %a for names for mkNamed" Variable.pp v2)
            | Some (C (CArray names)) -> extract_names names
            | Some _ -> failwith "Expected an array of strings."
           in
        let ty = Defs.mkNamed_vecsxp_ty names in
        A.Value (GTy.mk ty)
    | Call ((eid,_,Id v1), [(_, _,Id v2) as id2; ( _ ,_, idx); value]) 
      when (Variable.get_name v1 = Some "SET_VECTOR_ELT") ->
        let kind = VarMap.find_opt v2 vars in 
        let idx = match idx with 
          | Const (CInt n) -> n
          | Id v -> (match VarMap.find_opt v vars with 
              | Some (C (CInt n)) -> n
              | _ -> failwith "Expected an integer constant for the index in SET_VECTOR_ELT") 
          | _ -> failwith "Expected an integer constant for the index in SET_VECTOR_ELT"
        in
        let name = match kind with 
            | Some (L names) -> List.nth names idx
            | Some _ -> failwith (Format.asprintf "%a should be a list" Variable.pp v2)
            | None -> failwith (Format.asprintf "No variable %a for the 1st parameter of SET_VECTOR_ELT" Variable.pp v2)
           in
        let f = Defs.set_vector_elt_ty name in
       build_call (eid, A.Value (GTy.mk f)) [aux_e id2; aux_e value]
    (* General case for calls *)
    | Call (f,args) -> 
        let es = List.map aux_e args in 
        let f = aux_e f in 
        build_call f es
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
            | _,_,Const c -> typeof_const c 
            | _,_,Id v -> (match Defs.BuiltinVar.find_builtin v with 
                | Some ty -> ty
                | None -> failwith ("Invalid switch case expression: unknown define " ^ (Variable.get_unique_name v)))
            | _,_,Noop -> Ty.any (* Default case *)
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
            | _,_,Const CNull -> 
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
    let rec aux (eid, vars, e) = 
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
    f (eid, vars, e)
  in aux e


  let recognize_const_comparison e =
    let f expr = match expr with 
    | id, vars, (Binop (op, e, (_,_,Const c)) | Binop (op, (_,_,Const c), e))
    when Variable.equal op Defs.BuiltinOp.eq -> id, vars, TyCheck (e, typeof_const c)
    | id, vars, (Binop (op, e, (_,_,Id v)) | Binop (op, (_,_,Id v), e))
    when Variable.equal op Defs.BuiltinOp.eq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, vars, TyCheck (e, built)
      | None -> expr)
    | id, vars, (Binop (op, e, (_,_,Const c)) | Binop (op, (_,_,Const c), e))
    when Variable.equal op Defs.BuiltinOp.neq -> id, vars, TyCheck (e, Ty.neg (typeof_const c))
    | id, vars, (Binop (op, e, (_,_,Id v)) | Binop (op, (_,_,Id v), e))
    when Variable.equal op Defs.BuiltinOp.neq -> 
      (match Defs.BuiltinVar.find_builtin v with 
      | Some built -> id, vars, TyCheck (e, Ty.neg built)
      | None -> expr)
    | id, vars, (Binop (op, e, (_, _,Const (CInt c))) | Binop (op, (_, _,Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.inf_strict -> 
      id, vars, TyCheck (e, Rstt.Cint.interval (None, Some (c-1))) (* interval is closed by < is open on the right*)
    | id, vars, (Binop (op, e, (_, _,Const (CInt c))) | Binop (op, (_, _,Const (CInt c)), e))
    when Variable.equal op Defs.BuiltinOp.sup_strict -> 
      id, vars, TyCheck (e, Rstt.Cint.interval (Some (c+1), None)) (* interval is closed by > is open on the left*)
    | e -> e
    in
    map f e

  let propagate_const e =
    (* Traverse the AST while carrying an environment of known constants.
       Each node gets its `vars` field updated to the current constant env. *)

    (* Extract mkNamed names from the environment if possible. *)
    let names_of_var (env : kind VarMap.t) (v : Variable.t) : string list option =
      match VarMap.find_opt v env with
      | Some (C (CArray names)) -> Some (extract_names names)
      | _ -> None
    in

    (* Detect `mkNamed(VECSXP, names)` possibly wrapped in `PROTECT(...)`.
       Returns the variable holding the names array when matched. *)
    let mkNamed_names_arg (e : e) : Variable.t option =
      let unwrap ((_, _, expr) : e) : e' =
        match expr with
        | Call ((_, _, Id f), [arg]) when Variable.get_name f = Some "PROTECT" ->
            let (_, _, inner) = arg in
            inner
        | _ -> expr
      in
      match unwrap e with
      | Call ((_, _, Id f), [(_, _, Id vty); (_, _, Id vnames)])
        when Variable.get_name f = Some "mkNamed"
             && Variable.get_name vty = Some "VECSXP" ->
          Some vnames
      | _ -> None
    in

    let unop_const (op : Variable.t) (c : const) : const option =
      match Variable.get_name op, c with
      | Some "-__1", CInt n -> Some (CInt (-n))
      | _ -> None
    in

    let binop_const (op : Variable.t) (c1 : const) (c2 : const) : const option =
      match Variable.get_name op, c1, c2 with
      (* Int arithmetic *)
      | Some "+__2", CInt a, CInt b -> Some (CInt (a + b))
      | Some "-__2", CInt a, CInt b -> Some (CInt (a - b))
      | Some "*__2", CInt a, CInt b -> Some (CInt (a * b))
      | Some "/__2", CInt a, CInt b -> if b = 0 then None else Some (CInt (a / b))
      | Some "%__2", CInt a, CInt b -> if b = 0 then None else Some (CInt (a mod b))
      | _ -> None
    in

    let const_of_e (env : kind VarMap.t) ((_, _, expr) : e) : const option =
      match expr with
      | Const c -> Some c
      | Id v ->
          (match VarMap.find_opt v env with
          | Some (C c) -> Some c
          | _ -> None)
      | _ -> None
    in

    let rec aux ((id, _vars, expr) : e) (env : kind VarMap.t) : e * kind VarMap.t =
      match expr with
      | Const _ | Id _ | Noop | Break | Next -> ((id, env, expr), env)

      | Declare (v, e1) ->
          let e1', env' = aux e1 env in
          ((id, env, Declare (v, e1')), env')

      | Let (v, e1, e2) ->
          let e1', env1 = aux e1 env in
          let env_for_body =
            match const_of_e env1 e1' with
            | Some c -> add_const env1 v c
            | None -> env1
          in
          let e2', env2 = aux e2 env_for_body in
          ((id, env, Let (v, e1', e2')), env2)

      | VarAssign (v, e1) ->
          let e1', env1 = aux e1 env in
          (* Special case: v = mkNamed(VECSXP, names) (possibly wrapped in PROTECT)
             => v is a vector with label names *)
          let env' =
            match mkNamed_names_arg e1' with
            | Some vnames ->
                (match names_of_var env1 vnames with
                | Some names -> VarMap.add v (L names) env1
                | None -> env1)
            | None ->
                (match const_of_e env1 e1' with
                | Some c -> add_const env1 v c
                | None -> env1)
          in
          ((id, env, VarAssign (v, e1')), env')

      | Unop (op, e1) ->
          let e1', env1 = aux e1 env in
          let expr' =
            match const_of_e env1 e1' with
            | Some c ->
                (match unop_const op c with
                | Some c' -> Const c'
                | None -> Unop (op, e1'))
            | None -> Unop (op, e1')
          in
          ((id, env, expr'), env1)

      | Binop (op, e1, e2) ->
          let e1', env1 = aux e1 env in
          let e2', env2 = aux e2 env1 in
          let expr' =
            match (const_of_e env2 e1', const_of_e env2 e2') with
            | Some c1, Some c2 ->
                (match binop_const op c1 c2 with
                | Some c' -> Const c'
                | None -> Binop (op, e1', e2'))
            | _ -> Binop (op, e1', e2')
          in
          ((id, env, expr'), env2)

      | Call (f, args) ->
          let f', env1 = aux f env in
          let args', env2 =
            List.fold_left
              (fun (acc_args, acc_env) a ->
                let a', acc_env' = aux a acc_env in
                (acc_args @ [ a' ], acc_env'))
              ([], env1)
              args
          in
          (* No binding target here, but keep env updated by traversing args. *)
          ((id, env, Call (f', args')), env2)

      | If (cond, then_, else_) ->
          let cond', env1 = aux cond env in
          let then_', env2 = aux then_ env1 in
          let else_', env3 =
            match else_ with
            | None -> (None, env2)
            | Some e3 ->
                let e3', env3 = aux e3 env2 in
                (Some e3', env3)
          in
          ((id, env, If (cond', then_', else_')), env3)

      | Ite (cond, then_, else_) ->
          let cond', env1 = aux cond env in
          let then_', env2 = aux then_ env1 in
          let else_', env3 = aux else_ env2 in
          ((id, env, Ite (cond', then_', else_')), env3)

      | While (cond, body) ->
          let cond', env1 = aux cond env in
          let body', env2 = aux body env1 in
          ((id, env, While (cond', body')), env2)

      | Seq (e1, e2) ->
          let e1', env1 = aux e1 env in
          let e2', env2 = aux e2 env1 in
          ((id, env, Seq (e1', e2')), env2)

      | Return eo ->
          let eo', env' =
            match eo with
            | None -> (None, env)
            | Some e1 ->
                let e1', env' = aux e1 env in
                (Some e1', env')
          in
          ((id, env, Return eo'), env')

      | TyCheck (e1, ty) ->
          let e1', env1 = aux e1 env in
          ((id, env, TyCheck (e1', ty)), env1)

      | Cast (ty, e1) ->
          let e1', env1 = aux e1 env in
          ((id, env, Cast (ty, e1')), env1)

      | Function (name, ret_type, params, body) ->
          (* Do not propagate outer constants into function bodies. *)
          let body', _ = aux body VarMap.empty in
          ((id, env, Function (name, ret_type, params, body')), env)

      | Switch (e1, cases) ->
          let e1', env1 = aux e1 env in
          let cases', env' =
            List.fold_left
              (fun (acc_cases, acc_env) (case_e, body_e, has_break) ->
                let case_e', envc = aux case_e acc_env in
                let body_e', envb = aux body_e envc in
                (acc_cases @ [ (case_e', body_e', has_break) ], envb))
              ([], env1)
              cases
          in
          ((id, env, Switch (e1', cases')), env')
    in
    fst (aux e VarMap.empty)

  let to_mlsem e = 
    e |> recognize_const_comparison |> propagate_const |> aux_e |> Mlsem.Lang.Transform.transform