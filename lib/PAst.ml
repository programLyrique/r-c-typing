open Mlsem.Common
module MVariable = Mlsem.Lang.MVariable

module Position = struct
  type t = Position.t
  let pp fmt pos = if pos = Position.dummy then Format.fprintf fmt "dummy"
        else  
          let start = Position.start_of_position pos in 
          let end_ = Position.end_of_position pos in
          Format.fprintf fmt "(%d,%d)-(%d,%d)" 
          start.pos_lnum (start.pos_cnum - start.pos_bol) end_.pos_lnum (end_.pos_cnum - start.pos_bol)
end

type const =
 | CStr of string 
 | CChar of char
 | CFloat of float 
 | CInt of int 
 | CBool of bool
 | CNull
 | CNa
 | CArray of const list
 [@@deriving show]


 type top_level_unit' = 
  | Fundef of Ast.ctype * string * param list * e
  [@@deriving show]
 and top_level_unit = Position.t * top_level_unit'
  [@@deriving show]
 and e' = 
  | Const of const 
  | Id of string
  | Unop of string * e
  | Binop of string * (e * e) 
  | VarDeclare of Ast.ctype * e
  | VarAssign of e * e
  | Call of e * e list 
  | If of e * e * e option
  | Ite of e * e * e (* For ternary conditions *)
  | While of e * e
  | For of e * e option * e option * e (*init, condition, incr, body *)
  | Return of e option
  | Break
  | Next
  | Seq of e list
  | Comma of e * e
  | Case of e * e
  | Default of e
  | Switch of e * e list  (* expression on which to switch, list of cases/default *)
  | Cast of Ast.ctype * e (* Type cast expression *)
  [@@deriving show]
 and param = Ast.ctype * string
  [@@deriving show]
 and e = Position.t * e'
  [@@deriving show]


type definitions = top_level_unit list
[@@deriving show]

module StrSet = Set.Make(String)

(** Extract parameter names from a function definition 
  Types will be added back to the Ast.Function. This is just to check if some variables 
  in the body are defined as parameters. *)
let bv_params params = 
  List.map snd params |> StrSet.of_list

(** Extract variables from an expression*)
let rec bv_e in_lhs_assign (_,e) = 
  match e with 
  | Break | Next  | Const _ -> StrSet.empty
  | Id s when in_lhs_assign -> StrSet.singleton s
  | Id _ -> StrSet.empty
  | Unop (_, e) -> bv_e in_lhs_assign e
  | Binop (_, (e1,e2)) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | VarAssign ((_,Id s), e2) -> (StrSet.singleton s) |> StrSet.union (bv_e in_lhs_assign e2)
  | VarAssign (e,_) -> bv_e true e (* Gross overapproximation! Left side of assignment, we want to collect identifiers in the lhs. Args will be detected as been "assigned". *)
  | Call (f, args) -> List.fold_left (fun acc arg -> StrSet.union acc (bv_e in_lhs_assign arg)) (bv_e  false f) args
  | If (cond, then_, else_) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign then_) in
      begin match else_ with 
      | None -> acc
      | Some e -> StrSet.union acc (bv_e in_lhs_assign e)
      end
  | Ite (cond, then_, else_) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign then_) in
      StrSet.union acc (bv_e in_lhs_assign else_)
  | While (cond, body) -> 
      let acc = bv_e in_lhs_assign cond |> StrSet.union (bv_e in_lhs_assign body) in
      acc
  | For (init, cond, incr, body) ->
      let acc = bv_e in_lhs_assign init |> StrSet.union (match cond with 
        | None -> StrSet.empty
        | Some e -> bv_e in_lhs_assign e) in
      let acc = StrSet.union acc (match incr with 
        | None -> StrSet.empty
        | Some e -> bv_e in_lhs_assign e) in
      StrSet.union acc (bv_e in_lhs_assign body)
  | Return None -> StrSet.empty
  | Return (Some e) -> bv_e in_lhs_assign e
  | Case (e1, e2) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | Default e -> bv_e in_lhs_assign e
  | Switch (e, cases) ->
      let acc = bv_e in_lhs_assign e in
      List.fold_left (fun acc case -> StrSet.union acc (bv_e in_lhs_assign case)) acc cases
  | Seq exprs -> List.fold_left (fun acc e -> StrSet.union acc (bv_e in_lhs_assign e)) StrSet.empty exprs
  | Comma (e1, e2) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | Cast (_, e) -> bv_e in_lhs_assign e
  | VarDeclare (_, (_, Id s)) -> StrSet.singleton s
  | VarDeclare (_, _) -> failwith "Declaration must have an identifier" (*Should be unreachable*)

module StrMap = Map.Make(String)
type env = { id: Variable.t StrMap.t }

let var env str = 
  match StrMap.find_opt str env.id with 
 | None ->
    begin match Defs.BuiltinOp.find_builtin str with
    | None -> (
      match Defs.StrMap.find_opt str Defs.defs_map with 
      | None -> (Printf.printf "Creating fresh variable: %s\n" str; MVariable.create Immut (Some str))
      | Some v -> v
    )
    | Some v -> v
    end
  | Some v -> v

let add_var env str =
  let v = MVariable.create MVariable.Mut (Some str) in
  StrMap.add str v env


(* Check if variables in the body are defined as parameters.
  If yes, we create a fresh variable with let that gets the param. 
  pid: parameters 
  eid : variables
  e: expression to add after (let v = param in e)
  str: name of the variable to look at 
  TODO: we already have explicit declarations in C so we should rather use them instead of detecting variables *)
let add_def pid eid e str =
  let v = StrMap.find str eid in
  match StrMap.find_opt str pid with
  | None -> Eid.unique (), Ast.VarMap.empty, Ast.Declare (v, e)
  | _ -> e

let rec aux_const c = 
  match c with 
  | CChar c -> Ast.CChar c
  | CStr s -> Ast.CStr s 
  | CFloat s -> Ast.CDbl s
  | CInt i -> Ast.CInt i
  | CNull -> Ast.CNull
  | CNa -> Ast.CNa
  | CBool b -> Ast.CBool b
  | CArray lst -> Ast.CArray (List.map aux_const lst)



let rec aux_e env (pos,e) = 
  let eid = Eid.unique_with_pos pos in
  let e = match e with 
  | Const c -> Ast.Const (aux_const c)
  | Id s -> Ast.Id (var env s)
  | Unop (op, e) -> Ast.Unop (var env (op ^ "__1"), aux_e env e)
  | Binop (op, (e1,e2)) -> Ast.Binop (var env (op ^ "__2"), aux_e env e1, aux_e env e2)
  | VarAssign ((_,Id s), e2) -> Ast.VarAssign (var env s, aux_e env e2)
  | VarAssign ((loc1, Call ((_,Id "[]"), args)) ,e2) -> 
      Ast.Call (
        aux_e env (loc1, Id "[]<-"),
        (List.map (aux_e env) args) @ [aux_e env e2]
      )
  | VarAssign ((loc1, Unop (_op, e1)) ,e2) -> (* Currently, remove the * or & operator *)
     let _,_,e = aux_e env (loc1, VarAssign(e1, e2)) in e
  | VarAssign (_,_) -> failwith ("Unexpected left-hand side in assignment. Got: " ^ show_e (pos,e))
  | Call (f, args) -> process_call env f args
  | If (cond, then_, else_) -> 
      Ast.If (aux_e env cond, aux_e env then_, Option.map (aux_e env) else_)
  | Ite (cond, then_, else_) -> 
      Ast.Ite (aux_e env cond, aux_e env then_, aux_e env else_) 
  | While (cond, body) -> Ast.While (aux_e env cond, aux_e env body)
  | For (init, cond, incr, body) -> 
      (* Transform For into While *)
      let init_e = aux_e env init in
      let cond_e = match cond with 
        | None -> (Eid.unique (), Ast.VarMap.empty, Ast.Const (Ast.CBool true))
        | Some e -> aux_e env e
      in
      let incr_e = match incr with 
        | None -> (Eid.unique (), Ast.VarMap.empty, Ast.Const (Ast.CNull))
        | Some e -> aux_e env e
      in
      let while_body = 
        let body_e = aux_e env body in
        let seq_e = Eid.unique (), Ast.VarMap.empty, Ast.Seq (body_e, incr_e) in
        seq_e
      in
      let while_e = Eid.unique (), Ast.VarMap.empty, Ast.While (cond_e, while_body) in
      Ast.Seq (init_e, while_e)
  | Return None -> Ast.Return None
  | Return (Some e) -> Ast.Return (Some (aux_e env e))
  | Break -> Ast.Break
  | Next -> Ast.Next
  | Switch  (e, cases) -> 
      (*Detects break and return; remove break*)
      let remove_break body =
        match body with 
        | Seq sts -> 
            let has_break_or_return, sts = List.fold_left (fun (had_break, acc) stmt ->
              if had_break then (true, acc)
              else match stmt with 
              | _, Break -> (true, acc)
              | _, Return _ -> (true, acc @ [stmt])
              | _ -> (false, acc @ [stmt])
            ) (false, []) sts 
            in 
            (has_break_or_return, Seq sts)
        | Break -> (true, Const CNull)
        | Return _ -> (true, body)
        | e -> (false, e)
      in
      let aux_cases = function
        | _,Case (case_e, body_e) -> 
          let pos,b = body_e in 
          let has_break,b = remove_break b in 
           (aux_e env case_e, aux_e env (pos,b), has_break)
        | _,Default body_e -> 
          let pos,b = body_e in 
          let has_break,b = remove_break b in
          ((Eid.unique (), Ast.VarMap.empty, Ast.Noop), aux_e env (pos, b), has_break) 
        | _ -> failwith "Invalid case in switch"
      in
      Ast.Switch (aux_e env e, List.map aux_cases cases)
  | Case _ | Default _ -> failwith "Case and Default should be inside a switch"
  | Seq [] -> Ast.Const Ast.CNull
  | Seq (e::es) -> let _,_,e = List.fold_left (fun acc e ->
      Eid.unique (), Ast.VarMap.empty, Ast.Seq (acc, aux_e env e)) (aux_e env e) es in
      e
  | Comma _ -> failwith "Comma operator not supported yet"
  | Cast (ty, e) -> Ast.Cast (ty, aux_e env e)
  | VarDeclare (_typ, (_,Id _s)) -> Ast.Noop (* Rather generate a AST. Declare somewhere from them*)
  | VarDeclare (_, _) -> failwith "Declaration must have an identifier" (*Should be unreachable*)
  in
  (eid, Ast.VarMap.empty, e)
and process_call env f args = 
  (* Set calls modify in place in the R C API, but for typing reason, 
  we make it create a new value and then assign to the original variable. *)
  let e = match (f, args) with 
  | (loc1,Id "SET_VECTOR_ELT"),(_, Id v)::_ ->
   Ast.VarAssign (var env v,
      (Eid.unique_with_pos loc1, Ast.VarMap.empty, Ast.Call (
         aux_e env f,
        List.map (aux_e env) args
      ))
    )
  | _ -> Ast.Call (aux_e env f, List.map (aux_e env) args) in
  e
and transform env (pos, topl_unit) = 
  let eid = Eid.unique_with_pos pos in
  let e = match topl_unit with 
  | Fundef (ret_ty, name, params, body) -> 
    let param_vars = bv_params params in
    let pid = List.fold_left add_var env.id (StrSet.elements param_vars) in
    let env = {id=pid} in 
    let body_vars = bv_e false body in
    let eid = List.fold_left add_var env.id (StrSet.elements body_vars) in
    let env = {id=eid} in
    let e = List.fold_left (add_def pid eid) (aux_e env body) (StrSet.elements body_vars) in
    let params = List.map (fun (ty,name) -> ty,var env name) params in 
    Ast.Function (name, ret_ty, params, e) 
  in
  (eid, Ast.VarMap.empty, e)

let map f e = 
  let rec aux (pos, e) = 
    let e = match e with 
    | Const _ | Id _ | Break | Next -> e
    | Unop (op, e1) -> Unop (op, aux e1)
    | Binop (op, (e1, e2)) -> Binop (op, (aux e1, aux e2))
    | VarDeclare (ty, e1) -> VarDeclare (ty, aux e1)
    | VarAssign (e1, e2) -> VarAssign (aux e1, aux e2)
    | Call (f, args) -> Call (aux f, List.map aux args)
    | If (cond, then_, else_) -> 
        If (aux cond, aux then_, Option.map aux else_)
    | Ite (cond, then_, else_) ->
        Ite (aux cond, aux then_, aux else_)
    | While (cond, body) -> While (aux cond, aux body)
    | For (init, cond, incr, body) ->
        For (aux init, Option.map aux cond, Option.map aux incr, aux body)
    | Return e -> Return (Option.map aux e)
    | Case (e1, e2) -> Case (aux e1, aux e2)
    | Default e1 -> Default (aux e1)
    | Switch (e1, cases) -> Switch (aux e1, List.map aux cases)
    | Seq exprs -> Seq (List.map aux exprs)
    | Comma (e1, e2) -> Comma (aux e1, aux e2)
    | Cast (ty, e) -> Cast (ty, aux e)
    in
    f (pos, e)
  in aux e

