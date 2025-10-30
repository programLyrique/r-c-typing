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
 | CFloat of string 
 | CInt of int 
 | CNull
 | CNa
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
  (*| VarDeclare of Ast.ctype * e*)
  | VarAssign of e * e
  | Call of e * e list 
  | Ite of e * e * e option
  | Return of e option
  | Break
  | Next
  | Seq of e list
  | Comma of e * e
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
let rec bv_e (_,e) = 
  match e with 
  | Break | Next | Id _ | Const _ -> StrSet.empty
  | Unop (_, e) -> bv_e e
  | Binop (_, (e1,e2)) -> StrSet.union (bv_e e1) (bv_e e2)
  | VarAssign ((_,Id s), e2) -> (StrSet.singleton s) |> StrSet.union (bv_e e2)
  | VarAssign (_,_) -> failwith "Left side of assignment must be an identifier" (* and this should currently be unreachable given how PAst is built from Cst*)
  | Call (f, args) -> List.fold_left (fun acc arg -> StrSet.union acc (bv_e arg)) (bv_e f) args
  | Ite (cond, then_, else_) -> 
      let acc = bv_e cond |> StrSet.union (bv_e then_) in
      begin match else_ with 
      | None -> acc
      | Some e -> StrSet.union acc (bv_e e)
      end
  | Return None -> StrSet.empty
  | Return (Some e) -> bv_e e
  | Seq exprs -> List.fold_left (fun acc e -> StrSet.union acc (bv_e e)) StrSet.empty exprs
  | Comma (e1, e2) -> StrSet.union (bv_e e1) (bv_e e2)

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

(* Check if variables in the body are defined as parameters.
  If yes, we create a fresh variable with let that gets the param. 
  If not, we create a fresh variable with declare (mutable) *)
let add_def pid eid e str =
  let v = StrMap.find str eid in
  match StrMap.find_opt str pid with
  | None -> Eid.unique (), Ast.Declare (v, e)
  | Some v -> Eid.unique (), Ast.Let (v, (Eid.unique (), Ast.Id v), e)

let aux_const c = 
  match c with 
  | CStr s -> Ast.CStr s 
  | CFloat s -> Ast.CDbl s
  | CInt i -> Ast.CInt i
  | CNull -> Ast.CNull
  | CNa -> Ast.CNa


let rec aux_e env (pos,e) = 
  let eid = Eid.unique_with_pos pos in
  let e = match e with 
  | Const c -> Ast.Const (aux_const c)
  | Id s -> Ast.Id (var env s)
  | Unop (op, e) -> Ast.Unop (var env (op ^ "__1"), aux_e env e)
  | Binop (op, (e1,e2)) -> Ast.Binop (var env (op ^ "__2"), aux_e env e1, aux_e env e2)
  | VarAssign ((_,Id s), e2) -> Ast.VarAssign (var env s, aux_e env e2)
  | VarAssign (_,_) -> failwith "Left side of assignment must be an identifier" (* and this should currently be unreachable given how PAst is built from Cst*)
  | Call (f, args) -> Ast.Call (aux_e env f, List.map (aux_e env) args)
  | Ite (cond, then_, else_) -> 
      let else_ast = match else_ with 
      | None -> (Eid.unique (), Ast.Const Ast.CNull)
      | Some e -> aux_e env e
      in
      Ast.Ite (aux_e env cond, aux_e env then_, else_ast)
  | Return None -> Ast.Return None
  | Return (Some e) -> Ast.Return (Some (aux_e env e))
  | Break -> Ast.Break
  | Next -> Ast.Next
  | Seq [] -> Ast.Const Ast.CNull
  | Seq (e::es) -> List.fold_left (fun acc e ->
      Eid.unique (), Ast.Seq (acc, aux_e env e)) (aux_e env e) es |> snd
  | Comma _ -> failwith "Comma operator not supported yet"
  in
  (eid, e)
and transform env (pos, topl_unit) = 
  let eid = Eid.unique_with_pos pos in
  let e = match topl_unit with 
  | Fundef (ret_ty, _name, params, body) -> 
    let param_vars = bv_params params in
    let pid = List.fold_left add_var env.id (StrSet.elements param_vars) in
    let env = {id=pid} in 
    let params = List.map (fun (ty,name) -> ty,var env name) params in 
    let body_vars = bv_e body in
    let eid = List.fold_left add_var env.id (StrSet.elements body_vars) in
    let env = {id=eid} in
    let e = List.fold_left (add_def pid eid) (aux_e env body) (StrSet.elements body_vars) in
    Ast.Function (ret_ty, params, e) 
  in
  (eid, e)