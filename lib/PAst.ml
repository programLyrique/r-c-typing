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
  | TypeDecl of string * Ast.ctype
    (** Binds a named type in the decl map. Covers struct/union/enum
        declarations (payload is the corresponding [Ast.Struct]/[Ast.Union]/
        [Ast.Enum] ctype) as well as typedef aliases (payload is the aliased
        ctype). Both flow through a single arm downstream because they behave
        identically: add [name -> ctype] to [DeclMap]. *)
  | GlobalVar of string * Ast.ctype
    (** File-scope variable declaration or definition. Payload is the
        identifier and its declared type. Any initializer is dropped here;
        only the declared type seeds the identifier's binding in
        [Runner.infer_def]. Covers both [extern] declarations and in-file
        definitions. *)
  | Define of string * const
  | Include of top_level_unit list (* items parsed from an included external header *)
  [@@deriving show]
 and top_level_unit = Position.t * top_level_unit'
  [@@deriving show]
 and e' = 
  | Const of const 
  | Id of string
  | Unop of string * e
  | FieldAccess of e * string (* For struct field access, e.g. x.f *)
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

let top_level_unit_name top =
  match top with
  | _, Fundef (_, name, _, _) -> name
  | _, TypeDecl (name, _) -> name
  | _, GlobalVar (name, _) -> name
  | _, Define (name, _) -> name
  | _, Include _ -> ""


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
  | Break | Next | Const _ -> StrSet.empty
  | Id s when in_lhs_assign -> StrSet.singleton s
  | Id _ -> StrSet.empty
  | Unop (_, e) -> bv_e in_lhs_assign e
  | Binop (_, (e1,e2)) -> StrSet.union (bv_e in_lhs_assign e1) (bv_e in_lhs_assign e2)
  | VarAssign ((_,Id s), e2) -> (StrSet.singleton s) |> StrSet.union (bv_e in_lhs_assign e2)
  | FieldAccess (e, _) -> bv_e in_lhs_assign e
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

(* Object-like identifier-alias macros: [#define NAME TARGET] where [TARGET]
   is a bare C identifier. Recorded by the parser, resolved at use-site in
   [var]. Keeping the table here (rather than in [Parser]) avoids a module
   cycle: [Parser] depends on [PAst], and [var] needs to consult the table. *)
let define_aliases : string StrMap.t ref = ref StrMap.empty

let reset_define_aliases () = define_aliases := StrMap.empty

let remove_define_alias name =
  define_aliases := StrMap.remove name !define_aliases

let remember_define_alias name target =
  define_aliases := StrMap.add name target !define_aliases

(* Follow the alias chain transitively. A visited set terminates self-aliases
   and cycles on the last name in the chain. *)
let resolve_alias name =
  let rec loop seen n =
    if StrSet.mem n seen then n
    else
      match StrMap.find_opt n !define_aliases with
      | None -> n
      | Some target -> loop (StrSet.add n seen) target
  in
  loop StrSet.empty name

type env = {
  id: Variable.t StrMap.t;
  decl: Ast.ctype Ast.DeclMap.t;
}

(* [resolve_ctype] expands Typerefs and empty-struct forward references by
   looking them up in [decl]. [visited] records struct/typedef names currently
   being expanded so that self-referential types terminate: back-edges are left
   as [Typeref name] / [Struct (name, [])] for [Ast.typeof_ctype] to close via
   Sstt's recursive-type machinery. *)
let rec resolve_ctype_aux visited decl ty =
  match ty with
  | Ast.Typeref name ->
      if StrSet.mem name visited then ty
      else (
        match Ast.DeclMap.find_opt name decl with
        | Some t -> resolve_ctype_aux (StrSet.add name visited) decl t
        | None -> ty
      )
  | Ast.Struct (name, []) ->
      if StrSet.mem name visited then ty
      else (
        match Ast.DeclMap.find_opt name decl with
        | Some (Ast.Struct (_, fields)) ->
            let visited = StrSet.add name visited in
            Ast.Struct (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
        | _ -> ty
      )
  | Ast.Struct (name, fields) ->
      let visited = StrSet.add name visited in
      Ast.Struct (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
  | Ast.Enum (name, []) -> (
      match Ast.DeclMap.find_opt name decl with
      | Some (Ast.Enum (_, enumerators)) -> Ast.Enum (name, enumerators)
      | _ -> ty
    )
  | Ast.Enum (name, enumerators) -> Ast.Enum (name, enumerators)
  | Ast.Ptr t -> Ast.Ptr (resolve_ctype_aux visited decl t)
  | Ast.Array (t, len) -> Ast.Array (resolve_ctype_aux visited decl t, len)
  | Ast.Union (name, fields) ->
      let visited = StrSet.add name visited in
      Ast.Union (name, List.map (fun (fty, fname) -> (resolve_ctype_aux visited decl fty, fname)) fields)
  | _ -> ty

let resolve_ctype decl ty = resolve_ctype_aux StrSet.empty decl ty

let rec has_struct_type ty =
  match ty with
  | Ast.Struct _ -> true
  | Ast.Ptr t -> has_struct_type t
  | Ast.Array (t, _) -> has_struct_type t
  | _ -> false

(* Full lookup chain for [str]: local idenv, builtin op, builtin var, .ty
   bindings. Returns [Some v] if [str] resolves to a concrete binding,
   [None] if it would fall through to the fresh-variable case. *)
let lookup_binding env str =
  match StrMap.find_opt str env.id with
  | Some _ as r -> r
  | None ->
    (match Defs.BuiltinOp.find_builtin str with
     | Some _ as r -> r
     | None ->
       (match Defs.BuiltinVar.find_builtin_var str with
        | Some _ as r -> r
        | None -> Defs.StrMap.find_opt str Defs.defs_map))

let var env str =
  (* Resolve identifier-alias macros ([#define NAME TARGET]) before looking
     up. Keep the resolution only if [TARGET] is itself bound; otherwise the
     alias gives us nothing and we preserve the original name so inference
     sees the same fresh variable as before aliases were supported. *)
  let effective =
    let resolved = resolve_alias str in
    if resolved = str then str
    else match lookup_binding env resolved with
      | Some _ -> resolved
      | None -> str
  in
  match lookup_binding env effective with
  | Some v -> v
  | None ->
    Printf.printf "Creating fresh variable: %s\n" effective;
    MVariable.create Immut (Some effective)

let add_var env str =
  let v = MVariable.create MVariable.Mut (Some str) in
  StrMap.add str v env


(* Wrap [e] in a [Declare] for body variable [str], unless [str] is already
   bound as a parameter (parameters are lambda-bound, so no local Declare is
   needed). The param set is passed explicitly rather than reading from an
   [env.id] map because [env.id] now also carries file-scope globals, and a
   body-var that happens to shadow a global must still be locally declared.
   TODO: we already have explicit declarations in C so we should rather use
   them instead of detecting variables *)
let add_def params eid e str =
  let v = StrMap.find str eid in
  if StrSet.mem str params then e
  else
    let _, decl, _, _ = e in
    (Eid.unique (), decl, Ast.VarMap.empty, Ast.Declare (v, e))

let mk_e env eid expr =
  (eid, env.decl, Ast.VarMap.empty, expr)

let fresh_e env expr =
  mk_e env (Eid.unique ()) expr

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
  | VarAssign ((loc1, Call ((_,Id "[]"), ((_, Id base_name) :: _ as args))) ,e2) ->
      (* Model arr[i] = v as a functional update arr = []<-(arr, i, v) so
         subsequent reads can use the updated value. *)
      Ast.VarAssign (
        var env base_name,
        fresh_e env (Ast.Call (
          aux_e env (loc1, Id "[]<-"),
          (List.map (aux_e env) args) @ [aux_e env e2]
        ))
      )
  | VarAssign ((loc1, Call ((_,Id "[]"), args)) ,e2) ->
      Ast.Call (
        aux_e env (loc1, Id "[]<-"),
        (List.map (aux_e env) args) @ [aux_e env e2]
      )
  | VarAssign ((loc1, Unop (_op, e1)) ,e2) -> (* Currently, remove the * or & operator *)
      let _,_,_,e = aux_e env (loc1, VarAssign(e1, e2)) in e
  | VarAssign ((_, FieldAccess (e1, field)) ,e2) -> 
      Ast.FieldUpdate (aux_e env e1, field, aux_e env e2)
  | VarAssign (_,_) -> failwith ("Unexpected left-hand side in assignment. Got: " ^ show_e (pos,e))
  | FieldAccess (e, field) -> Ast.FieldRead (aux_e env e, field)
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
        | None -> fresh_e env (Ast.Const (Ast.CBool true))
        | Some e -> aux_e env e
      in
      let incr_e = match incr with 
        | None -> fresh_e env (Ast.Const (Ast.CNull))
        | Some e -> aux_e env e
      in
      let while_body = 
        let body_e = aux_e env body in
        let seq_e = fresh_e env (Ast.Seq (body_e, incr_e)) in
        seq_e
      in
      let while_e = fresh_e env (Ast.While (cond_e, while_body)) in
      Ast.Seq (init_e, while_e)
  | Return None -> Ast.Return None
  | Return (Some e) -> Ast.Return (Some (aux_e env e))
  | Break -> Ast.Break
  | Next -> Ast.Next
  | Switch  (e, cases) -> 
      (*Detects break and return; remove break.
        Recurses into nested Seq nodes so that a break inside a compound
        statement block (e.g. `case 1: { ...; break; }`) is properly detected
        and removed rather than leaking as an orphan Ret(BLoop) in the MLsem
        transform. We do NOT recurse into While/For/Switch bodies, so a break
        that targets an inner loop is left alone. *)
      let rec remove_break body =
        match body with
        | Seq sts ->
            let has_break_or_return, sts = List.fold_left (fun (had_break, acc) stmt ->
              if had_break then (true, acc)
              else match stmt with
              | _, Break -> (true, acc)
              | _, Return _ -> (true, acc @ [stmt])
              | pos, Seq _ ->
                  let has_brk, body' = remove_break (snd stmt) in
                  (has_brk, acc @ [(pos, body')])
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
          (fresh_e env Ast.Noop, aux_e env (pos, b), has_break) 
        | _ -> failwith "Invalid case in switch"
      in
      Ast.Switch (aux_e env e, List.map aux_cases cases)
  | Case _ | Default _ -> failwith "Case and Default should be inside a switch"
  | Seq [] -> Ast.Const Ast.CNull
  | Seq (e::es) ->
      let seq_e =
        List.fold_left
          (fun acc e2 -> fresh_e env (Ast.Seq (acc, aux_e env e2)))
          (aux_e env e)
          es
      in
      let _, _, _, e = seq_e in
      e
  | Comma (e1, e2) -> Ast.Seq (aux_e env e1, aux_e env e2)
  | Cast (ty, e) -> Ast.Cast (resolve_ctype env.decl ty, aux_e env e)
  | VarDeclare (typ, (_,Id s)) ->
      let typ = resolve_ctype env.decl typ in
      if has_struct_type typ then
        let null_e = fresh_e env (Ast.Const Ast.CNull) in
        Ast.VarAssign (var env s, fresh_e env (Ast.Cast (typ, null_e)))
      else
        Ast.Noop
  | VarDeclare (_, _) -> failwith "Declaration must have an identifier" (*Should be unreachable*)
  in
    mk_e env eid e
and process_call env f args = 
  (* Set calls modify in place in the R C API, but for typing reason, 
  we make it create a new value and then assign to the original variable. *)
  let e = match (f, args) with 
  | (loc1,Id "SET_VECTOR_ELT"),(_, Id v)::_ ->
   Ast.VarAssign (var env v,
      (mk_e env (Eid.unique_with_pos loc1) (Ast.Call (
         aux_e env f,
        List.map (aux_e env) args
      )))
    )
  | _ -> Ast.Call (aux_e env f, List.map (aux_e env) args) in
  e
and transform env (pos, topl_unit) = 
  let eid = Eid.unique_with_pos pos in
  let (decl, e) = match topl_unit with 
  | Fundef (ret_ty, name, params, body) ->

    let param_vars = bv_params params in
    let pid = List.fold_left add_var env.id (StrSet.elements param_vars) in
    let env = {env with id=pid} in
    let body_vars = bv_e false body in
    let eid = List.fold_left add_var env.id (StrSet.elements body_vars) in
    let env = {env with id=eid} in
    let e = List.fold_left (add_def param_vars eid) (aux_e env body) (StrSet.elements body_vars) in
    let params = List.map (fun (ty,name) -> resolve_ctype env.decl ty, var env name) params in
    let ret_ty = resolve_ctype env.decl ret_ty in
    (env.decl, Ast.Function (name, ret_ty, params, e))
  | TypeDecl (name, ty) -> (Ast.DeclMap.add name ty env.decl, Ast.Noop)
  | GlobalVar _ -> (env.decl, Ast.Noop) (* handled by infer_def directly *)
  | Define (_name, _value) -> (env.decl, Ast.Noop)
  | Include _ -> (env.decl, Ast.Noop) (* handled by infer_def directly *)
  in
  (eid, decl, Ast.VarMap.empty, e)

let map f e = 
  let rec aux (pos, e) = 
    let e = match e with 
    | Const _ | Id _ | Break | Next -> e
    | Unop (op, e1) -> Unop (op, aux e1)
    | Binop (op, (e1, e2)) -> Binop (op, (aux e1, aux e2))
    | VarDeclare (ty, e1) -> VarDeclare (ty, aux e1)
    | VarAssign (e1, e2) -> VarAssign (aux e1, aux e2)
    | FieldAccess (e, field) -> FieldAccess (aux e, field)
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

(** Extract function call names from an expression.
    [fun_names] is the set of known function names; bare Id references
    matching a function name in call arguments are treated as function
    pointer dependencies. *)
let rec extract_calls_from_expr ?(fun_names=StrSet.empty) (_pos, e') =
  let extract = extract_calls_from_expr ~fun_names in
  let extract_arg (_pos, e') =
    match e' with
    | Id name when StrSet.mem name fun_names -> [name]
    | _ -> extract (_pos, e')
  in
  match e' with
  | Const _ | Id _ | Break | Next -> []
  | Unop (_, e) -> extract e
  | Binop (_, (e1, e2)) -> extract e1 @ extract e2
  | FieldAccess (e, _) -> extract e
  | VarDeclare (_, e) -> extract e
  | VarAssign (e1, e2) -> extract e1 @ extract e2
  | Call ((_, Id fname), args) ->
      fname :: List.concat_map extract_arg args
  | Call (f, args) ->
      extract f @ List.concat_map extract_arg args
  | If (cond, then_, else_opt) ->
      let acc = extract cond @ extract then_ in
      (match else_opt with
       | None -> acc
       | Some e -> acc @ extract e)
  | Ite (cond, then_, else_) ->
      extract cond @ extract then_ @ extract else_
  | While (cond, body) -> extract cond @ extract body
  | For (init, cond_opt, incr_opt, body) ->
      let acc = extract init in
      let acc = match cond_opt with None -> acc | Some e -> acc @ extract e in
      let acc = match incr_opt with None -> acc | Some e -> acc @ extract e in
      acc @ extract body
  | Return e_opt ->
      (match e_opt with None -> [] | Some e -> extract e)
  | Case (e1, e2) -> extract e1 @ extract e2
  | Default e -> extract e
  | Switch (e, cases) -> extract e @ List.concat_map extract cases
  | Seq exprs -> List.concat_map extract exprs
  | Comma (e1, e2) -> extract e1 @ extract e2
  | Cast (_, e) -> extract e

(* Inline tests for extract_calls_from_expr *)
let%test "simple function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Call (mk_e (Id "foo"), [])) in
  let calls = extract_calls_from_expr expr in
  calls = ["foo"]

let%test "function call with arguments" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Call (mk_e (Id "bar"), [mk_e (Const (CInt 42)); mk_e (Id "x")])) in
  let calls = extract_calls_from_expr expr in
  calls = ["bar"]

let%test "function pointer passed as argument" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let fun_names = StrSet.of_list ["R_ToplevelExec"; "check_interrupt_fn"] in
  let expr = mk_e (Call (mk_e (Id "R_ToplevelExec"),
    [mk_e (Id "check_interrupt_fn"); mk_e (Const CNull)])) in
  let calls = extract_calls_from_expr ~fun_names expr in
  calls = ["R_ToplevelExec"; "check_interrupt_fn"]

let%test "non-function id argument not extracted" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let fun_names = StrSet.of_list ["bar"] in
  let expr = mk_e (Call (mk_e (Id "bar"), [mk_e (Const (CInt 42)); mk_e (Id "x")])) in
  let calls = extract_calls_from_expr ~fun_names expr in
  calls = ["bar"]

let%test "nested function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let inner_call = mk_e (Call (mk_e (Id "inner"), [])) in
  let expr = mk_e (Call (mk_e (Id "outer"), [inner_call])) in
  let calls = extract_calls_from_expr expr in
  calls = ["outer"; "inner"]

let%test "binary operation with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call1 = mk_e (Call (mk_e (Id "func1"), [])) in
  let call2 = mk_e (Call (mk_e (Id "func2"), [])) in
  let expr = mk_e (Binop ("+", (call1, call2))) in
  let calls = extract_calls_from_expr expr in
  calls = ["func1"; "func2"]

let%test "if-then-else with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let cond = mk_e (Call (mk_e (Id "is_ready"), [])) in
  let then_branch = mk_e (Call (mk_e (Id "process"), [])) in
  let else_branch = mk_e (Call (mk_e (Id "skip"), [])) in
  let expr = mk_e (If (cond, then_branch, Some else_branch)) in
  let calls = extract_calls_from_expr expr in
  calls = ["is_ready"; "process"; "skip"]

let%test "while loop with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let cond = mk_e (Call (mk_e (Id "has_next"), [])) in
  let body = mk_e (Call (mk_e (Id "process_next"), [])) in
  let expr = mk_e (While (cond, body)) in
  let calls = extract_calls_from_expr expr in
  calls = ["has_next"; "process_next"]

let%test "for loop with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let init = mk_e (Call (mk_e (Id "init_loop"), [])) in
  let cond = mk_e (Call (mk_e (Id "check_cond"), [])) in
  let incr = mk_e (Call (mk_e (Id "increment"), [])) in
  let body = mk_e (Call (mk_e (Id "process"), [])) in
  let expr = mk_e (For (init, Some cond, Some incr, body)) in
  let calls = extract_calls_from_expr expr in
  calls = ["init_loop"; "check_cond"; "increment"; "process"]

let%test "switch statement with function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let switch_expr = mk_e (Call (mk_e (Id "get_value"), [])) in
  let case1 = mk_e (Case (mk_e (Const (CInt 1)), mk_e (Call (mk_e (Id "handle_one"), [])))) in
  let default = mk_e (Default (mk_e (Call (mk_e (Id "handle_default"), [])))) in
  let expr = mk_e (Switch (switch_expr, [case1; default])) in
  let calls = extract_calls_from_expr expr in
  calls = ["get_value"; "handle_one"; "handle_default"]

let%test "sequence with multiple function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call1 = mk_e (Call (mk_e (Id "step1"), [])) in
  let call2 = mk_e (Call (mk_e (Id "step2"), [])) in
  let call3 = mk_e (Call (mk_e (Id "step3"), [])) in
  let expr = mk_e (Seq [call1; call2; call3]) in
  let calls = extract_calls_from_expr expr in
  calls = ["step1"; "step2"; "step3"]

let%test "return with function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call = mk_e (Call (mk_e (Id "compute"), [])) in
  let expr = mk_e (Return (Some call)) in
  let calls = extract_calls_from_expr expr in
  calls = ["compute"]

let%test "no function calls" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let expr = mk_e (Binop ("+", (mk_e (Const (CInt 1)), mk_e (Const (CInt 2))))) in
  let calls = extract_calls_from_expr expr in
  calls = []

let%test "assignment with function call" =
  let pos = Mlsem.Common.Position.dummy in
  let mk_e e' = (pos, e') in
  let call = mk_e (Call (mk_e (Id "getValue"), [])) in
  let expr = mk_e (VarAssign (mk_e (Id "x"), call)) in
  let calls = extract_calls_from_expr expr in
  calls = ["getValue"]
