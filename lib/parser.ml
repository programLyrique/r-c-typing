open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
open Mlsem.Common

let print_res (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) = 
  Printf.printf "Errors: %d. Lines with errors: %d / %d\n" res.stat.error_count
    res.stat.error_line_count res.stat.total_line_count ;
    List.iter (fun e -> 
      Printf.printf "Error: %s\n" (Tree_sitter_error.to_string e)
    ) res.errors ;
   match res.program with
  | None -> ()
  | Some prog ->
    Boilerplate.dump_extras res.extras ;
    let tree = Boilerplate.map_translation_unit () prog in
    Raw_tree.to_channel stdout tree 

let print_top_level_item (item: top_level_item) =
  let tree = Boilerplate.map_top_level_item () item in
  Raw_tree.to_channel stdout tree

let process_res res = res
(**
  Parse a file using FrontC and return the AST

  @param filename The path to the C file to parse
*)
let parse_file filename = 
  let res = Parse.file filename  in
  process_res res

let parse_string s = 
  let res = Parse.string s in 
  process_res res


(* Taken from E-Sh4rk/typed-r *)
let line_length = 0x10000
let conv_pos tspos =
  let bol = tspos.Loc.row*line_length in
  {
    Lexing.pos_fname = "" ;
    pos_lnum = tspos.Loc.row ;
    pos_bol = bol ;
    pos_cnum = bol + tspos.Loc.column ;
  }


let loc_to_pos (loc: Loc.t) : Position.t =
  let start_pos = conv_pos loc.start in
  let end_pos = conv_pos loc.end_ in
  Position.lex_join start_pos end_pos

(** Given to locations, create a position using the start of
   the first loc and the end of the second loc *)
let locs_to_pos (loc1: Loc.t) (loc2: Loc.t) : Position.t =
  let start_pos = conv_pos loc1.start in
  let end_pos = conv_pos loc2.end_ in
  Position.lex_join start_pos end_pos

let exprs_to_pos (expr1: A.e) (expr2: A.e) : Position.t =
  let pos1, _ = expr1 in
  let pos2, _ = expr2 in
  Position.join pos1 pos2


let aux_primitive_type t = 
  match t with 
  | "void" -> Ast.Void
  | "char" -> Ast.Char
  | "short" | "int" | "long" -> Ast.Int
  | "float" | "double" -> Ast.Float
  | "bool" -> Ast.Bool
  | _ -> failwith ("Unknown primitive type: " ^ t)

let aux_struct _struc = []


let token_to_string (_loc, s) = s

let aux_type_spec (type_spec : type_specifier)  =
  match type_spec with
  | `Prim_type tok -> let (_loc, s) = tok in aux_primitive_type s
  | `Id (_loc, s) when s = "SEXP" -> Ast.SEXP
  | `Id _ -> Ast.Any
  | `Struct_spec _ -> failwith "Not supported yet"
  | _ -> failwith "Not supported yet: type specifier"

let aux_decl_spec decl_spec = 
  let (_, type_spec, _) = decl_spec in
  aux_type_spec type_spec

(* For pointers *)
let rec aux_abstract_declarator (base_type: Ast.ctype) (abs_decl: abstract_declarator option) =
  let aux = function 
    `Abst_poin_decl (_,_,_,decl) -> Ast.Ptr (aux_abstract_declarator base_type decl)
    | _ -> failwith "Not supported yet: abstract declarator"
  in
  Option.fold ~none:base_type
    ~some:aux abs_decl

let aux_type_descriptor (type_desc: type_descriptor) : Ast.ctype =
  let (_qualifiers1, type_spec, _qualifiers2, abstract_decl) = type_desc in
  let base_type = aux_type_spec type_spec in
  (* For now, ignore qualifiers and abstract declarators
     TODO: Handle const, volatile, pointers, arrays, etc. *)
  aux_abstract_declarator base_type abstract_decl

let rec aux_decl_name (decl : declarator) : int * string =
  match decl with
  | `Id tok -> (0, token_to_string tok)
  | `Func_decl (decl, _, _, _) ->  aux_decl_name decl
  | `Poin_decl (_,_,_,_,decl) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  (* Array just adds a pointer indirection currently. We could also create an array type for the C side*)
  | `Array_decl (decl,_,_,_,_) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  | _ -> Boilerplate.map_declarator () decl |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: function name"

let aux_param (p: anon_choice_param_decl_4ac2852) = 
  match p with 
  | `Param_decl (decl_spec, Some (`Decl decl), _) -> 
    let ty = aux_decl_spec decl_spec in
    let level, name = aux_decl_name decl in
    (Ast.build_ptr level ty, name)
  | `Vari_param _ -> failwith "Not supported yet: variable number of parameters (...) in declaration"
  | _ -> failwith "Not supported yet: parameter declaration"

let rec aux_params (decl: declarator) : int * A.param list =
  match decl with 
  | `Func_decl (_, (_loc1, `Opt_choice_param_decl_rep_COMMA_choice_param_decl (Some (p1, params)), _loc2), _,_) -> 
     0,(aux_param p1) :: (List.map (fun (_, p) -> aux_param p) params)
  | `Poin_decl (_,_,_,_,decl) -> let level, params = aux_params decl in (level + 1, params)
  | _ -> (0, [])


let rec aux_expression (expr: expression) : A.e =
  match expr with 
  | `Choice_cond_exp e -> aux_not_bin_expression e
  | `Bin_exp e -> aux_bin_expression e
and aux_bin_expression (e: binary_expression) =
  let build_expr op e1 e2 = 
    let e1 = aux_expression e1 in
    let e2 = aux_expression e2 in
    (exprs_to_pos e1 e2, A.Binop (op, (e1, e2)))
  in
  match e with 
  | `Exp_PLUS_exp (e1, _, e2) -> build_expr "+" e1 e2
  | `Exp_DASH_exp (e1, _, e2) -> build_expr "-" e1 e2
  | `Exp_STAR_exp (e1, _, e2) -> build_expr "*" e1 e2
  | `Exp_SLASH_exp (e1, _, e2) -> build_expr "/" e1 e2
  | `Exp_PERC_exp (e1, _, e2) -> build_expr "%" e1 e2
  | `Exp_BARBAR_exp (e1, _, e2) -> build_expr "||" e1 e2
  | `Exp_AMPAMP_exp (e1, _, e2) -> build_expr "&&" e1 e2
  | `Exp_BAR_exp (e1, _, e2) -> build_expr "|" e1 e2
  | `Exp_HAT_exp (e1, _, e2) -> build_expr "^" e1 e2
  | `Exp_AMP_exp (e1, _, e2) -> build_expr "&" e1 e2
  | `Exp_EQEQ_exp (e1, _, e2) -> build_expr "==" e1 e2
  | `Exp_BANGEQ_exp (e1, _, e2) -> build_expr "!=" e1 e2
  | `Exp_GT_exp (e1, _, e2) -> build_expr ">" e1 e2
  | `Exp_GTEQ_exp (e1, _, e2) -> build_expr ">=" e1 e2
  | `Exp_LTEQ_exp (e1, _, e2) -> build_expr "<=" e1 e2
  | `Exp_LT_exp (e1, _, e2) -> build_expr "<" e1 e2
  | `Exp_LTLT_exp (e1, _, e2) -> build_expr "<<" e1 e2
  | `Exp_GTGT_exp (e1, _, e2) -> build_expr ">>" e1 e2

and aux_string (s: string_) = 
  let unescape  = function
    | `Imm_tok_prec_p1_pat_c7f65b4 (loc, s) -> (loc,s)
    | `Esc_seq (loc, s) -> (loc,s)
  in
  match s with 
  | `Str_lit (prefix,escaped,(end_loc,_)) -> 
    let pos_str = List.map unescape escaped in
    let str = String.concat "" (List.map snd pos_str) in
    let start_loc =
      match prefix with
      | `LDQUOT (loc, _) -> loc
      | `UDQUOT_c163aae (loc, _) -> loc
      | `UDQUOT_df3447d (loc, _) -> loc
      | `U8DQUOT (loc, _) -> loc
      | `DQUOT (loc, _) -> loc
    in
    (locs_to_pos start_loc end_loc, A.Const (A.CStr str))
  | _ -> failwith "Not supported yet: strings with escaped characters or concatenated strings"
and aux_num_lit  s = 
  A.Const(
    try 
      A.CInt (int_of_string s)
  with 
    | Failure _ -> A.CFloat (Float.of_string s)
  )

and aux_char_lit (c: char_literal) = 
  let (start, c, (loc2,_)) = c in 
  let loc1,c = match (start,c) with 
   | `SQUOT (loc1,_), [`Imm_tok_pat_36637e2 (_,c)] -> assert (String.length c == 1);loc1, c.[0]
   | _ -> failwith "Not supported yet: char literals with escape sequences"
  in
  let pos = locs_to_pos loc1 loc2 in
  (pos, A.Const (A.CChar c))
    
and aux_not_bin_expression (e : expression_not_binary) = 
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Num_lit (loc, s) -> (loc_to_pos loc, aux_num_lit s)
  | `Char_lit c -> aux_char_lit c
  | `Null _ -> (Position.dummy, A.Const A.CNull)
  | `Call_exp  call -> aux_call_expression call
  | `Str s -> aux_string s
  | `Assign_exp (e1, `EQ _, e2) -> 
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      (exprs_to_pos lhs rhs, A.VarAssign (lhs, rhs))
  | `Assign_exp (e1, op, e2) -> 
      (* Expand a op= e into a = a op e *)
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      let pos = exprs_to_pos lhs rhs in
      let binop = 
        match op with 
        | `PLUSEQ _ -> "+"
        | `DASHEQ _ -> "-"
        | `STAREQ _ -> "*"
        | `SLASHEQ _ -> "/"
        | `PERCEQ _ -> "%"
        | `AMPEQ _ -> "&"
        | `BAREQ _ -> "|"
        | `HATEQ _ -> "^"
        | `LTLTEQ _ -> "<<"
        | `GTGTEQ _ -> ">>"
        | `EQ _ -> failwith "Unreachable: `EQ handled in previous case"
      in
      let rhs = (exprs_to_pos lhs rhs, A.Binop (binop, (lhs, rhs))) in
      (pos, A.VarAssign (lhs, rhs))
  | `Un_exp (op, e1) -> 
      let loc1,op = match op with 
        | `DASH (loc, _) -> (loc, "-")
        | `BANG (loc, _) -> (loc, "!")
        | `TILDE (loc, _) -> (loc, "~")
        | `PLUS (loc, _) -> (loc, "+")
      in
      let expr = aux_expression e1 in 
      (Position.join (loc_to_pos loc1) (fst expr),
      A.Unop (op, expr))
  | `Subs_exp expr -> 
      aux_subscription_expression expr
  | `Cond_exp (cond, _, Some (`Exp then_), _, else_) -> 
      let cond = aux_expression cond in
      let then_ = aux_expression then_ in
      let else_ = aux_expression else_ in
      let pos = exprs_to_pos cond then_ in
      let pos = Position.join pos (fst else_) in
      (pos, A.Ite (cond, then_, else_))
  | `Update_exp expr -> aux_update_expression expr
  | `Paren_exp expr -> aux_paren_expr expr
  | `True (loc,_) -> (loc_to_pos loc, A.Const (A.CBool true))
  | `False (loc,_) -> (loc_to_pos loc, A.Const (A.CBool false))
  | `Cast_exp ((loc1,_), typ, _, expr) ->
      let e = aux_expression expr in
      let cast_type = aux_type_descriptor typ in
      let pos = Position.join (loc_to_pos loc1) (fst e) in
      (pos, A.Cast (cast_type, e))
  | `Poin_exp expr -> aux_pointer_expression expr
  | _ -> (
    Boilerplate.map_expression_not_binary () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: expresion_not_binary"
  )
and aux_update_expression (e: update_expression) =
  (* For typing purposes, we don't care about whether it is pre or post incr/decr*)
  let (op,e) = match e with
  | `Choice_DASHDASH_exp (op, e) -> (op, aux_expression e)
  | `Exp_choice_DASHDASH (e,op) -> (op, aux_expression e)
  in
  let op = match op with 
    | `DASHDASH (loc, _) -> (loc, "--")
    | `PLUSPLUS (loc, _) -> (loc, "++")
  in
  let pos = Position.join (loc_to_pos (fst op)) (fst e) in
  (pos, A.Unop (snd op, e))
and aux_subscription_expression (subs: subscript_expression) =
  let expr, _, index, _ = subs in
  let arr = aux_expression expr in
  let idx = aux_expression index in
  let pos = Position.join (fst arr) (fst idx) in
  (pos, A.Call ((pos, A.Id "[]"), [arr; idx])) (* A usual function? Or a specific operator*)
and aux_pointer_expression (poin: pointer_expression) =
  let op, expr = poin in
  let loc,op = match op with 
    | `AMP (loc, _) -> (loc, "&")
    | `STAR (loc, _) -> (loc, "*")
  in

  let e = aux_expression expr in
  let pos = Position.join (loc_to_pos loc) (fst e) in
  (pos, A.Unop (op, e))
and aux_assign_left_expression (e: assignment_left_expression) : A.e =
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Subs_exp expr -> aux_subscription_expression expr (*Like that so far, but may require special treatment *)
  | `Poin_exp expr -> aux_pointer_expression expr
  | _ -> (
    Boilerplate.map_assignment_left_expression () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: left side of assignment must be an identifier"
  )
and aux_call_expression (call: call_expression) =
  let expr, args = call in
  let func_ast = aux_expression expr in
  let pos1,_ = func_ast in 
  let pos2, args_ast = aux_argument_list args in
  (Position.join pos1 pos2, A.Call (func_ast, args_ast))
and aux_argument (arg: anon_choice_exp_f079e30) =
  match arg with 
  | `Exp e -> aux_expression e
  | `Comp_stmt comp -> aux_body comp
and aux_argument_list (arg_list: argument_list) =
  let (l1, _),args,(l2,_) = arg_list in
  let pos = locs_to_pos l1 l2 in
  match args with 
  | None -> (pos, [])
  | Some (arg1, others) ->
    (pos, aux_argument arg1 ::  List.map (fun (_, arg) -> aux_argument arg) others)

and  aux_comma_expression (expr: anon_choice_exp_55b4dba) =
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let _ = aux_expression e1 in
      aux_comma_expression e2 (* ? *)

and aux_return_statement (ret: return_statement)  =
  let (l1, _), expr_opt, (l2,_) = ret in
  let pos = locs_to_pos l1 l2 in
  match expr_opt with
  | None -> (pos, A.Return None)
  | Some expr -> (pos, Return (Some (aux_comma_expression expr)))

and aux_expression_statement (expr_stmt: expression_statement) =
  match expr_stmt with 
  | (Some comma_expr, _) -> aux_comma_expression comma_expr
  | (None, (loc,_)) -> (loc_to_pos loc, A.Return None)

and aux_for_cond (stmts: for_statement_body) =
  let decl,cond,_,incr = stmts in
  let init = match decl with 
  | `Decl decl -> aux_declarations decl
  | `Opt_choice_exp_SEMI expr -> aux_expression_statement expr
  in
  let cond = Option.map aux_comma_expression cond in 
  let incr = Option.map aux_comma_expression incr in
  init, cond, incr

and aux_for_statement (for_stmt: for_statement) =
  let (loc1,_),_,for_exprs,_,stmt = for_stmt in
  let init, cond, incr = aux_for_cond for_exprs in
  let body = aux_statement stmt in 
  (Position.join (loc_to_pos loc1) (fst body), A.For (init, cond, incr, body))

and aux_while_statement (while_stmt: while_statement) =
  let (loc1, _),cond,body = while_stmt in
  let b = aux_statement body in
  let pos = Position.join (loc_to_pos loc1) (fst b) in
  pos,A.While (aux_paren_expr cond, b)

and aux_do_statement (do_stmt: do_statement) =
  let _,_body,_,_cond,_ = do_stmt in
  failwith "Not supported yet: do statement"

and aux_compound_stmt (stmt: compound_statement) = 
  let (l1,_),block_items,(l2,_) = stmt in
  let stmts = List.map aux_block_item block_items in
  (locs_to_pos l1 l2, A.Seq stmts)

and aux_switch_statement (switch_stmt: switch_statement) =
  let (loc1,_),expr,cases = switch_stmt in
  let expr  = aux_paren_expr expr in
  let body = aux_compound_stmt cases in

  match body with 
  | pos, A.Seq lst -> (Position.join (loc_to_pos loc1) pos, A.Switch (expr, lst))
  | _ -> failwith "Unexpected body in switch statement"

and aux_non_case_statement (stmt: non_case_statement) =
  match stmt with
  | `Ret_stmt ret -> aux_return_statement ret
  | `Exp_stmt exp -> aux_expression_statement exp
  | `If_stmt if_stmt -> aux_if_statement if_stmt
  | `Comp_stmt comp -> aux_body comp
  | `For_stmt for_stmt -> aux_for_statement for_stmt
  | `While_stmt while_stmt -> aux_while_statement while_stmt
  | `Do_stmt do_stmt -> aux_do_statement do_stmt
  | `Brk_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Break)
  | `Cont_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Next)
  | `Goto_stmt ((l1, _),_, (l2, _)) -> (locs_to_pos l1 l2, Return None)
  | `Labe_stmt _ -> failwith "Not supported yet: labeled statements"
  | `Switch_stmt st -> aux_switch_statement st
  | `Attr_stmt _ -> failwith "Not supported yet: attribute statements"
  | _ -> failwith "Not supported yet: Seh_try and Seh_leave statements"

and aux_top_level_statement (stmt: top_level_statement) =
  match stmt with 
  | `Top_level_exp_stmt (Some exp, (loc2,_)) -> 
      let e = aux_not_bin_expression exp in
      let pos = Position.join (fst e) (loc_to_pos loc2) in
      (pos, A.Return (Some e))
  | _ -> failwith "Not supported yet: top level statements without expression"

and aux_case_statement (case_stmt: case_statement) =
  let (case, _, body) = case_stmt in 
  let case_body = List.map (function 
   | `Choice_attr_stmt st -> aux_non_case_statement st
   | `Decl decls -> aux_declarations decls
   | `Type_defi _typs -> failwith "Type definitions in case statements are not supported yet"
  ) body
  in
  let first_loc = 
    match case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let last_loc = 
    match List.rev case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let case_body = (Position.join first_loc last_loc, A.Seq case_body) in
  match case with 
  | `Case_exp ((loc1,_), e) ->
    let e = aux_expression e in
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Case (e, case_body))
  | `Defa ((loc1,_)) ->
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Default (case_body))


and aux_statement (stmt: statement) =
 match stmt with 
 | `Case_stmt st -> aux_case_statement st
 | `Choice_attr_stmt st -> aux_non_case_statement st
and aux_initializer_list (init_list: initializer_list) =
 let (l1,_), initializers, _, (l2,_) = init_list in 
 let pos = locs_to_pos l1 l2 in
 let process_init (init : anon_choice_init_pair_1a6981e) =
   match init with
   | `Exp e -> (let res = aux_expression e in
       match snd res with 
      | A.Const c -> c 
      | _ -> failwith "Not supported yet: initializer lists with non-constant expressions")
   | _ -> failwith "Not supported yet: initializer lists with nested initializer lists or pairs"
 in
 let vals = match initializers with 
  | None -> []
  | Some (init1,inits) ->
    (process_init init1) :: List.map (fun (_, init) -> process_init init) inits
  in
 (pos, A.Const (A.CArray vals)) 
and aux_declaration typ (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
  match decl with 
  |`Init_decl (declr, _, init) -> 
      let level,name = aux_decl_name declr in
      let name = A.Id name in
      let typ = Ast.build_ptr level typ in
      let e = match init with 
        | `Exp expr -> aux_expression expr
        | `Init_list init_list -> aux_initializer_list init_list
      in
      let var_decl = (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, name))) in
      let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
      (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
  | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) -> 
      let name = aux_decl2_name declr in
      (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, A.Id name)))
and aux_declarations ((decl_type, decl1, decls, _loc2): declaration) =
  let typ = aux_decl_spec decl_type in
  (Mlsem.Common.Position.dummy, A.Seq ((aux_declaration typ decl1) :: (List.map (fun (_, d) -> aux_declaration typ d) decls)))
and aux_decl2_name (decl: declaration_declarator) = 
  match decl  with
  | `Id tok -> token_to_string tok
  | _ -> failwith "Not supported yet: complex declaration names"
and aux_block_item (item : block_item) =
  match item with
  | `Stmt stmt -> aux_statement stmt
  | `Decl decls -> aux_declarations decls
  | `Attr_stmt _attr_stmt -> failwith "Not supported yet: attribute statements"
  | `Type_defi _ -> (Mlsem.Common.Position.dummy, Return None) (* Type definitions don't produce values *)
  | `Empty_decl _ -> (Mlsem.Common.Position.dummy, Return None)

  | `Prep_if _ | `Prep_ifdef _ | `Prep_incl _ | `Prep_def _ 
  | `Prep_func_def _ | `Prep_call _ -> 
      (Mlsem.Common.Position.dummy, Return None) (*TODO: Handle preprocessor! *)
      (* TODO: same as a function definition out of function*)
  | `Func_defi _ -> failwith "Function definitions inside a block not supported"
  | _ -> failwith "Not supported yet: old function defi or link spec"

and aux_body body =
  let (l1,_),block_items,(l2,_) = body in
  let stmts = List.map aux_block_item block_items in
  if Utils.is_singleton stmts then
    List.hd stmts
  else
   (locs_to_pos l1 l2, A.Seq stmts)

and aux_paren_expr (p_expr: parenthesized_expression) =
  let (l1,_), expr, (l2,_) = p_expr in
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let ast1 = aux_expression e1 in
      let ast2 = aux_comma_expression e2 in
      (locs_to_pos l1 l2, A.Comma (ast1,ast2))
  | `Comp_stmt comp -> aux_body comp

and aux_if_statement (if_stmt: if_statement) =
  let (l1,_), cond, then_stmt, else_opt = if_stmt in
  let ast_cond = aux_paren_expr cond in
  let ast_then = aux_statement then_stmt in
  let ast_else = Option.map (fun (_,st) -> aux_statement st) else_opt in
  let l2 = Option.map  
    (fun (pos,_) -> Position.end_of_position pos)
    ast_else
    |> Option.value ~default:(Position.end_of_position @@ fst ast_then)
  in

  (Position.lex_join (conv_pos l1.start) l2,A.If (ast_cond, ast_then, ast_else)) 

and aux_prep_func_def (func_def: preproc_function_def) = 
  let ((loc1, _), name, params, body, (loc2,_)) = func_def in
  let name = token_to_string name in
  let _,params,(end_param,_) = params in 
  let aux_prep_param (p: anon_choice_stmt_id_d3c4b5f) = 
    match p with 
    | `Id (_,s) -> s
    | `DOTDOTDOT (loc, _) -> failwith ("Not supported yet: variadic parameter in preprocessor function at " ^ (Position.string_of_lex_pos (conv_pos loc.start)))
  in
  let params = Option.fold ~none:[] ~some:(fun (p1, others) -> 
    aux_prep_param p1 :: List.map (fun (_, p) -> aux_prep_param p) others
  ) params |> 
    List.map (fun p -> (Ast.Any, p)) 
  in
  let body = match body with 
    | None -> (locs_to_pos end_param loc2, A.Return None)
    | Some (_loc3,comp) -> 
      let res = parse_string comp in
      match res.program with
      | None -> failwith ("Failed to parse preprocessor function body for " ^ name)
      | Some tu -> 
        Printf.printf "List length: %d\n" (List.length tu);
        let items = List.filter_map (fun item -> 
          match item with 
          | `Top_level_stmt stmt -> Some (aux_top_level_statement stmt)
          | _ -> Printf.printf "Not supported yet: top level item in define";
              print_top_level_item item; None
        ) tu in 
        List.hd items
  in
  (locs_to_pos loc1 loc2, A.Fundef (Ast.Any, name, params, body))

let aux_top_level_item (item : top_level_item) : A.top_level_unit option =
  match item with
  | `Func_defi (_, decl_spec, _, decl, body) -> (
     let _,name = aux_decl_name decl in 
     (* the part with the parameters can also carry the information about pointers for the return type! *)
     let level, params = aux_params decl in
     let return_type = Ast.build_ptr level (aux_decl_spec decl_spec) in 

     let body = aux_body body in 
     Some (Mlsem.Common.Position.dummy, Fundef (return_type, name, params, body))
     ) 
  | `Prep_func_def func_def -> Some (aux_prep_func_def func_def)
  | _ -> (Printf.printf "Not supported yet: top level item\n";print_top_level_item item ; None)


let aux_translation_unit tree =
  List.filter_map aux_top_level_item tree 

let to_ast (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> [] 
  | Some translation_unit ->
    aux_translation_unit translation_unit