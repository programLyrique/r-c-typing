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

let rec aux_decl_name (decl : declarator) : string =
  match decl with
  | `Id tok -> token_to_string tok
  | `Func_decl (decl, _, _, _) -> aux_decl_name decl
  | `Poin_decl (_,_,_,_,decl) -> aux_decl_name decl (* Should make it clear this is a pointer when it will be useful. *)
  | _ -> failwith "Not supported yet: function name"

let aux_param (p: anon_choice_param_decl_4ac2852) = 
  match p with 
  | `Param_decl (decl_spec, Some (`Decl decl), _) -> 
    let ty = aux_decl_spec decl_spec in
    let name = aux_decl_name decl in
    (ty, name)
  | `Vari_param _ -> failwith "Not supported yet: variable number of parameters (...) in declaration"
  | _ -> failwith "Not supported yet: parameter declaration"

let aux_params (decl: declarator) : A.param list =
  match decl with 
  | `Func_decl (_, (_loc1, `Opt_choice_param_decl_rep_COMMA_choice_param_decl (Some (p1, params)), _loc2), _,_) -> 
     (aux_param p1) :: (List.map (fun (_, p) -> aux_param p) params)
  | _ -> []


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
  | `Str_lit (_prefix,escaped, _) -> 
    let pos_str = List.map unescape escaped in
    let str = String.concat "" (List.map snd pos_str) in
    let start_loc = fst (List.hd pos_str) in
    let end_loc = fst (List.nth pos_str (List.length pos_str - 1) ) in
    (locs_to_pos start_loc end_loc, A.Const (A.CStr str))
  | _ -> failwith "Not supported yet: strings with escaped characters or concatenated strings"
and aux_num_lit  s = 
  A.Const(
    try 
      A.CInt (int_of_string s)
  with 
    | Failure _ -> A.CFloat (Float.of_string s)
  )
    
and aux_not_bin_expression (e : expression_not_binary) = 
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Num_lit (loc, s) -> (loc_to_pos loc, aux_num_lit s)
  | `Null _ -> (Position.dummy, A.Const A.CNull)
  | `Call_exp  call -> aux_call_expression call
  | `Str s -> aux_string s
  | `Assign_exp (e1, `EQ _, e2) -> 
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      (exprs_to_pos lhs rhs, A.VarAssign (lhs, rhs))
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
  | _ -> (
    Boilerplate.map_expression_not_binary () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: not binary expressions"
  )
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

and aux_for_cond (_stmts: for_statement_body) =
  failwith "Not supported yet: for loop condition"

and aux_for_statement (for_stmt: for_statement) =
  let _,_,_body,_,_stmt = for_stmt in
  (* Encode the for loop as a while loop. But rather at PAst -> ast?*)
  failwith "Not supported yet: for statement"

and aux_while_statement (while_stmt: while_statement) =
  let (loc1, _),cond,body = while_stmt in
  let b = aux_statement body in
  let pos = Position.join (loc_to_pos loc1) (fst b) in
  pos,A.While (aux_paren_expr cond, b)

and aux_do_statement (do_stmt: do_statement) =
  let _,_body,_,_cond,_ = do_stmt in
  failwith "Not supported yet: do statement"

and aux_non_case_statement (stmt: non_case_statement) =
  match stmt with
  | `Ret_stmt ret -> aux_return_statement ret
  | `Exp_stmt exp -> aux_expression_statement exp
  | `If_stmt if_stmt -> aux_if_statement if_stmt
  | `Comp_stmt comp -> aux_body comp
  | `For_stmt for_stmt -> aux_for_statement for_stmt
  | `While_stmt while_stmt -> aux_while_statement while_stmt
  | `Do_stmt do_stmt -> aux_do_statement do_stmt
  | `Brk_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, Return None)
  | `Cont_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, Return None)
  | `Goto_stmt ((l1, _),_, (l2, _)) -> (locs_to_pos l1 l2, Return None)
  | `Labe_stmt _ -> (Mlsem.Common.Position.dummy, Return None)
  | `Switch_stmt _ -> (Mlsem.Common.Position.dummy, Return None)
  | `Attr_stmt _ -> failwith "Not supported yet: attribute statements"
  | _ -> failwith "Not supported yet: Seh_try and Seh_leave statements"

and aux_statement (stmt: statement) =
 match stmt with 
 | `Case_stmt _ -> failwith "Case statements are not supported yet"
 | `Choice_attr_stmt st -> aux_non_case_statement st
and aux_declaration typ (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
  match decl with 
  |`Init_decl (declr, _, `Exp exp) -> 
      let name = A.Id (aux_decl_name declr) in
      let e = aux_expression exp in
      let var_decl = (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, name))) in
      let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
      (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
  | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) -> 
      let name = aux_decl2_name declr in
      (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, A.Id name)))
  | _ -> failwith "Not supported yet: other declaration types or initializer list"
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

  (Position.lex_join (conv_pos l1.start) l2,A.If (ast_cond, ast_then, ast_else)) (* TODO *)

let aux_top_level_item (item : top_level_item) : A.top_level_unit option =
  match item with
  | `Func_defi (_, decl_spec, _, decl, body) -> (
     let return_type = aux_decl_spec decl_spec in 
     let name = aux_decl_name decl in 
     let params = aux_params decl in
     let body = aux_body body in 
     Some (Mlsem.Common.Position.dummy, Fundef (return_type, name, params, body))
     ) 
  | _ -> (Printf.printf "Not supported yet: top level item\n";print_top_level_item item ; None)


let aux_translation_unit tree =
  List.filter_map aux_top_level_item tree 

let to_ast (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> [] 
  | Some translation_unit ->
    aux_translation_unit translation_unit