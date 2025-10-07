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



let aux_primitive_type t = 
  match t with 
  | "void" -> A.Void
  | "char" -> A.Char
  | "short" | "int" | "long" -> A.Int
  | "float" | "double" -> A.Float
  | _ -> failwith ("Unknown primitive type: " ^ t)

let aux_struct _struc = []


let token_to_string (_loc, s) = s

let aux_type_spec (type_spec : type_specifier)  =
  match type_spec with
  | `Prim_type tok -> let (_loc, s) = tok in aux_primitive_type s
  | `Id (_loc, s) when s = "SEXP" -> A.SEXP
  | `Id _ -> A.Any
  | `Struct_spec _ -> failwith "Not supported yet"
  | _ -> failwith "Not supported yet: type specifier"

let aux_decl_spec decl_spec = 
  let (_, type_spec, _) = decl_spec in
  aux_type_spec type_spec

let rec aux_fun_name (decl : declarator) : string =
  match decl with
  | `Id tok -> token_to_string tok
  | `Func_decl (decl, _, _, _) -> aux_fun_name decl
  | _ -> failwith "Not supported yet: function name"

let aux_params _params = []


let rec aux_expression (expr: expression) : A.e =
  match expr with 
  | `Choice_cond_exp e -> aux_not_bin_expression e
  | `Bin_exp e -> aux_bin_expression e
and aux_bin_expression (e: binary_expression) =
  match e with 
  | `Exp_PLUS_exp (e1, _, e2) -> 
      (Mlsem.Common.Position.dummy, A.Binop ("+", (aux_expression e1, aux_expression e2)))
  | `Exp_DASH_exp (e1, _, e2) -> 
      (Mlsem.Common.Position.dummy, A.Binop ("-", (aux_expression e1, aux_expression e2)))
  | `Exp_STAR_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("*", (aux_expression e1, aux_expression e2)))
  | `Exp_SLASH_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("/", (aux_expression e1, aux_expression e2)))
  | `Exp_PERC_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("%", (aux_expression e1, aux_expression e2)))
  | `Exp_BARBAR_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("||", (aux_expression e1, aux_expression e2)))
  | `Exp_AMPAMP_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("&&", (aux_expression e1, aux_expression e2)))
  | `Exp_BAR_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("|", (aux_expression e1, aux_expression e2)))
  | `Exp_HAT_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("^", (aux_expression e1, aux_expression e2)))
  | `Exp_AMP_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("&", (aux_expression e1, aux_expression e2)))
  | `Exp_EQEQ_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("==", (aux_expression e1, aux_expression e2)))
  | `Exp_BANGEQ_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("!=", (aux_expression e1, aux_expression e2)))
  | `Exp_GT_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop (">", (aux_expression e1, aux_expression e2)))
  | `Exp_GTEQ_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop (">=", (aux_expression e1, aux_expression e2)))
  | `Exp_LTEQ_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("<=", (aux_expression e1, aux_expression e2)))
  | `Exp_LT_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("<", (aux_expression e1, aux_expression e2)))
  | `Exp_LTLT_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop ("<<", (aux_expression e1, aux_expression e2)))
  | `Exp_GTGT_exp (e1, _, e2) ->
      (Mlsem.Common.Position.dummy, A.Binop (">>", (aux_expression e1, aux_expression e2)))

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
and aux_not_bin_expression (e : expression_not_binary) = 
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Num_lit (loc, s) -> (loc_to_pos loc, A.Const (A.CInt (int_of_string s)))
  | `Null _ -> (Position.dummy, A.Const A.CNull)
  | `Call_exp  call -> aux_call_expression call
  | `Str s -> aux_string s
  | _ -> (
    Boilerplate.map_expression_not_binary () e |> Tree_sitter_run.Raw_tree.to_channel stdout ;
    failwith "Not supported yet: not binary expressions"
  )
and aux_call_expression (call: call_expression) =
  let expr, args = call in
  let func_ast = aux_expression expr in
  let args_ast = aux_argument_list args in
  (Mlsem.Common.Position.dummy, A.Call (func_ast, args_ast))
and aux_argument (arg: anon_choice_exp_f079e30) =
  match arg with 
  | `Exp e -> aux_expression e
  | `Comp_stmt comp -> aux_body comp
and aux_argument_list (arg_list: argument_list) =
  let _,args,_ = arg_list in
  match args with 
  | None -> []
  | Some (arg1, others) ->
    aux_argument arg1 ::  List.map (fun (_, arg) -> aux_argument arg) others

and  aux_comma_expression (expr: anon_choice_exp_55b4dba) =
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let _ = aux_expression e1 in
      aux_comma_expression e2

and aux_return_statement (ret: return_statement)  =
  let _, expr_opt, _ = ret in
  match expr_opt with
  | None -> (Mlsem.Common.Position.dummy, A.Return None)
  | Some expr -> (Mlsem.Common.Position.dummy, Return (Some (aux_comma_expression expr)))

and aux_expression_statement (expr_stmt: expression_statement) =
  match expr_stmt with 
  | (Some comma_expr, _) -> aux_comma_expression comma_expr
  | (None, _) -> (Mlsem.Common.Position.dummy, A.Return None)


and aux_for_statement (for_stmt: for_statement) =
  let _,_,_body,_,_stmt = for_stmt in
  failwith "Not supported yet: for statement"

and aux_while_statement (while_stmt: while_statement) =
  let _,_cond,_body = while_stmt in
  failwith "Not supported yet: while statement"

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

and aux_block_item (item : block_item) =
  match item with
  | `Stmt stmt -> aux_statement stmt
  | `Decl _decl -> failwith "Not supported yet: declarations"
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

  (Position.lex_join (conv_pos l1.start) l2,A.Ite (ast_cond, ast_then, ast_else)) (* TODO *)

let aux_top_level_item (item : top_level_item) : A.top_level_unit option =
  match item with
  | `Func_defi (_, decl_spec, _, decl, body) -> (
     let return_type = aux_decl_spec decl_spec in 
     let name = aux_fun_name decl in 
     let params = aux_params decl in
     let body = aux_body body in 
     Some (Mlsem.Common.Position.dummy, Fundef (return_type, name, params, body))
     ) 
  | _ -> (Printf.printf "Not supported yet: top level item\n";None)


let aux_translation_unit tree =
  List.filter_map aux_top_level_item tree 

let to_ast (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> [] 
  | Some translation_unit ->
    aux_translation_unit translation_unit