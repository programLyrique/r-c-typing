open Tree_sitter_c
open Tree_sitter_run
open CST
module A = Ast

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



let aux_primitive_type t = 
  match t with 
  | "void" -> A.Void
  | "char" -> A.Char
  | "short" | "int" | "long" -> A.Int
  | "float" | "double" -> A.Float
  | _ -> failwith ("Unknown primitive type: " ^ t)

let aux_struct _struc = []


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
  | `Id tok -> let (_loc, s) = tok in s
  | `Func_decl (decl, _, _, _) -> aux_fun_name decl
  | _ -> failwith "Not supported yet: function name"

let aux_params _params = []

let aux_expression (_expr: expression) : A.e =
  (Mlsem.Common.Position.dummy, A.Return None) (* TODO *)

let rec aux_comma_expression (expr: anon_choice_exp_55b4dba) =
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let _ = aux_expression e1 in
      aux_comma_expression e2

let aux_return_statement (ret: return_statement)  =
  let _, expr_opt, _ = ret in
  match expr_opt with
  | None -> (Mlsem.Common.Position.dummy, A.Return None)
  | Some expr -> (Mlsem.Common.Position.dummy, Return (Some (aux_comma_expression expr)))

let aux_expression_statement (expr_stmt: expression_statement) =
  match expr_stmt with 
  | (Some comma_expr, _) -> aux_comma_expression comma_expr
  | (None, _) -> (Mlsem.Common.Position.dummy, A.Return None)

let aux_if_statement (if_stmt: if_statement) =
  let _, _ond, _then_stmt, _else_opt = if_stmt in
  (Mlsem.Common.Position.dummy, A.Return None) (* TODO *)

let aux_for_statement (for_stmt: for_statement) =
  let _,_,_body,_,_stmt = for_stmt in
  failwith "Not supported yet: for statement"

let aux_while_statement (while_stmt: while_statement) =
  let _,_cond,_body = while_stmt in
  failwith "Not supported yet: while statement"

let aux_do_statement (do_stmt: do_statement) =
  let _,_body,_,_cond,_ = do_stmt in
  failwith "Not supported yet: do statement"

let rec aux_non_case_statement (stmt: non_case_statement) =
  match stmt with
  | `Ret_stmt ret -> aux_return_statement ret
  | `Exp_stmt exp -> aux_expression_statement exp
  | `If_stmt if_stmt -> aux_if_statement if_stmt
  | `Comp_stmt comp -> aux_body comp
  | `For_stmt for_stmt -> aux_for_statement for_stmt
  | `While_stmt while_stmt -> aux_while_statement while_stmt
  | `Do_stmt do_stmt -> aux_do_statement do_stmt
  | `Brk_stmt _ -> (Mlsem.Common.Position.dummy, Return None)
  | `Cont_stmt _ -> (Mlsem.Common.Position.dummy, Return None)
  | `Goto_stmt _ -> (Mlsem.Common.Position.dummy, Return None)
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
  let _,block_items,_ = body in
  let stmts = List.map aux_block_item block_items in
  if Utils.is_singleton stmts then
    List.hd stmts
  else
   (Mlsem.Common.Position.dummy, A.Seq stmts)

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