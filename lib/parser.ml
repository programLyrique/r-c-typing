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

let aux_body _body = (Mlsem.Common.Position.dummy, A.Return None)

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