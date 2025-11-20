open R_c_typing
open Cmdliner

module StrMap = Map.Make(String)

let main cst_opt ast_opt ast2_opt mlsem_opt filename =
  let cst = Parser.parse_file filename in
  if cst_opt then Parser.print_res cst;
  let ast = Parser.to_ast cst in
  if ast_opt then
    Printf.printf "%s\n" (PAst.show_definitions ast);
  let env = {PAst.id = StrMap.empty} in
  let asts = List.map (PAst.transform env) ast in 
  if ast2_opt then
    Printf.printf "%s\n" (Ast.show_funcs asts);
  let mlsem_asts = List.map Ast.to_mlsem asts in
  if mlsem_opt then
    List.iter (fun mlsem_ast ->
      Format.printf "%a@." Mlsem.System.Ast.pp mlsem_ast
    ) mlsem_asts;
  ()

let cst_opt =
  let doc = "Print CST (concrete syntax tree)" in
  Arg.(value & flag & info ["cst"] ~doc)

let ast_opt =
  let doc = "Print AST (abstract syntax tree)" in
  Arg.(value & flag & info ["ast"] ~doc)

let ast2_opt =
  let doc = "Print AST2 (typed abstract syntax tree)" in
  Arg.(value & flag & info ["ast2"] ~doc)

let mlsem_opt = 
  let doc = "Print MLsem AST" in
  Arg.(value & flag & info ["mlsem"] ~doc)

let file_arg =
  let doc = "C source file to parse" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let open Term.Syntax in
  Cmd.v
    (Cmd.info "r-c-typing")
    (let+ cst = cst_opt
     and+ ast = ast_opt
     and+ ast2 = ast2_opt
     and+ mlsem = mlsem_opt
     and+ filename = file_arg in
     main cst ast ast2 mlsem filename)

let () = exit (Cmd.eval cmd)