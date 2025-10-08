open R_c_typing
open Cmdliner

module StrMap = Map.Make(String)

let main cst_opt ast_opt ast2_opt filename =
  let cst = Parser.parse_file filename in
  if cst_opt then Parser.print_res cst;
  let ast = Parser.to_ast cst in
  if ast_opt then
    Printf.printf "%s\n" (PAst.show_definitions ast);
  if ast2_opt then
    let env = {PAst.id = StrMap.empty} in
    let asts = List.map (PAst.transform env) ast in 
    Printf.printf "%s\n" (Ast.show_funcs asts);
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
     and+ filename = file_arg in
     main cst ast ast2 filename)

let () = exit (Cmd.eval cmd)