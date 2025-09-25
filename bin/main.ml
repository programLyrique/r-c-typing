open R_c_typing
open Cmdliner


let main cst ast filename =
  if cst then
    let cst = Parser.parse_file filename in
    Parser.print_res cst
  else if ast then
    let cst = Parser.parse_file filename in
    Printf.printf "%s\n" (Ast.show_definitions (Parser.to_ast cst))
  else
    Printf.eprintf "Usage: %s [--cst|--ast] <c_file>\n" Sys.argv.(0)

let cst_opt =
  let doc = "Print CST (concrete syntax tree)" in
  Arg.(value & flag & info ["cst"] ~doc)

let ast_opt =
  let doc = "Print AST (abstract syntax tree)" in
  Arg.(value & flag & info ["ast"] ~doc)

let file_arg =
  let doc = "C source file to parse" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let open Term.Syntax in
  Cmd.v
    (Cmd.info "r-c-typing")
    (let+ cst = cst_opt
     and+ ast = ast_opt
     and+ filename = file_arg in
     main cst ast filename)

let () = exit (Cmd.eval cmd)