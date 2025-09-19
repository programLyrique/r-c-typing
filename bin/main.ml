open R_c_typing

let main _ = 
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <c_file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    (* Add optional call to pre-processor?
      cpp -E -I ~/Documents/RLanguage/R-4.5.0/src/include test/cprogs/small.c -o test/cprogs/small-p.c
    *)
    let cst = Parser.parse_file filename in
    Parser.print_res cst;
    Printf.printf "%s\n" (Ast.show_definitions (Parser.to_ast cst));
    ()
    

let () = main ()