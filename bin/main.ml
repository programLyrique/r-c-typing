open R_c_typing

let main _ = 
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <c_file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = Parser.parse_file filename in
    Parser.print_parse_result ast
    

let () = main ()