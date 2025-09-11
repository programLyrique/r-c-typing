let main _ = 
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <c_file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = R_c_typing.Parser.parse_file filename in
    ()
    
let () = main ()