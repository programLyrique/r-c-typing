(**
  Parse a file using FrontC and return the AST

  @param filename The path to the C file to parse
*)
let parse_file filename = 
  let cabs = Frontc.parse_file filename Out_channel.stderr in
  cabs

let parse_string s = 
  In_channel_ext.with_string s ~f:(fun ic ->
      let cabs = Frontc.parse_channel ic Out_channel.stderr in
      cabs
  )


type cabs = [%import: Cabs.file] [@@deriving show]