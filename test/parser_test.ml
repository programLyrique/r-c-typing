open R_c_typing

let () =
  let _ast = Parser.parse_string "int main() { return 0; }" in
  (*Format.printf "%a@." (Pp.to_format ast)*)
  print_endline "Parsed successfully"