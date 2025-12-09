open R_c_typing

let () =
  let _ast = Parser.parse_string "int main() { return 0; }" in
  (*Format.printf "%a@." (Pp.to_format ast)*)
  print_endline "Parsed successfully";
  
  (* Test cast expressions *)
  let _cast_ast = Parser.parse_string "int main() { double x = 3.14; int y = (int)x; return y; }" in
  print_endline "Cast expression parsed successfully";
  
  let _nested_cast_ast = Parser.parse_string "int main() { double x = 3.7; double y = 2.3; int z = (int)(x + y); return z; }" in
  print_endline "Nested cast expression parsed successfully";