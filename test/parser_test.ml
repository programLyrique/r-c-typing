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

  let _char_ast = Parser.parse_string "int main() { char c = '\\n'; return c; }" in
  print_endline "Escaped char literal parsed successfully";

  let _prefixed_char_ast = Parser.parse_string "int main() { int c = u'\\u0041'; return c; }" in
  print_endline "Prefixed char literal parsed successfully";

  let _sized_type_ast = Parser.parse_string "int main() { unsigned long x = 1; return (int)x; }" in
  print_endline "Sized type specifier parsed successfully";

  let _sizeof_expr_ast = Parser.parse_string "int main() { int x = 1; return sizeof x; }" in
  print_endline "sizeof expression parsed successfully";

  let _sizeof_type_ast = Parser.parse_string "int main() { return sizeof(int); }" in
  print_endline "sizeof type parsed successfully";

  let _do_while_ast = Parser.parse_string "int main() { int i = 0; do { i = i + 1; } while (i < 3); return i; }" in
  print_endline "do while parsed successfully";

  let _nested_init_list_ast =
    Parser.parse_string
      "int main() { int m[2][2] = {{1, 2}, {3, 4}}; return m[0][0]; }"
  in
  print_endline "Nested initializer list parsed successfully";

  let _init_pair_ast =
    Parser.parse_string
      "typedef struct { int x; int y; } P; int main() { P p = {.x = 1, .y = 2}; return p.x; }"
  in
  print_endline "Initializer pair parsed successfully";