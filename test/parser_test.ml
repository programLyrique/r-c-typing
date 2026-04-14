open R_c_typing

let expect_type_decl name expected_ty defs =
  match List.find_opt (function _, PAst.TypeDecl (decl_name, _) -> decl_name = name | _ -> false) defs with
  | Some (_, PAst.TypeDecl (_, ty)) when ty = expected_ty -> ()
  | Some (_, PAst.TypeDecl (_, ty)) ->
      failwith
        (Printf.sprintf
           "Unexpected type declaration for %s: %s"
           name
           (Ast.show_ctype ty))
  | _ -> failwith (Printf.sprintf "Missing type declaration for %s" name)

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

  let _dynamic_init_list_ast =
    Parser.parse_string
      "int main() { int x = 1; int a[2] = {x, x + 1}; return a[0]; }"
  in
  print_endline "Non-constant initializer list parsed successfully";

  let enum_defs =
    Parser.parse_string
      "enum Color { RED, GREEN = 4, BLUE, CYAN = BLUE + 2 }; typedef enum { DIR_LEFT = -1, DIR_NONE = 0, DIR_RIGHT = 1 } Direction; enum Mask { MASK_READ = 1, MASK_WRITE = 1 << 1, MASK_RW = MASK_READ | MASK_WRITE };"
    |> Parser.to_ast
  in
  expect_type_decl
    "Color"
    (Ast.Enum ("Color", [("RED", Some 0); ("GREEN", Some 4); ("BLUE", Some 5); ("CYAN", None)]))
    enum_defs;
  expect_type_decl
    "Direction"
    (Ast.Enum ("", [("DIR_LEFT", Some (-1)); ("DIR_NONE", Some 0); ("DIR_RIGHT", Some 1)]))
    enum_defs;
  expect_type_decl
    "Mask"
    (Ast.Enum ("Mask", [("MASK_READ", Some 1); ("MASK_WRITE", None); ("MASK_RW", None)]))
    enum_defs;
  print_endline "Enum declarations parsed successfully";