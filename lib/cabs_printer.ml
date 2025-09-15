open Cabs 

let rec string_of_size = function
  | NO_SIZE -> "NO_SIZE"
  | SHORT -> "SHORT"
  | LONG -> "LONG"
  | LONG_LONG -> "LONG_LONG"

and string_of_sign = function
  | NO_SIGN -> "NO_SIGN"
  | SIGNED -> "SIGNED"
  | UNSIGNED -> "UNSIGNED"

and string_of_storage = function
  | NO_STORAGE -> "NO_STORAGE"
  | AUTO -> "AUTO"
  | STATIC -> "STATIC"
  | EXTERN -> "EXTERN"
  | REGISTER -> "REGISTER"

and string_of_base_type = function
  | NO_TYPE -> "NO_TYPE"
  | VOID -> "VOID"
  | BOOL -> "BOOL"
  | CHAR sign -> "CHAR(" ^ string_of_sign sign ^ ")"
  | INT (size, sign) -> "INT(" ^ string_of_size size ^ ", " ^ string_of_sign sign ^ ")"
  | BITFIELD (sign, expr) -> "BITFIELD(" ^ string_of_sign sign ^ ", " ^ string_of_expression expr ^ ")"
  | FLOAT long -> "FLOAT(" ^ string_of_bool long ^ ")"
  | DOUBLE long -> "DOUBLE(" ^ string_of_bool long ^ ")"
  | COMPLEX_FLOAT -> "COMPLEX_FLOAT"
  | COMPLEX_DOUBLE -> "COMPLEX_DOUBLE"
  | COMPLEX_LONG_DOUBLE -> "COMPLEX_LONG_DOUBLE"
  | PTR bt -> "PTR(" ^ string_of_base_type bt ^ ")"
  | RESTRICT_PTR bt -> "RESTRICT_PTR(" ^ string_of_base_type bt ^ ")"
  | ARRAY (bt, expr) -> "ARRAY(" ^ string_of_base_type bt ^ ", " ^ string_of_expression expr ^ ")"
  | STRUCT (name, ngs) -> "STRUCT(" ^ name ^ ", [" ^ String.concat "; " (List.map string_of_name_group ngs) ^ "])"
  | UNION (name, ngs) -> "UNION(" ^ name ^ ", [" ^ String.concat "; " (List.map string_of_name_group ngs) ^ "])"
  | PROTO proto -> "PROTO(" ^ string_of_proto proto ^ ")"
  | OLD_PROTO old_proto -> "OLD_PROTO(" ^ string_of_old_proto old_proto ^ ")"
  | NAMED_TYPE name -> "NAMED_TYPE(" ^ name ^ ")"
  | ENUM (name, items) -> "ENUM(" ^ name ^ ", [" ^ String.concat "; " (List.map string_of_enum_item items) ^ "])"
  | CONST bt -> "CONST(" ^ string_of_base_type bt ^ ")"
  | VOLATILE bt -> "VOLATILE(" ^ string_of_base_type bt ^ ")"
  | GNU_TYPE (attrs, bt) -> "GNU_TYPE([" ^ String.concat "; " (List.map string_of_gnu_attr attrs) ^ "], " ^ string_of_base_type bt ^ ")"
  | BUILTIN_TYPE name -> "BUILTIN_TYPE(" ^ name ^ ")"
  | TYPE_LINE (file, line, bt) -> "TYPE_LINE(" ^ file ^ ", " ^ string_of_int line ^ ", " ^ string_of_base_type bt ^ ")"

and string_of_name (name, bt, attrs, expr) =
  "(" ^ name ^ ", " ^ string_of_base_type bt ^ ", [" ^ String.concat "; " (List.map string_of_gnu_attr attrs) ^ "], " ^ string_of_expression expr ^ ")"

and string_of_name_group (bt, storage, names) =
  "(" ^ string_of_base_type bt ^ ", " ^ string_of_storage storage ^ ", [" ^ String.concat "; " (List.map string_of_name names) ^ "])"

and string_of_single_name (bt, storage, name) =
  "(" ^ string_of_base_type bt ^ ", " ^ string_of_storage storage ^ ", " ^ string_of_name name ^ ")"

and string_of_enum_item (name, expr) =
  "(" ^ name ^ ", " ^ string_of_expression expr ^ ")"

and string_of_proto (bt, names, variadic) =
  "(" ^ string_of_base_type bt ^ ", [" ^ String.concat "; " (List.map string_of_single_name names) ^ "], " ^ string_of_bool variadic ^ ")"

and string_of_old_proto (bt, names, variadic) =
  "(" ^ string_of_base_type bt ^ ", [" ^ String.concat "; " names ^ "], " ^ string_of_bool variadic ^ ")"

and string_of_definition = function
  | FUNDEF (sn, body) -> "FUNDEF(" ^ string_of_single_name sn ^ ", " ^ string_of_body body ^ ")"
  | OLDFUNDEF (sn, ngs, body) -> "OLDFUNDEF(" ^ string_of_single_name sn ^ ", [" ^ String.concat "; " (List.map string_of_name_group ngs) ^ "], " ^ string_of_body body ^ ")"
  | DECDEF ng -> "DECDEF(" ^ string_of_name_group ng ^ ")"
  | TYPEDEF (ng, attrs) -> "TYPEDEF(" ^ string_of_name_group ng ^ ", [" ^ String.concat "; " (List.map string_of_gnu_attr attrs) ^ "])"
  | ONLYTYPEDEF ng -> "ONLYTYPEDEF(" ^ string_of_name_group ng ^ ")"

and string_of_file defs =
  "[" ^ String.concat "; " (List.map string_of_definition defs) ^ "]"

and string_of_body (defs, stmt) =
  "([" ^ String.concat "; " (List.map string_of_definition defs) ^ "], " ^ string_of_statement stmt ^ ")"

and string_of_statement = function
  | NOP -> "NOP"
  | COMPUTATION expr -> "COMPUTATION(" ^ string_of_expression expr ^ ")"
  | BLOCK body -> "BLOCK(" ^ string_of_body body ^ ")"
  | SEQUENCE (stmt1, stmt2) -> "SEQUENCE(" ^ string_of_statement stmt1 ^ ", " ^ string_of_statement stmt2 ^ ")"
  | IF (expr, stmt1, stmt2) -> "IF(" ^ string_of_expression expr ^ ", " ^ string_of_statement stmt1 ^ ", " ^ string_of_statement stmt2 ^ ")"
  | WHILE (expr, stmt) -> "WHILE(" ^ string_of_expression expr ^ ", " ^ string_of_statement stmt ^ ")"
  | DOWHILE (expr, stmt) -> "DOWHILE(" ^ string_of_expression expr ^ ", " ^ string_of_statement stmt ^ ")"
  | FOR (expr1, expr2, expr3, stmt) -> "FOR(" ^ string_of_expression expr1 ^ ", " ^ string_of_expression expr2 ^ ", " ^ string_of_expression expr3 ^ ", " ^ string_of_statement stmt ^ ")"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | RETURN expr -> "RETURN(" ^ string_of_expression expr ^ ")"
  | SWITCH (expr, stmt) -> "SWITCH(" ^ string_of_expression expr ^ ", " ^ string_of_statement stmt ^ ")"
  | CASE (expr, stmt) -> "CASE(" ^ string_of_expression expr ^ ", " ^ string_of_statement stmt ^ ")"
  | DEFAULT stmt -> "DEFAULT(" ^ string_of_statement stmt ^ ")"
  | LABEL (label, stmt) -> "LABEL(" ^ label ^ ", " ^ string_of_statement stmt ^ ")"
  | GOTO label -> "GOTO(" ^ label ^ ")"
  | ASM str -> "ASM(" ^ str ^ ")"
  | GNU_ASM (str, args1, args2, strs) -> "GNU_ASM(" ^ str ^ ", [" ^ String.concat "; " (List.map string_of_gnu_asm_arg args1) ^ "], [" ^ String.concat "; " (List.map string_of_gnu_asm_arg args2) ^ "], [" ^ String.concat "; " strs ^ "])"
  | STAT_LINE (stmt, file, line) -> "STAT_LINE(" ^ string_of_statement stmt ^ ", " ^ file ^ ", " ^ string_of_int line ^ ")"

and string_of_gnu_asm_arg (str1, str2, expr) =
  "(" ^ str1 ^ ", " ^ str2 ^ ", " ^ string_of_expression expr ^ ")"

and string_of_binary_operator = function
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | BAND -> "BAND"
  | BOR -> "BOR"
  | XOR -> "XOR"
  | SHL -> "SHL"
  | SHR -> "SHR"
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | ASSIGN -> "ASSIGN"
  | ADD_ASSIGN -> "ADD_ASSIGN"
  | SUB_ASSIGN -> "SUB_ASSIGN"
  | MUL_ASSIGN -> "MUL_ASSIGN"
  | DIV_ASSIGN -> "DIV_ASSIGN"
  | MOD_ASSIGN -> "MOD_ASSIGN"
  | BAND_ASSIGN -> "BAND_ASSIGN"
  | BOR_ASSIGN -> "BOR_ASSIGN"
  | XOR_ASSIGN -> "XOR_ASSIGN"
  | SHL_ASSIGN -> "SHL_ASSIGN"
  | SHR_ASSIGN -> "SHR_ASSIGN"

and string_of_unary_operator = function
  | MINUS -> "MINUS"
  | PLUS -> "PLUS"
  | NOT -> "NOT"
  | BNOT -> "BNOT"
  | MEMOF -> "MEMOF"
  | ADDROF -> "ADDROF"
  | PREINCR -> "PREINCR"
  | PREDECR -> "PREDECR"
  | POSINCR -> "POSINCR"
  | POSDECR -> "POSDECR"

and string_of_expression = function
  | NOTHING -> "NOTHING"
  | UNARY (op, expr) -> "UNARY(" ^ string_of_unary_operator op ^ ", " ^ string_of_expression expr ^ ")"
  | BINARY (op, expr1, expr2) -> "BINARY(" ^ string_of_binary_operator op ^ ", " ^ string_of_expression expr1 ^ ", " ^ string_of_expression expr2 ^ ")"
  | QUESTION (expr1, expr2, expr3) -> "QUESTION(" ^ string_of_expression expr1 ^ ", " ^ string_of_expression expr2 ^ ", " ^ string_of_expression expr3 ^ ")"
  | CAST (bt, expr) -> "CAST(" ^ string_of_base_type bt ^ ", " ^ string_of_expression expr ^ ")"
  | CALL (expr, exprs) -> "CALL(" ^ string_of_expression expr ^ ", [" ^ String.concat "; " (List.map string_of_expression exprs) ^ "])"
  | COMMA exprs -> "COMMA([" ^ String.concat "; " (List.map string_of_expression exprs) ^ "])"
  | CONSTANT const -> "CONSTANT(" ^ string_of_constant const ^ ")"
  | VARIABLE str -> "VARIABLE(" ^ str ^ ")"
  | EXPR_SIZEOF expr -> "EXPR_SIZEOF(" ^ string_of_expression expr ^ ")"
  | TYPE_SIZEOF bt -> "TYPE_SIZEOF(" ^ string_of_base_type bt ^ ")"
  | INDEX (expr1, expr2) -> "INDEX(" ^ string_of_expression expr1 ^ ", " ^ string_of_expression expr2 ^ ")"
  | MEMBEROF (expr, str) -> "MEMBEROF(" ^ string_of_expression expr ^ ", " ^ str ^ ")"
  | MEMBEROFPTR (expr, str) -> "MEMBEROFPTR(" ^ string_of_expression expr ^ ", " ^ str ^ ")"
  | GNU_BODY body -> "GNU_BODY(" ^ string_of_body body ^ ")"
  | DESIGNATED (str, expr) -> "DESIGNATED(" ^ str ^ ", " ^ string_of_expression expr ^ ")"
  | EXPR_LINE (expr, file, line) -> "EXPR_LINE(" ^ string_of_expression expr ^ ", " ^ file ^ ", " ^ string_of_int line ^ ")"

and string_of_constant = function
  | CONST_INT str -> "CONST_INT(" ^ str ^ ")"
  | CONST_FLOAT str -> "CONST_FLOAT(" ^ str ^ ")"
  | CONST_CHAR str -> "CONST_CHAR(" ^ str ^ ")"
  | CONST_STRING str -> "CONST_STRING(" ^ str ^ ")"
  | CONST_COMPOUND exprs -> "CONST_COMPOUND([" ^ String.concat "; " (List.map string_of_expression exprs) ^ "])"

and string_of_gnu_attr = function
  | GNU_NONE -> "GNU_NONE"
  | GNU_CALL (str, attrs) -> "GNU_CALL(" ^ str ^ ", [" ^ String.concat "; " (List.map string_of_gnu_attr attrs) ^ "])"
  | GNU_ID str -> "GNU_ID(" ^ str ^ ")"
  | GNU_CST const -> "GNU_CST(" ^ string_of_constant const ^ ")"
  | GNU_EXTENSION -> "GNU_EXTENSION"
  | GNU_INLINE -> "GNU_INLINE"
  | GNU_TYPE_ARG (bt, storage) -> "GNU_TYPE_ARG(" ^ string_of_base_type bt ^ ", " ^ string_of_storage storage ^ ")"

and string_of_gnu_attrs attrs =
  "[" ^ String.concat "; " (List.map string_of_gnu_attr attrs) ^ "]"

and string_of_bool b = if b then "true" else "false"
