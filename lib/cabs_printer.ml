open Cabs

let indent level =
  String.make (level * 2) ' '

let new_line level =
  "\n" ^ indent level

let rec string_of_size _ = function
  | NO_SIZE -> "NO_SIZE"
  | SHORT -> "SHORT"
  | LONG -> "LONG"
  | LONG_LONG -> "LONG_LONG"

and string_of_sign _ = function
  | NO_SIGN -> "NO_SIGN"
  | SIGNED -> "SIGNED"
  | UNSIGNED -> "UNSIGNED"

and string_of_storage _ = function
  | NO_STORAGE -> "NO_STORAGE"
  | AUTO -> "AUTO"
  | STATIC -> "STATIC"
  | EXTERN -> "EXTERN"
  | REGISTER -> "REGISTER"

and string_of_base_type level = function
  | NO_TYPE -> "NO_TYPE"
  | VOID -> "VOID"
  | BOOL -> "BOOL"
  | CHAR sign -> "CHAR(" ^ string_of_sign level sign ^ ")"
  | INT (size, sign) ->
      "INT(" ^ string_of_size level size ^ ", " ^ string_of_sign level sign ^ ")"
  | BITFIELD (sign, expr) ->
      "BITFIELD(" ^ string_of_sign level sign ^ ", " ^ string_of_expression level expr ^ ")"
  | FLOAT long -> "FLOAT(" ^ string_of_bool level long ^ ")"
  | DOUBLE long -> "DOUBLE(" ^ string_of_bool level long ^ ")"
  | COMPLEX_FLOAT -> "COMPLEX_FLOAT"
  | COMPLEX_DOUBLE -> "COMPLEX_DOUBLE"
  | COMPLEX_LONG_DOUBLE -> "COMPLEX_LONG_DOUBLE"
  | PTR bt -> "PTR(" ^ string_of_base_type level bt ^ ")"
  | RESTRICT_PTR bt -> "RESTRICT_PTR(" ^ string_of_base_type level bt ^ ")"
  | ARRAY (bt, expr) ->
      "ARRAY(" ^ string_of_base_type level bt ^ ", " ^ string_of_expression level expr ^ ")"
  | STRUCT (name, ngs) ->
      let ngs_str = String.concat "; " (List.map (string_of_name_group level) ngs) in
      "STRUCT(" ^ name ^ ", [" ^ ngs_str ^ "])"
  | UNION (name, ngs) ->
      let ngs_str = String.concat "; " (List.map (string_of_name_group level) ngs) in
      "UNION(" ^ name ^ ", [" ^ ngs_str ^ "])"
  | PROTO proto -> "PROTO(" ^ new_line (level + 1) ^ string_of_proto (level+1) proto ^ ")"
  | OLD_PROTO old_proto -> "OLD_PROTO(" ^ string_of_old_proto level old_proto ^ ")"
  | NAMED_TYPE name -> "NAMED_TYPE(" ^ name ^ ")"
  | ENUM (name, items) ->
      let items_str = String.concat "; " (List.map (string_of_enum_item level) items) in
      "ENUM(" ^ name ^ ", [" ^ items_str ^ "])"
  | CONST bt -> "CONST(" ^ string_of_base_type level bt ^ ")"
  | VOLATILE bt -> "VOLATILE(" ^ string_of_base_type level bt ^ ")"
  | GNU_TYPE (attrs, bt) ->
      let attrs_str = String.concat "; " (List.map (string_of_gnu_attr level) attrs) in
      "GNU_TYPE([" ^ attrs_str ^ "], " ^ string_of_base_type level bt ^ ")"
  | BUILTIN_TYPE name -> "BUILTIN_TYPE(" ^ name ^ ")"
  | TYPE_LINE (file, line, bt) ->
      "TYPE_LINE(" ^ file ^ ", " ^ string_of_int line ^ ", " ^ string_of_base_type level bt ^ ")"

and string_of_name level (name, bt, attrs, expr) =
  "(" ^ name ^ ", " ^ string_of_base_type level bt ^ ", [" ^
  String.concat "; " (List.map (string_of_gnu_attr level) attrs) ^ "], " ^
  string_of_expression level expr ^ ")"

and string_of_name_group level (bt, storage, names) =
  "(" ^ string_of_base_type level bt ^ ", " ^ string_of_storage level storage ^ ", [" ^
  String.concat "; " (List.map (string_of_name level) names) ^ "])"

and string_of_single_name level (bt, storage, name) =
  "(" ^ string_of_base_type level bt ^ ", " ^ string_of_storage level storage ^
  ", " ^ string_of_name level name ^ ")"

and string_of_enum_item level (name, expr) =
  "(" ^ name ^ ", " ^ string_of_expression level expr ^ ")"

and string_of_proto level (bt, names, variadic) =
  "(" ^ string_of_base_type level bt ^ "," ^ new_line (level + 1) ^ "[" ^
  String.concat "; " (List.map (string_of_single_name (level+1)) names) ^ "], " ^
  string_of_bool (level+1) variadic ^ ")"

and string_of_old_proto level (bt, names, variadic) =
  "(" ^ string_of_base_type level bt ^ ", [" ^ String.concat "; " names ^ "], " ^
  string_of_bool level variadic ^ ")"

and string_of_definition level = function
  | FUNDEF (sn, body) ->
      "FUNDEF(" ^ new_line (level + 1) ^ string_of_single_name (level + 1) sn ^ ", " ^
      new_line (level + 2) ^
      string_of_body (level + 2) body ^ ")"
  | OLDFUNDEF (sn, ngs, body) ->
      let ngs_str = String.concat "; " (List.map (string_of_name_group level) ngs) in
      "OLDFUNDEF(" ^ new_line (level + 1) ^ string_of_single_name level sn ^ ", [" ^ ngs_str ^ "], " ^
      string_of_body (level + 2) body ^ ")"
  | DECDEF ng -> "DECDEF(" ^ string_of_name_group level ng ^ ")"
  | TYPEDEF (ng, attrs) ->
      let attrs_str = String.concat "; " (List.map (string_of_gnu_attr level) attrs) in
      "TYPEDEF(" ^ string_of_name_group level ng ^ ", [" ^ attrs_str ^ "])"
  | ONLYTYPEDEF ng -> "ONLYTYPEDEF(" ^ string_of_name_group level ng ^ ")"

and string_of_file level defs =
  "[" ^ String.concat "; " (List.map (string_of_definition level) defs) ^ "]"

and string_of_body level (defs, stmt) =
  "([" ^ String.concat "; " (List.map (string_of_definition level) defs) ^ "], " ^
  string_of_statement level stmt ^ ")"

and string_of_statement level = function
  | NOP -> "NOP"
  | COMPUTATION expr -> "COMPUTATION(" ^ string_of_expression level expr ^ ")"
  | BLOCK body ->
      "BLOCK(" ^ new_line (level + 1) ^ string_of_body (level + 1) body ^ new_line level ^ ")"
  | SEQUENCE (stmt1, stmt2) ->
      "SEQUENCE(" ^ new_line (level + 1) ^ string_of_statement (level + 1) stmt1 ^
      ", " ^ new_line (level + 1) ^ string_of_statement (level + 1) stmt2 ^
      new_line level ^ ")"
  | IF (expr, stmt1, stmt2) ->
      "IF(" ^ string_of_expression level expr ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt1 ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt2 ^ new_line level ^ ")"
  | WHILE (expr, stmt) ->
      "WHILE(" ^ string_of_expression level expr ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | DOWHILE (expr, stmt) ->
      "DOWHILE(" ^ string_of_expression level expr ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | FOR (expr1, expr2, expr3, stmt) ->
      "FOR(" ^ string_of_expression level expr1 ^ ", " ^
      string_of_expression level expr2 ^ ", " ^ string_of_expression level expr3 ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | RETURN expr -> "RETURN(" ^ string_of_expression level expr ^ ")"
  | SWITCH (expr, stmt) ->
      "SWITCH(" ^ string_of_expression level expr ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | CASE (expr, stmt) ->
      "CASE(" ^ string_of_expression level expr ^ ", " ^
      new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | DEFAULT stmt ->
      "DEFAULT(" ^ new_line (level + 1) ^ string_of_statement (level + 1) stmt ^ new_line level ^ ")"
  | LABEL (label, stmt) ->
      "LABEL(" ^ label ^ ", " ^ new_line (level + 1) ^ string_of_statement (level + 1) stmt ^
      new_line level ^ ")"
  | GOTO label -> "GOTO(" ^ label ^ ")"
  | ASM str -> "ASM(" ^ str ^ ")"
  | GNU_ASM (str, args1, args2, strs) ->
      let args1_str = String.concat "; " (List.map (string_of_gnu_asm_arg level) args1) in
      let args2_str = String.concat "; " (List.map (string_of_gnu_asm_arg level) args2) in
      let strs_str = String.concat "; " strs in
      "GNU_ASM(" ^ str ^ ", [" ^ args1_str ^ "], [" ^ args2_str ^ "], [" ^ strs_str ^ "])"
  | STAT_LINE (stmt, file, line) ->
      "STAT_LINE(" ^ new_line (level + 1) ^ string_of_statement (level + 1) stmt ^
      ", " ^ file ^ ", " ^ string_of_int line ^ new_line level ^ ")"

and string_of_gnu_asm_arg level (str1, str2, expr) =
  "(" ^ str1 ^ ", " ^ str2 ^ ", " ^ string_of_expression level expr ^ ")"

and string_of_binary_operator _ = function
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

and string_of_unary_operator _ = function
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

and string_of_expression level = function
  | NOTHING -> "NOTHING"
  | UNARY (op, expr) ->
      "UNARY(" ^ string_of_unary_operator level op ^ ", " ^ string_of_expression level expr ^ ")"
  | BINARY (op, expr1, expr2) ->
      "BINARY(" ^ string_of_binary_operator level op ^ ", " ^
      string_of_expression level expr1 ^ ", " ^ string_of_expression level expr2 ^ ")"
  | QUESTION (expr1, expr2, expr3) ->
      "QUESTION(" ^ string_of_expression level expr1 ^ ", " ^
      string_of_expression level expr2 ^ ", " ^ string_of_expression level expr3 ^ ")"
  | CAST (bt, expr) ->
      "CAST(" ^ string_of_base_type level bt ^ ", " ^ string_of_expression level expr ^ ")"
  | CALL (expr, exprs) ->
      let exprs_str = String.concat "; " (List.map (string_of_expression level) exprs) in
      "CALL(" ^ string_of_expression level expr ^ ", [" ^ exprs_str ^ "])"
  | COMMA exprs ->
      let exprs_str = String.concat "; " (List.map (string_of_expression level) exprs) in
      "COMMA([" ^ exprs_str ^ "])"
  | CONSTANT const -> "CONSTANT(" ^ string_of_constant level const ^ ")"
  | VARIABLE str -> "VARIABLE(" ^ str ^ ")"
  | EXPR_SIZEOF expr -> "EXPR_SIZEOF(" ^ string_of_expression level expr ^ ")"
  | TYPE_SIZEOF bt -> "TYPE_SIZEOF(" ^ string_of_base_type level bt ^ ")"
  | INDEX (expr1, expr2) ->
      "INDEX(" ^ string_of_expression level expr1 ^ ", " ^ string_of_expression level expr2 ^ ")"
  | MEMBEROF (expr, str) ->
      "MEMBEROF(" ^ string_of_expression level expr ^ ", " ^ str ^ ")"
  | MEMBEROFPTR (expr, str) ->
      "MEMBEROFPTR(" ^ string_of_expression level expr ^ ", " ^ str ^ ")"
  | GNU_BODY body -> "GNU_BODY(" ^ string_of_body level body ^ ")"
  | DESIGNATED (str, expr) ->
      "DESIGNATED(" ^ str ^ ", " ^ string_of_expression level expr ^ ")"
  | EXPR_LINE (expr, file, line) ->
      "EXPR_LINE(" ^ string_of_expression level expr ^ ", " ^ file ^ ", " ^
      string_of_int line ^ ")"

and string_of_constant level = function
  | CONST_INT str -> "CONST_INT(" ^ str ^ ")"
  | CONST_FLOAT str -> "CONST_FLOAT(" ^ str ^ ")"
  | CONST_CHAR str -> "CONST_CHAR(" ^ str ^ ")"
  | CONST_STRING str -> "CONST_STRING(" ^ str ^ ")"
  | CONST_COMPOUND exprs ->
      let exprs_str = String.concat "; " (List.map (string_of_expression level) exprs) in
      "CONST_COMPOUND([" ^ exprs_str ^ "])"

and string_of_gnu_attr level = function
  | GNU_NONE -> "GNU_NONE"
  | GNU_CALL (str, attrs) ->
      let attrs_str = String.concat "; " (List.map (string_of_gnu_attr level) attrs) in
      "GNU_CALL(" ^ str ^ ", [" ^ attrs_str ^ "])"
  | GNU_ID str -> "GNU_ID(" ^ str ^ ")"
  | GNU_CST const -> "GNU_CST(" ^ string_of_constant level const ^ ")"
  | GNU_EXTENSION -> "GNU_EXTENSION"
  | GNU_INLINE -> "GNU_INLINE"
  | GNU_TYPE_ARG (bt, storage) ->
      "GNU_TYPE_ARG(" ^ string_of_base_type level bt ^ ", " ^ string_of_storage level storage ^ ")"

and string_of_bool _ b = if b then "true" else "false"
