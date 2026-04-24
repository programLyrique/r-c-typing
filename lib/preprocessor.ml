open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
module U = Parser_utils

module StrSet = Set.Make(String)
module StrMap = Map.Make(String)

let warn_unsupported = ref true

let set_warn_unsupported value =
  warn_unsupported := value

let default_predefined_defines =
  match Sys.os_type with
  | "Win32" ->
      StrSet.of_list ["_WIN32"; "WIN32"; "__WIN32"; "__WINDOWS__"]
  | _ ->
      StrSet.of_list
        ["__linux__"; "__linux"; "linux"; "__gnu_linux__"; "__unix__"; "__unix"; "unix"]

let predefined_defines = ref default_predefined_defines

let set_predefined_defines defines =
  predefined_defines := StrSet.of_list defines

let current_predefined_defines () =
  !predefined_defines

(** TEMPORARY WORKAROUND: preloaded string values for a few macros we can't
    currently resolve through normal preprocessing. These are either (a) in
    system headers we don't parse (e.g. [<Rinternals.h>], [<inttypes.h>]'s
    deeper expansions), or (b) guarded by [#if R_VERSION < R_Version(...)]
    conditions that our [eval_preproc_expression] can't evaluate because it
    lacks numeric-valued defines and function-like macro support.

    The long-term plan is to extend [eval_preproc_expression] with numeric
    defines and function-like macro evaluation (at minimum [R_Version])
    so the conditions in package headers resolve correctly. Until then,
    grow this list as new packages surface missing macros. *)
let default_predefined_define_constants : A.const StrMap.t =
  List.fold_left (fun m (k, v) -> StrMap.add k v m) StrMap.empty [
    "R_PRIdXLEN_T", A.CStr "d";
    "PRIu64", A.CStr "lu";
    "PRId64", A.CStr "ld";
    "PRIx64", A.CStr "lx";
    "PRIX64", A.CStr "lX";
    "PRIo64", A.CStr "lo";
  ]

let default_include_dirs = ["/usr/include"; "/usr/local/include"]

let processed_headers : (string, unit) Hashtbl.t = Hashtbl.create 64

let define_constants : A.const StrMap.t ref = ref default_predefined_define_constants

let reset_define_constants () =
  define_constants := default_predefined_define_constants

let remove_define_constant name =
  define_constants := StrMap.remove name !define_constants

let remember_define_constant name value =
  define_constants := StrMap.add name value !define_constants

let resolve_defined_string name =
  match StrMap.find_opt name !define_constants with
  | Some (A.CStr s) -> Some s
  | _ -> None

let reset_processed_headers () =
  Hashtbl.clear processed_headers

let include_dirs : string list ref = ref default_include_dirs

let set_include_dirs dirs =
  include_dirs := dirs;
  reset_processed_headers ()

let resolve_header relative_path =
  List.find_map (fun dir ->
    let full = Filename.concat dir relative_path in
    if Sys.file_exists full then Some full else None
  ) !include_dirs

(* These callbacks are the small part of Parser that Preprocessor still needs
   to re-enter recursively. Shared stateless helpers live in Parser_utils;
   only the functions that would otherwise create a Parser <-> Preprocessor
   module cycle stay injectable here. *)
type parser_callbacks = {
  parse_string : string -> (CST.translation_unit, CST.extra) Parsing_result.t;
  top_level_statement : top_level_statement -> A.e;
  parse_file : string -> (CST.translation_unit, CST.extra) Parsing_result.t;
  parse_translation_unit : translation_unit -> A.top_level_unit list;
}

let preproc_define_name ((_, name_tok, _, _) : preproc_def) =
  U.token_to_string name_tok

let preproc_ifdef_is_ifndef = function
  | `Pat_25b90ba _ -> false
  | `Pat_9d92f6a _ -> true

let preproc_elifdef_is_ifndef = function
  | `Pat_0307ca2 _ -> false
  | `Pat_a6d4183 _ -> true

let rec eval_preproc_expression defines (expr : preproc_expression) : int =
  match expr with
  | `Id (_loc, name) ->
      if StrSet.mem name defines then 1 else 0
  | `Num_lit (_loc, s) ->
      let core = U.strip_int_suffix s in
      (try int_of_string core with Failure _ -> 0)
  | `Char_lit char_lit ->
      Option.value (U.eval_char_literal char_lit) ~default:0
  | `Prep_defi defined ->
      let name = match defined with
        | `Defi_LPAR_id_RPAR (_, _, (_loc, name), _) -> name
        | `Defi_id (_, (_loc, name)) -> name
      in
      if StrSet.mem name defines then 1 else 0
  | `Prep_un_exp (op, sub_expr) ->
      let v = eval_preproc_expression defines sub_expr in
      (match op with
       | `BANG _ -> if v = 0 then 1 else 0
       | `TILDE _ -> lnot v
       | `DASH _ -> -v
       | `PLUS _ -> v)
  | `Prep_bin_exp bin_expr ->
      eval_preproc_binary defines bin_expr
  | `Prep_paren_exp (_, sub_expr, _) ->
      eval_preproc_expression defines sub_expr
  | `Prep_call_exp ((_loc, _name), _args) ->
      0

and eval_preproc_binary defines (bin_expr : preproc_binary_expression) : int =
  let eval = eval_preproc_expression defines in
  match bin_expr with
  | `Prep_exp_PLUS_prep_exp (l, _, r) -> eval l + eval r
  | `Prep_exp_DASH_prep_exp (l, _, r) -> eval l - eval r
  | `Prep_exp_STAR_prep_exp (l, _, r) -> eval l * eval r
  | `Prep_exp_SLASH_prep_exp (l, _, r) ->
      let rv = eval r in if rv = 0 then 0 else eval l / rv
  | `Prep_exp_PERC_prep_exp (l, _, r) ->
      let rv = eval r in if rv = 0 then 0 else eval l mod rv
  | `Prep_exp_BARBAR_prep_exp (l, _, r) ->
      if eval l <> 0 then 1 else if eval r <> 0 then 1 else 0
  | `Prep_exp_AMPAMP_prep_exp (l, _, r) ->
      if eval l = 0 then 0 else if eval r <> 0 then 1 else 0
  | `Prep_exp_BAR_prep_exp (l, _, r) -> eval l lor eval r
  | `Prep_exp_HAT_prep_exp (l, _, r) -> eval l lxor eval r
  | `Prep_exp_AMP_prep_exp (l, _, r) -> eval l land eval r
  | `Prep_exp_EQEQ_prep_exp (l, _, r) -> if eval l = eval r then 1 else 0
  | `Prep_exp_BANGEQ_prep_exp (l, _, r) -> if eval l <> eval r then 1 else 0
  | `Prep_exp_GT_prep_exp (l, _, r) -> if eval l > eval r then 1 else 0
  | `Prep_exp_GTEQ_prep_exp (l, _, r) -> if eval l >= eval r then 1 else 0
  | `Prep_exp_LTEQ_prep_exp (l, _, r) -> if eval l <= eval r then 1 else 0
  | `Prep_exp_LT_prep_exp (l, _, r) -> if eval l < eval r then 1 else 0
  | `Prep_exp_LTLT_prep_exp (l, _, r) -> eval l lsl eval r
  | `Prep_exp_GTGT_prep_exp (l, _, r) -> eval l asr eval r

exception VariadicParameter of string

let parse_preproc_function_def helpers (func_def : preproc_function_def) =
  let ((loc1, _), name, params, body, (loc2, _)) = func_def in
  let name = U.token_to_string name in
  let _, params, (end_param, _) = params in
  let aux_prep_param (p : anon_choice_stmt_id_d3c4b5f) =
    match p with
    | `Id (_, s) -> s
    | `DOTDOTDOT (loc, _) ->
        raise
          (VariadicParameter
             ("Not supported yet: variadic parameter in preprocessor function at "
             ^ U.string_of_loc_start loc))
  in
  try
    let params =
      Option.fold ~none:[] ~some:(fun (p1, others) ->
        aux_prep_param p1 :: List.map (fun (_, p) -> aux_prep_param p) others
      ) params
      |> List.map (fun p -> (Ast.Any, p))
    in
    let body = match body with
        | None -> (U.locs_to_pos end_param loc2, A.Return None)
      | Some (_loc3, comp) ->
          let res = helpers.parse_string comp in
          match res.program with
          | None -> failwith ("Failed to parse preprocessor function body for " ^ name)
          | Some tu ->
              Printf.printf "List length: %d\n" (List.length tu);
              let items = List.filter_map (fun item ->
                match item with
                | `Top_level_stmt stmt -> Some (helpers.top_level_statement stmt)
                | _ ->
                    if !warn_unsupported then begin
                      Printf.printf "Not supported yet: top level item in define";
                      Boilerplate.map_top_level_item () item |> Raw_tree.to_channel stdout
                    end;
                    None
              ) tu in
              List.hd items
    in
    Some (U.locs_to_pos loc1 loc2, A.Fundef (Ast.Any, name, params, body))
  with
  | Failure msg ->
      if !warn_unsupported then
        Printf.printf "Not supported yet: failed to parse preprocessor function body for %s: %s\n" name msg;
      None
  | VariadicParameter msg ->
      if !warn_unsupported then
        Printf.printf "%s\n" msg;
      None

let parse_preproc_define (_callbacks : parser_callbacks) (prepoc : preproc_def) : A.top_level_unit option =
  let ((loc1, _), name_tok, value_opt, _nl) = prepoc in
  let name = U.token_to_string name_tok in
  let rec strip_outer_parens s =
    let s = String.trim s in
    let len = String.length s in
    if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
      strip_outer_parens (String.sub s 1 (len - 2))
    else s
  in
  let is_c_identifier s =
    let n = String.length s in
    n > 0
    && (match s.[0] with 'A'..'Z' | 'a'..'z' | '_' -> true | _ -> false)
    && (let rec ok i =
          i = n
          ||
          (match s.[i] with
           | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ok (i + 1)
           | _ -> false)
        in ok 1)
  in
  let parse_char_literal s =
    let len = String.length s in
    if len = 3 && s.[0] = '\'' && s.[2] = '\'' then Some (A.CChar s.[1])
    else None
  in
  let parse_define_value s =
    let s = strip_outer_parens s in
    let len = String.length s in
    if len = 0 then None
    else
      match s with
      | "NULL" | "nullptr" -> Some A.CNull
      | "true" | "TRUE" -> Some (A.CBool true)
      | "false" | "FALSE" -> Some (A.CBool false)
      | _ when len >= 2 && s.[0] = '"' && s.[len - 1] = '"' ->
          Some (A.CStr (String.sub s 1 (len - 2)))
      | _ ->
          begin
            match parse_char_literal s with
            | Some c -> Some c
            | None -> U.parse_define_literal s
          end
  in
  match value_opt with
  | None -> None
  | Some (_, raw_value) ->
      let raw_value = String.trim raw_value in
      let stripped = strip_outer_parens raw_value in
      if is_c_identifier stripped && stripped <> name then begin
        A.remember_define_alias name stripped;
        None
      end else
      begin
        try
          match parse_define_value raw_value with
          | Some c -> Some (U.loc_to_pos loc1, A.Define (name, c))
          | None ->
              if !warn_unsupported then
                Printf.printf "Not supported yet: #define %s with empty/invalid value\n" name;
              None
        with Failure _ ->
          if !warn_unsupported then
            Printf.printf "Not supported yet: #define %s with non-literal value `%s`\n" name raw_value;
          None
      end

let system_include callbacks (_loc, path) =
  let relative = String.sub path 1 (String.length path - 2) in
  match resolve_header relative with
  | None ->
      Printf.eprintf "Could not find system header: %s\n" relative;
      []
  | Some full_path ->
      if Hashtbl.mem processed_headers full_path then
        []
      else begin
        Hashtbl.add processed_headers full_path ();
        Printf.printf "Processing system header: %s\n" full_path;
        let cst = callbacks.parse_file full_path in
        match cst.program with
        | None -> []
        | Some tree ->
          [Mlsem.Common.Position.dummy, A.Include (callbacks.parse_translation_unit tree)]
      end

let fold_top_level_items aux_item defines items =
  let defines, rev_items =
    List.fold_left
      (fun (defines, rev_items) item ->
        let defines, current = aux_item defines item in
        (defines, List.rev_append (List.rev current) rev_items))
      (defines, []) items
  in
  (defines, List.rev rev_items)

let rec top_level_items ~parser_callbacks ~top_level_item ~top_level_block_item defines items =
  fold_top_level_items
    (top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item)
    defines items

and top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines items =
  fold_top_level_items
    (top_level_block_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item)
    defines items

and top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch =
  match else_branch with
  | `Prep_else (_else_tok, body) ->
      top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
  | `Prep_elif_5b2d46e (_elif_tok, condition, _newline, body, else_opt) ->
      let value = eval_preproc_expression defines condition in
      if value <> 0 then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_elif_b56056c (directive, name_tok, body, else_opt) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_elifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)

and top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines item =
  match item with
  | `Prep_ifdef (directive, name_tok, body, else_opt, _endif_tok) ->
      let name = U.token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_ifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_if (_if_tok, condition, _newline, body, else_opt, _endif_tok) ->
      let value = eval_preproc_expression defines condition in
      if value <> 0 then
        top_level_block_items ~parser_callbacks ~top_level_item ~top_level_block_item defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch ->
            top_level_else_branch ~parser_callbacks ~top_level_item ~top_level_block_item defines else_branch)
  | `Prep_def prepoc_def ->
      let name = preproc_define_name prepoc_def in
      if StrMap.mem name default_predefined_define_constants then
        (defines, [])
      else
        let defines = StrSet.add name defines in
        remove_define_constant name;
        A.remove_define_alias name;
        let item = parse_preproc_define parser_callbacks prepoc_def in
        begin
          match item with
          | Some (_, A.Define (name, value)) -> remember_define_constant name value
          | _ -> ()
        end;
        (defines,
         match item with
         | None -> []
         | Some item -> [item])
  | `Prep_func_def func_def ->
      (defines,
       match parse_preproc_function_def parser_callbacks func_def with
       | None -> []
       | Some item -> [item])
  | `Prep_incl (_, `System_lib_str path, _) ->
      (defines, system_include parser_callbacks path)
  | `Prep_incl include_item ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: non-system include\n";
        Boilerplate.map_top_level_item () (`Prep_incl include_item) |> Raw_tree.to_channel stdout
      end;
      (defines, [])
  | _ ->
      top_level_item defines item

  and top_level_block_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines item =
  match item with
  | `Prep_if prep_if ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_if prep_if)
  | `Prep_ifdef prep_ifdef ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_ifdef prep_ifdef)
  | `Prep_incl prep_include ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_incl prep_include)
  | `Prep_def prep_def ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_def prep_def)
  | `Prep_func_def prep_func_def ->
      top_level_item_dispatch ~parser_callbacks ~top_level_item ~top_level_block_item defines (`Prep_func_def prep_func_def)
    | _ ->
      top_level_block_item defines item

let with_test_state f =
  let saved_warn = !warn_unsupported in
  let saved_predefined = !predefined_defines in
  let saved_include_dirs = !include_dirs in
  let saved_define_constants = !define_constants in
  Fun.protect
    ~finally:(fun () ->
      warn_unsupported := saved_warn;
      predefined_defines := saved_predefined;
      include_dirs := saved_include_dirs;
      define_constants := saved_define_constants;
      reset_processed_headers ();
      A.reset_define_aliases ())
    (fun () ->
      set_warn_unsupported false;
      reset_processed_headers ();
      A.reset_define_aliases ();
      reset_define_constants ();
      f ())


let parse_translation_unit_exn source =
  match Parse.string source with
  | { Parsing_result.program = Some translation_unit; _ } -> translation_unit
  | _ -> failwith "Failed to parse test translation unit"


let first_top_level_item_exn source =
  match parse_translation_unit_exn source with
  | item :: _ -> item
  | [] -> failwith "Expected a top-level item"


let first_preproc_if_condition_exn source =
  match first_top_level_item_exn source with
  | `Prep_if (_, condition, _, _, _, _) -> condition
  | _ -> failwith "Expected a #if item"


let first_preproc_define_exn source =
  match first_top_level_item_exn source with
  | `Prep_def define -> define
  | _ -> failwith "Expected a #define item"


let make_test_callbacks () =
  let top_level_item_stub defines _ = (defines, []) in
  let top_level_block_item_stub defines _ = (defines, []) in
  let top_level_statement_stub _ = (Mlsem.Common.Position.dummy, A.Seq []) in
  let rec parse_translation_unit translation_unit =
    snd
      (top_level_items
         ~parser_callbacks
         ~top_level_item:top_level_item_stub
         ~top_level_block_item:top_level_block_item_stub
         (current_predefined_defines ())
         translation_unit)
  and parser_callbacks =
    {
      parse_string = Parse.string;
      top_level_statement = top_level_statement_stub;
      parse_file = Parse.file;
      parse_translation_unit;
    }
  in
  parser_callbacks


let preprocess_source_with_defines defines source =
  let parser_callbacks = make_test_callbacks () in
  snd
    (top_level_items
       ~parser_callbacks
       ~top_level_item:(fun defines _ -> (defines, []))
       ~top_level_block_item:(fun defines _ -> (defines, []))
       defines
       (parse_translation_unit_exn source))


let strip_positions items =
  List.map snd items


let with_temp_header header_name contents f =
  let dir = Filename.temp_file "rctyping-preprocessor" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let header_path = Filename.concat dir header_name in
  let oc = open_out header_path in
  output_string oc contents;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      Sys.remove header_path;
      Unix.rmdir dir)
    (fun () -> f dir)


let%test "eval_preproc_expression handles identifiers unary operators and chars" =
  with_test_state (fun () ->
    let expr =
      first_preproc_if_condition_exn
        "#if FOO && !BAZ && ('A' == 65) && (~0 < 0) && (+3 == 3) && (-1 < 0)\n#endif\n"
    in
    eval_preproc_expression (StrSet.of_list ["FOO"]) expr <> 0)


let%test "eval_preproc_expression covers arithmetic logical bitwise and shifts" =
  with_test_state (fun () ->
    let expr =
      first_preproc_if_condition_exn
        "#if ((1 + 2) == 3) && ((5 - 3) == 2) && ((2 * 3) == 6) && ((7 / 0) == 0) && ((7 % 0) == 0) && ((1 || 0) == 1) && ((1 && 2) == 1) && ((1 | 2) == 3) && ((3 ^ 1) == 2) && ((3 & 1) == 1) && ((2 == 2) == 1) && ((2 != 3) == 1) && ((3 > 2) == 1) && ((3 >= 3) == 1) && ((2 <= 2) == 1) && ((2 < 3) == 1) && ((1 << 3) == 8) && ((8 >> 1) == 4)\n#endif\n"
    in
    eval_preproc_expression StrSet.empty expr <> 0)


let%test "parse_preproc_define recognizes numeric string and char literals" =
  with_test_state (fun () ->
  (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define COUNT 12\n") with
   | Some (_, A.Define ("COUNT", A.CInt 12)) -> true
     | _ -> false)
  && (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define MSG \"ok\"\n") with
    | Some (_, A.Define ("MSG", A.CStr "ok")) -> true
        | _ -> false)
    && (match parse_preproc_define (make_test_callbacks ()) (first_preproc_define_exn "#define CH 'x'\n") with
        | Some (_, A.Define ("CH", A.CChar 'x')) -> true
        | _ -> false))


let%test "top_level_items records string constants and identifier aliases" =
  with_test_state (fun () ->
    let items =
      preprocess_source_with_defines
        StrSet.empty
        "#define GREETING \"hi\"\n#define HELLO GREETING\n"
    in
      strip_positions items = [A.Define ("GREETING", A.CStr "hi")]
    && resolve_defined_string "GREETING" = Some "hi"
    && A.resolve_alias "HELLO" = "GREETING")


let%test "top_level_items skips predefined fallback constants and rejects non literals" =
  with_test_state (fun () ->
    preprocess_source_with_defines StrSet.empty "#define PRIu64 12\n#define SUM (1 + 2)\n" = []
    && resolve_defined_string "PRIu64" = Some "lu")


let%test "top_level_items selects branches for ifdef ifndef and elif" =
  with_test_state (fun () ->
    let ifdef_items =
      preprocess_source_with_defines
        (StrSet.of_list ["FOO"])
        "#ifdef FOO\n#define YES 1\n#else\n#define NO 0\n#endif\n"
    in
    let ifndef_items =
      preprocess_source_with_defines
        StrSet.empty
        "#ifndef FOO\n#define MISSING 1\n#else\n#define PRESENT 1\n#endif\n"
    in
    let elif_items =
      preprocess_source_with_defines
        StrSet.empty
        "#if 0\n#define NOPE 0\n#elif 1\n#define YES 2\n#else\n#define ALT 3\n#endif\n"
    in
    strip_positions ifdef_items = [A.Define ("YES", A.CInt 1)]
    && strip_positions ifndef_items = [A.Define ("MISSING", A.CInt 1)]
    && strip_positions elif_items = [A.Define ("YES", A.CInt 2)])


let%test "top_level_items ignores non-system includes" =
  with_test_state (fun () ->
    preprocess_source_with_defines StrSet.empty "#include \"local.h\"\n" = [])


let%test "system includes are parsed once and cached" =
  with_test_state (fun () ->
    with_temp_header "sample.h" "#define FROM_HEADER 7\n" (fun dir ->
      set_include_dirs [dir];
      let items =
        preprocess_source_with_defines
          StrSet.empty
          "#include <sample.h>\n#include <sample.h>\n"
      in
      match items with
          | [_, A.Include nested_items] -> strip_positions nested_items = [A.Define ("FROM_HEADER", A.CInt 7)]
      | _ -> false))