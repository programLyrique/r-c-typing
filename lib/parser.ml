open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
open Mlsem.Common

module StrSet = Set.Make(String)

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

let default_include_dirs = ["/usr/include"; "/usr/local/include"]

(** Memoization cache: each resolved header path is parsed and processed at
    most once per run. Without this, every [#include <stdio.h>] across a
    package's translation units would reparse glibc's header closure, causing
    combinatorial blow-up. Also breaks include cycles since we mark a path as
    processed before recursing into it. *)
let processed_headers : (string, unit) Hashtbl.t = Hashtbl.create 64

let reset_processed_headers () = Hashtbl.clear processed_headers

let include_dirs : string list ref = ref default_include_dirs

let set_include_dirs dirs =
  include_dirs := dirs;
  (* Different search paths can resolve the same relative header to a
     different file, so invalidate the cache when paths change. *)
  reset_processed_headers ()

let resolve_header relative_path =
  List.find_map (fun dir ->
    let full = Filename.concat dir relative_path in
    if Sys.file_exists full then Some full else None
  ) !include_dirs

let print_res (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) = 
  Printf.printf "Errors: %d. Lines with errors: %d / %d\n" res.stat.error_count
    res.stat.error_line_count res.stat.total_line_count ;
    List.iter (fun e -> 
      Printf.printf "Error: %s\n" (Tree_sitter_error.to_string e)
    ) res.errors ;
   match res.program with
  | None -> ()
  | Some prog ->
    Boilerplate.dump_extras res.extras ;
    let tree = Boilerplate.map_translation_unit () prog in
    Raw_tree.to_channel stdout tree 

let print_top_level_item (item: top_level_item) =
  let tree = Boilerplate.map_top_level_item () item in
  Raw_tree.to_channel stdout tree

let process_res res = res
(**
  Parse a file using FrontC and return the AST

  @param filename The path to the C file to parse
*)
let parse_file filename = 
  let res = Parse.file filename  in
  process_res res

let parse_string s = 
  let res = Parse.string s in 
  process_res res


  let gen_id = 
    let id =ref 0 in 
    fun () -> 
      let current_id = !id in 
      id := current_id + 1 ;
      current_id

(* Taken from E-Sh4rk/typed-r *)
let line_length = 0x10000
let conv_pos tspos =
  let bol = tspos.Loc.row*line_length in
  {
    Lexing.pos_fname = "" ;
    pos_lnum = tspos.Loc.row ;
    pos_bol = bol ;
    pos_cnum = bol + tspos.Loc.column ;
  }


let loc_to_pos (loc: Loc.t) : Position.t =
  let start_pos = conv_pos loc.start in
  let end_pos = conv_pos loc.end_ in
  Position.lex_join start_pos end_pos

(** Given to locations, create a position using the start of
   the first loc and the end of the second loc *)
let locs_to_pos (loc1: Loc.t) (loc2: Loc.t) : Position.t =
  let start_pos = conv_pos loc1.start in
  let end_pos = conv_pos loc2.end_ in
  Position.lex_join start_pos end_pos

let exprs_to_pos (expr1: A.e) (expr2: A.e) : Position.t =
  let pos1, _ = expr1 in
  let pos2, _ = expr2 in
  Position.join pos1 pos2


let aux_primitive_type t =
  match t with
  | "void" -> Ast.Void
  | "char" -> Ast.Char
  | "short" | "int" | "long"
  | "size_t" | "ssize_t" | "ptrdiff_t" | "intptr_t" | "uintptr_t"
  | "int8_t" | "int16_t" | "int32_t" | "int64_t"
  | "uint8_t" | "uint16_t" | "uint32_t" | "uint64_t"
  | "off_t" | "pid_t" | "mode_t" | "time_t" | "clock_t"
    -> Ast.Int
  | "float" | "double" -> Ast.Float
  | "bool" -> Ast.Bool
  | _ -> failwith ("Unknown primitive type: " ^ t)

let token_to_string (_loc, s) = s

type init_step =
  | InitIndex of A.e
  | InitField of string

type init_plan =
  | InitConst of A.const
  | InitDynamic of (init_step list * A.e) list

let rec placeholder_const_of_ctype ty =
  match ty with
  | Ast.Void -> A.CNull
  | Ast.Int -> A.CNa
  | Ast.Float -> A.CFloat 0.
  | Ast.Char -> A.CChar '\000'
  | Ast.Bool -> A.CBool false
  | Ast.Ptr _ | Ast.SEXP | Ast.Any -> A.CNull
  | Ast.Array (elem_ty, _) -> A.CArray [placeholder_const_of_ctype elem_ty]
  | Ast.Struct _ | Ast.Union _ | Ast.Enum _ | Ast.Typeref _ -> A.CNull

let top_level_initializer_count ((_, initializers, _, _) : initializer_list) =
  match initializers with
  | None -> 0
  | Some (_, inits) -> 1 + List.length inits

let rec aux_struct_fields name fields =
  let rec aux_field_decl_name (decl : field_declarator) : int * string =
    match decl with
    | `Id (_loc, s) -> (0, s)
    | `Poin_field_decl (_, _, _, _, d) ->
        let level, field_name = aux_field_decl_name d in
        (level + 1, field_name)
    | `Array_field_decl (d, _, _, _, _) ->
        let level, field_name = aux_field_decl_name d in
        (level + 1, field_name)
    | `Paren_field_decl (_, _, d, _) -> aux_field_decl_name d
    | `Attr_field_decl (d, _) -> aux_field_decl_name d
    | `Func_field_decl _ -> failwith "Not supported yet: function pointer fields"
  in
  let aux_field_decl (field_decl : field_declaration_declarator) =
    let first_decl, first_bits, other_decls = field_decl in
    let all_decls =
      (first_decl, first_bits)
      :: List.map (fun (_, decl, bits) -> (decl, bits)) other_decls
    in
    all_decls
  in
  (* We keep an explicit collector instead of a direct [List.map] because each
     input item can produce 0, 1, or many fields (e.g. preprocessor entries are
     skipped, and a declaration may contain several declarators), while also
     emitting warnings for unsupported cases. *)
  let rec collect acc = function
    | [] -> List.rev acc
    | item :: rest ->
        begin
          match item with
          | `Field_decl (decl_spec, field_decl, _attrs, _semi) ->
              let base_ty = aux_decl_spec decl_spec in
              let fields_from_item =
                match field_decl with
                | None -> []
                | Some fd ->
                    aux_field_decl fd
                    |> List.map (fun (decl, bits) ->
                           (match bits with
                           | None -> ()
                           | Some _ when !warn_unsupported ->
                               Printf.printf
                                 "Not supported yet: bitfield width is ignored in struct field declaration\n"
                           | Some _ -> ());
                           let level, field_name = aux_field_decl_name decl in
                           (Ast.build_ptr level base_ty, field_name))
              in
              collect (List.rev_append fields_from_item acc) rest
          | `Prep_def _ | `Prep_func_def _ | `Prep_call _
          | `Prep_if_in_field_decl_list _ | `Prep_ifdef_in_field_decl_list _ ->
              if !warn_unsupported then
                Printf.printf
                  "Not supported yet: preprocessor directives inside struct fields\n";
              collect acc rest
        end
  in
  Ast.Struct (name, collect [] fields)

and aux_struct struc = 
  match struc with 
  |`Id_opt_field_decl_list ((_loc, name), Some (_,fields,_)) -> aux_struct_fields name fields
  (* Anonymous struct: we don't give it a name so far *)
  | `Field_decl_list (_,fields,_) -> aux_struct_fields "" fields
  (* Named reference to an already declared struct, e.g. `struct Point p;`.
     We keep the struct tag and resolve it later through DeclMap. *)
  | `Id_opt_field_decl_list ((_loc, name), None) -> Ast.Struct (name, [])

and aux_sized_type_spec (_sized : sized_type_specifier) : Ast.ctype =
  let aux_choice_type = function
    | `Prim_type (_loc, s) -> aux_primitive_type s
    | `Id (_loc, s) when s = "SEXP" -> Ast.SEXP
    | `Id (_, s) -> Ast.Typeref s
  in
  match _sized with
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (_, Some t, _)
  | `Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed (_, _, Some t, _) ->
      aux_choice_type t
  (* E.g. "unsigned", "long long": defaults to integer family. *)
  | `Rep_choice_signed_opt_choice_id_rep1_choice_signed (_, None, _)
  | `Rep1_choice_signed_rep_type_qual_opt_choice_id_rep_choice_signed (_, _, None, _) ->
      Ast.Int

and aux_type_spec (type_spec : type_specifier)  =
  match type_spec with
  | `Prim_type tok -> let (_loc, s) = tok in aux_primitive_type s
  | `Id (_loc, s) when s = "SEXP" -> Ast.SEXP
  | `Id (_, s) -> Ast.Typeref s
  | `Struct_spec (_,_, _,struc, _) -> aux_struct struc
  (* Treat all enum forms as int *)
  | `Enum_spec _ -> Ast.Int
  | `Sized_type_spec sized -> aux_sized_type_spec sized
  | _ ->
      (* Unknown type specifier (e.g. tree-sitter [Macro_type_spec] for
         [__typeof__(...)], [decltype(...)], [__attribute_nonnull__(...)]),
         common in glibc multiarch headers. Previously failwith'd; that
         crashed the whole package as soon as any header pulled one of these
         in via a struct field. Degrade to [Ast.Any] — downstream treats it
         as opaque, which is the right behavior for specifiers we cannot
         interpret. *)
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: type specifier; using Ast.Any\n";
        let tree = Boilerplate.map_type_specifier () type_spec in
        Raw_tree.to_channel stderr tree
      end;
      Ast.Any

and aux_decl_spec decl_spec = 
  let (_, type_spec, _) = decl_spec in
  aux_type_spec type_spec

(* For pointers *)
let rec aux_abstract_declarator (base_type: Ast.ctype) (abs_decl: abstract_declarator option) =
  let aux = function 
    `Abst_poin_decl (_,_,_,decl) -> Ast.Ptr (aux_abstract_declarator base_type decl)
    | _ -> failwith "Not supported yet: abstract declarator"
  in
  Option.fold ~none:base_type
    ~some:aux abs_decl

let aux_type_descriptor (type_desc: type_descriptor) : Ast.ctype =
  let (_qualifiers1, type_spec, _qualifiers2, abstract_decl) = type_desc in
  let base_type = aux_type_spec type_spec in
  (* For now, ignore qualifiers and abstract declarators
     TODO: Handle const, volatile, pointers, arrays, etc. *)
  aux_abstract_declarator base_type abstract_decl

let rec aux_decl_name (decl : declarator) : int * string =
  match decl with
  | `Id tok -> (0, token_to_string tok)
  | `Func_decl (decl, _, _, _) ->  aux_decl_name decl
  | `Poin_decl (_,_,_,_,decl) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  (* Array just adds a pointer indirection currently. We could also create an array type for the C side*)
  | `Array_decl (decl,_,_,_,_) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  | _ -> Boilerplate.map_declarator () decl |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: function name"

let rec aux_type_declarator (td : type_declarator) : int * string =
  match td with
  | `Id tok -> (0, token_to_string tok)
  | `Poin_type_decl (_, _, _, _, inner) ->
      let (level, name) = aux_type_declarator inner in (level + 1, name)
  | `Array_type_decl (inner, _, _, _, _) ->
      let (level, name) = aux_type_declarator inner in (level + 1, name)
  | `Paren_type_decl (_, _, inner, _) -> aux_type_declarator inner
  | `Attr_type_decl (inner, _) -> aux_type_declarator inner
  | `Func_type_decl _ ->
      Printf.eprintf "Not supported yet: function pointer typedef\n"; (0, "__func_ptr__")
  | `Choice_signed _ | `Prim_type _ ->
      Printf.eprintf "Not supported yet: primitive in type declarator\n"; (0, "__prim__")

let aux_type_definition ((_, _, type_def_ty, (first_decl, rest_decls), _, _) : type_definition) =
  let (_, type_spec, _) = type_def_ty in
  let base_ty = aux_type_spec type_spec in
  let aux_one td =
    let (ptr_level, name) = aux_type_declarator td in
    (Mlsem.Common.Position.dummy, A.TypeDecl (name, Ast.build_ptr ptr_level base_ty))
  in
  aux_one first_decl :: List.map (fun (_, td) -> aux_one td) rest_decls

let aux_param (p: anon_choice_param_decl_4ac2852) = 
  match p with 
  | `Param_decl (decl_spec, Some (`Decl decl), _) -> 
    let ty = aux_decl_spec decl_spec in
    let level, name = aux_decl_name decl in
    (Ast.build_ptr level ty, name)
  | `Param_decl (decl_spec, None, _) ->
    let ty = aux_decl_spec decl_spec in
    (ty, "anon_param_" ^ string_of_int (gen_id ()))
  | `Vari_param _ -> failwith "Not supported yet: variable number of parameters (...) in declaration"
  | _ -> Boilerplate.map_anon_choice_param_decl_4ac2852 () p |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: parameter declaration"

let rec aux_params (decl: declarator) : int * A.param list =
  match decl with
  | `Func_decl (_, (_loc1, `Opt_choice_param_decl_rep_COMMA_choice_param_decl (Some (p1, params)), _loc2), _,_) ->
     let all = (aux_param p1) :: (List.map (fun (_, p) -> aux_param p) params) in
     (* In C, f(void) means no parameters — filter out void params *)
     0, List.filter (fun (ty, _) -> ty <> Ast.Void) all
  | `Poin_decl (_,_,_,_,decl) -> let level, params = aux_params decl in (level + 1, params)
  | _ -> (0, [])


let rec aux_expression (expr: expression) : A.e =
  match expr with 
  | `Choice_cond_exp e -> aux_not_bin_expression e
  | `Bin_exp e -> aux_bin_expression e
and aux_bin_expression (e: binary_expression) =
  let build_expr op e1 e2 = 
    let e1 = aux_expression e1 in
    let e2 = aux_expression e2 in
    (exprs_to_pos e1 e2, A.Binop (op, (e1, e2)))
  in
  match e with 
  | `Exp_PLUS_exp (e1, _, e2) -> build_expr "+" e1 e2
  | `Exp_DASH_exp (e1, _, e2) -> build_expr "-" e1 e2
  | `Exp_STAR_exp (e1, _, e2) -> build_expr "*" e1 e2
  | `Exp_SLASH_exp (e1, _, e2) -> build_expr "/" e1 e2
  | `Exp_PERC_exp (e1, _, e2) -> build_expr "%" e1 e2
  | `Exp_BARBAR_exp (e1, _, e2) -> build_expr "||" e1 e2
  | `Exp_AMPAMP_exp (e1, _, e2) -> build_expr "&&" e1 e2
  | `Exp_BAR_exp (e1, _, e2) -> build_expr "|" e1 e2
  | `Exp_HAT_exp (e1, _, e2) -> build_expr "^" e1 e2
  | `Exp_AMP_exp (e1, _, e2) -> build_expr "&" e1 e2
  | `Exp_EQEQ_exp (e1, _, e2) -> build_expr "==" e1 e2
  | `Exp_BANGEQ_exp (e1, _, e2) -> build_expr "!=" e1 e2
  | `Exp_GT_exp (e1, _, e2) -> build_expr ">" e1 e2
  | `Exp_GTEQ_exp (e1, _, e2) -> build_expr ">=" e1 e2
  | `Exp_LTEQ_exp (e1, _, e2) -> build_expr "<=" e1 e2
  | `Exp_LT_exp (e1, _, e2) -> build_expr "<" e1 e2
  | `Exp_LTLT_exp (e1, _, e2) -> build_expr "<<" e1 e2
  | `Exp_GTGT_exp (e1, _, e2) -> build_expr ">>" e1 e2

and aux_string (s: string_) = 
  let unescape  = function
    | `Imm_tok_prec_p1_pat_c7f65b4 (loc, s) -> (loc,s)
    | `Esc_seq (loc, s) -> (loc,s)
  in
  match s with 
  | `Str_lit (prefix,escaped,(end_loc,_)) -> 
    let pos_str = List.map unescape escaped in
    let str = String.concat "" (List.map snd pos_str) in
    let start_loc =
      match prefix with
      | `LDQUOT (loc, _) -> loc
      | `UDQUOT_c163aae (loc, _) -> loc
      | `UDQUOT_df3447d (loc, _) -> loc
      | `U8DQUOT (loc, _) -> loc
      | `DQUOT (loc, _) -> loc
    in
    (locs_to_pos start_loc end_loc, A.Const (A.CStr str))
  | _ -> failwith "Not supported yet: strings with escaped characters or concatenated strings"
and aux_num_lit  s = 
  let has_float_syntax s =
    let rec loop i =
      if i >= String.length s then false
      else
        match s.[i] with
        | '.' | 'e' | 'E' | 'p' | 'P' -> true
        | _ -> loop (i + 1)
    in
    loop 0
  in
  let strip_int_suffix s =
    let i = ref (String.length s - 1) in
    while
      !i >= 0
      && match s.[!i] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false
    do
      decr i
    done;
    String.sub s 0 (!i + 1)
  in
  let strip_float_suffix s =
    if String.length s = 0 then s
    else
      match s.[String.length s - 1] with
      | 'f' | 'F' | 'l' | 'L' -> String.sub s 0 (String.length s - 1)
      | _ -> s
  in
  let normalize_int_literal s =
    let len = String.length s in
    if len >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X' || s.[1] = 'b' || s.[1] = 'B') then s
    else if len > 1 && s.[0] = '0' then
      let rec all_octal i =
        i >= len || ((s.[i] >= '0' && s.[i] <= '7') && all_octal (i + 1))
      in
      if all_octal 1 then "0o" ^ String.sub s 1 (len - 1) else s
    else s
  in
  let parse_int s =
    let core = s |> strip_int_suffix |> normalize_int_literal in
    int_of_string core
  in
  let parse_float s =
    let core = strip_float_suffix s in
    Float.of_string core
  in
  if has_float_syntax s then
    A.Const (A.CFloat (parse_float s))
  else
    A.Const
      (try A.CInt (parse_int s)
       with Failure _ -> A.CFloat (parse_float s))

and aux_char_lit (c: char_literal) =
  let decode_escape_sequence s =
    let fail () = failwith ("Invalid C escape sequence: " ^ s) in
    if String.length s < 2 || s.[0] <> '\\' then fail ()
    else
      let parse_int base digits =
        if String.length digits = 0 then fail () else int_of_string (base ^ digits)
      in
      match s.[1] with
      | 'a' when String.length s = 2 -> 7
      | 'b' when String.length s = 2 -> 8
      | 'f' when String.length s = 2 -> 12
      | 'n' when String.length s = 2 -> 10
      | 'r' when String.length s = 2 -> 13
      | 't' when String.length s = 2 -> 9
      | 'v' when String.length s = 2 -> 11
      | '\\' when String.length s = 2 -> Char.code '\\'
      | '\'' when String.length s = 2 -> Char.code '\''
      | '"' when String.length s = 2 -> Char.code '"'
      | '?' when String.length s = 2 -> Char.code '?'
      | 'x' -> parse_int "0x" (String.sub s 2 (String.length s - 2))
      | 'u' when String.length s = 6 -> parse_int "0x" (String.sub s 2 4)
      | 'U' when String.length s = 10 -> parse_int "0x" (String.sub s 2 8)
      | c2 when '0' <= c2 && c2 <= '7' -> parse_int "0o" (String.sub s 1 (String.length s - 1))
      | c2 when String.length s = 2 -> Char.code c2
      | _ -> fail ()
  in
  let (start, chars, (loc2, _)) = c in
  let loc1 =
    match start with
    | `LSQUOT (loc, _)
    | `USQUOT_d861d39 (loc, _)
    | `USQUOT_2701bdc (loc, _)
    | `U8SQUOT (loc, _)
    | `SQUOT (loc, _) -> loc
  in
  let values =
    List.map
      (function
        | `Imm_tok_pat_36637e2 (_, s) ->
            assert (String.length s = 1);
            Char.code s.[0]
        | `Esc_seq (_, s) -> decode_escape_sequence s)
      chars
  in
  let is_plain_char =
    match (start, values) with
    | (`SQUOT _, [v]) when v >= 0 && v <= 255 -> true
    | _ -> false
  in
  let const =
    match values with
    | [v] when is_plain_char -> A.CChar (Char.chr v)
    | _ ->
        (* C multi-char constants are implementation-defined; use a stable byte packing. *)
        let packed = List.fold_left (fun acc v -> (acc lsl 8) lor (v land 0xFF)) 0 values in
        A.CInt packed
  in
  (locs_to_pos loc1 loc2, A.Const const)
    
and aux_not_bin_expression (e : expression_not_binary) = 
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Num_lit (loc, s) -> (loc_to_pos loc, aux_num_lit s)
  | `Char_lit c -> aux_char_lit c
  | `Null _ -> (Position.dummy, A.Const A.CNull)
  | `Call_exp  call -> aux_call_expression call
  | `Str s -> aux_string s
  | `Assign_exp (e1, `EQ _, e2) -> 
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      (exprs_to_pos lhs rhs, A.VarAssign (lhs, rhs))
  | `Assign_exp (e1, op, e2) -> 
      (* Expand a op= e into a = a op e *)
      let lhs = aux_assign_left_expression e1 in
      let rhs = aux_expression e2 in
      let pos = exprs_to_pos lhs rhs in
      let binop = 
        match op with 
        | `PLUSEQ _ -> "+"
        | `DASHEQ _ -> "-"
        | `STAREQ _ -> "*"
        | `SLASHEQ _ -> "/"
        | `PERCEQ _ -> "%"
        | `AMPEQ _ -> "&"
        | `BAREQ _ -> "|"
        | `HATEQ _ -> "^"
        | `LTLTEQ _ -> "<<"
        | `GTGTEQ _ -> ">>"
        | `EQ _ -> failwith "Unreachable: `EQ handled in previous case"
      in
      let rhs = (exprs_to_pos lhs rhs, A.Binop (binop, (lhs, rhs))) in
      (pos, A.VarAssign (lhs, rhs))
  | `Un_exp (op, e1) -> 
      let loc1,op = match op with 
        | `DASH (loc, _) -> (loc, "-")
        | `BANG (loc, _) -> (loc, "!")
        | `TILDE (loc, _) -> (loc, "~")
        | `PLUS (loc, _) -> (loc, "+")
      in
      let expr = aux_expression e1 in 
      (Position.join (loc_to_pos loc1) (fst expr),
      A.Unop (op, expr))
  | `Subs_exp expr -> 
      aux_subscription_expression expr
  | `Cond_exp (cond, _, Some (`Exp then_), _, else_) -> 
      let cond = aux_expression cond in
      let then_ = aux_expression then_ in
      let else_ = aux_expression else_ in
      let pos = exprs_to_pos cond then_ in
      let pos = Position.join pos (fst else_) in
      (pos, A.Ite (cond, then_, else_))
  | `Update_exp expr -> aux_update_expression expr
  | `Paren_exp expr -> aux_paren_expr expr
  | `True (loc,_) -> (loc_to_pos loc, A.Const (A.CBool true))
  | `False (loc,_) -> (loc_to_pos loc, A.Const (A.CBool false))
  | `Cast_exp ((loc1,_), typ, _, expr) ->
      let e = aux_expression expr in
      let cast_type = aux_type_descriptor typ in
      let pos = Position.join (loc_to_pos loc1) (fst e) in
      (pos, A.Cast (cast_type, e))
  | `Poin_exp expr -> aux_pointer_expression expr
  | `Field_exp expr -> aux_field_expression expr
  | `Sizeof_exp ((loc1,_), arg) ->
      let sizeof_id = (loc_to_pos loc1, A.Id "sizeof") in
      begin
        match arg with
        | `Exp e ->
            let e = aux_expression e in
            let pos = Position.join (loc_to_pos loc1) (fst e) in
            (pos, A.Call (sizeof_id, [e]))
        | `LPAR_type_desc_RPAR ((_loc_lp, _), ty, (loc_rp, _)) ->
            (* Lower sizeof(type) to sizeof((type)0) to stay in expression space. *)
            let ty = aux_type_descriptor ty in
            let zero = (loc_to_pos loc1, A.Const (A.CInt 0)) in
            let cast_zero = (locs_to_pos loc1 loc_rp, A.Cast (ty, zero)) in
            (locs_to_pos loc1 loc_rp, A.Call (sizeof_id, [cast_zero]))
      end
  | _ ->
    Boilerplate.map_expression_not_binary () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: expresion_not_binary"

and aux_field_expression (field_expr: field_expression) =
  (* Do we really care if it is . or -> here? *)
  let expr, _, (loc2, field_name) = field_expr in
  let e = aux_expression expr in
  let pos = Position.join (fst e) (loc_to_pos loc2) in
  (pos, A.FieldAccess (e,  field_name))
and aux_update_expression (e: update_expression) =
  (* For typing purposes, we don't care about whether it is pre or post incr/decr*)
  let (op,e) = match e with
  | `Choice_DASHDASH_exp (op, e) -> (op, aux_expression e)
  | `Exp_choice_DASHDASH (e,op) -> (op, aux_expression e)
  in
  let op = match op with 
    | `DASHDASH (loc, _) -> (loc, "--")
    | `PLUSPLUS (loc, _) -> (loc, "++")
  in
  let pos = Position.join (loc_to_pos (fst op)) (fst e) in
  (pos, A.Unop (snd op, e))
and aux_subscription_expression (subs: subscript_expression) =
  let expr, _, index, _ = subs in
  let arr = aux_expression expr in
  let idx = aux_expression index in
  let pos = Position.join (fst arr) (fst idx) in
  (pos, A.Call ((pos, A.Id "[]"), [arr; idx])) (* A usual function? Or a specific operator*)
and aux_pointer_expression (poin: pointer_expression) =
  let op, expr = poin in
  let loc,op = match op with 
    | `AMP (loc, _) -> (loc, "&")
    | `STAR (loc, _) -> (loc, "*")
  in

  let e = aux_expression expr in
  let pos = Position.join (loc_to_pos loc) (fst e) in
  (pos, A.Unop (op, e))
and aux_assign_left_expression (e: assignment_left_expression) : A.e =
  match e with 
  | `Id (loc, s) -> (loc_to_pos loc, A.Id s)
  | `Subs_exp expr -> aux_subscription_expression expr (*Like that so far, but may require special treatment *)
  | `Poin_exp expr -> aux_pointer_expression expr
  | `Field_exp expr -> aux_field_expression expr
  | _ -> (
    Boilerplate.map_assignment_left_expression () e |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: left side of assignment must be an identifier"
  )
and aux_call_expression (call: call_expression) =
  let expr, args = call in
  let func_ast = aux_expression expr in
  let pos1,_ = func_ast in 
  let pos2, args_ast = aux_argument_list args in
  (Position.join pos1 pos2, A.Call (func_ast, args_ast))
and aux_argument (arg: anon_choice_exp_f079e30) =
  match arg with 
  | `Exp e -> aux_expression e
  | `Comp_stmt comp -> aux_body comp
and aux_argument_list (arg_list: argument_list) =
  let (l1, _),args,(l2,_) = arg_list in
  let pos = locs_to_pos l1 l2 in
  match args with 
  | None -> (pos, [])
  | Some (arg1, others) ->
    (pos, aux_argument arg1 ::  List.map (fun (_, arg) -> aux_argument arg) others)

and  aux_comma_expression (expr: anon_choice_exp_55b4dba) =
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let _ = aux_expression e1 in
      aux_comma_expression e2 (* ? *)

and aux_return_statement (ret: return_statement)  =
  let (l1, _), expr_opt, (l2,_) = ret in
  let pos = locs_to_pos l1 l2 in
  match expr_opt with
  | None -> (pos, A.Return None)
  | Some expr -> (pos, Return (Some (aux_comma_expression expr)))

and aux_expression_statement (expr_stmt: expression_statement) =
  match expr_stmt with 
  | (Some comma_expr, _) -> aux_comma_expression comma_expr
  | (None, (loc,_)) -> (loc_to_pos loc, A.Return None)

and aux_for_cond (stmts: for_statement_body) =
  let decl,cond,_,incr = stmts in
  let init = match decl with 
  | `Decl decl -> aux_declarations decl
  | `Opt_choice_exp_SEMI expr -> aux_expression_statement expr
  in
  let cond = Option.map aux_comma_expression cond in 
  let incr = Option.map aux_comma_expression incr in
  init, cond, incr

and aux_for_statement (for_stmt: for_statement) =
  let (loc1,_),_,for_exprs,_,stmt = for_stmt in
  let init, cond, incr = aux_for_cond for_exprs in
  let body = aux_statement stmt in 
  (Position.join (loc_to_pos loc1) (fst body), A.For (init, cond, incr, body))

and aux_while_statement (while_stmt: while_statement) =
  let (loc1, _),cond,body = while_stmt in
  let b = aux_statement body in
  let pos = Position.join (loc_to_pos loc1) (fst b) in
  pos,A.While (aux_paren_expr cond, b)

and aux_do_statement (do_stmt: do_statement) =
  (* Transform do { body } while (cond) into while(true) { body; if(!cond) break; }
     The previous approach Seq([body, While(cond, body)]) duplicated the body, which
     placed break statements from the first copy outside any loop, causing orphan
     Ret(BLoop) nodes during the MLsem control-flow transformation. *)
  let (loc_do, _), body, _, cond, (loc_semi, _) = do_stmt in
  let pos = locs_to_pos loc_do loc_semi in
  let body_ast = aux_statement body in
  let cond_ast = aux_paren_expr cond in
  let true_cond = (Mlsem.Common.Position.dummy, A.Const (A.CBool true)) in
  let break_stmt = (Mlsem.Common.Position.dummy, A.Break) in
  let cond_check = (Mlsem.Common.Position.dummy, A.If (cond_ast, (Mlsem.Common.Position.dummy, A.Const A.CNull), Some break_stmt)) in
  let loop_body = (pos, A.Seq [body_ast; cond_check]) in
  (pos, A.While (true_cond, loop_body))

and aux_compound_stmt (stmt: compound_statement) = 
  let (l1,_),block_items,(l2,_) = stmt in
  let stmts = List.map aux_block_item block_items in
  (locs_to_pos l1 l2, A.Seq stmts)

and aux_switch_statement (switch_stmt: switch_statement) =
  let (loc1,_),expr,cases = switch_stmt in
  let expr  = aux_paren_expr expr in
  let body = aux_compound_stmt cases in

  match body with 
  | pos, A.Seq lst -> (Position.join (loc_to_pos loc1) pos, A.Switch (expr, lst))
  | _ -> failwith "Unexpected body in switch statement"

and aux_non_case_statement (stmt: non_case_statement) =
  match stmt with
  | `Ret_stmt ret -> aux_return_statement ret
  | `Exp_stmt exp -> aux_expression_statement exp
  | `If_stmt if_stmt -> aux_if_statement if_stmt
  | `Comp_stmt comp -> aux_body comp
  | `For_stmt for_stmt -> aux_for_statement for_stmt
  | `While_stmt while_stmt -> aux_while_statement while_stmt
  | `Do_stmt do_stmt -> aux_do_statement do_stmt
  | `Brk_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Break)
  | `Cont_stmt ((l1, _), (l2, _)) -> (locs_to_pos l1 l2, A.Next)
  | `Goto_stmt ((l1, _),_, (l2, _)) -> (locs_to_pos l1 l2, Return None)
  | `Labe_stmt _ -> failwith "Not supported yet: labeled statements"
  | `Switch_stmt st -> aux_switch_statement st
  | `Attr_stmt _ -> failwith "Not supported yet: attribute statements"
  | _ -> failwith "Not supported yet: Seh_try and Seh_leave statements"

and aux_top_level_statement (stmt: top_level_statement) =
  match stmt with 
  | `Top_level_exp_stmt (Some exp, (loc2,_)) -> 
      let e = aux_not_bin_expression exp in
      let pos = Position.join (fst e) (loc_to_pos loc2) in
      (pos, A.Return (Some e))
  | _ -> failwith "Not supported yet: top level statements without expression"

and aux_case_statement (case_stmt: case_statement) =
  let (case, _, body) = case_stmt in 
  let case_body = List.map (function 
   | `Choice_attr_stmt st -> aux_non_case_statement st
   | `Decl decls -> aux_declarations decls
   | `Type_defi _typs -> failwith "Type definitions in case statements are not supported yet"
  ) body
  in
  let first_loc = 
    match case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let last_loc = 
    match List.rev case_body with
    | [] -> Position.dummy
    | (pos, _) :: _ -> pos
  in
  let case_body = (Position.join first_loc last_loc, A.Seq case_body) in
  match case with 
  | `Case_exp ((loc1,_), e) ->
    let e = aux_expression e in
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Case (e, case_body))
  | `Defa ((loc1,_)) ->
    let pos = Position.join (loc_to_pos loc1) last_loc in
    (pos, A.Default (case_body))


and aux_statement (stmt: statement) =
 match stmt with 
 | `Case_stmt st -> aux_case_statement st
 | `Choice_attr_stmt st -> aux_non_case_statement st

and aux_initializer_list_plan (init_list: initializer_list) =
  let rec const_of_expr e =
    match snd (aux_expression e) with
    | A.Const c -> Some c
    | _ -> None
  and const_of_pair (pair : initializer_pair) =
    match pair with
    | `Rep1_choice_subs_desi_EQ_choice_exp (_, _, _)
    | `Id_COLON_choice_exp (_, _, _) -> None
  and const_of_initializer (init : anon_choice_init_pair_1a6981e) =
    match init with
    | `Exp e -> const_of_expr e
    | `Init_list nested -> const_of_initializer_list nested
    | `Init_pair pair -> const_of_pair pair
  and const_of_initializer_list ((_, initializers, _, _) : initializer_list) =
    match initializers with
    | None -> Some (A.CArray [])
    | Some (init1, inits) ->
        let rec gather acc = function
          | [] -> Some (A.CArray (List.rev acc))
          | init :: tl ->
              begin
                match const_of_initializer init with
                | Some c -> gather (c :: acc) tl
                | None -> None
              end
        in
        gather [] (init1 :: List.map snd inits)
  in
  match const_of_initializer_list init_list with
  | Some c -> InitConst c
  | None ->
      let rec rhs_to_dynamic base_path (init : anon_choice_exp_3078596) =
        match init with
        | `Exp e -> [ (base_path, aux_expression e) ]
        | `Init_list nested -> dynamic_of_initializer_list base_path nested
      and path_with_designator path d =
        match d with
        | `Subs_desi (_, idx, _) -> path @ [InitIndex (aux_expression idx)]
        | `Field_desi (_, (_, field_name)) -> path @ [InitField field_name]
        | `Subs_range_desi _ -> failwith "Not supported yet: subscript range designators"
      and item_to_dynamic base_path seq_index (init : anon_choice_init_pair_1a6981e) =
        let default_path = base_path @ [InitIndex (Position.dummy, A.Const (A.CInt seq_index))] in
        match init with
        | `Exp e -> [ (default_path, aux_expression e) ]
        | `Init_list nested -> dynamic_of_initializer_list default_path nested
        | `Init_pair pair ->
            begin
              match pair with
              | `Rep1_choice_subs_desi_EQ_choice_exp (designators, _, rhs) ->
                  let path = List.fold_left path_with_designator base_path designators in
                  rhs_to_dynamic path rhs
              | `Id_COLON_choice_exp ((_, field_name), _, rhs) ->
                  rhs_to_dynamic (base_path @ [InitField field_name]) rhs
            end
      and dynamic_of_initializer_list base_path ((_, initializers, _, _) : initializer_list) =
        match initializers with
        | None -> []
        | Some (init1, inits) ->
            let all_inits = init1 :: List.map snd inits in
            let rec aux idx acc = function
              | [] -> List.rev acc
              | init :: tl ->
                  let leaves = item_to_dynamic base_path idx init in
                  aux (idx + 1) (List.rev_append leaves acc) tl
            in
            aux 0 [] all_inits
      in
      InitDynamic (dynamic_of_initializer_list [] init_list)

and aux_initializer_list (init_list: initializer_list) =
  let plan = aux_initializer_list_plan init_list in
  let ((l1, _), _, _, (l2, _)) = init_list in
  let pos = locs_to_pos l1 l2 in
  match plan with
  | InitConst c -> (pos, A.Const c)
  | InitDynamic _ ->
      failwith "Internal error: dynamic initializer list cannot be converted to a single expression"

and aux_declaration typ (decl: anon_choice_opt_ms_call_modi_decl_decl_opt_gnu_asm_exp_2fa2f9e) =
  match decl with 
  |`Init_decl (declr, _, init) -> 
      let level,name = aux_decl_name declr in
      let name = A.Id name in
      let typ = Ast.build_ptr level typ in
      let var_decl = (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, name))) in
      let rec lhs_from_steps base (steps : init_step list) =
        match steps with
        | [] -> base
        | InitIndex idx :: tl ->
            let lhs = (Position.dummy, A.Call ((Position.dummy, A.Id "[]"), [base; idx])) in
            lhs_from_steps lhs tl
        | InitField field :: tl ->
            let lhs = (Position.dummy, A.FieldAccess (base, field)) in
            lhs_from_steps lhs tl
      in
      begin
        match init with
        | `Exp expr ->
            let e = aux_expression expr in
            let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
            (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
        | `Init_list init_list ->
            begin
              match aux_initializer_list_plan init_list with
              | InitConst c ->
                  let e = (Position.dummy, A.Const c) in
                  let var_assign = (Mlsem.Common.Position.dummy, A.VarAssign ((Mlsem.Common.Position.dummy, name), e)) in
                  (Mlsem.Common.Position.dummy, A.Seq [var_decl; var_assign])
              | InitDynamic entries ->
                  let base = (Position.dummy, name) in
                  let element_ty =
                    match typ with
                    | Ast.Ptr elem_ty -> elem_ty
                    | _ -> Ast.Any
                  in
                  let placeholder = placeholder_const_of_ctype element_ty in
                  let init_len = top_level_initializer_count init_list in
                  let bootstrap_vals = List.init init_len (fun _ -> placeholder) in
                  let init_empty =
                    (Position.dummy,
                     A.VarAssign (base, (Position.dummy, A.Const (A.CArray bootstrap_vals))))
                  in
                  let assigns =
                    List.map
                      (fun (steps, rhs) ->
                        let lhs = lhs_from_steps base steps in
                        (Position.dummy, A.VarAssign (lhs, rhs)))
                      entries
                  in
                  (Mlsem.Common.Position.dummy, A.Seq (var_decl :: init_empty :: assigns))
            end
      end
  | `Opt_ms_call_modi_decl_decl_opt_gnu_asm_exp (_, declr, _) -> 
      let (level, name) = aux_decl2_name declr in
      let typ = Ast.build_ptr level typ in
      (Mlsem.Common.Position.dummy, A.VarDeclare (typ, (Mlsem.Common.Position.dummy, A.Id name)))
and aux_declarations ((decl_type, decl1, decls, _loc2): declaration) =
  let typ = aux_decl_spec decl_type in
  (Mlsem.Common.Position.dummy, A.Seq ((aux_declaration typ decl1) :: (List.map (fun (_, d) -> aux_declaration typ d) decls)))
and aux_decl2_name (decl: declaration_declarator) = 
  match decl  with
  | `Id tok -> (0, token_to_string tok)
  | `Poin_decl (_,_,_,_,decl) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  | `Array_decl (decl,_,_,_,_) -> let (level, name) = aux_decl_name decl in (level + 1, name)
  | _ -> 
    Boilerplate.map_declaration_declarator () decl |> Tree_sitter_run.Raw_tree.to_channel stderr ;
    failwith "Not supported yet: complex declaration names"
and aux_block_item (item : block_item) =
  match item with
  | `Stmt stmt -> aux_statement stmt
  | `Decl decls -> aux_declarations decls
  | `Attr_stmt _attr_stmt -> failwith "Not supported yet: attribute statements"
  | `Type_defi _ -> (Mlsem.Common.Position.dummy, Const CNull) (* Type definitions don't produce values *)
  | `Empty_decl _ -> (Mlsem.Common.Position.dummy, Const CNull)

  | `Prep_if _ | `Prep_ifdef _ | `Prep_incl _ | `Prep_def _
  | `Prep_func_def _ | `Prep_call _ ->
      (Mlsem.Common.Position.dummy, Const CNull) (*TODO: Handle preprocessor! *)
  | `Func_defi _ -> failwith "Function definitions inside a block not supported"
  | _ -> failwith "Not supported yet: old function defi or link spec"

and aux_body body =
  let (l1,_),block_items,(l2,_) = body in
  let stmts = List.map aux_block_item block_items in
  if Utils.is_singleton stmts then
    List.hd stmts
  else
   (locs_to_pos l1 l2, A.Seq stmts)

and aux_paren_expr (p_expr: parenthesized_expression) =
  let (l1,_), expr, (l2,_) = p_expr in
  match expr with
  | `Exp e -> aux_expression e
  | `Comma_exp (e1, _, e2) -> 
      let ast1 = aux_expression e1 in
      let ast2 = aux_comma_expression e2 in
      (locs_to_pos l1 l2, A.Comma (ast1,ast2))
  | `Comp_stmt comp -> aux_body comp

and aux_if_statement (if_stmt: if_statement) =
  let (l1,_), cond, then_stmt, else_opt = if_stmt in
  let ast_cond = aux_paren_expr cond in
  let ast_then = aux_statement then_stmt in
  let ast_else = Option.map (fun (_,st) -> aux_statement st) else_opt in
  let l2 = Option.map  
    (fun (pos,_) -> Position.end_of_position pos)
    ast_else
    |> Option.value ~default:(Position.end_of_position @@ fst ast_then)
  in

  (Position.lex_join (conv_pos l1.start) l2,A.If (ast_cond, ast_then, ast_else)) 

let rec body_has_trailing_return ((_, expr) : A.e) =
  match expr with
  | A.Return _ -> true
  | A.Seq [] -> false
  | A.Seq exprs -> body_has_trailing_return (List.hd (List.rev exprs))
  | _ -> false

let ensure_void_return body =
  if body_has_trailing_return body then body
  else
    let pos, expr = body in
    let trailing_return = (pos, A.Return None) in
    match expr with
    | A.Seq exprs -> (pos, A.Seq (exprs @ [trailing_return]))
    | _ -> (pos, A.Seq [body; trailing_return])

and aux_prep_func_def (func_def: preproc_function_def) = 
  let ((loc1, _), name, params, body, (loc2,_)) = func_def in
  let name = token_to_string name in
  let _,params,(end_param,_) = params in 
  let aux_prep_param (p: anon_choice_stmt_id_d3c4b5f) = 
    match p with 
    | `Id (_,s) -> s
    | `DOTDOTDOT (loc, _) -> failwith ("Not supported yet: variadic parameter in preprocessor function at " ^ (Position.string_of_lex_pos (conv_pos loc.start)))
  in
  let params = Option.fold ~none:[] ~some:(fun (p1, others) -> 
    aux_prep_param p1 :: List.map (fun (_, p) -> aux_prep_param p) others
  ) params |> 
    List.map (fun p -> (Ast.Any, p)) 
  in
  try
    let body = match body with 
      | None -> (locs_to_pos end_param loc2, A.Return None)
      | Some (_loc3,comp) -> 
        let res = parse_string comp in
        match res.program with
        | None -> failwith ("Failed to parse preprocessor function body for " ^ name)
        | Some tu -> 
          Printf.printf "List length: %d\n" (List.length tu);
          let items = List.filter_map (fun item -> 
            match item with 
            | `Top_level_stmt stmt -> Some (aux_top_level_statement stmt)
            | _ ->
                if !warn_unsupported then begin
                  Printf.printf "Not supported yet: top level item in define";
                  print_top_level_item item
                end;
                None
          ) tu in 
          List.hd items
    in
    Some(locs_to_pos loc1 loc2, A.Fundef (Ast.Any, name, params, body))
  with Failure msg ->
    if !warn_unsupported then
      Printf.printf "Not supported yet: failed to parse preprocessor function body for %s: %s\n" name msg;
    None

let aux_preproc_define (prepoc : preproc_def) : A.top_level_unit option =
  let ((loc1, _), name_tok, value_opt, _nl) = prepoc in
  let name = token_to_string name_tok in
  let rec strip_outer_parens s =
    let s = String.trim s in
    let len = String.length s in
    if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
      strip_outer_parens (String.sub s 1 (len - 2))
    else s
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
            | None ->
                (match aux_num_lit s with
                | A.Const c -> Some c
                | _ -> None)
          end
  in
  match value_opt with
  | None -> None
  | Some (_, raw_value) ->
      let raw_value = String.trim raw_value in
      begin
        try
          match parse_define_value raw_value with
          | Some c -> Some (loc_to_pos loc1, A.Define (name, c))
          | None ->
              if !warn_unsupported then
                Printf.printf "Not supported yet: #define %s with empty/invalid value\n" name;
              None
        with Failure _ ->
          if !warn_unsupported then
            Printf.printf "Not supported yet: #define %s with non-literal value `%s`\n" name raw_value;
          None
      end

let preproc_define_name ((_, name_tok, _, _) : preproc_def) =
  token_to_string name_tok

let preproc_ifdef_is_ifndef = function
  | `Pat_25b90ba _ -> false
  | `Pat_9d92f6a _ -> true

let preproc_elifdef_is_ifndef = function
  | `Pat_0307ca2 _ -> false
  | `Pat_a6d4183 _ -> true

let fold_top_level_items aux_item defines items =
  let defines, rev_items =
    List.fold_left
      (fun (defines, rev_items) item ->
        let defines, current = aux_item defines item in
        (defines, List.rev_append (List.rev current) rev_items))
      (defines, []) items
  in
  (defines, List.rev rev_items)

let rec aux_top_level_items defines items =
  fold_top_level_items aux_top_level_item defines items

and aux_top_level_block_items defines items =
  fold_top_level_items aux_top_level_block_item defines items

and aux_top_level_else_branch defines else_branch =
  match else_branch with
  | `Prep_else (_else_tok, body) -> aux_top_level_block_items defines body
  | `Prep_elif_5b2d46e _ ->
      if !warn_unsupported then
        Printf.printf "Not supported yet: top level #elif with expression\n";
      (defines, [])
  | `Prep_elif_b56056c (directive, name_tok, body, else_opt) ->
      let name = token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_elifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then aux_top_level_block_items defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch -> aux_top_level_else_branch defines else_branch)

and aux_top_level_item defines (item : top_level_item) =
  match item with
  | `Func_defi (_, decl_spec, _, decl, body) ->
      let _,name = aux_decl_name decl in
      (* the part with the parameters can also carry the information about pointers for the return type! *)
      let level, params = aux_params decl in
      let return_type = Ast.build_ptr level (aux_decl_spec decl_spec) in
      let body = aux_body body in
      let body =
        match return_type with
        | Ast.Void -> ensure_void_return body
        | _ -> body
      in
      (defines, [Mlsem.Common.Position.dummy, A.Fundef (return_type, name, params, body)])
  | `Prep_ifdef (directive, name_tok, body, else_opt, _endif_tok) ->
      let name = token_to_string name_tok in
      let is_defined = StrSet.mem name defines in
      let take_body =
        if preproc_ifdef_is_ifndef directive then not is_defined else is_defined
      in
      if take_body then aux_top_level_block_items defines body
      else (
        match else_opt with
        | None -> (defines, [])
        | Some else_branch -> aux_top_level_else_branch defines else_branch)
  | `Prep_def prepoc_def ->
      let defines = StrSet.add (preproc_define_name prepoc_def) defines in
      (defines,
       match aux_preproc_define prepoc_def with
       | None -> []
       | Some item -> [item])
  | `Prep_func_def func_def ->
      (defines,
       match aux_prep_func_def func_def with
       | None -> []
       | Some item -> [item])
  | `Empty_decl (type_spec, _tok) ->
      let s = aux_type_spec type_spec in
      (match s with
       | Ast.Struct (name, _) | Ast.Union (name, _) | Ast.Enum (name, _) ->
           (defines, [Mlsem.Common.Position.dummy, A.TypeDecl (name, s)])
       | Ast.Typeref _ ->
           (* Bare identifier as an empty declaration. In system headers this
              is overwhelmingly glibc linkage/attribute macros that expand to
              nothing under C (__BEGIN_DECLS, __END_DECLS, __THROW, …). Since
              we don't run a macro expander, tree-sitter sees them as
              identifiers and classifies them as empty declarations. They
              carry no binding, so drop silently. *)
           (defines, [])
       | _ ->
           if !warn_unsupported then
             Printf.printf "Empty declaration is not a struct/union/enum; ignoring (ctype=%s)\n"
               (Ast.show_ctype s);
           (defines, [Mlsem.Common.Position.dummy, A.TypeDecl ("", s)]))
  | `Type_defi type_def ->
      (defines, aux_type_definition type_def)
  | `Prep_incl (_, `System_lib_str path, _) ->
      (defines, aux_system_lib_include path)
  | _ ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: top level item\n";
        print_top_level_item item
      end;
      (defines, [])

and aux_top_level_block_item defines (item : block_item) =
  match item with
  | `Func_defi func_def -> aux_top_level_item defines (`Func_defi func_def)
  | `Old_style_func_defi old_func_def -> aux_top_level_item defines (`Old_style_func_defi old_func_def)
  | `Link_spec link_spec -> aux_top_level_item defines (`Link_spec link_spec)
  | `Decl decl -> aux_top_level_item defines (`Decl decl)
  | `Attr_stmt attr_stmt -> aux_top_level_item defines (`Attr_stmt attr_stmt)
  | `Type_defi type_def -> aux_top_level_item defines (`Type_defi type_def)
  | `Empty_decl empty_decl -> aux_top_level_item defines (`Empty_decl empty_decl)
  | `Prep_if prep_if -> aux_top_level_item defines (`Prep_if prep_if)
  | `Prep_ifdef prep_ifdef -> aux_top_level_item defines (`Prep_ifdef prep_ifdef)
  | `Prep_incl prep_include -> aux_top_level_item defines (`Prep_incl prep_include)
  | `Prep_def prep_def -> aux_top_level_item defines (`Prep_def prep_def)
  | `Prep_func_def prep_func_def -> aux_top_level_item defines (`Prep_func_def prep_func_def)
  | `Prep_call prep_call -> aux_top_level_item defines (`Prep_call prep_call)
  | `Stmt _ ->
      if !warn_unsupported then begin
        Printf.printf "Not supported yet: statement inside top level preprocessor branch\n";
        Boilerplate.map_block_item () item |> Tree_sitter_run.Raw_tree.to_channel stderr
      end;
      (defines, [])

and aux_system_lib_include (_loc, path) =
  (* Strip surrounding < > from the system lib string token *)
  let relative = String.sub path 1 (String.length path - 2) in
  match resolve_header relative with
  | None -> []
  | Some full_path ->
      if Hashtbl.mem processed_headers full_path then
        (* Already processed once in this run; its declarations are already
           in the typing state. Return empty to avoid redundant work (and to
           break cycles — see [processed_headers] docstring). *)
        []
      else begin
        (* Mark before recursing so a cycle A -> B -> A terminates. *)
        Hashtbl.add processed_headers full_path ();
        let cst = parse_file full_path in
        match cst.program with
        | None -> []
        | Some tree ->
            let items = snd (aux_top_level_items !predefined_defines tree) in
            [Mlsem.Common.Position.dummy, A.Include items]
      end

let aux_translation_unit tree =
  snd (aux_top_level_items !predefined_defines tree)

let to_ast (res: (CST.translation_unit, CST.extra) Tree_sitter_run.Parsing_result.t) =
  match res.program with
  | None -> []
  | Some translation_unit ->
    aux_translation_unit translation_unit

let%test "aux_num_lit parses integer long suffix" =
  aux_num_lit "1L" = A.Const (A.CInt 1)

let%test "aux_num_lit parses hexadecimal integer" =
  aux_num_lit "0x2a" = A.Const (A.CInt 42)

let%test "aux_num_lit parses hexadecimal integer with suffixes" =
  aux_num_lit "0x2aUL" = A.Const (A.CInt 42)

let%test "aux_num_lit parses c-style octal" =
  aux_num_lit "077" = A.Const (A.CInt 63)

let%test "aux_num_lit parses float suffix" =
  match aux_num_lit "3.5f" with
  | A.Const (A.CFloat f) -> Float.abs (f -. 3.5) < 1e-12
  | _ -> false

let%test "aux_num_lit parses hexadecimal float" =
  match aux_num_lit "0x1.fp3" with
  | A.Const (A.CFloat f) -> Float.abs (f -. 15.5) < 1e-12
  | _ -> false

let%test "preproc define with literal creates PAst.Define" =
  let res = parse_string "#define ONE 1L\nint f() { return ONE; }" in
  match to_ast res with
  | (_, A.Define ("ONE", A.CInt 1)) :: _ -> true
  | _ -> false

let%test "valueless define still enables subsequent ifdef" =
  let res =
    parse_string
      "#define FLAG\n#ifdef FLAG\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "ifndef keeps else branch skipped for undefined macro" =
  let res =
    parse_string
      "#ifndef FLAG\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "elifndef branch is selected when prior branches fail" =
  let res =
    parse_string
      "#ifdef FIRST\nint a() { return 1; }\n#elifndef SECOND\nint b() { return 2; }\n#else\nint c() { return 3; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "b", _, _))] -> true
  | _ -> false

let%test "linux predefined macros are enabled by default" =
  let res =
    parse_string
      "#ifdef __linux__\nint f() { return 1; }\n#else\nint g() { return 2; }\n#endif\n"
  in
  match to_ast res with
  | [(_, A.Fundef (_, "f", _, _))] -> true
  | _ -> false

let%test "void function gets trailing return on fallthrough" =
  let res = parse_string "void f() { 1; }" in
  match to_ast res with
  | [(_, A.Fundef (Ast.Void, "f", [], (_, A.Seq [(_, A.Const (A.CInt 1)); (_, A.Return None)])))] -> true
  | _ -> false

let%test "void function does not duplicate explicit return" =
  let res = parse_string "void f() { return; }" in
  match to_ast res with
  | [(_, A.Fundef (Ast.Void, "f", [], (_, A.Return None)))] -> true
  | _ -> false