open Tree_sitter_c
open Tree_sitter_run
open CST
module A = PAst
open Mlsem.Common

let token_to_string (_loc, s) = s

(* Taken from E-Sh4rk/typed-r *)
let line_length = 0x10000

let conv_pos tspos =
  let bol = tspos.Loc.row * line_length in
  {
    Lexing.pos_fname = "";
    pos_lnum = tspos.Loc.row;
    pos_bol = bol;
    pos_cnum = bol + tspos.Loc.column;
  }

let loc_to_pos (loc : Loc.t) : Position.t =
  let start_pos = conv_pos loc.start in
  let end_pos = conv_pos loc.end_ in
  Position.lex_join start_pos end_pos

let locs_to_pos (loc1 : Loc.t) (loc2 : Loc.t) : Position.t =
  let start_pos = conv_pos loc1.start in
  let end_pos = conv_pos loc2.end_ in
  Position.lex_join start_pos end_pos

let string_of_loc_start (loc : Loc.t) =
  Position.string_of_lex_pos (conv_pos loc.start)

let strip_int_suffix s =
  let i = ref (String.length s - 1) in
  while
    !i >= 0
    && match s.[!i] with 'u' | 'U' | 'l' | 'L' -> true | _ -> false
  do
    decr i
  done;
  String.sub s 0 (!i + 1)

let normalize_int_literal s =
  let len = String.length s in
  if len >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X' || s.[1] = 'b' || s.[1] = 'B') then s
  else if len > 1 && s.[0] = '0' then
    let rec all_octal i =
      i >= len || ((s.[i] >= '0' && s.[i] <= '7') && all_octal (i + 1))
    in
    if all_octal 1 then "0o" ^ String.sub s 1 (len - 1) else s
  else s

let parse_c_int_literal s =
  let core = s |> strip_int_suffix |> normalize_int_literal in
  int_of_string core

let decode_c_escape_sequence s =
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

let decode_c_char_literal_values chars =
  List.map
    (function
      | `Imm_tok_pat_36637e2 (_, s) ->
          assert (String.length s = 1);
          Char.code s.[0]
      | `Esc_seq (_, s) -> decode_c_escape_sequence s)
    chars

let int_of_char_literal (char_lit : char_literal) =
  let _, chars, _ = char_lit in
  let values = decode_c_char_literal_values chars in
  match values with
  | [value] -> value
  | _ -> List.fold_left (fun acc value -> (acc lsl 8) lor (value land 0xFF)) 0 values

let parse_number_literal s =
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
  let strip_float_suffix s =
    if String.length s = 0 then s
    else
      match s.[String.length s - 1] with
      | 'f' | 'F' | 'l' | 'L' -> String.sub s 0 (String.length s - 1)
      | _ -> s
  in
  let parse_float s =
    let core = strip_float_suffix s in
    Float.of_string core
  in
  if has_float_syntax s then
    A.CFloat (parse_float s)
  else
    try A.CInt (parse_c_int_literal s)
    with Failure _ -> A.CFloat (parse_float s)

let parse_define_literal s =
  Some (parse_number_literal s)

let eval_char_literal char_lit =
  Some (int_of_char_literal char_lit)