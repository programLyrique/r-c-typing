(** Parse files containing the type definition for variables 

Such a file contain one type per line. 

A line is: symbol: type 

The type itself is parsed using rstt
*)
(* open Mlsem.Types *)
open Rstt

let parse_type_line line = 
  match String.split_on_char ':' line with 
  | [sym; ty_str] -> 
      let ty_str = String.trim ty_str in 
      let ty = Rstt_repl.IO.parse_type ty_str in 
      Some (String.trim sym, ty)
  | _ -> Printf.eprintf "Warning: could not parse line: %s@." line; None


let parse_type_file filename = 
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "Type file not found: %s" filename);
  let contents = In_channel.with_open_text filename In_channel.input_lines in
  List.filter_map parse_type_line contents




let%test "parse simple types" =
  let line = "x: v(int)" in
  match parse_type_line line with
  | Some (sym, ty) ->
      sym = "x" && Builder.(TVec PInt) = ty
  | None -> false

let%test "parse arrow types" =
  let line = "f: (v(int)) -> v(dbl)" in
  match parse_type_line line with
  | Some (sym, ty) ->
      (* let _,vty = Builder.resolve Builder.empty_env ty in
      let ti_map = Builder.TIdMap.empty in
      let vty = Builder.build ti_map vty in
      Format.printf "Parsed type: %a@." Rstt.Pp.ty vty; *)
      sym = "f" && Builder.(TArrow (TVec PInt, TVec PDbl)) = ty
  | None -> false