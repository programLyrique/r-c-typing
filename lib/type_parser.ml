(** Parse files containing the type definition for variables 

Such a file contain one type per line. 

A line is: symbol: type 

The type itself is parsed using rstt
*)
open Rstt

let parse_type_line line = 
  match Utils.split_once ':' line with 
  | (_, "") -> 
      Printf.eprintf "Warning: could not parse line (missing type): %s@." line; None
  | (sym, ty_str) -> 
      let ty_str = String.trim ty_str in 
      let ty = Rstt_repl.IO.parse_type ty_str in 
      Some (String.trim sym, ty)


let parse_type_file filename = 
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "Type file not found: %s" filename);
  let contents = In_channel.with_open_text filename In_channel.input_lines in
  List.filter_map parse_type_line contents


let build_types ti_map env type_list = 
  List.fold_left (fun acc (sym, ty) ->
    let open Builder in 
    let (ty_env, env) = acc in 
      let env,ty = Builder.resolve env ty in
      let ty = build ti_map ty in
      (StrMap.add sym ty ty_env, env)
    ) (Builder.StrMap.empty, env) type_list


let mk_arg l =
  let open Builder in
  TArg { pos = []; pos_named =l; tl = TOption TEmpty; named = [] }

let%test "parse simple types" =
  let line = "x: v(int)" in
  match parse_type_line line with
  | Some (sym, ty) ->
      sym = "x" && Builder.(TVec (AnyLength PInt) ) = ty
  | None -> false

let%test "parse arrow types" =
  let line = "f: (a:v(int)) -> v(dbl)" in
  match parse_type_line line with
  | Some (sym, ty) ->
      sym = "f" && Builder.(TArrow (mk_arg [("a", TVec (AnyLength PInt))], TVec (AnyLength PDbl))) = ty
  | None -> false

let%test "parse several types" =
  let lines = [
    "x: v(int)";
    "y: v(dbl)";
    "f: (a: v(int)) -> v(dbl)";
  ] in
  let results = List.map parse_type_line lines in
  match results with
  | [Some (s1,t1); Some (s2,t2); Some (s3,t3)] ->
      s1 = "x" && Builder.(TVec (AnyLength PInt)) = t1 &&
      s2 = "y" && Builder.(TVec (AnyLength PDbl)) = t2 &&
      s3 = "f" && Builder.(TArrow (mk_arg [("a", TVec (AnyLength PInt))], TVec (AnyLength PDbl))) = t3
  | _ -> false

let%test "build types" =
let open Builder in 
  let lines = [
    "x: v(int)";
    "y: v(dbl)"
  ] in
  let parsed_types = List.filter_map parse_type_line lines in
  let ti_map = TIdMap.empty in
  let type_map, _ = build_types ti_map empty_env parsed_types in
  let open Rstt in
  let int_vec = Prim.Int.any |> Prim.mk |> (fun v -> Vec.AnyLength v) |> Vec.mk |> Attr.mk_anyclass in
  let dbl_vec = Prim.Dbl.any |> Prim.mk |> (fun v -> Vec.AnyLength v) |> Vec.mk |> Attr.mk_anyclass in
  Ty.equiv (StrMap.find "x" type_map) int_vec  &&
  Ty.equiv (StrMap.find "y" type_map) dbl_vec


let%test "build from file" =
  let open Builder in
  let filename = "test_types.txt" in
  let oc = open_out filename in
  Printf.fprintf oc "x: v(int)\n";
  Printf.fprintf oc "y: v(dbl)\n";
  close_out oc;
  let parsed_types = parse_type_file filename in
  let ti_map = TIdMap.empty in
  let type_map, _ = build_types ti_map Builder.empty_env parsed_types in
  let open Rstt in
  let int_vec = Prim.Int.any |> Prim.mk |> (fun v -> Vec.AnyLength v) |> Vec.mk |> Attr.mk_anyclass in
  let dbl_vec = Prim.Dbl.any |> Prim.mk |> (fun v -> Vec.AnyLength v) |> Vec.mk |> Attr.mk_anyclass in
  Sys.remove filename;
  Ty.equiv (StrMap.find "x" type_map) int_vec  &&
  Ty.equiv (StrMap.find "y" type_map) dbl_vec 

let find_file_in_ancestors ~start ~target =
  let rec aux dir =
    let candidate = Filename.concat dir target in
    if Sys.file_exists candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else aux parent
  in
  aux start

let find_types_base_ty () =
  (* In dune inline-tests, the cwd is typically a sandbox (under _build/.sandbox/...).
     Prefer a search from cwd (will work if the file is staged via (inline_tests (deps ...))).
     Fall back to the source tree location while developing outside dune. *)
  match find_file_in_ancestors ~start:(Sys.getcwd ()) ~target:"types/base.ty" with
  | Some f -> f
  | None -> (
      match find_file_in_ancestors ~start:(Filename.dirname __FILE__) ~target:"../types/base.ty" with
      | Some f -> f
      | None -> "types/base.ty"
    )

let%test "load file" = 
    let open Builder in
    let filename = find_types_base_ty () in
    let parsed_types = parse_type_file filename in
    let ti_map = TIdMap.empty in
    let type_map, _ = build_types ti_map Builder.empty_env parsed_types in
    (* print types in the type map *)
    StrMap.iter (fun sym ty ->
      Format.printf "%s: @[<h>%a@]@." sym Rstt.Pp.ty ty
    ) type_map;
    true