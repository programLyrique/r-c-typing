(** Parse files containing the type definition for variables 

Such a file contain one type per line. 

A line is: symbol: type 

The type itself is parsed using rstt
*)
open Rstt

type type_kind = Def | Alias

let parse_type_line line =
  (* Remove inline comments: everything after // is ignored.
     This allows writing things like: mkCharCE: t(c_string, any) -> p(chr) // encoding *)
  let line =
    let before_slash, after_slash = Utils.split_once '/' line in
    if after_slash <> "" && String.length after_slash > 0 && after_slash.[0] = '/' then
      before_slash
    else
      line
  in
  let line = String.trim line in
  if line = "" || String.starts_with ~prefix:"//" line then
    None
  else
    match Utils.split_once_any [':';'='] line with
    | ('\000', _, "") ->
        Printf.eprintf "Warning: could not parse line (missing type): %s@." line;
        None
    | (sep, sym, ty_str) ->
        let ty_str = String.trim ty_str in
        let ty = Rstt_repl.IO.parse_type ty_str in
        match sep with
        | ':' -> Some (String.trim sym, ty, Def)
        | '=' -> Some (String.trim sym, ty, Alias)
        | _ -> failwith "Unreachable case in parse_type_line."

let parse_type_file filename = 
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "Type file not found: %s" filename);
  let contents = In_channel.with_open_text filename In_channel.input_lines in
  List.filter_map parse_type_line contents

let add_struct_guards t =
  let open Rstt.Builder in
  let aux t =
    match t with
    | TArrow (l,r) -> TArrow (TStruct l,r)
    | t -> t
  in
  map aux Fun.id Fun.id t

let build_types ti_map env type_list = 
  List.fold_left (fun acc (sym, ty, kind) ->
    let open Builder in 
   
    let (ty_env, ti_map, env) = acc in 
    let id = TId.create () in (* useless when it is not a type alias *)
    let env = if kind = Alias then 
      {env with 
        tids = StrMap.add sym id env.tids
      } 
    else env in

    let env, ty =
      try Builder.resolve env ty
      with Not_found ->
        Printf.eprintf "Not_found while resolving symbol '%s' during type resolution.@." sym;
        raise Not_found
    in
    let ty = build ti_map ty in
    let ti_map = if kind = Alias then TIdMap.add id ty ti_map else ti_map in
    (StrMap.add sym ty ty_env, ti_map, env)
    ) (Builder.StrMap.empty, ti_map, env) type_list

let load_file filename =
  let parsed_types = parse_type_file filename in
  let ti_map = Builder.TIdMap.empty in
  let type_map, _,_ = build_types ti_map Builder.empty_env parsed_types in
  type_map


let mk_arg l =
  let open Builder in
  TArg { pos = []; pos_named =l; tl = TOption TEmpty; named = [] }

let%test "parse simple types" =
  let line = "x: v(int)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "x" && Builder.(TVec (AnyLength PInt) ) = ty && kind = Def
  | None -> false

let%test "parse type alias" =
  let line = "my_int = v(int)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "my_int" && Builder.(TVec (AnyLength PInt) ) = ty && kind = Alias
  | None -> false

let%test "parse arrow types" =
  let line = "f: (a:v(int)) -> v(dbl)" in
  match parse_type_line line with
  | Some (sym, ty, kind) ->
      sym = "f" && Builder.(TArrow (mk_arg [("a", TVec (AnyLength PInt))], TVec (AnyLength PDbl))) = ty && kind = Def
  | None -> false

let%test "parse several types" =
  let lines = [
    "x: v(int)";
    "y: v(dbl)";
    "f: (a: v(int)) -> v(dbl)";
  ] in
  let results = List.map parse_type_line lines in
  match results with
  | [Some (s1,t1,kind1); Some (s2,t2,kind2); Some (s3,t3,kind3)] ->
      s1 = "x" && Builder.(TVec (AnyLength PInt)) = t1 && kind1 = Def &&
      s2 = "y" && Builder.(TVec (AnyLength PDbl)) = t2 && kind2 = Def &&
      s3 = "f" && Builder.(TArrow (mk_arg [("a", TVec (AnyLength PInt))], TVec (AnyLength PDbl))) = t3 && kind3 = Def
  | _ -> false

let%test "build types" =
let open Builder in 
  let lines = [
    "x: v(int)";
    "y: v(dbl)"
  ] in
  let parsed_types = List.filter_map parse_type_line lines in
  let ti_map = TIdMap.empty in
  let type_map, _,_ = build_types ti_map empty_env parsed_types in
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
  let type_map, _,_ = build_types ti_map Builder.empty_env parsed_types in
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
    
    let type_map = try 
      load_file filename 
    with Not_found ->
      StrMap.empty in
    StrMap.is_empty type_map |> not
    (* if StrMap.is_empty type_map then false else (
      StrMap.iter (fun sym ty ->
        Format.printf "%s: @[<h>%a@]@." sym Rstt.Pp.ty ty
      ) type_map;
      true
    ) *)

let%test "parse c_bool" =
  try let _ = Rstt_repl.IO.parse_type "c_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow with c_int to c_int" =
  try let _ = Rstt_repl.IO.parse_type "t(c_int, c_int) -> c_int" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "debug parse examples" =
  let cases = [
    "c_bool";
    "c_int";
    "t(c_int) -> c_int";
    "t(c_int, c_int) -> c_int";
    "t(c_int, c_int) -> c_bool";
    "t(c_double, c_double) -> c_bool";
    "t(any, any) -> c_bool"
  ] in
  List.iter (fun s ->
    try
      let _ = Rstt_repl.IO.parse_type s in
      ()
    with Rstt_repl__IO.SyntaxError (_, m) ->
      Printf.eprintf "FAILED PARSE: '%s' -> %s\n" s m
  ) cases;
  true

let%test "parse arrow int int to int" =
  try let _ = Rstt_repl.IO.parse_type "t(int, int) -> int" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow int int to c_bool" =
  try let _ = Rstt_repl.IO.parse_type "t(int, int) -> c_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow cint cint to cint" =
  try let _ = Rstt_repl.IO.parse_type "t(cint, cint) -> cint" in true
  with Rstt_repl__IO.SyntaxError _ -> false

let%test "parse arrow C_int C_int to C_bool" =
  try let _ = Rstt_repl.IO.parse_type "t(C_int, C_int) -> C_bool" in true
  with Rstt_repl__IO.SyntaxError _ -> false