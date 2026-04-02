open Mlsem.Common
open Mlsem.Types
module System = Mlsem.System
module MVariable = Mlsem.Lang.MVariable

module StrMap = Map.Make(String)

type cmd_options = {
  cst : bool;
  past : bool;
  ast : bool;
  mlsem : bool;
  typing : bool;
  debug : bool;
  filter: string option;
}

let make_substring_pred = function
  | None -> (fun _ -> true)
  | Some sub ->
      let re = Str.regexp_string sub in
      fun s ->
        try
          ignore (Str.search_forward re s 0);
          true
        with Not_found -> false

(* idenv: str -> Variable.t 
   env: typing environment: Variable.t -> TyScheme.ty 
*)

(**  Give the any type to any free variables (not in the environment) *)
let extend_env mlast env =
  let fv = System.Ast.fv mlast in
  let dom = Env.domain env |> VarSet.of_list in
  let missing = VarSet.diff fv dom in
  missing |> VarSet.elements |> List.fold_left
    (fun env v -> (Printf.printf "Missing: %s at %s \n" (Variable.get_unique_name v) (Position.string_of_pos (Variable.get_location v));
     Env.add v (TyScheme.mk_mono GTy.dyn) env)) env

let infer_ast visible opts (idenv, env, decl) (ast : Ast.e) =
  let name,v = 
    match ast with 
    | _,_,_,Ast.Function (name, _, _, _) -> name,MVariable.create Immut (Some name)
    | _ -> failwith "Expected a function definition at the top level."
  in
  let mlsem_ast = Ast.to_mlsem ast in 
  if opts.mlsem && visible then 
    Format.printf "%a@." Mlsem.System.Ast.pp mlsem_ast;
  if opts.debug then
    Format.printf "Type inference for function %s@." name;
  try 
    if opts.typing then
        begin
        let _env = extend_env mlsem_ast env in (*TODO: bring it back when the inference deals with any in a more appropriate way*)
        let renvs = System.Refinement.refinements env mlsem_ast in
        let reconstructed = System.Reconstruction.infer env renvs mlsem_ast in
        let typ = System.Checker.typeof_def env reconstructed mlsem_ast in
        let tys = TyScheme.norm_and_simpl typ in
        if visible then Format.printf "%a: @[<h>%a@]@.@." Variable.pp v TyScheme.pp_short tys ;
        let (vars, typ) = TyScheme.get tys in 
        let typ = GTy.ub typ in 
        (*Format.printf "%a: upper bound= %a@.@." Variable.pp v  Ty.pp typ ;*)
        (* We only keep the upper bound as type for v and add it to the environment *)
        let tys = TyScheme.mk vars (GTy.mk typ) in
        StrMap.add name v idenv, Env.add v tys env, decl
      end
    else 
      idenv, env, decl
  with System.Checker.Untypeable err ->
    Format.printf "%s:@.untypeable: %s@." name err.title;
    err.descr |> Option.iter (Format.printf "%s@." ) ;
    if not opts.mlsem && opts.debug  then (* Still print the mlsem ast*)
      Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp mlsem_ast ;
    idenv, env, decl

(** past: the parsed AST *)
let infer_fun_def visible_name opts (idenv, env, decl) past = 
  let name = 
    match past with 
    | _,PAst.Fundef (_,name, _, _) -> name
    | _,PAst.Struct (Ast.Struct (name, _)) -> name
    | _ -> failwith "Expected a function definition or a struct declaration at the top level."
  in
  let visible = visible_name name in 

  let e = PAst.transform {PAst.id = idenv; decl} past in
  if opts.ast && visible then
    Printf.printf "%s\n" (Ast.show_e e);
  match e with
  | _, decl', _, Ast.Noop -> (idenv, env, decl')
  | _, decl', _, _ -> infer_ast visible opts (idenv, env, decl') e

let run_on_file opts filename idenv env =
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "File not found: %s" filename);
  let cst = Parser.parse_file filename in
  if opts.cst then Parser.print_res cst;
  let past = Parser.to_ast cst in
  let visible_name = make_substring_pred opts.filter in
  if opts.past then begin
    let visible_past =
      match opts.filter with
      | None -> past
      | Some _ ->
          List.filter
            (function
              | _, PAst.Fundef (_, name, _, _) -> visible_name name
              | _, PAst.Struct _ -> false)
            past
    in
    Printf.printf "%s\n" (PAst.show_definitions visible_past)
  end;
  let idenv, env, _ = List.fold_left (infer_fun_def visible_name opts) (idenv, env, Ast.DeclMap.empty) past in
  (idenv, env)

let run_on_files opts filenames ?entry_points idenv env =
  (* Parse all the files to past *)
  let pasts = List.map (fun filename ->
      if not (Sys.file_exists filename) then
        failwith (Printf.sprintf "File not found: %s" filename);
      if opts.debug then
        Printf.printf "Parsing file: %s\n" filename;
      let cst = Parser.parse_file filename in
      if opts.cst then Parser.print_res cst;
      let past = Parser.to_ast cst in
      (filename, past)
    ) filenames in
  let call_graph = Call_graph.of_past_list (List.map snd pasts) in
  let call_graph = match entry_points with
    | None -> call_graph
    | Some entries -> Call_graph.keep_reachable call_graph entries in
  (* Only keep the transitive closure of the entry points *)
  let filtered_pasts = List.concat_map (fun (filename, past) ->
      List.filter_map (fun item ->
        match item with
        | (_,PAst.Fundef (_, name, _, _)) when Call_graph.Callgraph.has_node call_graph name ->
            Some (filename, item)
        | _ -> None
      ) past
    ) pasts in
  (* Sort the call graph *)
  let sorted_pasts = Call_graph.topo_sort filtered_pasts call_graph in

  let visible_name = make_substring_pred opts.filter in
  if opts.past then begin
    let visible_past = match opts.filter with
      | None -> List.map snd sorted_pasts
      | Some _ ->
          List.filter_map (fun (_, item) ->
            match item with
            | _, PAst.Fundef (_, name, _, _) when visible_name name -> Some item
            | _ -> None
          ) sorted_pasts
    in
    Printf.printf "%s\n" (PAst.show_definitions visible_past)
  end;
  let idenv, env, _ =
    List.fold_left
      (fun acc (_, item) -> infer_fun_def visible_name opts acc item)
      (idenv, env, Ast.DeclMap.empty)
      sorted_pasts
  in
  (idenv, env)


let run_on_package opts path idenv env =
  let native_calls = Package.find_native_calls path in
  if opts.debug then begin
    Printf.printf "Native calls found in package %s:\n" path;
    List.iter (fun (func_name, convention) ->
      Printf.printf "  %s: %s\n" func_name (Package.calling_convention_to_string convention)
    ) native_calls;
    Printf.printf "\n"
  end;

  (*Get all C files in src/ *)
  let c_files = Package.get_c_files path in
  if opts.debug then begin
    Printf.printf "C source files found in package %s:\n" path;
    List.iter (fun filepath ->
      Printf.printf "  %s\n" filepath
    ) c_files;
    Printf.printf "\n"
  end;
  (* Infer types for .Call entrypoints *)
  let entry_points = native_calls |> List.filter_map (fun (func_name, convention) ->
    match convention with
    | Package.Call -> Some func_name
    | _ -> None
  ) in
  (* Count entry points by calling convention *)
  let count_convention conv =
    List.length (List.filter (fun (_, c) -> c = conv) native_calls)
  in
  let n_call = count_convention Package.Call in
  let n_c = count_convention Package.C in
  let n_fortran = count_convention Package.Fortran in
  let n_external = count_convention Package.External in
  Printf.printf "Entry points detected: Call=%d, C=%d, Fortran=%d, External=%d\n" n_call n_c n_fortran n_external;
  Printf.printf "Entry points for .Call convention:\n";
  List.iter (fun entry -> Printf.printf "  %s\n" entry) entry_points;
  Printf.printf "\n";
  run_on_files opts c_files ~entry_points idenv env
  
let () =
  Mlsem_types.PEnv.add_printer_param (Rstt.Pp.printer_params ()) ;
  Mlsem_system.Config.normalization_fun := Rstt.Simplify.partition_vecs

let%test "filter predicate with Some substring" =
  let pred = make_substring_pred (Some "from") in
  pred "from_str" && pred "substring_from" && not (pred "to_str")

let%test "filter predicate with None accepts all" =
  let pred = make_substring_pred None in
  pred "anything" && pred "everything"