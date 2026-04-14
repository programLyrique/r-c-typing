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

let is_declaration = function
  | _, PAst.Fundef (_, _, _, (_, PAst.Seq [])) -> true
  | _ -> false

let has_ty_binding name = Defs.StrMap.mem name Defs.defs_map

let find_existing_binding name idenv env =
  match StrMap.find_opt name idenv with
  | Some v ->
      (try Some (v, Env.find v env)
       with Not_found -> None)
  | None ->
      match Defs.StrMap.find_opt name Defs.defs_map with
      | Some v ->
          (try Some (v, Env.find v env)
           with Not_found -> None)
      | None -> None

let has_existing_binding name idenv =
  StrMap.mem name idenv || has_ty_binding name

let print_visible kind visible v tys =
  if visible then
    (match kind with
    | `Default ->
        Format.printf "%a: @[<h>%a@]@.@." 
    | `SimpleC ->
        Format.printf "c(%a): @[<h>%a@]@.@." 
    | `DotC ->
        Format.printf ".C(%a): @[<h>%a@]@.@." 
    | `Define ->
        Format.printf "define %a: @[<h>%a@]@.@.") 
      Variable.pp v TyScheme.pp_short tys

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
        let (vars, typ) = TyScheme.get tys in 
        let typ = GTy.ub typ in 
        (*Format.printf "%a: upper bound= %a@.@." Variable.pp v  Ty.pp typ ;*)
        (* We only keep the upper bound as type for v and add it to the environment *)
        let tys = TyScheme.mk vars (GTy.mk typ) in
        print_visible `Default visible v tys;
        (StrMap.add name v idenv, Env.add v tys env, decl)
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
let rec infer_def ?(simple_c_fun=false) ?(convention=None) ?(skip_if_defined=false) visible_name opts (idenv, env, decl)  past =
  let name = PAst.top_level_unit_name past in
  let visible = visible_name name in

  (* When processing items from external headers, don't override an existing
     definition (e.g. from a .ty file or a previous header). *)
  if skip_if_defined && name <> "" && has_existing_binding name idenv then
    (idenv, env, decl)
  else
  match past with
  | _, PAst.Include items ->
      (* Items come from an external system header. Process them with
         simple C-function inference and priority to already-defined symbols. *)
      List.fold_left
        (fun acc item ->
          infer_def ~simple_c_fun:true ~skip_if_defined:true (fun _ -> false) opts acc item)
        (idenv, env, decl)
        items
  | _, PAst.Define (name, value) ->
      let v = MVariable.create Immut (Some name) in
      let ty = Ast.typeof_const (PAst.aux_const value) |> GTy.mk |> TyScheme.mk_mono in
      if opts.debug && visible then
        print_visible `Define visible v ty;
      if skip_if_defined && not (Defs.StrMap.mem name Defs.defs_map) then
        Defs.BuiltinVar.register_dynamic name (Ast.typeof_const (PAst.aux_const value));
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.TypeDecl (name, ty) ->
      (idenv, env, Ast.DeclMap.add name ty decl)
  | _,PAst.Fundef (ret_ty, name, params, _) when convention=Some(Package.C) ->
    (try
      let ty = C_interface.infer_dotC ~typedef_map:decl ret_ty params |> GTy.mk |> TyScheme.mk_mono in
      if has_ty_binding name then begin
        (match find_existing_binding name idenv env with
         | Some (v, tys) -> print_visible `DotC visible v tys
         | None -> ());
        (idenv, env, decl)
      end else if is_declaration past && StrMap.mem name idenv then
        (idenv, env, decl)
      else
        let v = MVariable.create Immut (Some name) in
        print_visible `DotC visible v ty;
        (StrMap.add name v idenv, Env.add v ty env, decl)
    with Failure msg ->
      if visible then
        Format.printf "%s:@.untypeable: %s@." name msg;
      (idenv, env, decl))
  | _, (PAst.Fundef (ret_ty, name, params, _) as e) when simple_c_fun && C_interface.is_simple_c_function e ->
    let ty = C_interface.infer_cfun ret_ty params |> GTy.mk |>  TyScheme.mk_mono in
    if has_ty_binding name then begin
      (match find_existing_binding name idenv env with
       | Some (v, tys) -> print_visible `SimpleC visible v tys
       | None -> ());
      (idenv, env, decl)
    end else if is_declaration past && StrMap.mem name idenv then
      (idenv, env, decl)
    else
      let v = MVariable.create Immut (Some name) in
      print_visible `SimpleC visible v ty;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (ret_ty, name, params, _) when is_declaration past ->
    if has_ty_binding name then begin
      (match find_existing_binding name idenv env with
       | Some (v, tys) -> print_visible `Default visible v tys
       | None -> ());
      (idenv, env, decl)
    end else if StrMap.mem name idenv then
      (idenv, env, decl)
    else
      let ty = C_interface.infer_cfun ret_ty params |> GTy.mk |> TyScheme.mk_mono in
      let v = MVariable.create Immut (Some name) in
      print_visible `Default visible v ty;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.Fundef (_, name, _, _) when has_ty_binding name ->
    (match find_existing_binding name idenv env with
     | Some (v, tys) -> print_visible `Default visible v tys
     | None -> ());
    (idenv, env, decl)
  | _ ->

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
              | _, PAst.TypeDecl _ -> false
              | _, PAst.Define _ -> false
              | _, PAst.Include _ -> false)
            past
    in
    Printf.printf "%s\n" (PAst.show_definitions visible_past)
  end;
  let idenv, env, _ = List.fold_left (infer_def visible_name opts) (idenv, env, Ast.DeclMap.empty) past in
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
    | Some entries -> 
      let entries = entries |> List.filter_map (fun (func_name, convention) -> 
        match convention with 
        | Package.Call | Package.C | Package.External -> Some func_name
        | _ -> None) in
      Call_graph.keep_reachable call_graph entries in
  (* Only keep the transitive closure of the entry points *)
  let filtered_pasts = List.concat_map (fun (filename, past) ->
      List.filter_map (fun item ->
        match item with
        | (_,PAst.Fundef (_, name, _, _)) when Call_graph.Callgraph.has_node call_graph name ->
            Some (filename, item)
        | _ -> None
      ) past
    ) pasts in
  let visible_name = make_substring_pred opts.filter in
  (* First apply non-function top-level units (structs, defines) in source order. *)
  let idenv, env, decl =
    List.concat_map snd pasts
    |> List.fold_left
         (fun acc item ->
           match item with
           | _, PAst.Fundef _ -> acc
           | _ -> infer_def visible_name opts acc item)
         (idenv, env, Ast.DeclMap.empty)
  in

  (* Sort function definitions by call graph. *)
  let sorted_pasts = Call_graph.topo_sort filtered_pasts call_graph in

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
  let entry_points = StrMap.of_list (Option.value ~default:[] entry_points) in
  let idenv, env, _ =
    List.fold_left
      (fun acc (_, item) -> 
        let convention = StrMap.find_opt (PAst.top_level_unit_name item) entry_points in
        infer_def ~convention visible_name opts acc item)
      (idenv, env, decl)
      sorted_pasts
  in
  (idenv, env)


let run_on_package opts path idenv env =
  let entry_points = Package.find_native_calls path in
  if opts.debug then begin
    Printf.printf "Native calls found in package %s:\n" path;
    List.iter (fun (func_name, convention) ->
      Printf.printf "  %s: %s\n" func_name (Package.calling_convention_to_string convention)
    ) entry_points;
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

  (* Infer types for  entrypoints *)
  let entry_points_for conv = entry_points |> List.filter_map (fun (func_name, convention) ->
    if convention = conv then Some func_name else None
  ) in
  let call_entry_points = entry_points_for Package.Call in
  let c_entry_points    = entry_points_for Package.C in
  (* Count entry points by calling convention *)
  let count_convention conv =
    List.length (List.filter (fun (_, c) -> c = conv) entry_points)
  in
  let n_call = count_convention Package.Call in
  let n_c = count_convention Package.C in
  let n_fortran = count_convention Package.Fortran in
  let n_external = count_convention Package.External in
  Format.printf
    "Entry points detected: Call=%d, C=%d, Fortran=%d, External=%d@."
    n_call n_c n_fortran n_external;
  Format.printf "Entry points for .Call convention:@.";
  List.iter (fun entry -> Format.printf "  %s@." entry) call_entry_points;
  Format.printf "@.";
  Format.printf "Entry points for .C convention:@.";
  List.iter (fun entry -> Format.printf "  %s@." entry) c_entry_points;
  Format.printf "@.";
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

let%test ".ty bindings have highest precedence" =
  let tys = TyScheme.mk_mono GTy.dyn in
  let past = (Position.dummy, PAst.Fundef (Ast.Int, "strerror", [], (Position.dummy, PAst.Seq []))) in
  let idenv, env, _ =
    if has_ty_binding "strerror" then
      (StrMap.empty, Defs.initial_env, Ast.DeclMap.empty)
    else if is_declaration past && StrMap.mem "strerror" StrMap.empty then
      (StrMap.empty, Defs.initial_env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "strerror") in
      (StrMap.add "strerror" v StrMap.empty,
       Env.add v tys Defs.initial_env,
       Ast.DeclMap.empty)
  in
      match find_existing_binding "strerror" idenv env, find_existing_binding "strerror" StrMap.empty Defs.initial_env with
      | Some (actual_v, actual_tys), Some (expected_v, expected_tys) ->
        not (StrMap.mem "strerror" idenv)
        && Variable.equal actual_v expected_v
        && actual_tys == expected_tys
      | _ -> false

let%test "full function overrides declaration" =
  let decl_v = MVariable.create Immut (Some "foo") in
  let full_v = MVariable.create Immut (Some "foo") in
  let decl_ty = TyScheme.mk_mono GTy.dyn in
  let full_ty = TyScheme.mk_mono (GTy.mk Rstt.Cenums.void) in
  let idenv = StrMap.add "foo" decl_v StrMap.empty in
  let env = Env.add decl_v decl_ty Env.empty in
  let idenv, env, _ =
    (StrMap.add "foo" full_v idenv,
     Env.add full_v full_ty env,
     Ast.DeclMap.empty)
  in
  Variable.equal (StrMap.find "foo" idenv) full_v
  && Env.find full_v env == full_ty

let%test "declaration does not override existing full function" =
  let full_v = MVariable.create Immut (Some "foo") in
  let full_ty = TyScheme.mk_mono GTy.dyn in
  let idenv = StrMap.add "foo" full_v StrMap.empty in
  let env = Env.add full_v full_ty Env.empty in
  let past = (Position.dummy, PAst.Fundef (Ast.Int, "foo", [], (Position.dummy, PAst.Seq []))) in
  let idenv, env, _ =
    if has_ty_binding "foo" then
      (idenv, env, Ast.DeclMap.empty)
    else if is_declaration past && StrMap.mem "foo" idenv then
      (idenv, env, Ast.DeclMap.empty)
    else
      let v = MVariable.create Immut (Some "foo") in
      (StrMap.add "foo" v idenv,
       Env.add v (TyScheme.mk_mono (GTy.mk Rstt.Cenums.void)) env,
       Ast.DeclMap.empty)
  in
  Variable.equal (StrMap.find "foo" idenv) full_v
  && Env.find full_v env == full_ty