open Mlsem.Common
open Mlsem.Types
module System = Mlsem.System
module MVariable = Mlsem.Lang.MVariable

module StrMap = Map.Make(String)

exception Inference_timeout of float

type cmd_options = {
  cst : bool;
  past : bool;
  ast : bool;
  mlsem : bool;
  typing : bool;
  debug : bool;
  filter: string option;
  timeout : float option;
  (* When full-body inference fails (untypeable, Not_found, or timeout), bind
     the function at its declared C signature ([ret_ty params -> ret_ty]) so
     callers can still be typed instead of cascading "unbound variable". The
     original error is still printed; the fallback is additionally reported on
     a [fallback:] line. Off by default. *)
  fallback_c_signature : bool;
  (* When [Some path], dump the call graph (after [keep_reachable]) in
     Graphviz .dot format to [path]. Cycles are colored, files clustered,
     entry points marked. *)
  call_graph : string option;
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

(**  Give the [dyn] (gradual) type to any free variables not already in the
     environment. These are C identifiers that neither had a [.ty] binding nor
     a declaration in the translation unit — e.g. calls into another package's
     C code. Binding them gradually lets inference tolerate the unknown type
     instead of propagating constraints from a free variable. *)
let extend_env mlast env =
  let fv = System.Ast.fv mlast in
  let dom = Env.domain env |> VarSet.of_list in
  let missing = VarSet.diff fv dom in
  missing |> VarSet.elements |> List.fold_left
    (fun env v -> Env.add v (TyScheme.mk_mono GTy.dyn) env) env

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

let register_enum_constants ty : (Variable.t * Ty.t) list =
  let rec aux ty =
    match ty with
    | Ast.Enum (_, enumerators) ->
        List.filter_map
          (fun (name, value) ->
            match value with
            | Some value ->
                let ty = Rstt.Cint.singl value in
                let v = Defs.BuiltinVar.register_dynamic name ty in
                Some (v, ty)
            | None -> None)
          enumerators
    | Ast.Ptr inner -> aux inner
    | Ast.Array (inner, _) -> aux inner
    | _ -> []
  in
  aux ty

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

let with_inference_timeout timeout thunk =
  match timeout with
  | None -> thunk ()
  | Some seconds ->
      let previous_handler =
        Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise (Inference_timeout seconds)))
      in
      let previous_timer = Unix.getitimer Unix.ITIMER_REAL in
      let restore () =
        ignore (Unix.setitimer Unix.ITIMER_REAL previous_timer);
        Sys.set_signal Sys.sigalrm previous_handler
      in
      Fun.protect
        ~finally:restore
        (fun () ->
          ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = seconds });
          thunk ())

let infer_ast ?fallback visible opts (idenv, env, decl) (ast : Ast.e) =
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
  (* [fallback] is a thunk that produces the declared C signature when body
     inference fails. Kept lazy so the cost (and possible [Failure] from
     [typeof_ctype]) is only paid on the exception path. The original error
     is still printed above; on success this adds a [fallback:] line naming
     the substituted type and binds [name] in env — stopping the "unbound
     variable" cascade through callers. *)
  let apply_fallback () =
    match fallback with
    | None -> (idenv, env, decl)
    | Some compute ->
        (try
          let ty = compute () in
          let tys = ty |> GTy.mk |> TyScheme.mk_mono in
          (* Indent the fallback line so downstream parsers (parse_output.R)
             don't mistake [fallback: <type>] for a new function header. *)
          if visible then
            Format.printf "  fallback: @[<h>%a@]@.@." TyScheme.pp_short tys;
          (StrMap.add name v idenv, Env.add v tys env, decl)
        with Failure _ -> (idenv, env, decl))
  in
  try
    if opts.typing then
        with_inference_timeout opts.timeout (fun () ->
          let _env = extend_env mlsem_ast env in
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
          (StrMap.add name v idenv, Env.add v tys env, decl))
    else
      idenv, env, decl
  with
  | Inference_timeout seconds ->
      Format.printf "%s:@.timeout: inference/checking exceeded %.6g seconds@." name seconds;
      if not opts.mlsem && opts.debug then
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp mlsem_ast ;
      apply_fallback ()
  | System.Checker.Untypeable err ->
      Format.printf "%s:@.untypeable: %s@." name err.title;
      err.descr |> Option.iter (Format.printf "%s@." ) ;
      if not opts.mlsem && opts.debug  then (* Still print the mlsem ast*)
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp mlsem_ast ;
      apply_fallback ()
  | Not_found ->
      (* Refinement/reconstruction occasionally raises Not_found when an
         identifier referenced in the body isn't bound in the env (e.g. R C
         API macros, file-scope globals). Treat as untypeable so the rest of
         the package still gets processed. *)
      Format.printf "%s:@.untypeable: internal error: Not_found@." name;
      if opts.debug then
        Format.eprintf "%s@." (Printexc.get_backtrace ()) ;
      if not opts.mlsem && opts.debug then
        Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp mlsem_ast ;
      apply_fallback ()

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
      let ty = Ast.typeof_const (PAst.aux_const value) |> GTy.mk |> TyScheme.mk_mono in
      let v =
        if skip_if_defined && not (Defs.StrMap.mem name Defs.defs_map) then
          Defs.BuiltinVar.register_dynamic name (Ast.typeof_const (PAst.aux_const value))
        else
          MVariable.create Immut (Some name)
      in
      if opts.debug && visible then
        print_visible `Define visible v ty;
      (StrMap.add name v idenv, Env.add v ty env, decl)
  | _, PAst.TypeDecl (name, ty) ->
      let env =
        register_enum_constants ty
        |> List.fold_left (fun env (v, ty) -> Env.add v (TyScheme.mk_mono (GTy.mk ty)) env) env
      in
      (idenv, env, Ast.DeclMap.add name ty decl)
  | _, PAst.GlobalVar (name, ctype) ->
      (* File-scope variable. A [.ty] binding always wins (manually-curated
         types must not be overridden). For every other pre-existing binding
         we overwrite: in well-formed C, repeated declarations have the same
         ctype so the env binding is unchanged, and when the prior binding
         came from the unknown-name fallback in [PAst.var] (type [any]) the
         more precise ctype-derived type takes over.

         [Env.replace] (not [Env.add]) is required because [register_dynamic]
         returns the same [Variable.t] for repeated declarations of a global
         across translation units (shared [extern] in a header included from
         several .c files). [Env.add] asserts the variable is unbound and
         would crash the package on the second sighting. *)
      if has_ty_binding name then
        (idenv, env, decl)
      else begin
        let resolved = PAst.resolve_ctype decl ctype in
        let ty =
          try Ast.typeof_ctype resolved
          with Failure _ -> Ty.any
        in
        let v = Defs.BuiltinVar.register_dynamic name ty in
        let tys = ty |> GTy.mk |> TyScheme.mk_mono in
        if opts.debug && visible then
          print_visible `Default visible v tys;
        (StrMap.add name v idenv, Env.replace v tys env, decl)
      end
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
    let ty = C_interface.infer_cfun ~typedef_map:decl ret_ty params |> GTy.mk |>  TyScheme.mk_mono in
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
      let ty = C_interface.infer_cfun ~typedef_map:decl ret_ty params |> GTy.mk |> TyScheme.mk_mono in
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
      (* Provide a declared-C-signature fallback for Fundefs when [opts] asks
         for it. Only meaningful for Fundefs — everything else reaching this
         branch (if any) just gets a best-effort inference with no fallback. *)
      let fallback =
        if opts.fallback_c_signature then
          match past with
          | _, PAst.Fundef (ret_ty, _, params, _) ->
              Some (fun () -> C_interface.infer_cfun ~typedef_map:decl ret_ty params)
          | _ -> None
        else None
      in
      match e with
      | _, decl', _, Ast.Noop -> (idenv, env, decl')
      | _, decl', _, _ -> infer_ast ?fallback visible opts (idenv, env, decl') e

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
              | _, PAst.GlobalVar _ -> false
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
  let entry_names = match entry_points with
    | None -> []
    | Some entries ->
      entries |> List.filter_map (fun (func_name, convention) ->
        match convention with
        | Package.Call | Package.C | Package.External -> Some func_name
        | _ -> None)
  in
  let call_graph = match entry_points with
    | None -> call_graph
    | Some _ -> Call_graph.keep_reachable call_graph entry_names in
  (* Optional: dump the call graph for inspection (after [keep_reachable]).
     A name -> source-file map is built from [pasts] for per-file clustering. *)
  (match opts.call_graph with
   | None -> ()
   | Some path ->
       let module H = Hashtbl in
       let file_of = H.create 256 in
       let rec scan filename = function
         | _, PAst.Fundef (_, n, _, _) -> H.replace file_of n (Filename.basename filename)
         | _, PAst.Include items -> List.iter (scan filename) items
         | _ -> ()
       in
       List.iter (fun (filename, items) ->
         List.iter (scan filename) items) pasts;
       let file_of_node n = H.find_opt file_of n in
       let dot = Call_graph.to_dot ~file_of_node ~entry_points:entry_names call_graph in
       let oc = open_out path in
       output_string oc dot;
       close_out oc;
       Printf.printf "Call graph written to %s\n" path);
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
  Mlsem_types.PrinterCfg.set_descr_printer Rstt.Pp.print_descr_ctx ;
  Mlsem_types.PrinterCfg.set_printer Rstt.Pp.print ;
  Mlsem_types.PrinterCfg.add_printer_param (Rstt.Pp.printer_params ()) ;
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