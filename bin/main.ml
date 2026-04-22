open R_c_typing
open Cmdliner
open Mlsem.Types
module System = Mlsem.System


let positive_float =
  let parse value =
    match float_of_string_opt value with
    | Some seconds when seconds > 0. -> Ok seconds
    | Some _ -> Error (`Msg "timeout must be strictly positive")
    | None -> Error (`Msg (Printf.sprintf "invalid float: %s" value))
  in
  Arg.conv (parse, Format.pp_print_float)

let main opts include_dirs path =
  System.Config.infer_overload := true;
  Mlsem.Types.Recording.start_recording ();
  let env_dirs = match Sys.getenv_opt "C_INCLUDE_PATH" with
    | None | Some "" -> []
    | Some s -> String.split_on_char ':' s |> List.filter (fun x -> x <> "")
  in
  (* Ask the system compiler where it looks for headers. On Debian this
     picks up /usr/include/x86_64-linux-gnu (multiarch), where most libs'
     headers now actually live. Falls back to the hardcoded list if no
     compiler is available. *)
  let gcc_dirs = R_c_typing.Utils.detect_gcc_include_dirs () in
  R_c_typing.Parser.set_include_dirs
    (include_dirs @ env_dirs @ gcc_dirs @ R_c_typing.Parser.default_include_dirs);
  let idenv = Runner.StrMap.empty in
  let env = Defs.initial_env in
  if not (Sys.file_exists path) then
    failwith (Printf.sprintf "Path not found: %s" path);
  let is_package = Sys.is_directory path in
  Printf.printf "Typing %s: %s\n"
    (if is_package then "package" else "file")
    path;
  if is_package then
    Runner.run_on_package opts path idenv env |> ignore
  else
    Runner.run_on_file opts path idenv env |> ignore;
  Mlsem.Types.Recording.save_to_file "mlsem_recording.json" (Mlsem.Types.Recording.tally_calls ());
  ()

let cst_opt =
  let doc = "Print CST (concrete syntax tree)" in
  Arg.(value & flag & info ["cst"] ~doc)

let past_opt =
  let doc = "Print parsed AST" in
  Arg.(value & flag & info ["past"] ~doc)

let ast_opt =
  let doc = "Print AST (typed abstract syntax tree)" in
  Arg.(value & flag & info ["ast"] ~doc)

let mlsem_opt = 
  let doc = "Print MLsem AST" in
  Arg.(value & flag & info ["mlsem"] ~doc)

let no_typing_opt =
  let doc = "Disable type inference" in
  Arg.(value & flag & info ["no-typing"] ~doc)

let debug_opt = 
  let doc = "Enable debug mode" in
  Arg.(value & flag & info ["debug"] ~doc)

let filter_opt =
  let doc = "Filter output to only *show* variables matching the given substring" in
  Arg.(value & opt (some string) None & info ["f";"filter"] ~docv:"SUBSTRING" ~doc)

let include_dir_opt =
  let doc = "Add a system include search directory (repeatable). Also honors C_INCLUDE_PATH env var." in
  Arg.(value & opt_all string [] & info ["I"; "include-dir"] ~docv:"DIR" ~doc)

let timeout_opt =
  let doc = "Set a per-function timeout in seconds for full function body inference/checking (default: no timeout)." in
  Arg.(value & opt (some positive_float) None & info ["timeout"] ~docv:"SECONDS" ~doc)

let path_arg =
  let doc = "C source file to parse or package directory to analyze" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PATH" ~doc)


let cmd =
  let open Term.Syntax in
  Cmd.v
    (Cmd.info "r-c-typing")
    (let+ cst = cst_opt
     and+ past = past_opt
     and+ ast = ast_opt
     and+ mlsem = mlsem_opt
     and+ no_typing = no_typing_opt
     and+ debug = debug_opt
     and+ filter = filter_opt
     and+ include_dirs = include_dir_opt
     and+ timeout = timeout_opt
    and+ path = path_arg in
    PEnv.sequential_handler PEnv.empty (fun path -> main {cst; past; ast; mlsem ; typing = not no_typing ; debug ; filter; timeout} include_dirs path) path |> fst)
     

let () = exit (Cmd.eval cmd)