open R_c_typing
open Mlsem.Types
module System = Mlsem.System

let usage () =
  prerr_endline "Usage: package_test.exe <package-path>";
  exit 2

let () =
  if Array.length Sys.argv <> 2 then usage ();
  let package_path = Sys.argv.(1) in
  
  (* Options: enable parsing of the PAst and typing, but do not print CST/AST/MLsem. *)
  let opts = Runner.{ cst = false; past = false; ast = false; mlsem = false;
                      typing = true; debug = false; filter = None; timeout = None;
                      fallback_c_signature = false; call_graph = None } in
  Parser.set_warn_unsupported false;
  let idenv = Runner.StrMap.empty in
  let env = Defs.initial_env in
  ignore (PEnv.sequential_handler PEnv.empty 
    (fun path -> Runner.run_on_package opts path idenv env)
    package_path);
  (* Ensure all output is flushed before exiting *) 
  flush stdout;
  Format.pp_print_flush Format.std_formatter ()
