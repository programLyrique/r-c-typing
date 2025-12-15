open R_c_typing
open Mlsem.Types
module System = Mlsem.System

let usage () =
  prerr_endline "Usage: typing_test.exe <c-file>";
  exit 2

let () =
  System.Config.infer_overload := true ;
  if Array.length Sys.argv <> 2 then usage ();
  let full_c_file = Sys.argv.(1) in
  (* Options: enable parsing of the PAst and typing, but do not print CST/AST/MLsem. *)
  let opts = Runner.{ cst = false; past = false; ast = false; mlsem = false;
                      typing = true; debug = false; filter = None } in
  let idenv = Runner.StrMap.empty in
  let env = Defs.initial_env in
  (* Runner.run_on_file already prints the interesting output to stdout via
     Format/Printf, which dune will capture in the rules using with-stdout-to. *)
  ignore (PEnv.sequential_handler PEnv.empty 
    (fun filename -> Runner.run_on_file opts filename idenv env)
    full_c_file);
  (* Ensure all output is flushed before exiting *) 
  flush stdout;
  Format.pp_print_flush Format.std_formatter ()
