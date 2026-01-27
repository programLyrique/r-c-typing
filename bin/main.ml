open R_c_typing
open Cmdliner
open Mlsem.Types
module System = Mlsem.System


let main opts filename =
  System.Config.infer_overload := true;
  Mlsem.Types.Recording.start_recording ();
  let idenv = Runner.StrMap.empty in
  let env = Defs.initial_env in
  Runner.run_on_file opts filename idenv env |> ignore;
  Mlsem.Types.Recording.save_to_file "mlsen_recording.json" (Mlsem.Types.Recording.tally_calls ());
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

let file_arg =
  let doc = "C source file to parse" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)


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
     and+ filename = file_arg in
     PEnv.sequential_handler PEnv.empty (fun filename -> main {cst; past; ast; mlsem ; typing = not no_typing ; debug ; filter} filename) filename |> fst)
     

let () = exit (Cmd.eval cmd)