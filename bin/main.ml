open R_c_typing
open Cmdliner
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
}


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

let infer_ast opts (idenv, env) (ast : Ast.e) =
  try 
    let name,v = 
      match ast with 
      | _,Ast.Function (name, _, _, _) -> name,MVariable.create Immut (Some name)
      | _ -> failwith "Expected a function definition at the top level."
    in
    let mlsem_ast = Ast.to_mlsem ast in 
    if opts.mlsem then 
      Format.printf "%a@." Mlsem.System.Ast.pp mlsem_ast;
    let _env = extend_env mlsem_ast env in
    let renvs = System.Refinement.refinement_envs env mlsem_ast in
    let reconstructed = System.Reconstruction.infer env renvs mlsem_ast in
    let typ = System.Checker.typeof_def env reconstructed mlsem_ast in
    let tys = TyScheme.norm_and_simpl typ in 
    Format.printf "%a: %a@.@." Variable.pp v TyScheme.pp_short tys ;
    let (vars, typ) = TyScheme.get tys in 
    let typ = GTy.ub typ in 
    Format.printf "Upper bound: %a@.@." Ty.pp typ ;
    (* We only keep the upper bound as type for v and add it to the environment *)
    let tys = TyScheme.mk vars (GTy.mk typ) in
    StrMap.add name v idenv, Env.add v tys env
  with System.Checker.Untypeable err ->
    Format.printf "Untypeable: %s@." err.title;
    err.descr |> Option.iter (Format.printf "%s@." ) ;
    idenv, env

(** past: the parsed AST *)
let infer_fun_def opts (idenv, env) past = 
  let e = PAst.transform  {PAst.id = idenv} past in
  if opts.ast then
    Printf.printf "%s\n" (Ast.show_e e);
 infer_ast opts (idenv, env) e


let main opts filename =
  System.Config.infer_overload := false ;
  Mlsem.Types.Recording.start_recording ();
  let cst = Parser.parse_file filename in
  if opts.cst then Parser.print_res cst;
  let past = Parser.to_ast cst in
  if opts.past then
    Printf.printf "%s\n" (PAst.show_definitions past);
  let idenv = StrMap.empty in
  let env = Defs.initial_env in
  List.fold_left (infer_fun_def opts) (idenv, env) past |> ignore;
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
     and+ filename = file_arg in
     PEnv.sequential_handler PEnv.empty (fun filename -> main {cst; past; ast; mlsem} filename) filename |> fst)
     

let () = exit (Cmd.eval cmd)