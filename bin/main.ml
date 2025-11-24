open R_c_typing
open Cmdliner
open Mlsem.Common
open Mlsem.Types
module System = Mlsem.System
module MVariable = Mlsem.Lang.MVariable

module StrMap = Map.Make(String)


(* idenv: str -> Variable.t 
   env: typing environment: Variable.t -> TyScheme.ty 
*)

(**  Give the any type to any free variables (not in the environment) *)
let extend_env mlast env =
let fv = System.Ast.fv mlast in
let dom = Env.domain env |> VarSet.of_list in
let missing = VarSet.diff fv dom in
missing |> VarSet.elements |> List.fold_left
  (fun env v -> Env.add v (TyScheme.mk_mono GTy.dyn) env) env

let infer_ast verbose (idenv, env) (ast : Ast.e) =
  try 
    let v = 
      match ast with 
      | _,Ast.Function (name, _, _, _) -> MVariable.create Immut (Some name)
      | _ -> failwith "Expected a function definition at the top level."
    in
    let mlsem_ast = Ast.to_mlsem ast in 
    if verbose then 
      Format.printf "%a@." Mlsem.System.Ast.pp mlsem_ast;
    let env = extend_env mlsem_ast env in
    let renvs = System.Refinement.refinement_envs env mlsem_ast in
    let reconstructed = System.Reconstruction.infer env renvs mlsem_ast in
    let typ = System.Checker.typeof_def env reconstructed mlsem_ast in
    let typ = TyScheme.norm_and_simpl typ in 
    Format.printf "%a: %a@.@." Variable.pp v TyScheme.pp_short typ ;
    idenv, env
  with System.Checker.Untypeable err ->
    Format.printf "Untypeable: %s@." err.title;
    err.descr |> Option.iter (Format.printf "%s@." ) ;
    idenv, env

(** past: the parsed AST *)
let _infer_fun_def verbose (idenv, env) past = 
  let e = PAst.transform  {PAst.id = idenv} past in
  if verbose then
    Printf.printf "%s\n" (Ast.show_e e);
 infer_ast verbose (idenv, env) e

type cmd_options = {
  cst : bool;
  past : bool;
  ast : bool;
  mlsem : bool;
}


let main opts filename =
  let cst = Parser.parse_file filename in
  if opts.cst then Parser.print_res cst;
  let past = Parser.to_ast cst in
  if opts.past then
    Printf.printf "%s\n" (PAst.show_definitions past);
  let env = {PAst.id = StrMap.empty} in
  let asts = List.map (PAst.transform env) past in 
  if opts.ast then
    Printf.printf "%s\n" (Ast.show_funcs asts);
  let mlsem_asts = List.map Ast.to_mlsem asts in
  if opts.mlsem then
    List.iter (fun mlsem_ast ->
      Format.printf "%a@." Mlsem.System.Ast.pp mlsem_ast
    ) mlsem_asts;
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
     main {cst; past; ast; mlsem} filename)

let () = exit (Cmd.eval cmd)