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

(* TODO: better to build the regex only once *)
let contains_substring s sub =
  let open Str in
  try
    ignore (search_forward (regexp_string sub) s 0);
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

let infer_ast visible opts (idenv, env) (ast : Ast.e) =
  let name,v = 
    match ast with 
    | _,_,Ast.Function (name, _, _, _) -> name,MVariable.create Immut (Some name)
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
        StrMap.add name v idenv, Env.add v tys env
      end
    else 
      idenv,env
  with System.Checker.Untypeable err ->
    Format.printf "%s:@.untypeable: %s@." name err.title;
    err.descr |> Option.iter (Format.printf "%s@." ) ;
    if not opts.mlsem && opts.debug  then (* Still print the mlsem ast*)
      Format.printf "MLsem AST:@.%a@." Mlsem.System.Ast.pp mlsem_ast ;
    idenv, env

(** past: the parsed AST *)
let infer_fun_def opts (idenv, env) past = 
  let name = 
    match past with 
    | _,PAst.Fundef (_,name, _, _) -> name
  in
  let visible = Option.map (fun s -> contains_substring name s ) opts.filter |> Option.value  ~default:true in 

  let e = PAst.transform  {PAst.id = idenv} past in
  if opts.ast && visible then
    Printf.printf "%s\n" (Ast.show_e e);
 infer_ast visible opts (idenv, env) e

let run_on_file opts filename idenv env =
  if not (Sys.file_exists filename) then
    failwith (Printf.sprintf "File not found: %s" filename);
  let cst = Parser.parse_file filename in
  if opts.cst then Parser.print_res cst;
  let past = Parser.to_ast cst in
  if opts.past then
    Printf.printf "%s\n" (PAst.show_definitions past);
  List.fold_left (infer_fun_def opts) (idenv, env) past

  let () =
    Mlsem_types.PEnv.add_printer_param (Rstt.Pp.printer_params ()) ;
    Mlsem_system.Config.normalization_fun := Rstt.Simplify.partition_vecs