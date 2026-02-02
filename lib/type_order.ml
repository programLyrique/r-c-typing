(** Terms in the type schemes are not ordered, which is a problem for the non-regression tests.
We order them by lexicographic order. *)
open Sstt
open Mlsem.Types



let pp fmt ts = 
  let _,gty = TyScheme.get ts in
  let ty = GTy.ub gty in 
  let s = TVOp.shorten_names (TVOp.vars ty) in
  let ty = Subst.apply s ty in 
  let ast = Printer.get (PEnv.printer_params ()) ty in 
  (* Ensure extensions can still be pretty-printed deterministically (used by non-regression tests).
  Returns string*op. The string is carried around and used to order terms when an operator is commutative. 
  TODO: We might rather use string list instead of string so that 
  a | bd -> u < ab | d -> u *)
  let rec aux ast =
    Printer.(
      match ast with
      | Extension ext -> Printf.printf "Extension found\n";
          (* Specialize for the various extension printers *)
          (Format.asprintf "%a"
            (fun fmt ext -> print_extension_node_ctx (-1) NoAssoc fmt ext)
            ext,
             ast)
      | Alias s  -> (s, Alias s)
      | Node n -> ((if NodeId.has_name n then NodeId.name n else NodeId.hash n |> string_of_int), Node n)
      | Builtin Empty -> ("empty", ast)
      | Builtin _ -> ("any", ast)
      | Var v -> (Var.name v, ast)
      | Enum e -> (Sstt.Enum.name e, ast)
      | Tag (tag, d) -> let s,d' = aux d.op in ((Sstt.Tag.name tag) ^ s, Tag (tag, {op=d'; ty=d.ty;}))
      | Interval (low, high) -> ((Option.fold ~none:"" ~some:Z.to_string low) ^ ".." ^ (Option.fold ~none:"" ~some:Z.to_string high), ast)
      | Record  _ -> (* TODO?*) ("", ast)
      | Varop (op, l) -> let res = List.map (fun d -> let (s, op) = aux d.op in (s, {op;ty=d.ty})) l in 
        Format.printf "Varop before sorting: %a@." (Format.pp_print_list (fun fmt (s, _) -> Format.fprintf fmt "%s" s)) res ;
        let res = List.sort (fun (s1, _)(s2, _) -> String.compare s1 s2) res in 
        let s,d' = List.split res in 
        (String.concat "" s, Varop (op, d'))
      | Binop (op, l, r) -> 
          (* Diff and Arrow. They are not commutative! *)
          let s1, l' = aux l.op in 
          let s2, r' = aux r.op in 
          (s1 ^ s2, Binop (op, {op=l'; ty=l.ty}, {op=r'; ty=r.ty}))
      | Unop (op, v) -> 
          let s, v' = aux v.op in 
          (s, Unop (op, {op=v'; ty=v.ty}))
    )
  in
  Format.printf "Main op before ordering: %a@." (fun fmt ast -> Printer.print fmt ast) ast;
  let _,res = aux ast.main.op in 
  let ast = { ast with main = { ast.main with op = res } } in
  Printer.print fmt ast