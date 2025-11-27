open Mlsem.Types

module Prim = struct (* TODO: extension (for printing) *)
  let tt = Enum.define "tt" |> Enum.typ
  let ff = Enum.define "ff" |> Enum.typ
  let int = Enum.define "int" |> Enum.typ
  let lgl =
    let t = Ty.disj [tt;ff] in
    (* Enum.define "lgl" |> Enum.typ *)
    PEnv.add_printer_param { extensions=[] ; aliases=[t,"lgl"] } ;
    t
  let dbl = Enum.define "dbl" |> Enum.typ
  let clx = Enum.define "clx" |> Enum.typ
  let chr = Enum.define "chr" |> Enum.typ
  let raw = Enum.define "raw" |> Enum.typ
  let any =
    let t = Ty.disj [int;lgl;dbl;clx;chr;raw] in
    PEnv.add_printer_param { Sstt.Printer.aliases = [t, "prim"] ; Sstt.Printer.extensions = [] } ;
    t
  let na = Enum.define "na" |> Enum.typ
  let any_na =
    let t = Ty.disj [na;any] in
    PEnv.add_printer_param { Sstt.Printer.aliases = [t, "prim?"] ; Sstt.Printer.extensions = [] } ;
    t
end

module C  = struct 
  let int = Ty.int
  let zero = Ty.interval (Some Z.zero) (Some Z.zero)
  let not_zero = Ty.diff Ty.int zero
  let double = Enum.define "C_double" |> Enum.typ

  let one = Ty.interval (Some Z.one) (Some Z.one)
  let not_one = Ty.diff Ty.int one
  let str = Enum.define "C_str" |> Enum.typ
  let void = Enum.define "C_void" |> Enum.typ
end


module Null = struct
  let null = Enum.define "Null" |> Enum.typ
end

module Vecs = struct
  open Sstt.Prec
  open Sstt

  let tag = Sstt.Tag.mk' "v" (Sstt.Tag.Monotonic {preserves_cap=true; preserves_cup=false})
  let mk v l =
    let ty = Tuple.mk [Ty.cap v Prim.any_na ; Ty.cap l Mlsem.Types.Ty.int] in
    TagComp.mk (tag, ty) |> Descr.mk_tagcomp |> Ty.mk_descr
  let mk_singl v = mk v (Mlsem.Types.Ty.interval (Some Z.one) (Some Z.one))
  let mk_unsized v = mk v Mlsem.Types.Ty.int
  let any = mk Ty.any Ty.any

  let map f l =
    l |> List.map (fun ((v,l),ns) ->
       ((f v, f l), ns |> List.map (fun (v,l) -> (f v, f l)))
      )

  let extract_pair (_,ty) =
    if Ty.vars_toplevel ty |> VarSet.is_empty |> not then invalid_arg "Invalid vector encoding." ; 
    Ty.get_descr ty |> Descr.get_tuples |> Tuples.get 2 |>
    Op.TupleComp.approx |> (function [a;b] -> a,b | _ -> assert false)
  let extract dnf =
    dnf |> List.map (fun (ps, ns) ->
      let vs,ls = ps |> List.map extract_pair |> List.split in
      let v, l = Ty.conj vs, Ty.conj ls in
      let ns = ns |> List.map extract_pair in
      (v,l), ns
    )

  let to_t node ctx comp =
    let dnf = TagComp.dnf comp in
    let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
    if Ty.leq ty any then Some (extract dnf |> map (node ctx))
    else None

  let destruct ty =
    ty |> Ty.get_descr |> Descr.get_tags |> Tags.get tag |> TagComp.dnf |> extract
  let length ty =
    let l ((_,l),_) = l in
    destruct ty |> List.map l |> Ty.disj
  let content ty =
    let v ((v,_),_) = v in
    destruct ty |> List.map v |> Ty.disj

  let print_seq f sep =
    Format.(pp_print_list  ~pp_sep:(fun fmt () -> pp_print_string fmt sep) f)
  let print prec assoc fmt t =
    let print_atom fmt (v,l) =
      Format.fprintf fmt "%a[%a](%a)" Tag.pp tag
       Printer.print_descr l Printer.print_descr v
    in
    let print_atom_neg fmt (v,l) =
      let sym,_,_ = unop_info Neg in
      Format.fprintf fmt "%s%a" sym print_atom (v,l)
    in
    let print_line prec assoc fmt (a, ns) =
      if ns <> [] then
        let sym,_,_ as opinfo = varop_info Cap in
        fprintf prec assoc opinfo fmt "%a%s%a"
          print_atom a sym (print_seq print_atom_neg sym) ns
      else
        Format.fprintf fmt "%a" print_atom a
    in
    let sym,prec',_ as opinfo = varop_info Cup in
    fprintf prec assoc opinfo fmt "%a" (print_seq (print_line prec' NoAssoc) sym) t

  let printer_builder =
    Printer.builder ~to_t:to_t ~map:map ~print:print
  let printer_params = Printer.{ aliases = []; extensions = [(tag, printer_builder)]}
  let () = Mlsem.Types.PEnv.add_printer_param printer_params
end