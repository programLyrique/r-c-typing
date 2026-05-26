(** Custom printer for the [Rstt.Vec] tag.

    Mirrors the upstream [Rstt.Vec] printer (so [v(chr)] still renders as
    [chr], [v[5](chr)] as [chr5], etc.) but disables [Prim_pp.wrap] for every
    nested [Pp.print_descr] call. Without this, the [p(...)] wrapping that
    [Prim_pp] applies to bare primitives would leak into vector shorthand,
    turning [chr5] into [p(chr)5]. *)

open Rstt

let prim_int = Prim.mk Prim.Int.any'

(* Same as [Rstt]'s internal [Utils.prune_printer_descr] (not exposed in the
   public API): replaces sub-descriptors that subsume [any] with a bare
   [Builtin Any] so the printer does not redundantly spell out their
   structure. *)
let prune_printer_descr ~any d =
  let rec aux d =
    let any_d = { Printer.ty = any; op = Printer.Builtin Printer.Any } in
    if Ty.leq any d.Printer.ty then any_d
    else
      let op =
        match d.Printer.op with
        | Varop (Cap, lst) ->
          let lst =
            match
              List.filter (fun d -> Ty.leq any d.Printer.ty |> not) lst
            with
            | [] -> [ any_d ]
            | lst -> List.map aux lst
          in
          Printer.Varop (Cap, lst)
        | Varop (Cup, lst) -> Varop (Cup, List.map aux lst)
        | Binop (Diff, d1, d2) -> Binop (Diff, aux d1, d2)
        | op -> op
      in
      { Printer.op; ty = d.ty }
  in
  aux d

let to_t (ctx : Printer.build_ctx) comp =
  let ty = Descr.mk_tagcomp comp |> Ty.mk_descr in
  if Ty.leq ty Vec.any then Some (Vec.destruct ty |> Vec.map ctx.build)
  else None

let map = Vec.map

let print prec assoc fmt t =
  let ccmp f a b r = if r <> 0 then r else f a b in
  let cmp v1 v2 =
    match (v1, v2) with
    | Vec.VarLength (l1, v1), Vec.VarLength (l2, v2) ->
      Pp.Compare.descr l1 l2 |> ccmp Pp.Compare.descr v1 v2
    | AnyLength v1, AnyLength v2 -> Pp.Compare.descr v1 v2
    | CstLength (n1, v1), CstLength (n2, v2) ->
      Stdlib.compare n1 n2 |> ccmp Pp.Compare.descr v1 v2
    | VarLength _, _ -> -1
    | _, VarLength _ -> 1
    | AnyLength _, _ -> -1
    | _, AnyLength _ -> 1
  in
  let shortcut_v v =
    let str = Format.asprintf "%a" Pp.print_descr v in
    let prefix = "^" in
    if String.starts_with ~prefix str then
      String.sub str (String.length prefix)
        (String.length str - String.length prefix)
    else str
  in
  let print_v ~len fmt v =
    if Ty.leq Prim.any v.Printer.ty then Format.fprintf fmt "vec%s" len
    else if Ty.equiv Prim.any' v.Printer.ty then
      Format.fprintf fmt "^vec%s" len
    else if Prim.is_simple v.Printer.ty then
      Format.fprintf fmt "%a%s" Pp.print_descr v len
    else
      let v = prune_printer_descr ~any:Prim.any v in
      Format.fprintf fmt "%a%s(%a)" Tag.pp Vec.tag len Pp.print_descr v
  in
  let print_atom _prec _assoc fmt = function
    | Vec.VarLength (l, v) ->
      let l = prune_printer_descr ~any:prim_int l in
      let len = Format.asprintf "@[<h>[%a]@]" Pp.print_descr l in
      Format.fprintf fmt "%a" (print_v ~len) v
    | AnyLength v -> Format.fprintf fmt "%a" (print_v ~len:"") v
    | CstLength (n, v) ->
      if n = 1 && Prim.is_singleton v.Printer.ty then
        Format.fprintf fmt "%s" (shortcut_v v)
      else
        let len = Format.asprintf "%i" n in
        Format.fprintf fmt "%a" (print_v ~len) v
  in
  Prim_pp.without_wrap @@ fun () ->
  let t = t |> List.map (fun (p, ns) -> ([ p ], ns)) in
  Pp.print_non_empty_dnf ~cmp ~any:"" print_atom prec assoc fmt t

let printer_builder = Printer.builder ~to_t ~map ~print

let printer_params =
  Printer.{ aliases = []; extensions = [(Vec.tag, printer_builder)] }
