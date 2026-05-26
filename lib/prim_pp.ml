(** Custom printer for the [Rstt.Prim] tag.

    Rstt's stock printer renders a primitive component like [Prim.mk Chr.any]
    as just [chr], which collides with [chr] used as the [.ty] shorthand for
    [v(chr)] (a vector). This override wraps non-trivial components in
    [p(...)] so [p(chr)] round-trips through the printer and stops colliding
    with the vector form.

    The mutable [wrap] flag lets [Vec_pp] suppress wrapping while printing the
    inner of a vector, since [v(chr)] / [chr5] are still meant to print
    without [p(...)]. *)

open Rstt

type t = TAny | TAny' | TComp of Sstt.Printer.descr

let any_p = Prim.destruct Prim.any
let any_p' = Prim.destruct Prim.any'

let wrap = ref true

let without_wrap f =
  let old = !wrap in
  wrap := false ;
  Fun.protect ~finally:(fun () -> wrap := old) f

let to_t (ctx : Printer.build_ctx) comp =
  let (_, pty) = Op.TagComp.as_atom comp in
  if Ty.leq pty any_p && VarSet.is_empty (Ty.vars_toplevel pty)
  then
    if Ty.leq any_p pty then Some TAny
    else if Ty.equiv any_p' pty then Some TAny'
    else Some (TComp (ctx.build pty))
  else None

let map f = function
  | TAny -> TAny
  | TAny' -> TAny'
  | TComp d -> TComp (f d)

let print _prec _assoc fmt = function
  | TAny -> Format.fprintf fmt "prim"
  | TAny' -> Format.fprintf fmt "^prim"
  | TComp d ->
    if !wrap then Format.fprintf fmt "p(%a)" Pp.print_descr d
    else Format.fprintf fmt "%a" Pp.print_descr d

let printer_builder = Printer.builder ~to_t ~map ~print

let printer_params =
  Printer.{ aliases = []; extensions = [(Prim.tag, printer_builder)] }
