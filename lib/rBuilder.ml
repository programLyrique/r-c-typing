open Vectors
open Mlsem.Types

module Ext = struct
  type prim = INT | LGL | DBL | CLX | CHR | RAW
  (* type t =
    | Vec of bool * prim
    | AnyVec of bool
    | Scalar of prim
    | AnyScalar
    | Ell of t TyExpr.t *)
  type t =
  | Vec of t TyExpr.t * t TyExpr.t
  | AnyVec
  | Na
  | Prim of bool * prim
  | AnyPrim of bool


  let prim_to_typ p =
    match p with
    | INT -> Prim.int
    | LGL -> Prim.lgl
    | DBL -> Prim.dbl
    | CLX -> Prim.clx
    | CHR -> Prim.chr
    | RAW -> Prim.raw

  let to_typ f t =
    match t with
    | AnyPrim true -> Prim.any_na
    | AnyPrim false -> Prim.any
    | Na -> Prim.na
    | Prim (b,p) ->
      let ty = prim_to_typ p in
      if b then Ty.cup Prim.na ty else ty
    | AnyVec -> Vecs.any
    | Vec (p,i) -> Vecs.mk (f p) (f i)
    (* | Vec (false, INT) -> Vecs.int
    | Vec (false, LGL) -> Vecs.lgl
    | Vec (false, DBL) -> Vecs.dbl
    | Vec (false, CLX) -> Vecs.clx
    | Vec (false, CHR) -> Vecs.chr
    | Vec (false, RAW) -> Vecs.raw
    | Vec (true, INT) -> Vecs.int_na
    | Vec (true, LGL) -> Vecs.lgl_na
    | Vec (true, DBL) -> Vecs.dbl_na
    | Vec (true, CLX) -> Vecs.clx_na
    | Vec (true, CHR) -> Vecs.chr_na
    | Vec (true, RAW) -> Vecs.raw_na
    | AnyVec false -> Vecs.vec
    | AnyVec true -> Vecs.vec_na
    | Scalar INT -> Scalars.int
    | Scalar LGL -> Scalars.lgl
    | Scalar DBL -> Scalars.dbl
    | Scalar CLX -> Scalars.clx
    | Scalar CHR -> Scalars.chr
    | Scalar RAW -> Scalars.raw
    | AnyScalar -> Scalars.scalar *)
end

include (Builder'.Make(Ext))

type type_def =
| Sig of string * type_expr
| Aliases of (string * string list * type_expr) list
type type_defs = type_def list