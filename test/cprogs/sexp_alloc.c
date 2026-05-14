#include <Rinternals.h>

/* Cases exercising the SEXPTYPE-int rewrite in PAst.process_call.
   Whenever Rf_allocVector / allocVector / R_allocResizableVector is called
   with a first arg that resolves to one of the known SEXPTYPE values
   (NILSXP=0, ..., INTSXP=13, REALSXP=14, ..., RAWSXP=24), the int is
   rewritten to the corresponding named [Id] so the call matches the
   [prim] domain of [allocVector] in [types/base.ty]. Out-of-range values
   are left alone and surface as a normal untypeable application. */

#define MY_TYPE 14   /* REALSXP */

enum vctrs_type_mirror {
  R_TYPE_logical = 10,
  R_TYPE_integer = 13,
  R_TYPE_double  = 14,
  R_TYPE_raw     = 24
};

/* (1) literal integer in range: 13 == INTSXP */
SEXP alloc_int_literal(R_xlen_t n) {
  return Rf_allocVector(13, n);
}

/* (2) enum constant: R_TYPE_integer == 13 == INTSXP */
SEXP alloc_int_enum(R_xlen_t n) {
  return Rf_allocVector(R_TYPE_integer, n);
}

/* (3) #define expanding to a literal int: MY_TYPE == 14 == REALSXP */
SEXP alloc_double_define(R_xlen_t n) {
  return Rf_allocVector(MY_TYPE, n);
}

/* (4) literal integer OUT of range: 99 is not a valid SEXPTYPE.
   The rewrite is skipped and the call fails as untypeable application. */
SEXP alloc_oob(R_xlen_t n) {
  return Rf_allocVector(99, n);
}
