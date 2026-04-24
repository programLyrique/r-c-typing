// Packages sometimes define their own typedef alias for R's core SEXP type
// rather than using SEXP directly. The canonical form is R's own
// [typedef struct SEXPREC *SEXP;]. Resolving a [Ptr] whose pointee is
// [struct SEXPREC] to [SEXP] lets base.ty signatures (length, xlength, ...)
// apply unchanged.
//
// This file mimics vctrs/rlang's [typedef struct SEXPREC r_obj;] without
// depending on Rinternals.h.

typedef struct SEXPREC r_obj;
typedef long R_xlen_t;

// Declared here so the checker picks the base.ty binding for [Rf_xlength]
// (automatic Rf_ alias of [xlength]) via its normal lookup chain.
R_xlen_t Rf_xlength(r_obj* x);

// Direct application through the typedef — the interesting case.
R_xlen_t r_length(r_obj* x) {
  return Rf_xlength(x);
}

// Through a chain of typedefs.
typedef r_obj my_obj;
R_xlen_t my_length(my_obj* x) {
  return Rf_xlength(x);
}

// Raw [struct SEXPREC *] (no typedef at all) should resolve the same way.
R_xlen_t raw_length(struct SEXPREC* x) {
  return Rf_xlength(x);
}
