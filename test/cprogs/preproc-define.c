#include <Rinternals.h>

#define ONE 1L
#define MAGIC 0x2A

SEXP add_one(SEXP x) {
    int y = INTEGER(x)[0] + ONE;
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    INTEGER(result)[0] = y;
    UNPROTECT(1);
    return result;
}

SEXP add_magic(SEXP x) {
    int y = INTEGER(x)[0] + MAGIC;
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    INTEGER(result)[0] = y;
    UNPROTECT(1);
    return result;
}

/* Identifier-alias macros: bare identifier on the RHS is treated as an
   alias and resolved at each use site to pick up the target's binding. */
#define KEEP PROTECT
#define FREE UNPROTECT

SEXP alias_simple(SEXP x) {
    SEXP r = KEEP(allocVector(INTSXP, 1));
    INTEGER(r)[0] = INTEGER(x)[0];
    FREE(1);
    return r;
}

/* Transitive alias chain: ALIAS_A -> ALIAS_B -> PROTECT. */
#define ALIAS_A ALIAS_B
#define ALIAS_B PROTECT

SEXP alias_chain(SEXP x) {
    SEXP r = ALIAS_A(allocVector(INTSXP, 1));
    INTEGER(r)[0] = INTEGER(x)[0];
    UNPROTECT(1);
    return r;
}

/* Cyclic aliases: must not crash or hang during parse. No use site
   references CYC_X or CYC_Y, so they contribute nothing to the output. */
#define CYC_X CYC_Y
#define CYC_Y CYC_X
