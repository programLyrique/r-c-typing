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
