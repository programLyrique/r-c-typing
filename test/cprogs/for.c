#include <Rinternals.h>

SEXP sum_all(SEXP v) {
    if(!isReal(v)) {
        error("Input must be a real vector");
    }
    int n = LENGTH(v);
    double total = 0.0;
    double *vals = REAL(v);
    int i = 0;
    for (i = 0; i < n; i++) {
        total += vals[i];
    }
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = total;
    UNPROTECT(1);
    return result;
}