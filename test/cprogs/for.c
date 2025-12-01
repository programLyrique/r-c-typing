#include <Rinternals.h>

SEXP sum_all(SEXP v) {
    // if(!isReal(v)) {
    //     error("Input must be a real vector");
    // }
    // R_xlen_t n = XLENGTH(v);
    // double total = 0.0;
    // double *vals = REAL(v);
    for (R_xlen_t i = 0; i < n; i++) {
        total += vals[i];
    }
    // SEXP result = PROTECT(allocVector(REALSXP, 1));
    // REAL(result)[0] = total;
    // UNPROTECT(1);
    // return result;
}