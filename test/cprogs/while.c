#include <Rinternals.h>

SEXP test_while(SEXP n) {
    int i = 0;
    while(INTEGER(n)[0] != 0) {
        INTEGER(n)[0] = INTEGER(n)[0] + 1;
    }
    return n;
}

SEXP sum_while(SEXP v) {
    int i = 0;
    int total = 0;
    int n = LENGTH(v);
    int* vals = REAL(v);
    while(i < n) {
        total = total + vals[i];
        i = i + 1;
    }
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = total;
    UNPROTECT(1);
    return result;
}