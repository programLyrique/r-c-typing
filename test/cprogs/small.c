#include <Rinternals.h>

// This is just a test
SEXP small_f(SEXP x, int test) {
    //test = test + 1;
    int y = 1;
    double z;
    if (test > 0) {
        printf("test is positive\n");
    } else {
        printf("test is non-positive\n");
    }
    return x;
}

SEXP incr(SEXP a) {
    if (!isInteger(a) || LENGTH(a) != 1) {
        error("Input must be a single integer");
    }
    int val = INTEGER(a)[0];
    val = val + 1;
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    INTEGER(result)[0] = val;
    UNPROTECT(1);
    return result;
}