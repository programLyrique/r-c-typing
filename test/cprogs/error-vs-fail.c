#include <Rinternals.h>

// This should fail to type
SEXP assertPositive_error(SEXP v) {
    if(!isReal(v) || LENGTH(v) != 1) {
        error("Input must be a single real number");
    }
    double x = REAL(v)[0];
    if (x < 0.){
        error("Input is not positive");
    }

    return v;
}

SEXP assertPositive_fail(SEXP v) {
    if(!isReal(v) || LENGTH(v) != 1) {
        error("Input must be a single real number");
    }
    double x = REAL(v)[0];
    if (x < 0.){
        fail("Input is not positive");
    }

    return v;
}