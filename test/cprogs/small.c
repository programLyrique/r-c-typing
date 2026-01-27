#include <Rinternals.h>

// This is just a test for parsing
/*SEXP small_f(SEXP x, int test) {
    //test = test + 1;
    int y = 1;
    double z;
    if (test > 0) {
        printf("test is positive\n");<
    } else {
        printf("test is non-positive\n");
    }
    return x;
}*/

SEXP change_int(SEXP result) {
    INTEGER(result)[0] = 3;
    return result;
}

SEXP errorIntScalar(SEXP a) {
    if (!isInteger(a) || LENGTH(a) != 1) {
        error("Input must be a single integer"); // Ty.empty -> Ty.any
    }
}

SEXP make_scalar() {
    SEXP result = PROTECT(allocVector(INTSXP, 1)); 
    UNPROTECT(1);
    return result;
}


int isIntScalar(SEXP a) {
    if (!isInteger(a) || LENGTH(a) != 1) {
        return 0;
    }
    return 1;
}


// //make_scalar and isIntScalar were correctly typed and stored in the typing environment
SEXP check() {
    SEXP res = make_scalar();
    if(!isIntScalar(res)) {
        error("Should be an integer scalar");
    }
    return 0;
}

SEXP incr(SEXP a) {
    if (!isInteger(a) || LENGTH(a) != 1) {
        error("Input must be a single integer"); // Ty.empty -> Ty.any
    }
    int val = INTEGER(a)[0]; // int[1] -> C_int
    val = val + 1; // (C_int,C_int) -> C_int
    SEXP result = PROTECT(allocVector(INTSXP, 1)); // INTSXP * 1 -> int[1] 
    INTEGER(result)[0] = val;
    UNPROTECT(1);
    return result;
}

INLINE_FUN Rboolean isFunction(SEXP s)
{
    return (TYPEOF(s) == CLOSXP ||
	    TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isPrimitive(SEXP s)
{
    return (TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}

INLINE_FUN Rboolean isNewList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == VECSXP);
}