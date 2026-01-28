#include <Rinternals.h>

SEXP mk_value(SEXP a)
{
    if (TYPEOF(a) == INTSXP)
    {
        // SEXP result = PROTECT(allocVector(INTSXP, 1));
        // INTEGER(result)[0] = INTEGER(a)[0];
        // UNPROTECT(1);
        // return result;
        return a;
    }
    // else if (TYPEOF(a) == ENVSXP)
    // {
    //     // SEXP result = PROTECT(allocVector(INTSXP, 1));
    //     // INTEGER(result)
    //     // [0] = 2;
    //     // UNPROTECT(1);
    //     // return result;
    //     return a;
    // }
    else
    {
        error("Input must be integer or an environment");
    }
}
