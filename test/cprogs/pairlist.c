#include <Rinternals.h>

SEXP reflist_add(SEXP x, SEXP target)
{
    if (!Rf_isPairList(x))
        error("Not a LISTSXP");
    return (Rf_cons(target, x));
}