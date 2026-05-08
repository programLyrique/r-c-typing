#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP entry_leaf(SEXP x)
{
    return x;
}

SEXP entry_root(SEXP x)
{
    return entry_leaf(x);
}

SEXP target_leaf(SEXP x)
{
    return x;
}

SEXP target_root(SEXP x)
{
    return target_leaf(x);
}

SEXP unreachable(SEXP x)
{
    return x;
}

static const R_CallMethodDef callMethods[] = {
    {"entry_root", (DL_FUNC)&entry_root, 1},
    {NULL, NULL, 0}};

void R_init_filterclosure(DllInfo *info)
{
    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
