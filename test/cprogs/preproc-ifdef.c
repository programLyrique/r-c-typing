#include <Rinternals.h>

#define FEATURE_FLAG

#ifdef FEATURE_FLAG
SEXP selected_by_ifdef(SEXP x) {
    return x;
}
#else
SEXP skipped_ifdef(SEXP x) {
    return R_NilValue;
}
#endif

#ifndef MISSING_FLAG
SEXP selected_by_ifndef(SEXP x) {
    return x;
}
#else
SEXP skipped_ifndef(SEXP x) {
    return R_NilValue;
}
#endif

#ifdef __linux__
SEXP selected_by_linux_define(SEXP x) {
    return x;
}
#elifndef __linux__
SEXP skipped_elifndef(SEXP x) {
    return R_NilValue;
}
#else
SEXP skipped_linux_else(SEXP x) {
    return R_NilValue;
}
#endif