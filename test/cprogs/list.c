#include <Rinternals.h>


SEXP create_pos_list() {
    return allocVector(VECSXP, 3);
}

SEXP create_list() {
    int n = 3;
    return allocVector(VECSXP, n);
}

SEXP create_named_list() {
    const char *names[] = {"a", "b", ""};
    return mkNamed(VECSXP, names);
}

SEXP create_list_const_prop() {
    int n = 3, m = 4;
    return allocVector(VECSXP, n + m);
}

SEXP create_list_const_prop2() {
    return allocVector(VECSXP, 3 + 4);
}

SEXP create_named_list_with_elems() {
    const char *names[] = {"a", "b", ""};
    SEXP l = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(l, 0, ScalarReal(3));
    SET_VECTOR_ELT(l, 1, ScalarReal(6));
    UNPROTECT(1);
    return l;
}

SEXP create_named_list_with_int() {
    const char *names[] = {"a", "b", ""};
    SEXP l = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(l, 0, ScalarInteger(3));
    //SET_VECTOR_ELT(l, 1, ScalarInteger(6));
    UNPROTECT(1);
    return l;
}