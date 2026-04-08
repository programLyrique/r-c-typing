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
    SET_VECTOR_ELT(l, 1, ScalarInteger(6));
    UNPROTECT(1);
    return l;
}

SEXP create_named_list_set_only_one() {
    const char *names[] = {"a", "b", ""};
    SEXP l = PROTECT(mkNamed(VECSXP, names));
    SET_VECTOR_ELT(l, 0, ScalarInteger(3));
    UNPROTECT(1);
    return l;
}

int nested_initializer_list() {
    int m[2][2] = {{1, 2}, {3, 4}};
    return m[0][0] + m[1][1];
}

int non_constant_initializer_list(int x) {
    int a[2] = {x, x + 1};
    return a[0] + a[1];
}