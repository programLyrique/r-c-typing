#include <Rinternals.h>

int sizeof_type_pattern() {
    return sizeof(int);
}

int sizeof_expr_pattern(unsigned long x) {
    return sizeof x;
}

unsigned long sized_decl_pattern(unsigned short a) {
    unsigned long acc = a;
    unsigned long step = sizeof(unsigned short);
    return acc + step;
}

int mixed_pattern(unsigned long a, unsigned int b) {
    unsigned long tmp = a + b;
    return (int)(tmp + sizeof(tmp));
}

typedef int Sint;

SEXP sint_to_sexp(Sint x) {
    return ScalarInteger(x);
}

int nested_initializer_list() {
    int m[2][2] = {{1, 2}, {3, 4}};
    return m[0][0] + m[1][1];
}

int non_constant_initializer_list(int x) {
    int a[2] = {x, x + 1};
    return a[0] + a[1];
}