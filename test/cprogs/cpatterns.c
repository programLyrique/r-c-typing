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