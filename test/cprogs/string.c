#include <Rinternals.h>

SEXP create_str_from_c_string() {
    return mkString("hello");
}

SEXP create_str_from_chr() {
    return ScalarString(mkChar("world"));
}

const char* get_c_string_from_str(SEXP s) {
    if (TYPEOF(s) != STRSXP || LENGTH(s) == 0) {
        return NULL; 
    }
    return CHAR(STRING_ELT(s, 0));
}

bool is_blank_string(SEXP s) {
   return StringBlank(s);
}


SEXP to_char(SEXP s) {
    return asChar(s);
}