#include <Rinternals.h>

SEXP create_str_from_c_string()
{
    return mkString("hello");
}

SEXP create_str_from_chr()
{
    return ScalarString(mkChar("world"));
}

SEXP create_str_from_concat()
{
    return mkString("hello"
                    " world");
}

const char *get_c_string_from_str(SEXP s)
{
    if (TYPEOF(s) != STRSXP || LENGTH(s) == 0)
    {
        return NULL;
    }
    return CHAR(STRING_ELT(s, 0));
}

bool is_blank_string(SEXP s)
{
    return StringBlank(s);
}

SEXP to_char(SEXP s)
{
    return asChar(s);
}

// type2char is too slow to type currently
// char *type_to_char(SEXP s)
// {
//     return type2char(TYPEOF(s));
// }

// from the curl package
SEXP make_url_names()
{
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 9));
    SET_STRING_ELT(names, 0, Rf_mkChar("url"));
    SET_STRING_ELT(names, 1, Rf_mkChar("scheme"));
    SET_STRING_ELT(names, 2, Rf_mkChar("host"));
    SET_STRING_ELT(names, 3, Rf_mkChar("port"));
    SET_STRING_ELT(names, 4, Rf_mkChar("path"));
    SET_STRING_ELT(names, 5, Rf_mkChar("query"));
    SET_STRING_ELT(names, 6, Rf_mkChar("fragment"));
    SET_STRING_ELT(names, 7, Rf_mkChar("user"));
    SET_STRING_ELT(names, 8, Rf_mkChar("password"));
    UNPROTECT(1);
    return names;
}


// From the jsonlite package (parse.c)
// Currently fails because we encode C strings differently than pointers
SEXP R_parse(SEXP x, SEXP bigint_as_char) {
      const char* json = translateCharUTF8(asChar(x));
      const int bigint = asLogical(bigint_as_char);

      if(json[0] == '\xEF' && json[1] == '\xBB' && json[2] == '\xBF'){
        warningcall(R_NilValue, "JSON string contains (illegal) UTF8 byte-order-mark!");
        json = json + 3;
      }

      if(json[0] == '\x1E'){
        json = json + 1;
      }
      // more
}
