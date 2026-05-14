#include <Rinternals.h>

SEXP larger_than_3(SEXP v)
{
    if (!isInteger(v) || LENGTH(v) != 1)
    {
        fail("Input must be a single integer"); // empty -> any
    }
    int val = INTEGER(v)[0]; // int[1] -> C_int
    if (val == 3)
    {                                        // (C_int,C_int) -> C_bool
        error("Input must be larger than 3"); // any -> empty
    }
    return v;
}

SEXP size_bound(SEXP v)
{
    if (LENGTH(v) > 10)
    {
        fail("Input length must be at most 10");
    }
    return v;
}

void bail_if(int err, const char *what)
{
    if (err)
        Rf_errorcall(R_NilValue, "System failure for: %s (%s)", what, strerror(errno));
}

int cond_on_int(int v)
{
    if (v)
    {
        fail("Input must be zero");
    }
    return v;
}

int isNull(int *ptr)
{
    if (!ptr)
    {
        fail("Input pointer must be NULL");
    }
    return 0;
}

SEXP C_escape_chars(SEXP x) {
  if (!isString(x))
    fail("x must be a character vector.");
}
SEXP C_escape_chars_one(SEXP str) {
  // This is just a stub for testing the type of the return value of this function
//   if(TYPEOF(str) != CHARSXP) {
//     fail("Expected a single charsxp");
//   }
  return str;
}

SEXP C_escape_chars2(SEXP x) {
  if (!isString(x))
    fail("x must be a character vector.");
  if (x == R_NilValue || Rf_length(x) == 0)
    return x;

  int len = Rf_length(x);
  SEXP out = PROTECT(allocVector(STRSXP, len));

  for (int i=0; i<len; i++) {
    SET_STRING_ELT(out, i, C_escape_chars_one(STRING_ELT(x, i)));
  }
  UNPROTECT(1);
  return out;
}

//#include <string.h>

char * strstr(const char *haystack, const char *needle);

SEXP R_split_string(SEXP string, SEXP split){
  const char * str = CHAR(STRING_ELT(string, 0));
  cetype_t enc = Rf_getCharCE(STRING_ELT(string, 0));
  const char * cut = CHAR(STRING_ELT(split, 0));
  char * out = strstr(str, cut);
  char * out = cut;
  if(!out)
    return string;
  SEXP res = PROTECT(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(res, 0, Rf_mkCharLenCE(str, out - str, enc));
  SET_STRING_ELT(res, 1, Rf_mkCharCE(out + strlen(cut), enc));
  UNPROTECT(1);
  return res;
}
