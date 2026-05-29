#include <Rinternals.h>

/* Class attribute is specialised to v(chr). */
SEXP C_first_class(SEXP x) {
  return STRING_ELT(getAttrib(x, R_ClassSymbol), 0);
}

int C_n_classes(SEXP x) {
  return LENGTH(getAttrib(x, R_ClassSymbol));
}

/* Non-class attributes go through the generic any_sexp path. */
SEXP C_names(SEXP x) {
  return getAttrib(x, R_NamesSymbol);
}

SEXP C_dim(SEXP x) {
  return getAttrib(x, R_DimSymbol);
}

/* setAttrib(_, R_ClassSymbol, _): the result keeps the value's content and
   carries the class given by the third argument. When that argument's type
   exposes concrete class-name singletons (here mkString("myclass") :
   v[1](chr("myclass"))) the class is set to exactly those names. */
SEXP C_set_class(SEXP x) {
  return setAttrib(x, R_ClassSymbol, mkString("myclass"));
}

/* The value's content is preserved: tagging a VECSXP keeps the list shape. */
SEXP C_set_class_on_list(void) {
  SEXP l = allocVector(VECSXP, 2);
  return setAttrib(l, R_ClassSymbol, mkString("myclass"));
}

/* When the class value's names are not statically known, the class falls back
   to "any class" (attr_any). */
SEXP C_set_class_unknown(SEXP x, SEXP cls) {
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Build-up idiom: a multi-element class vector assembled with SET_STRING_ELT.
   allocVector(STRSXP,2) seeds "" (R's R_BlankString default) and each
   SET_STRING_ELT unions its string into the element type, so cls types as
   v[2]("" | "data.frame" | "tbl_df"). The class reader drops the "" allocation
   default, so setAttrib recovers exactly <data.frame, tbl_df>. */
SEXP C_set_class_buildup(SEXP x) {
  SEXP cls = allocVector(STRSXP, 2);
  SET_STRING_ELT(cls, 0, mkChar("tbl_df"));
  SET_STRING_ELT(cls, 1, mkChar("data.frame"));
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Partial build-up: slot 1 left unset, so cls soundly types as v[2]("" | "a")
   (slot 1 really is ""). The "" allocation default is still dropped from the
   recovered class, leaving <a>. */
SEXP C_set_class_buildup_partial(SEXP x) {
  SEXP cls = allocVector(STRSXP, 2);
  SET_STRING_ELT(cls, 0, mkChar("a"));
  return setAttrib(x, R_ClassSymbol, cls);
}

/* Refined branch: setAttrib installs a concrete class and getAttrib reads it
   back, narrowing to v[N](class-name singletons). LENGTH yields the class count
   and STRING_ELT preserves the singleton through its 'a parameter. */
SEXP C_refined_class(SEXP x) {
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return getAttrib(tagged, R_ClassSymbol);
}
int C_refined_class_length(SEXP x) {
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return LENGTH(getAttrib(tagged, R_ClassSymbol));
}
SEXP C_refined_class_first(SEXP x) {
  SEXP tagged = setAttrib(x, R_ClassSymbol, mkString("myclass"));
  return STRING_ELT(getAttrib(tagged, R_ClassSymbol), 0);
}
