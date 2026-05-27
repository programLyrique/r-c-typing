#include <Rinternals.h>

/* Implicit C [void* -> T*] conversion at initialization.
   [malloc] returns the top pointer [c_ptr]; storing it into a concrete
   [char*] must narrow it to [char*], otherwise subscript writes and the
   char*-typed string API ([mkCharCE]) reject it.
   Before the fix, [s] stayed [c_ptr] and this was an untypeable application. */
SEXP C_malloc_to_charptr(int n) {
  char *s = malloc(n + 3);
  s[0] = '[';
  s[1] = ']';
  s[2] = '\0';
  return mkCharCE(s, 0);
}

/* A [void*] declaration must NOT clobber a more precise initializer: the
   conversion only fires for concrete (non-[void]) pointer targets. Here the
   initializer is already a [char*] (c_string), so [p] keeps it and the
   char*-typed use type-checks without an explicit cast. */
extern char *get_buf(void);
SEXP C_voidptr_keeps_precision(void) {
  void *p = get_buf();
  return mkCharCE(p, 0);
}
