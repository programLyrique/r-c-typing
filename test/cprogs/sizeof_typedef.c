/* Regression test: sizeof(typedef-name).
 * Tree-sitter has no symbol table, so [sizeof(AP)] is parsed as the
 * expression form rather than the type form. PAst rewrites it to
 * [sizeof((AP)0)] when [AP] is a known typedef so the typedef passes
 * through resolve_ctype instead of being looked up as a value.
 * Without the rewrite this fails with "unbound variable: AP". */

typedef struct { int n; double *c; } AP;

AP *ap_create(int n) {
    AP *p = (AP*) 0;
    int s = sizeof(AP);
    return p;
}
