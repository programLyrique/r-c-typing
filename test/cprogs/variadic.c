int my_printf(const char *fmt, ...);

int call_no_varargs(void) {
  return my_printf("hello");
}

int call_one_int(int x) {
  return my_printf("%d", x);
}

int call_int_and_str(int x, const char *s) {
  return my_printf("%d %s", x, s);
}
