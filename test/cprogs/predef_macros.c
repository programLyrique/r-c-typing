/* Standard predefined preprocessor macros are injected by cpp at
   expansion time, so the C source sees bare identifiers. They are bound
   in base.ty as the non-singleton C types ([c_string] for [__FILE__],
   [__DATE__], [__TIME__]; [c_int] for [__LINE__]) so call sites resolve
   without producing "Creating fresh variable" warnings. */

extern void log_msg(const char *msg);
extern void log_line(int line);
extern int len(const char *s);

void use_file(void) {
  log_msg(__FILE__);
}

void use_line(void) {
  log_line(__LINE__);
}

void use_date_time(void) {
  log_msg(__DATE__);
  log_msg(__TIME__);
}

int file_len(void) {
  return len(__FILE__);
}
