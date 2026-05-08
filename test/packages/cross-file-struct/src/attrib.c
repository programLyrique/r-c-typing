struct r_obj_t {};
typedef struct r_obj_t r_obj;

struct r_globals_syms {
  r_obj* abort;
  r_obj* class_;
};

extern struct r_globals_syms r_syms;
extern r_obj* r_attrib_get(r_obj* x, r_obj* tag);

r_obj* my_class(r_obj* x) {
  return r_attrib_get(x, r_syms.class_);
}
