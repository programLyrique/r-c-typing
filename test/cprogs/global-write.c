static int work[3];
static int counter;
static double shadowed;

void write_global_array(int *p) {
  work[0] = *p;
  *p = work[0];
}

void write_global_scalar(int v) {
  counter = v;
}

int read_global_scalar(void) {
  return counter;
}

int local_shadows_global(int *p) {
  int counter;
  counter = *p;
  return counter;
}

double read_shadowed_global(void) {
  return shadowed;
}

int local_shadows_different_global(int *p) {
  int shadowed;
  shadowed = *p;
  return shadowed;
}
