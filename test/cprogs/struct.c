#include <Rinternals.h>


struct Point {
  int x;
  int y;
};

void f(struct Point p) {
  int a = p.x;
  int b = p.y;
}

struct Point g() {
    struct Point p;
    p.x = 1;
    p.y = 2;
    return p;
}