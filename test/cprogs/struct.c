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

void h(struct Point* p) {
  int a = p->x;
  int b = p->y;
}

void h2(struct Point* p) {
  int a = (*p).x;
  int b = (*p).y;
}

void i(struct Point* p) {
  p->x = 3;
  p->y = 4;
}

void i2(struct Point* p) {
  (*p).x = 3;
  (*p).y = 4;
}