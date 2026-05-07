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


// Currently takes too long to type: 1min43s on a PC (2026)
// // from package clue 
// typedef struct{
//   int        n; 
//   double   **C; 
//   double   **c; 
//   int       *s; 
//   int       *f; 
//   int       na; 
//   int     runs; 
//   double  cost; 
//   time_t rtime; 
// } AP;


// void ap_free(AP *p)
// {
//     int i;

//     free(p->s);
//     free(p->f);

//     for(i = 1; i <= p->n; i++){
//       free(p->C[i]);
//       free(p->c[i]);
//     }
    
//     free(p->C);
//     free(p->c);
//     free(p);
// }