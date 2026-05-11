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


// // from package clue 
typedef struct{
  int        n; 
  double   **C; 
  double   **c; 
  int       *s; 
  int       *f; 
  int       na; 
  int     runs; 
  double  cost; 
  time_t rtime; 
} AP;

// ap_free currently takes too long to type: 1min43s on a PC (2026)
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

AP *ap_create_problem(double *t, int n)
{
    int i,j;
    AP *p;
    
    p = (AP*) malloc(sizeof(AP)); 
    if(p == NULL)
	return NULL;
    
   p->n = n;

  //   p->C  = (double **) malloc((n + 1) * sizeof(double *));
  //   p->c  = (double **) malloc((n + 1) * sizeof(double *));
  //   if(p->C == NULL || p->c == NULL)
	// return NULL;
    
  //   for(i = 1; i <= n; i++){
	// p->C[i] = (double *) calloc(n + 1, sizeof(double));
	// p->c[i] = (double *) calloc(n + 1, sizeof(double));
	// if(p->C[i] == NULL || p->c[i] == NULL)
	//     return NULL;
  //   }
  
    
    for(i = 1; i <= n; i++)
	for( j = 1; j <= n; j++){
	    p->C[i][j] = t[n*(j - 1) + i - 1];
	    // p->c[i][j] = t[n*(j - 1) + i - 1];
	}
    p->cost = 0;
    p->s = NULL;
    p->f = NULL;
    return p;
}