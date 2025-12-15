/* Test file for cast expressions */

// int test_simple_cast() {
//     double x = 3.14;
//     int y = (int)x;  /* Cast double to int */
//     return y;
// }

// int test_multiple_casts() {
//     double a = 1.9;
//     double b = 2.1;
//     int x = (int)a;  /* Cast double to int */
//     int y = (int)b;  /* Cast double to int */
//     return x + y;
// }

// int test_nested_cast() {
//     double x = 3.7;
//     double y = 2.3;
//     int z = (int)(x + y);  /* Cast result of expression */
//     return z;
// }

int test_ptr_cast() {
    int a = 42;
    void* p = (void*)&a;  /* Cast int* to void* */
    int* q = (int*)p;     /* Cast void* back to int* */
    return *q;
}
