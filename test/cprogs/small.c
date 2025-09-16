#define VOID void

// This is just a test
VOID small_f(int test) {
    if (test > 0) {
        printf("test is positive\n");
    } else {
        printf("test is non-positive\n");
    }
    return;
}