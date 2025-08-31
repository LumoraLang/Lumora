#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

// Just print a string (side-effect only)
int printf_wrapper(const char* s) {
    if (!s) return -1;
    return fputs(s, stdout);
}

// Write file (no return string)
int c_write_file(const char* path, const char* content) {
    if (!path || !content) return -1;
    FILE* f = fopen(path, "w");
    if (!f) return -1;
    fputs(content, f);
    fclose(f);
    return 0;
}

// Math
double c_sqrt(double x) { return sqrt(x); }
double c_pow(double x, double y) { return pow(x, y); }

// Random
int c_rand() { 
    static int seeded = 0;
    if (!seeded) { srand((unsigned)time(NULL)); seeded = 1; }
    return rand(); 
}

// Example C function called from Lumora
int my_c_function(const char* str) {
    if (!str) return -1;
    printf("Received from Lumora: %s\n", str);
    return 0;
}

