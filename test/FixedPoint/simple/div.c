
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

int main(int argc, char *argv[]) {

  double a;
  klee_make_symbolic(&a, sizeof(a), "a");

  double b = a / 4;
  if (b > 1.0)  {
    printf("gt\n");
  } else if (b < 1.0) {
    printf("lt\n");
  } else {
    printf("eq\n");
  }
}
