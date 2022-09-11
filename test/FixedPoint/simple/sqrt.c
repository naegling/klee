
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

int main(int argc, char *argv[]) {

  double a;
  double b;
  klee_make_symbolic(&a, sizeof(a), "a");

  b = sqrt(a);
  b = sqrt(b);

  if (b > 1.0)  {
    printf("gt\n");
  } else if (b < 1.0) {
    printf("lt\n");
  } else {
    printf("eq\n");
  }
}
