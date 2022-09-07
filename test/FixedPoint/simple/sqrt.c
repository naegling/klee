
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

int main(int argc, char *argv[]) {

  double a;
  double b;
  // double c;
  klee_make_symbolic(&a, sizeof(a), "a");

  b = sqrt(a/2);
  b = sqrt(b/2);

  if (b > 1.0)  {
    printf("gt1\n");
  } else if (b < 1.0) {
    printf("lt1\n");
  } else {
    printf("eq1\n");
  }
}
