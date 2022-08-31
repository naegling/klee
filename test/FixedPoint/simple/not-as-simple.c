
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

int main(int argc, char *argv[]) {

  double a;
  klee_make_symbolic(&a, sizeof(a), "a");

  double b = sqrt((2 * a) + 1);
  if (b > 1.0)  {
    printf("gt1\n");
  } else if (b < 1.0) {
    printf("lt1\n");
  } else {
    printf("eq1\n");
  }
}
