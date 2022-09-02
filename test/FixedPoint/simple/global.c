
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

double a = 0.0f;

int main(int argc, char *argv[]) {

  klee_make_symbolic(&a, sizeof(a), "a");

  double b = (2 * a) + 1;
  if (b > 1.0)  {
    printf("gt1\n");
  } else if (b < 1.0) {
    printf("lt1\n");
  } else {
    printf("eq1\n");
  }
}
