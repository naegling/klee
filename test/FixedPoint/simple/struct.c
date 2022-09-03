
#include <stdio.h>
#include <math.h>
#include <klee/klee.h>

struct my_struct {
  double f0;
  int f1;
};

struct my_struct a;

int main(int argc, char *argv[]) {

  klee_make_symbolic(&a, sizeof(a), "a");

  double b = a.f0 / 4;
  if (b > 1.0)  {
    printf("gt1\n");
  } else if (b < 1.0) {
    printf("lt1\n");
  } else {
    printf("eq1\n");
  }
}
