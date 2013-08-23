// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// expected-no-diagnostics

void foo() {}

int main (int argc, char **argv) {
  int b = argc, c, d, e, f, g;
  static int a;
  #pragma omp threadprivate(a)
// CHECK: static int a;
// CHECK-NEXT: #pragma omp threadprivate(a)
#pragma omp single
  a=2;
// CHECK-NEXT: #pragma omp single
// CHECK-NEXT: a = 2;
#pragma omp parallel
#pragma omp single private(argc,b),firstprivate(argv, c),copyprivate(a) nowait
  foo();
// CHECK-NEXT: #pragma omp parallel
// CHECK-NEXT: #pragma omp single private(argc,b) firstprivate(argv,c) copyprivate(a) nowait
// CHECK-NEXT: foo();
  return (0);
}
