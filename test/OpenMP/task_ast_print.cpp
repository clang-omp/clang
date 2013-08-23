// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// expected-no-diagnostics

void foo() {}

int main (int argc, char **argv) {
  int b = argc, c, d, e, f, g;
  static int a;
// CHECK: static int a;
#pragma omp task
  a=2;
// CHECK-NEXT: #pragma omp task
// CHECK-NEXT: a = 2;
#pragma omp task if(b), final(a), untied, default(shared) mergeable, private(argc,b),firstprivate(argv, c),shared(d,f)
  foo();
// CHECK: #pragma omp task if(.omp.if.var.) final(.omp.final.var.) untied default(shared) mergeable private(argc,b) firstprivate(argv,c) shared(d,f)
// CHECK-NEXT: foo();
  return (0);
}
