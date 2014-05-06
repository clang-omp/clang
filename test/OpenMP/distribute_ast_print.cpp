// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

void foo() {}

int main (int argc, char **argv) {
  int b = argc, c, d, e, f, g;
  static int a;
// CHECK: static int a;
#pragma omp distribute
// CHECK-NEXT: #pragma omp distribute
  for (int i=0; i < 2; ++i)a=2;
// CHECK-NEXT: for (int i = 0; i < 2; ++i)
// CHECK-NEXT: a = 2;
#pragma omp target
#pragma omp teams
#pragma omp distribute private(argc,b),firstprivate(argv, c), collapse(2), dist_schedule(static)
  for (int i = 0; i < 10; ++i)
  for (int j = 0; j < 10; ++j)foo();
// CHECK-NEXT: #pragma omp target
// CHECK-NEXT: #pragma omp teams
// CHECK-NEXT: #pragma omp distribute private(argc,b) firstprivate(argv,c) collapse(2) dist_schedule(static)
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: for (int j = 0; j < 10; ++j)
// CHECK-NEXT: foo();
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp distribute dist_schedule(static, argc)
// CHECK: #pragma omp distribute dist_schedule(static, argc)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
  return (0);
}

#endif
