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
#pragma omp for
// CHECK-NEXT: #pragma omp for
  for (int i=0; i < 2; ++i)a=2;
// CHECK-NEXT: for (int i = 0; i < 2; ++i)
// CHECK-NEXT: a = 2;
#pragma omp parallel
#pragma omp for private(argc,b),firstprivate(argv, c),lastprivate(d,f),reduction(+:e) reduction(min : g), ordered nowait, collapse(2), schedule(dynamic)
  for (int i = 0; i < 10; ++i)
  for (int j = 0; j < 10; ++j)foo();
// CHECK-NEXT: #pragma omp parallel
// CHECK-NEXT: #pragma omp for private(argc,b) firstprivate(argv,c) lastprivate(d,f) reduction(+: e) reduction(min: g) ordered nowait collapse(2) schedule(dynamic, 1)
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: for (int j = 0; j < 10; ++j)
// CHECK-NEXT: foo();
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for nowait, schedule(guided, 12)
// CHECK-NEXT: #pragma omp for nowait schedule(guided, 12)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for schedule(static, argc)
// CHECK: #pragma omp for schedule(static, .omp.schedule.var.)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for ordered, schedule(auto)
// CHECK: #pragma omp for ordered schedule(auto)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for schedule(runtime)
// CHECK-NEXT: #pragma omp for schedule(runtime)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
  return (0);
}

#endif
