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
#pragma omp sections
{
  a=2;
}
// CHECK-NEXT: #pragma omp sections
// CHECK-NEXT: {
// CHECK-NEXT: a = 2;
// CHECK-NEXT: }
#pragma omp parallel
#pragma omp sections private(argc,b),firstprivate(argv, c),lastprivate(d,f),reduction(+:e) reduction(min : g), nowait
{
  foo();
  #pragma omp section
  #pragma omp sections
  {
    #pragma omp section
    foo();
  }
}
// CHECK-NEXT: #pragma omp parallel
// CHECK-NEXT: #pragma omp sections private(argc,b) firstprivate(argv,c) lastprivate(d,f) reduction(+: e) reduction(min: g) nowait
// CHECK-NEXT: {
// CHECK-NEXT: foo();
// CHECK-NEXT: #pragma omp section
// CHECK-NEXT: #pragma omp sections
// CHECK-NEXT: {
// CHECK-NEXT: #pragma omp section
// CHECK-NEXT: foo();
// CHECK-NEXT: }
// CHECK-NEXT: }
  return (0);
}

#endif
