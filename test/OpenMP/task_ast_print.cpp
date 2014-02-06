// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

void foo() {}

int main (int argc, char **argv) {
  int b = argc, c, d, e, f, g;
  int arr[25][123][2234];
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

#endif
