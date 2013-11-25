// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

int main (int argc, char **argv) {
// CHECK: int main(int argc, char **argv) {
#pragma omp flush(argc)
// CHECK-NEXT: #pragma omp flush(argc)
// CHECK-NEXT: return argc;
  return argc;
}

#endif
