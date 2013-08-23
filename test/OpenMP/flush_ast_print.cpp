// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// expected-no-diagnostics

int main (int argc, char **argv) {
// CHECK: int main(int argc, char **argv) {
#pragma omp flush(argc)
// CHECK-NEXT: #pragma omp flush(argc)
// CHECK-NEXT: return argc;
  return argc;
}
