// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// expected-no-diagnostics

void foo() {}

int main (int argc, char **argv) {
  int a;
// CHECK: int a;
#pragma omp atomic
a++;
// CHECK-NEXT: #pragma omp atomic
// CHECK-NEXT: a++;
#pragma omp atomic update
a--;
// CHECK-NEXT: #pragma omp atomic update
// CHECK-NEXT: a--;
#pragma omp atomic seq_cst
++a;
// CHECK-NEXT: #pragma omp atomic seq_cst
// CHECK-NEXT: ++a;
#pragma omp atomic update seq_cst
--a;
// CHECK-NEXT: #pragma omp atomic update seq_cst
// CHECK-NEXT: --a;
#pragma omp atomic read
a = argc;
// CHECK-NEXT: #pragma omp atomic read
// CHECK-NEXT: a = argc;
#pragma omp atomic read seq_cst
argc = a;
// CHECK-NEXT: #pragma omp atomic read seq_cst
// CHECK-NEXT: argc = a;
#pragma omp atomic write
a = argc * 2;
// CHECK-NEXT: #pragma omp atomic write
// CHECK-NEXT: a = argc * 2;
#pragma omp atomic write seq_cst
argc = a - 3;
// CHECK-NEXT: #pragma omp atomic write seq_cst
// CHECK-NEXT: argc = a - 3;
#pragma omp atomic capture
a = argc++;
// CHECK-NEXT: #pragma omp atomic capture
// CHECK-NEXT: a = argc++;
#pragma omp atomic capture seq_cst
{argc = a; ++a;}
// CHECK-NEXT: #pragma omp atomic capture seq_cst
// CHECK-NEXT: {
// CHECK-NEXT: argc = a;
// CHECK-NEXT: ++a;
// CHECK-NEXT: }
  return (0);
}
