// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// expected-no-diagnostics

#pragma omp declare reduction (+ : int, char: omp_out *= omp_in)
// CHECK: #pragma omp declare reduction (+ : int : omp_out *= omp_in)
// CHECK-NEXT: #pragma omp declare reduction (+ : char : omp_out *= omp_in)

#pragma omp declare reduction (fun : float : omp_out += omp_in) initializer (omp_priv=omp_orig + 15)
// CHECK: #pragma omp declare reduction (fun : float : omp_out += omp_in) initializer(omp_priv = omp_orig + 15)

struct SSS {
  int field;
};

void init(struct SSS *priv, struct SSS orig);

#pragma omp declare reduction (fun : struct SSS : omp_out = omp_in) initializer (init(&omp_priv, omp_orig))
// CHECK: #pragma omp declare reduction (fun : struct SSS : omp_out = omp_in) initializer(init(&omp_priv, omp_orig))
