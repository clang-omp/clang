// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

#pragma omp declare reduction (+ : int, char: omp_out *= omp_in)
// CHECK: #pragma omp declare reduction (+ : int : omp_out *= omp_in)
// CHECK-NEXT: #pragma omp declare reduction (+ : char : omp_out *= omp_in)

// CHECK: #pragma omp declare reduction (fun : int : omp_out += omp_in) initializer(omp_priv omp_orig + 15)

template <class T>
class SSS {
public:
#pragma omp declare reduction (fun : T : omp_out += omp_in) initializer (omp_priv omp_orig + 15)
// CHECK: #pragma omp declare reduction (fun : T : omp_out += omp_in) initializer(omp_priv omp_orig + 15)
};

SSS<int> d;

void init(SSS<int> &lhs, SSS<int> rhs);

#pragma omp declare reduction (fun : SSS<int> : omp_out = omp_in) initializer (init(omp_priv, omp_orig))
// CHECK: #pragma omp declare reduction (fun : SSS<int> : omp_out = omp_in) initializer(init(omp_priv, omp_orig))

int main() {
  int i = 0;
  SSS<int> sss;
  #pragma omp parallel reduction(SSS<int>::fun : i)
// CHECK: #pragma omp parallel reduction(SSS<int>::fun: i)
  {
    i += 1;
  }
  #pragma omp parallel reduction(::fun:sss)
// CHECK: #pragma omp parallel reduction(::fun: sss)
  {
  }
}

#endif
