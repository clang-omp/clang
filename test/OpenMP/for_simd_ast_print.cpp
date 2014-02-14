// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

void foo() {}

template<class T, class N> T reduct(T* arr, N num) {
  N i;
  N ind;
  T sum = (T)0;
// CHECK: T sum = (T)0;
#pragma omp for simd  linear(ind), reduction(+:sum)
// CHECK-NEXT: #pragma omp for simd linear(ind) reduction(+: sum)
  for (i = 0; i < num; ++i) {
    T cur = arr[ind];
    ++ind;
    sum += cur;
  }
}

template<class T> struct S {
  S(const T &a)
    :m_a(a)
  {}
  T result(T *v) const {
    T res;
// CHECK: T res;
#pragma omp for simd lastprivate(res) safelen(7)
// CHECK-NEXT: #pragma omp for simd lastprivate(res) safelen(7)
    for (T i = 7; i < m_a; ++i) {
      res = v[i-7] + m_a;
    }
    return res;
  }
  ~S()
  {}
  T m_a;
};

int main (int argc, char **argv) {
  int b = argc, c, d, e, f, g;
  int k1=0,k2=0;
  static int *a;
// CHECK: static int *a;
#pragma omp for simd
// CHECK-NEXT: #pragma omp for simd
  for (int i=0; i < 2; ++i)*a=2;
// CHECK-NEXT: for (int i = 0; i < 2; ++i)
// CHECK-NEXT: *a = 2;
#pragma omp parallel
#pragma omp for simd private(argc,b),lastprivate(d,f),reduction(+:e) reduction(min : g),  collapse(2) safelen(2) aligned(a:4), linear(k1,k2:8)
  for (int i = 0; i < 10; ++i)
  for (int j = 0; j < 10; ++j) {foo(); k1 += 8; k2 += 8;}
// CHECK-NEXT: #pragma omp parallel
// CHECK-NEXT: #pragma omp for simd private(argc,b) lastprivate(d,f) reduction(+: e) reduction(min: g) collapse(2) safelen(2) aligned(a: 4) linear(k1,k2: 8)
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: for (int j = 0; j < 10; ++j) {
// CHECK-NEXT: foo();
// CHECK-NEXT: k1 += 8;
// CHECK-NEXT: k2 += 8;
// CHECK-NEXT: }
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for simd aligned(a) linear(a)
// CHECK-NEXT: #pragma omp for simd aligned(a) linear(a)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for simd collapse(1)
// CHECK: #pragma omp for simd collapse(1)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
  const int CLEN=4;
#pragma omp for simd safelen(CLEN)
// CHECK: #pragma omp for simd safelen(4)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp for simd aligned(a:CLEN) linear(a:CLEN) safelen(CLEN)
// CHECK-NEXT: #pragma omp for simd aligned(a: 4) linear(a: 4) safelen(4)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
  return (0);
}

#endif
