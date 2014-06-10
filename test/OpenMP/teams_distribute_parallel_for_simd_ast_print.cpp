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
#pragma omp target
#pragma omp teams distribute parallel for simd reduction(+:sum) proc_bind(master) dist_schedule(static) default(shared)
// CHECK-NEXT: #pragma omp target
// CHECK-NEXT: #pragma omp teams distribute parallel for simd reduction(+: sum) proc_bind(master) dist_schedule(static)
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
#pragma omp target
#pragma omp teams distribute parallel for simd lastprivate(res) if(m_a)
// CHECK-NEXT: #pragma omp target
// CHECK-NEXT: #pragma omp teams distribute parallel for simd lastprivate(res) if(this->m_a)
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
#pragma omp target
#pragma omp teams distribute parallel for simd
// CHECK-NEXT: #pragma omp target
// CHECK-NEXT: #pragma omp teams distribute parallel for simd
  for (int i=0; i < 2; ++i)*a=2;
// CHECK-NEXT: for (int i = 0; i < 2; ++i)
// CHECK-NEXT: *a = 2;
#pragma omp target
#pragma omp teams distribute parallel for simd private(argc,b),lastprivate(d,f),reduction(+:e) reduction(min : g),  collapse(2) dist_schedule(static, 3) default(none) firstprivate(f) num_teams(b) num_threads(argc) shared(k1, k2)
  for (int i = 0; i < 10; ++i)
  for (int j = 0; j < 10; ++j) {foo(); k1 += 8; k2 += 8;}
// CHECK-NEXT: #pragma omp target
// CHECK-NEXT: #pragma omp teams distribute parallel for simd private(argc,b) lastprivate(d,f) reduction(+: e) reduction(min: g) collapse(2) dist_schedule(static, 3) default(none) firstprivate(f) num_teams(b) num_threads(argc) shared(k1,k2)
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: for (int j = 0; j < 10; ++j) {
// CHECK-NEXT: foo();
// CHECK-NEXT: k1 += 8;
// CHECK-NEXT: k2 += 8;
// CHECK-NEXT: }
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
#pragma omp target
#pragma omp teams distribute parallel for simd collapse(1)
// CHECK:      #pragma omp target
// CHECK-NEXT: #pragma omp teams distribute parallel for simd collapse(1)
  for (int i = 0; i < 10; ++i)foo();
// CHECK-NEXT: for (int i = 0; i < 10; ++i)
// CHECK-NEXT: foo();
  return (0);
}

#endif
