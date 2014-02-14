// RUN: %clang_cc1 -verify -fopenmp -x c++ -std=c++11 -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

#pragma omp declare simd uniform(d)
void add_1(float *d);

// CHECK: #pragma omp declare simd uniform(d)
// CHECK-NEXT: void add_1(float *d);
//

#define MY_ALIGN 2*8

#pragma omp declare simd uniform(hp)
#pragma omp declare simd simdlen(8) linear(hq, lin: MY_ALIGN)
template <class C> void h(C *hp, C *hp2, C *hq, C *lin) {
}

// CHECK: #pragma omp declare simd uniform(hp)
// CHECK-NEXT: #pragma omp declare simd simdlen(8) linear(hq,lin: 16)
// CHECK: template <class C> void h(C *hp, C *hp2, C *hq, C *lin) {
// CHECK-NEXT: }
//


// Instatiate with <C=int> explicitly.
// Pragmas need to be same, otherwise standard says that's undefined behavior.
#pragma omp declare simd uniform(hp)
#pragma omp declare simd simdlen(8) linear(hq, lin: MY_ALIGN)
template <>
void h(int *hp, int *hp2, int *hq, int *lin)
{
  // Instatiate with <C=float> implicitly.
  // This is special case where the directive is stored by Sema and is
  // generated together with the (pending) function instatiation.
  h((float*) hp, (float*) hp2, (float*) hq, (float*) lin);
}

// CHECK: #pragma omp declare simd uniform(hp)
// CHECK-NEXT: #pragma omp declare simd simdlen(8) linear(hq,lin: 16)
// CHECK: void h(int *hp, int *hp2, int *hq, int *lin) {
// CHECK-NEXT: h((float *)hp, (float *)hp2, (float *)hq, (float *)lin);
// CHECK-NEXT: }
//

class VV {
  #pragma omp declare simd
  int add(int a, int b) { return a + b; }

  #pragma omp declare simd aligned(a,b : 32)
  float addpf(float *a, float *b) { return *a + *b; }
};

template <int X>
class TVV {
// CHECK: template <int X> class TVV {
  #pragma omp declare simd
  int tadd(int a, int b) { return a + b; }

// CHECK: #pragma omp declare simd
// CHECK-NEXT: int tadd(int a, int b) {
// CHECK-NEXT: return a + b;
// CHECK-NEXT: }


  #pragma omp declare simd aligned(a,b : X)
  float taddpf(float *a, float *b) { return *a + *b; }

// CHECK: #pragma omp declare simd aligned(a,b: X)
// CHECK-NEXT: float taddpf(float *a, float *b) {
// CHECK-NEXT: return *a + *b;
// CHECK-NEXT: }
};
// CHECK: };

// CHECK-NEXT: TVV<16> t16;
TVV<16> t16;

void f() {
  float a = 1.0f, b = 2.0f;
  float r = t16.taddpf(&a, &b);
}

#endif
