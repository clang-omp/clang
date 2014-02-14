// RUN: %clang_cc1 -verify -fopenmp -ast-print %s | FileCheck %s
// RUN: %clang_cc1 -fopenmp -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -include-pch %t -fsyntax-only -verify %s -ast-print | FileCheck %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

#pragma omp declare simd
void add_1(float *d, float *s1, float *s2);

// CHECK: #pragma omp declare simd
// CHECK-NEXT: void add_1(float *d, float *s1, float *s2)

#define MY_ALIGN 2*8

#pragma omp declare simd inbranch uniform(hp) aligned(hp, hq:MY_ALIGN) linear(hq, lin:8)
#pragma omp declare simd notinbranch linear(hp, hq:MY_ALIGN) aligned(hq, lin:8) simdlen(4)
void h(char *hp, char *hp2, char *hq, char *lin)
{
}

// CHECK: #pragma omp declare simd inbranch uniform(hp) aligned(hp,hq: 16) linear(hq,lin: 8)
// CHECK-NEXT: #pragma omp declare simd notinbranch linear(hp,hq: 16) aligned(hq,lin: 8) simdlen(4)
// CHECK-NEXT: void h(char *hp, char *hp2, char *hq, char *lin) {
// CHECK-NEXT: }


#endif
