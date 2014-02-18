// RUN: %clang_cc1 -fopenmp -emit-llvm %s -o - | FileCheck %s

void h(float *c, float *a, float *b, int size)
{
  int t = 0;
#pragma omp simd safelen(16) aligned(a, b, c) linear(t)
  for (int i = 0; i < size; ++i) {
    c[i] = a[i] * a[i] + b[i] * b[t];
    ++t;
  }
}

// Metadata for h:
// CHECK: [[LOOP_VEC_1:![0-9]+]] = metadata !{metadata [[LOOP_VEC_1]], metadata [[LOOP_VEC_2:![0-9]+]], metadata [[LOOP_VEC_3:![0-9]+]]}
// CHECK: [[LOOP_VEC_2]] = metadata !{metadata !"llvm.vectorizer.width", i32 16}
// CHECK: [[LOOP_VEC_3]] = metadata !{metadata !"llvm.vectorizer.enable", i32 1}

