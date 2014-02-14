// RUN: %clang_cc1 -fopenmp -emit-llvm %s -o - | FileCheck %s

#ifdef NOOMP
__attribute__((vector(uniform(x), linear(y:4), linear(z:1))))
#else
#pragma omp declare simd uniform(x) linear(y:4) linear(z)
#endif
void g(int x, int y, int z)
{
}

// Metadata for g:
// CHECK: [[GN:![0-9]+]] = metadata !{metadata !"arg_name", metadata !"x", metadata !"y", metadata !"z"}
// CHECK: [[GS:![0-9]+]] = metadata !{metadata !"arg_step", i32 0, i32 4, i32 1}
// CHECK: [[GT:![0-9]+]] = metadata !{metadata !"vec_length", i32 undef, i32 {{[0-9]+}}}

#define MY_ALIGN 16

#ifdef NOOMP
__attribute__((vector(uniform(hp))))
#else
#pragma omp declare simd uniform(hp) aligned(hp, hq:MY_ALIGN)
#endif
void h(char *hp, char *hp2, char *hq)
{
}

// Metadata for h:
// CHECK: metadata !{metadata !"arg_name", metadata !"hp", metadata !"hp2", metadata !"hq"}
// CHECK: [[HS:![0-9]+]] = metadata !{metadata !"arg_step", i32 0, i32 undef, i32 undef}
// CHECK: [[HA:![0-9]+]] = metadata !{metadata !"arg_alig", i32 16, i32 undef, i32 16}
// CHECK: metadata !{metadata !"vec_length", i8* undef, i32 {{[0-9]+}}}


#ifdef NOOMP
__attribute__((vector(mask, vectorlength(64))))
#else
#pragma omp declare simd inbranch simdlen(64)
#endif
void k(char (*kf)(int), int a)
{
  kf(a);
}

// Metadata for k:
// CHECK: metadata !{metadata !"arg_name", metadata !"kf", metadata !"a"}
// CHECK: metadata !{metadata !"arg_step", i32 undef, i32 undef}
// CHECK: metadata !{metadata !"vec_length", i8 (i32)* undef, i32 64}

