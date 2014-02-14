// RUN: %clang_cc1 -fopenmp -x c++ -std=c++11 -emit-llvm %s -o - | FileCheck %s

#pragma omp declare simd uniform(x) linear(y:4)
void g(int x, int y)
{
}

// Tests for global-function-template with 'omp declare simd'

// Metadata for g:
// CHECK: [[G1:![0-9]+]] = metadata !{void (i32, i32)* @_Z1gii[[GMETA:(, metadata ![0-9]+)+]]}
// CHECK: [[GN:![0-9]+]] = metadata !{metadata !"arg_name", metadata !"x", metadata !"y"}
// CHECK: [[GS:![0-9]+]] = metadata !{metadata !"arg_step", i32 0, i32 4}
// CHECK: [[GT:![0-9]+]] = metadata !{metadata !"vec_length", i32 undef, i32 {{[0-9]+}}}

#define MY_ALIGN 2*9

#pragma omp declare simd uniform(hp)
#pragma omp declare simd simdlen(8) linear(hq, lin: MY_ALIGN)
template <class C>
void h(C *hp, C *hp2, C *hq, C *lin)
{
}

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

// For the function template h we have 2 template instantiations
// (one implicit with <C=float> and one explicit with <C=int>) and
// each of them should have 2 simd-variants, as specified by the pragma.
// CHECK: [[HI1:![0-9]+]] = metadata !{void (i32*, i32*, i32*, i32*)* @_Z1hIiEvPT_S1_S1_S1_[[HI1META:(, metadata ![0-9]+)+]]}
// CHECK: [[HI2:![0-9]+]] = metadata !{void (i32*, i32*, i32*, i32*)* @_Z1hIiEvPT_S1_S1_S1_[[HI2META:(, metadata ![0-9]+)+]]}
// CHECK: [[HF1:![0-9]+]] = metadata !{void (float*, float*, float*, float*)* @_Z1hIfEvPT_S1_S1_S1_[[HF1META:(, metadata ![0-9]+)+]]}
// CHECK: [[HF2:![0-9]+]] = metadata !{void (float*, float*, float*, float*)* @_Z1hIfEvPT_S1_S1_S1_[[HF2META:(, metadata ![0-9]+)+]]}


// Tests for special cases for class method and for class-template method

class VV {
  #pragma omp declare simd
  int add(int a, int b) { return a + b; }

  #pragma omp declare simd aligned(a,b : 32)
  float addpf(float *a, float *b) { return *a + *b; }
};


template <int X>
class TVV {
  #pragma omp declare simd
  int tadd(int a, int b) { return a + b; }

  #pragma omp declare simd aligned(a,b : X)
  float taddpf(float *a, float *b) { return *a + *b; }
};

TVV<16> t16;
VV vv;

// The following function instantiates the class and template
void f() {
  float a = 1.0f, b = 2.0f;
  float r = t16.taddpf(&a, &b);
  float q = vv.addpf(&a, &b);
}

// Metadata for the class
// CHECK: [[C01:![0-9]+]] = metadata !{float (%class.VV*, float*, float*)* @_ZN2VV5addpfEPfS0_[[C01META:(, metadata ![0-9]+)+]]}
//
// Metadata for the class-template
// CHECK: [[CT1:![0-9]+]] = metadata !{float (%class.TVV*, float*, float*)* @_ZN3TVVILi16EE6taddpfEPfS1_[[CT1META:(, metadata ![0-9]+)+]]}


