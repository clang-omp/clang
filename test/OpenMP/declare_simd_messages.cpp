// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -x c++ -std=c++11 %s

// expected-error@+3 {{expected '(' after 'uniform'}}
// expected-error@+2 {{expected unqualified-id}}
// expected-error@+1 {{expected identifier}}
#pragma omp declare simd uniform 

// expected-error@+1 {{expected '(' after 'uniform'}}
#pragma omp declare simd uniform d

// expected-error@+3 {{expected '(' after 'uniform'}}
// expected-error@+2 {{expected unqualified-id}}
// expected-error@+1 {{expected identifier}}
#pragma omp declare simd uniform{d}

// expected-error@+1 {{expression is not a positive integer value}}
#pragma omp declare simd simdlen(-32)

void add_1(float *d);
// expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}}
#pragma omp declare simd aligned(a : 32)
#pragma omp declare simd aligned(b : 32) // ok: b is array
#pragma omp declare simd aligned(c : 32) // ok: c is pointer
// expected-error@+1 {{argument of an aligned clause should be array, pointer, reference to array or reference to pointer}}
#pragma omp declare simd aligned(d : 32)
#pragma omp declare simd aligned(e : 32) // ok: reference to pointer
// expected-error@+1 {{expression is not a positive integer value}}
#pragma omp declare simd aligned(f : 0)
#pragma omp declare simd linear(f : 4) // ok
#pragma omp declare simd linear(f : 8) // ok : different step in other simd variant
// expected-error@+2 {{each argument may be in at most one uniform or linear clause}}
// expected-note@+1 {{previously referenced here}}
#pragma omp declare simd linear(f : 2) linear(f : 2)
// expected-error@+2 {{each argument may be in at most one aligned clause}}
// expected-note@+1 {{previously referenced here}}
#pragma omp declare simd aligned(f : 2) aligned(f : 2)
#pragma omp declare simd inbranch inbranch inbranch inbranch // ok
#pragma omp declare simd notinbranch notinbranch notinbranch notinbranch // ok
// expected-error@+2 {{declare simd variant cannot be inbranch and notinbranch at the same time}}
// expected-note@+1 {{previously specified here}}
#pragma omp declare simd inbranch notinbranch
// expected-error@+2 {{declare simd variant cannot be inbranch and notinbranch at the same time}}
// expected-note@+1 {{previously specified here}}
#pragma omp declare simd notinbranch inbranch
// expected-error@+2 2 {{declare simd variant cannot be inbranch and notinbranch at the same time}}
// expected-note@+1 2 {{previously specified here}}
#pragma omp declare simd inbranch notinbranch notinbranch
float add_2(float a, float b[], float *c, float &d, float *&e, float **f);


#define MY_ALIGN -2*8

#pragma omp declare simd uniform(hp)
// expected-error@+1 {{expression is not a positive integer value}}
#pragma omp declare simd simdlen(8) linear(hq, lin: MY_ALIGN)
template <class C>
void h(C *hp, C *hp2, C *hq, C *lin)
{
}

#pragma omp declare simd uniform(hp)
// expected-error@+1 {{cannot find the function argument with the name requested in the openmp clause}}
#pragma omp declare simd simdlen(8) linear(hq, lin, lin2: -MY_ALIGN)
template <>
void h(int *hp, int *hp2, int *hq, int *lin)
{
  h((float*) hp, (float*) hp2, (float*) hq, (float*) lin);
}


