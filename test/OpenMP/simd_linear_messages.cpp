// RUN: %clang_cc1 -fsyntax-only -ferror-limit 0 -fopenmp -verify %s

namespace X {
  int x;
};

struct A {
  int a;
  void bar() {
    #pragma omp simd linear(X::x)
    for (int i = 0; i < 10; i++) ;
    int j;
    #pragma omp simd linear(j)
    for (int i = 0; i < 10; i++) ;

    // TODO: Should be error here or no error in next directive?
    #pragma omp simd linear(j:this->a)
    for (int i = 0; i < 10; i++) ;
    // expected-error@+1 {{expected variable name}}
    #pragma omp simd linear(a)
    for (int i = 0; i < 10; i++) ;
  }
};


int z;
const int C1 = 1;
const int C2 = 2;
void test_linear()
{
  int y;
  char c;
  A a;
  a.a = 2;

  #pragma omp simd linear(X::x)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(y:z)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(X::x : ::z)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(y,::z, X::x)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(::z)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(y:C1+C2)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(c:y)
  for (int i = 0; i < 10; ++i) ;
  #pragma omp simd linear(y:c)
  for (int i = 0; i < 10; ++i) ;
}

template<class T, class N> T test_template(T* arr, N num) {
  N i;
  float ind;
  T sum = (T)0;
  // expected-error@+1 {{argument of a linear clause should be of integral or pointer type}}
#pragma omp simd linear(ind), reduction(+:sum)
  for (i = 0; i < num; ++i) {
    T cur = arr[ind];
    ++ind;
    sum += cur;
  }
}


