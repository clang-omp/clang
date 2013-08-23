// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main() {
  #pragma omp taskgroup
  ;
  #pragma omp taskgroup nowait // expected-error {{unexpected OpenMP clause 'nowait' in directive '#pragma omp taskgroup'}}
  #pragma omp taskgroup unknown // expected-warning {{extra tokens at the end of '#pragma omp taskgroup' are ignored}}
  foo();
  {
    #pragma omp taskgroup
  } // expected-error {{expected statement}}
  #pragma omp for
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp taskgroup
    foo();
  }
  #pragma omp sections
  {
    #pragma omp taskgroup
    foo();
  }
  #pragma omp single
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp taskgroup
    foo();
  }
  #pragma omp task
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp taskgroup
    foo();
  }

  return 0;
}

int foo() {
  L1:
    foo();
  #pragma omp taskgroup
  {
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp taskgroup
  {
    L2:
    foo();
  }

  return 0;
}
