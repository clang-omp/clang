// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main() {
  #pragma omp single
  ;
  #pragma omp single nowait nowait // expected-error {{directive '#pragma omp single' cannot contain more than one 'nowait' clause}}
  foo();
  {
    #pragma omp single
  } // expected-error {{expected statement}}
  #pragma omp for
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside a worksharing region}}
    foo();
  }
  #pragma omp sections
  {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside a worksharing region}}
    foo();
  }
  #pragma omp single
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside a worksharing region}}
    foo();
  }
  #pragma omp master
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside a master region}}
    foo();
  }
  #pragma omp critical
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside a critical region}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i)
  #pragma omp ordered
  {
    foo();
    #pragma omp single // expected-error {{region cannot be closely nested inside an ordered region}}
    foo();
  }
  return 0;
}

int foo() {
  L1:
    foo();
  #pragma omp single
  {
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp single
  {
    L2:
    foo();
  }

  return 0;
}
