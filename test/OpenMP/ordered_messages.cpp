// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main() {
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp ordered
    ;
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp ordered nowait // expected-error {{unexpected OpenMP clause 'nowait' in directive '#pragma omp ordered'}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
  #pragma omp ordered unknown // expected-warning {{extra tokens at the end of '#pragma omp ordered' are ignored}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp ordered
  } // expected-error {{expected statement}}
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp parallel
    #pragma omp ordered // expected-error {{region must be closely nested inside loop or parallel loop region with 'ordered' clause}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp critical
    #pragma omp ordered // expected-error {{region cannot be closely nested inside a critical region}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp atomic
    #pragma omp ordered // expected-error {{region cannot be closely nested inside an atomic region}}
    foo();
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp task
    #pragma omp ordered // expected-error {{region cannot be closely nested inside explicit task region}}
    foo();
  }

  return 0;
}

int foo() {
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    L1:
      foo();
    #pragma omp ordered
    {
      foo();
      goto L1; // expected-error {{use of undeclared label 'L1'}}
    }
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i) {
    foo();
    goto L2; // expected-error {{use of undeclared label 'L2'}}
    #pragma omp ordered
    {
      L2:
      foo();
    }
  }

  return 0;
}
