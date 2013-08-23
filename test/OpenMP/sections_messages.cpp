// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main() {
  #pragma omp section // expected-error {{orphaned '#pragma omp section' is prohibited}}
  ;
  #pragma omp section // expected-error {{orphaned '#pragma omp section' is prohibited}}
  foo();
  #pragma omp sections
  {
    foo();
    #pragma omp parallel
    #pragma omp section // expected-error {{orphaned '#pragma omp section' is prohibited}}
    foo();
  }
  #pragma omp sections
  {
    foo();
    foo(); // expected-error {{the statement for '#pragma omp sections' must be '#pragma omp section'}}
  }
  #pragma omp sections
  {
    #pragma omp section
    foo();
    foo(); // expected-error {{the statement for '#pragma omp sections' must be '#pragma omp section'}}
  }
  #pragma omp sections
  foo(); // expected-error {{the statement for '#pragma omp sections' must be compound statement}}
  #pragma omp sections nowait nowait // expected-error {{directive '#pragma omp sections' cannot contain more than one 'nowait' clause}}
  {
    foo();
  }
  #pragma omp sections
  {
    foo();
    #pragma omp section
    {
      foo();
      foo();
    }
    #pragma omp section
  } // expected-error {{expected statement}}
  #pragma omp sections
  {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside a worksharing region}}
    {
      foo();
    }
  }
  #pragma omp for
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside a worksharing region}}
    {
      foo();
    }
  }
  #pragma omp single
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside a worksharing region}}
    {
      foo();
    }
  }
  #pragma omp master
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside a master region}}
    {
      foo();
    }
  }
  #pragma omp critical
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside a critical region}}
    {
      foo();
    }
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i)
  #pragma omp ordered
  {
    foo();
    #pragma omp sections // expected-error {{region cannot be closely nested inside an ordered region}}
    {
      foo();
    }
  }

  return 0;
}

int foo() {
  L1:
    foo();
  #pragma omp sections
  {
    LS:
    #pragma omp section
    {
      foo();
      goto L1; // expected-error {{use of undeclared label 'L1'}}
    }
    #pragma omp section
    {
      foo();
      goto LS; // expected-error {{use of undeclared label 'LS'}}
    }
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp sections
  {
    L2:
    foo();
  }

  return 0;
}
