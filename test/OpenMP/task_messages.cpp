// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main() {
  #pragma omp task
  ;
  #pragma omp task untied untied // expected-error {{directive '#pragma omp task' cannot contain more than one 'untied' clause}}
  foo();
  #pragma omp task mergeable mergeable // expected-error {{directive '#pragma omp task' cannot contain more than one 'mergeable' clause}}
  foo();
  {
    #pragma omp task
  } // expected-error {{expected statement}}

  return 0;
}

int foo() {
  L1:
    foo();
  #pragma omp task
  {
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp task
  {
    L2:
    foo();
  }

  return 0;
}
