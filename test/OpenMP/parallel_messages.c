// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() { }

int main(int argc, char **argv) {
  L1:
    foo();
  #pragma omp parallel
  {
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp parallel
  L2:
  foo();

  return 0;
}
