// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() { }

int main(int argc, char **argv) {
  L1:
    foo();
  #pragma omp target
  #pragma omp teams distribute
  for(int i = 0; i < argc; ++i)
  {
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp target
  #pragma omp teams distribute
  for(int i = 0; i < argc; ++i) {
    L2:
    foo();
  }

  return 0;
}
