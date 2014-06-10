// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

#pragma omp target teams distribute // expected-error {{unexpected OpenMP directive '#pragma omp target teams distribute'}}

int main(int argc, char **argv) {
#pragma omp target teams distribute
  for (int i = 0; i < argc; ++i)
    foo();
#pragma omp target
  {
#pragma omp target teams distribute
    for (int i = 0; i < argc; ++i)
      foo();
  }
#pragma omp target teams distribute unknown() // expected-warning {{extra tokens at the end of '#pragma omp target teams distribute' are ignored}}
  for (int i = 0; i < argc; ++i)
    foo();
L1:
  for (int i = 0; i < argc; ++i)
    foo();
#pragma omp target teams distribute
  for (int i = 0; i < argc; ++i)
    foo();
#pragma omp target teams distribute
  for (int i = 0; i < argc; ++i) {
    goto L1; // expected-error {{use of undeclared label 'L1'}}
    argc++;
  }

#pragma omp target teams distribute default(none)
  for (int i = 0; i < 10; ++i)
    ++argc; // expected-error {{variable 'argc' must have explicitly specified data sharing attributes}}

  return 0;
}
