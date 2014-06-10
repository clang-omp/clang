// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

#pragma omp target teams distribute parallel for // expected-error {{unexpected OpenMP directive '#pragma omp target teams distribute parallel for'}}

int main(int argc, char **argv) {
  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; ++i) foo();
  {
  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; ++i) foo();
  }
  {
    for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; ++i) foo();
  }
  #pragma omp target teams distribute parallel for unknown() // expected-warning {{extra tokens at the end of '#pragma omp target teams distribute parallel for' are ignored}}
  for (int i = 0; i < 10; ++i) {
  L1:
    for (int i = 0; i < 10; ++i) foo();
  }
  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute parallel for
  for (int i = 0; i < 10; ++i) foo();
  {
    goto L1; // expected-error {{use of undeclared label 'L1'}}
    argc++;
  }

  for (int i = 0; i < 10; ++i) {
    switch(argc) {
     case (0):
      #pragma omp target teams distribute parallel for
      for (int i = 0; i < 10; ++i)
      {
        foo();
        break; // expected-error {{cannot break from a '#pragma omp target teams distribute parallel for' loop}}
        continue;
      }
      default:
       break;
    }
  }
  #pragma omp target teams distribute parallel for default(none)
  for (int i = 0; i < 10; ++i)
  ++argc; // expected-error {{variable 'argc' must have explicitly specified data sharing attributes}}

  return 0;
}
