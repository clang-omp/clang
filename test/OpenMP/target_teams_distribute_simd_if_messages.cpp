// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

int main(int argc, char **argv) {
  #pragma omp target teams distribute simd if // expected-error {{expected '(' after 'if'}} expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if () // expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (argc)) // expected-warning {{extra tokens at the end of '#pragma omp target teams distribute simd' are ignored}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (argc > 0 ? argv[1] : argv[2])
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (foobool(argc)), if (true) // expected-error {{directive '#pragma omp target teams distribute simd' cannot contain more than one 'if' clause}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target teams distribute simd if (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();

  return 0;
}
