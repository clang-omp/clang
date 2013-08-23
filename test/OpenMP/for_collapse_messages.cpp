// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

extern S1 v1;

struct S2{
  int f;
  operator int() { return f; }
  operator bool() { return f; }
} v2;

struct S3 {
  int f;
  operator int() { return f; }
} v3;

int main(int argc, char **argv) { // expected-note {{declared here}}
  #pragma omp for collapse // expected-error {{expected '(' after 'collapse'}} expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse () // expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (argc // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{expression is not an integral constant expression}} expected-note {{read of non-const variable 'argc' is not allowed in a constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (argc > 0 ? argv[1] : argv[2]) // expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (foobool(argc)) collapse(1) // expected-error {{directive '#pragma omp for' cannot contain more than one 'collapse' clause}} expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (v1) // expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (v2) // expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (v3) // expected-error {{expression is not an integral constant expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (0) // expected-error {{expression is not a positive integer value}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for collapse (-1) // expected-error {{expression is not a positive integer value}}
  for (int i = 0; i < 10; ++i) foo();

  return 0;
}
