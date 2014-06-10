// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -std=c++11 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}} expected-note {{forward declaration of 'S1'}}

extern S1 v1;

struct S2{
  int f;
  operator int() { return f; } // expected-note {{conversion to integral type 'int'}}
  operator bool() { return f; } // expected-note {{conversion to integral type 'bool'}}
} v2;

struct S3 {
  int f;
  explicit operator int() { return f; } // expected-note {{conversion to integral type 'int'}}
} v3;

int main(int argc, char **argv) {
  #pragma omp target teams distribute simd thread_limit // expected-error {{expected '(' after 'thread_limit'}} expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit () // expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (argc > 0 ? argv[1] : argv[2]) // expected-error {{statement requires expression of integer type ('char *' invalid)}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (foobool(argc)) thread_limit(3) // expected-error {{directive '#pragma omp target teams distribute simd' cannot contain more than one 'thread_limit' clause}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{statement requires expression of integer type ('char *' invalid)}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (v1) // expected-error {{expression has incomplete type 'S1'}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (v2) // expected-error {{multiple conversions from expression type 'struct S2' to an integral or enumeration type}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (v3) // expected-error {{expression type 'struct S3' requires explicit conversion to 'int'}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (0) // expected-error {{expression is not a positive integer value}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd thread_limit (-1) // expected-error {{expression is not a positive integer value}}
  for (int i = 0; i < argc; ++i) foo();

  return 0;
}
