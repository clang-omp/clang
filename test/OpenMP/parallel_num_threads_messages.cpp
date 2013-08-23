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
  #pragma omp parallel num_threads // expected-error {{expected '(' after 'num_threads'}} expected-error {{expected expression}}
  #pragma omp parallel num_threads ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp parallel num_threads () // expected-error {{expected expression}}
  #pragma omp parallel num_threads (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp parallel num_threads (argc > 0 ? argv[1] : argv[2]) // expected-error {{statement requires expression of integer type ('char *' invalid)}}
  #pragma omp parallel num_threads (foobool(argc)) num_threads(3) // expected-error {{directive '#pragma omp parallel' cannot contain more than one 'num_threads' clause}}
  #pragma omp parallel num_threads (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp parallel num_threads (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{statement requires expression of integer type ('char *' invalid)}}
  #pragma omp parallel num_threads (v1) // expected-error {{expression has incomplete type 'S1'}}
  #pragma omp parallel num_threads (v2) // expected-error {{multiple conversions from expression type 'struct S2' to an integral or enumeration type}}
  #pragma omp parallel num_threads (v3) // expected-error {{expression type 'struct S3' requires explicit conversion to 'int'}}
  #pragma omp parallel num_threads (0) // expected-error {{expression is not a positive integer value}}
  #pragma omp parallel num_threads (-1) // expected-error {{expression is not a positive integer value}}
  foo();

  return 0;
}
