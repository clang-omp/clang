// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

int main(int argc, char **argv) {
  #pragma omp target
  #pragma omp teams distribute dist_schedule // expected-error {{expected '(' after 'dist_schedule'}} expected-error {{expected 'static' in OpenMP clause 'dist_schedule'}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule ( // expected-error {{expected 'static' in OpenMP clause 'dist_schedule'}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule () // expected-error {{expected 'static' in OpenMP clause 'dist_schedule'}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static, // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (argc)) // expected-error {{expected 'static' in OpenMP clause 'dist_schedule'}} expected-warning {{extra tokens at the end of '#pragma omp teams distribute' are ignored}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static, argc > 0 ? argv[1] : argv[2]) // expected-error {{statement requires expression of integer type ('char *' invalid)}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static), dist_schedule (static, 1) // expected-error {{directive '#pragma omp teams distribute' cannot contain more than one 'dist_schedule' clause}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static, S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute dist_schedule (static, argv[1]=2) // expected-error {{statement requires expression of integer type ('char *' invalid)}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();

  return 0;
}
