// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

int main(int argc, char **argv) {
  #pragma omp for schedule // expected-error {{expected '(' after 'schedule'}} expected-error {{expected 'static', 'dynamic', 'guided', 'auto' or 'runtime' in OpenMP clause 'schedule'}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule ( // expected-error {{expected 'static', 'dynamic', 'guided', 'auto' or 'runtime' in OpenMP clause 'schedule'}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule () // expected-error {{expected 'static', 'dynamic', 'guided', 'auto' or 'runtime' in OpenMP clause 'schedule'}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (static // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (guided, // expected-error {{expected ')'}} expected-note {{to match this '('}} expected-error {{expected expression}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (argc)) // expected-error {{expected 'static', 'dynamic', 'guided', 'auto' or 'runtime' in OpenMP clause 'schedule'}} expected-warning {{extra tokens at the end of '#pragma omp for' are ignored}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (dynamic, argc > 0 ? argv[1] : argv[2]) // expected-error {{statement requires expression of integer type ('char *' invalid)}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (auto), schedule (runtime) // expected-error {{directive '#pragma omp for' cannot contain more than one 'schedule' clause}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (guided, S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp for schedule (dynamic, argv[1]=2) // expected-error {{statement requires expression of integer type ('char *' invalid)}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();

  return 0;
}
