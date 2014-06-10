// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo();

int main(int argc, char **argv) {
  #pragma omp target
  #pragma omp teams distribute default // expected-error {{expected '(' after 'default'}} expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute default ( // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute default () // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute default (none // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < 10; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute default (shared), default(shared) // expected-error {{directive '#pragma omp teams distribute' cannot contain more than one 'default' clause}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute default (x) // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  for (int i = 0; i < argc; ++i) foo();

  return 0;
}
