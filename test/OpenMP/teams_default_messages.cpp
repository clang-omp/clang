// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo();

int main(int argc, char **argv) {
  #pragma omp target
  #pragma omp teams default // expected-error {{expected '(' after 'default'}} expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  foo();
  #pragma omp target
  #pragma omp teams default ( // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();
  #pragma omp target
  #pragma omp teams default () // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  foo();
  #pragma omp target
  #pragma omp teams default (none // expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();
  #pragma omp target
  #pragma omp teams default (shared), default(shared) // expected-error {{directive '#pragma omp teams' cannot contain more than one 'default' clause}}
  foo();
  #pragma omp target
  #pragma omp teams default (x) // expected-error {{expected 'none' or 'shared' in OpenMP clause 'default'}}
  foo();

  return 0;
}
