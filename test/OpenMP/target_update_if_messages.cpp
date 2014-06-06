// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

int main(int argc, char **argv) {
  #pragma omp target update if // expected-error {{expected '(' after 'if'}} expected-error {{expected expression}}
  #pragma omp target update if ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update if () // expected-error {{expected expression}}
  #pragma omp target update if (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update if (argc)) // expected-warning {{extra tokens at the end of '#pragma omp target update' are ignored}}
  #pragma omp target update if (argc > 0 ? argv[1] : argv[2])
  #pragma omp target update if (foobool(argc)), if (true) // expected-error {{directive '#pragma omp target update' cannot contain more than one 'if' clause}}
  #pragma omp target update if (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp target update if (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();

  return 0;
}
