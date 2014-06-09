// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}

int main(int argc, char **argv) {
  #pragma omp target teams if // expected-error {{expected '(' after 'if'}} expected-error {{expected expression}}
  foo();
  #pragma omp target teams if ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();
  #pragma omp target teams if () // expected-error {{expected expression}}
  foo();
  #pragma omp target teams if (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();
  #pragma omp target teams if (argc)) // expected-warning {{extra tokens at the end of '#pragma omp target teams' are ignored}}
  foo();
  #pragma omp target teams if (argc > 0 ? argv[1] : argv[2])
  foo();
  #pragma omp target teams if (foobool(argc)), if (true) // expected-error {{directive '#pragma omp target teams' cannot contain more than one 'if' clause}}
  foo();
  #pragma omp target teams if (S1) // expected-error {{'S1' does not refer to a value}}
  foo();
  #pragma omp target teams if (argv[1]=2) // expected-error {{expected ')'}} expected-note {{to match this '('}}
  foo();

  return 0;
}
