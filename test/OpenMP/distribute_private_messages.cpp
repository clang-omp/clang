// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}} expected-note{{forward declaration of 'S1'}}
extern S1 a;
class S2 {
  mutable int a;
public:
  S2():a(0) { }
  static float S2s; // expected-note {{predetermined as shared}}
};
const S2 b;
const S2 ba[5];
class S3 {
  int a;
public:
  S3():a(0) { }
};
const S3 c; // expected-note {{predetermined as shared}}
const S3 ca[5]; // expected-note {{predetermined as shared}}
extern const int f;  // expected-note {{predetermined as shared}}
class S4 { // expected-note {{'S4' declared here}}
  int a;
  S4();
public:
  S4(int v):a(v) { }
};
class S5 { // expected-note {{'S5' declared here}}
  int a;
  S5():a(0) {}
public:
  S5(int v):a(v) { }
};

S3 h;
#pragma omp threadprivate(h) // expected-note {{defined as threadprivate or thread local}}

int main(int argc, char **argv) {
  const int d = 5;  // expected-note {{predetermined as shared}}
  const int da[5] = { 0 }; // expected-note {{predetermined as shared}}
  S4 e(4); // expected-note {{'e' defined here}}
  S5 g(5); // expected-note {{'g' defined here}}
  int i;
  int &j = i; // expected-note {{'j' defined here}}
  #pragma omp distribute private // expected-error {{expected '(' after 'private'}} expected-error {{expected expression}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private () // expected-error {{expected expression}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (argc)
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (S1) // expected-error {{'S1' does not refer to a value}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (a, b, c, d, f) // expected-error {{private variable with incomplete type 'S1'}} expected-error 3 {{shared variable cannot be private}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private (argv[1]) // expected-error {{expected variable name}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(ba)
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(ca) // expected-error {{shared variable cannot be private}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(da) // expected-error {{shared variable cannot be private}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(S2::S2s) // expected-error {{shared variable cannot be private}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(e, g) // expected-error 2 {{private variable must have an accessible, unambiguous default constructor}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(h) // expected-error {{threadprivate or thread local variable cannot be private}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute shared(i) // expected-error {{unexpected OpenMP clause 'shared' in directive '#pragma omp distribute'}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp target
  #pragma omp teams
  {
    int i; // expected-note {{predetermined as private}}
    #pragma omp distribute firstprivate(i), private(i) // expected-error {{private variable cannot be firstprivate}}
    for (int k = 0; k < argc; ++k) ++k;
  }
  #pragma omp target
  #pragma omp teams private(i)
  #pragma omp distribute private(j) // expected-error {{arguments of OpenMP clause 'private' cannot be of reference type}}
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp parallel private(i)
  #pragma omp target
  #pragma omp teams firstprivate(i)
  #pragma omp parallel private(i)
  #pragma omp target
  #pragma omp teams reduction(+:i)
  #pragma omp distribute private(i)
  for (int k = 0; k < argc; ++k) ++k;
  #pragma omp distribute private(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target
    #pragma omp teams private(i)
    #pragma omp distribute private(i)
    for (int x = 0; x < 10; ++x) foo();
  }
  #pragma omp target
  #pragma omp teams
  #pragma omp distribute firstprivate(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target
    #pragma omp teams firstprivate(i)
    #pragma omp distribute private(i)
    for (int x = 0; x < 10; ++x) foo();
  }
  #pragma omp target
  #pragma omp teams reduction(+:i)
  #pragma omp distribute
  for (int k = 0; k < 10; ++k) {
    #pragma omp target
    #pragma omp teams reduction(+:i)
    #pragma omp distribute private(i)
    for (int x = 0; x < 10; ++x) foo();
  }

  return 0;
}
