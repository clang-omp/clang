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
  #pragma omp target teams distribute simd private // expected-error {{expected '(' after 'private'}} expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private () // expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (argc)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (a, b, c, d, f) // expected-error {{private variable with incomplete type 'S1'}} expected-error 3 {{shared variable cannot be private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private (argv[1]) // expected-error {{expected variable name}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(ba)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(ca) // expected-error {{shared variable cannot be private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(da) // expected-error {{shared variable cannot be private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(S2::S2s) // expected-error {{shared variable cannot be private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(e, g) // expected-error 2 {{private variable must have an accessible, unambiguous default constructor}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(h) // expected-error {{threadprivate or thread local variable cannot be private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd shared(i), private(i) // expected-error {{shared variable cannot be private}} expected-note {{defined as shared}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(i)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target teams distribute simd private(j) // expected-error {{arguments of OpenMP clause 'private' cannot be of reference type}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp for private(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp target teams
  #pragma omp parallel for firstprivate(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp target teams
  #pragma omp parallel for reduction(+:i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp target teams
  #pragma omp parallel for lastprivate(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp parallel private(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp parallel firstprivate(i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }
  #pragma omp parallel reduction(+:i)
  for (int k = 0; k < 10; ++k) {
    #pragma omp target teams distribute simd private(i)
    for (int i = 0; i < argc; ++i) foo();
  }

  return 0;
}
