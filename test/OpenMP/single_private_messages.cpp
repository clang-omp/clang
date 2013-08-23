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
  #pragma omp single private // expected-error {{expected '(' after 'private'}} expected-error {{expected expression}}
  #pragma omp parallel
  #pragma omp single private ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp parallel
  #pragma omp single private () // expected-error {{expected expression}}
  #pragma omp parallel
  #pragma omp single private (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp parallel
  #pragma omp single private (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp parallel
  #pragma omp single private (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  #pragma omp parallel
  #pragma omp single private (argc)
  #pragma omp parallel
  #pragma omp single private (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp parallel
  #pragma omp single private (a, b, c, d, f) // expected-error {{private variable with incomplete type 'S1'}} expected-error 3 {{shared variable cannot be private}}
  #pragma omp parallel
  #pragma omp single private (argv[1]) // expected-error {{expected variable name}}
  #pragma omp parallel
  #pragma omp single private(ba)
  #pragma omp parallel
  #pragma omp single private(ca) // expected-error {{shared variable cannot be private}}
  #pragma omp parallel
  #pragma omp single private(da) // expected-error {{shared variable cannot be private}}
  #pragma omp parallel
  #pragma omp single private(S2::S2s) // expected-error {{shared variable cannot be private}}
  #pragma omp parallel
  #pragma omp single private(e, g) // expected-error 2 {{private variable must have an accessible, unambiguous default constructor}}
  #pragma omp parallel
  #pragma omp single private(h) // expected-error {{threadprivate or thread local variable cannot be private}}
  #pragma omp parallel
  #pragma omp single shared(i) // expected-error {{unexpected OpenMP clause 'shared' in directive '#pragma omp single'}}
  #pragma omp parallel
  #pragma omp single firstprivate(i), private(i) // expected-error {{firstprivate variable cannot be private}} expected-note {{defined as firstprivate}}
  foo();
  #pragma omp parallel shared(i)
  #pragma omp parallel private(i)
  #pragma omp single private(j) // expected-error {{arguments of OpenMP clause 'private' cannot be of reference type}}
  foo();
  #pragma omp parallel shared(i)
  #pragma omp parallel private(i)
  #pragma omp parallel firstprivate(i)
  #pragma omp parallel reduction(+:i)
  #pragma omp single private(i)
  foo();
  #pragma omp single private(i)
  {
    #pragma omp parallel
    #pragma omp single private(i)
    foo();
  }
  #pragma omp parallel
  #pragma omp single firstprivate(i)
  {
    #pragma omp parallel
    #pragma omp single private(i)
    foo();
  }
  #pragma omp parallel private(i)
  #pragma omp single copyprivate(i)
  {
    #pragma omp parallel
    #pragma omp single private(i)
    foo();
  }

  return 0;
}