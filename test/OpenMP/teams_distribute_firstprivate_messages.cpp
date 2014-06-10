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
  S2(S2 &s2):a(s2.a) { }
  static float S2s;
  static const float S2sc;
};
const float S2::S2sc = 0;
const S2 b;
const S2 ba[5];
class S3 {
  int a;
public:
  S3():a(0) { }
  S3(S3 &s3):a(s3.a) { }
};
const S3 c;
const S3 ca[5];
extern const int f;
class S4 { // expected-note {{'S4' declared here}}
  int a;
  S4();
  S4(const S4 &s4);
public:
  S4(int v):a(v) { }
};
class S5 { // expected-note {{'S5' declared here}}
  int a;
  S5():a(0) {}
  S5(const S5 &s5):a(s5.a) { }
public:
  S5(int v):a(v) { }
};

S3 h;
#pragma omp threadprivate(h) // expected-note {{defined as threadprivate or thread local}}

int main(int argc, char **argv) {
  const int d = 5;
  const int da[5] = { 0 };
  S4 e(4); // expected-note {{'e' defined here}}
  S5 g(5); // expected-note {{'g' defined here}}
  int i;
  int &j = i; // expected-note {{'j' defined here}}
  #pragma omp target
  #pragma omp teams distribute firstprivate // expected-error {{expected '(' after 'firstprivate'}} expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate () // expected-error {{expected expression}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (argc)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (S1) // expected-error {{'S1' does not refer to a value}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (a, b, c, d, f) // expected-error {{firstprivate variable with incomplete type 'S1'}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate (argv[1]) // expected-error {{expected variable name}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(ba)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(ca)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(da)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(S2::S2s)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(S2::S2sc)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(e, g) // expected-error 2 {{firstprivate variable must have an accessible, unambiguous copy constructor}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(h) // expected-error {{threadprivate or thread local variable cannot be firstprivate}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute private(i), firstprivate(i) // expected-error {{private variable cannot be firstprivate}} expected-note{{defined as private}}
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(i)
  for (int i = 0; i < argc; ++i) foo();
  #pragma omp target
  #pragma omp teams distribute firstprivate(j) // expected-error {{arguments of OpenMP clause 'firstprivate' cannot be of reference type}}
  for (int i = 0; i < argc; ++i) foo();

  return 0;
}
