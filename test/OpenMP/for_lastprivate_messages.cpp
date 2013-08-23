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
  static float S2s; // expected-note {{predetermined as shared}}
  static const float S2sc;
};
const float S2::S2sc = 0; // expected-note {{predetermined as shared}}
const S2 b;
const S2 ba[5];
class S3 { // expected-note {{'S3' declared here}}
  int a;
  S3& operator =(const S3& s3);
public:
  S3():a(0) { }
  S3(S3 &s3):a(s3.a) { }
};
const S3 c; // expected-note {{predetermined as shared}}
const S3 ca[5]; // expected-note {{predetermined as shared}}
extern const int f; // expected-note {{predetermined as shared}}
class S4 { // expected-note 2 {{'S4' declared here}}
  int a;
  S4();
  S4(const S4 &s4);
public:
  S4(int v):a(v) { }
};
class S5 { // expected-note {{'S5' declared here}}
  int a;
  S5():a(0) {}
public:
  S5(const S5 &s5):a(s5.a) { }
  S5(int v):a(v) { }
};

S3 h;
#pragma omp threadprivate(h) // expected-note {{defined as threadprivate or thread local}}

int main(int argc, char **argv) {
  const int d = 5; // expected-note {{predetermined as shared}}
  const int da[5] = { 0 }; // expected-note {{predetermined as shared}}
  S4 e(4); // expected-note 2 {{'e' defined here}}
  S5 g(5); // expected-note {{'g' defined here}}
  S3 m; // expected-note {{'m' defined here}}
  int i;
  int &j = i; // expected-note {{'j' defined here}}
  #pragma omp parallel
  #pragma omp for lastprivate // expected-error {{expected '(' after 'lastprivate'}} expected-error {{expected expression}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate () // expected-error {{expected expression}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (argc)
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (S1) // expected-error {{'S1' does not refer to a value}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (a, b, c, d, f) // expected-error {{lastprivate variable with incomplete type 'S1'}} expected-error 3 {{shared variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate (argv[1]) // expected-error {{expected variable name}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(ba)
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(ca) // expected-error {{shared variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(da) // expected-error {{shared variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(S2::S2s) // expected-error {{shared variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(S2::S2sc) // expected-error {{shared variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for firstprivate (g), lastprivate(e, g) // expected-error {{lastprivate variable must have an accessible, unambiguous default constructor}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(e, g) // expected-error 2 {{lastprivate variable must have an accessible, unambiguous default constructor}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(m) // expected-error {{lastprivate variable must have an accessible, unambiguous copy assignment operator}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(h) // expected-error {{threadprivate or thread local variable cannot be lastprivate}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for private(i), lastprivate(i) // expected-error {{private variable cannot be lastprivate}} expected-note{{defined as private}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel shared(i)
  #pragma omp for lastprivate(i)
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel private(i) // expected-note {{defined as private}}
  #pragma omp for lastprivate(i) // expected-error {{private variable in '#pragma omp parallel' cannot be lastprivate in '#pragma omp for'}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel reduction(+:i) // expected-note {{defined as reduction}}
  #pragma omp for lastprivate(i) // expected-error {{reduction variable in '#pragma omp parallel' cannot be lastprivate in '#pragma omp for'}}
  for (i = 0; i < argc; ++i) foo();
  #pragma omp parallel
  #pragma omp for lastprivate(j) // expected-error {{arguments of OpenMP clause 'lastprivate' cannot be of reference type}}
  for (i = 0; i < argc; ++i) foo();

  return 0;
}
