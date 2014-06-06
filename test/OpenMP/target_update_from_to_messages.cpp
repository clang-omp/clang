// RUN: %clang_cc1 -verify -fopenmp -ferror-limit 100 %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note 2 {{declared here}}
extern S1 a;
class S2 {
  mutable int a;
public:
  S2():a(0) { }
  S2(S2 &s2):a(s2.a) { }
  static float S2s; // expected-note 4 {{mappable type cannot contain static members}}
  static const float S2sc; // expected-note 4 {{mappable type cannot contain static members}}
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
class S4 {
  int a;
  S4();
  S4(const S4 &s4);
public:
  S4(int v):a(v) { }
};
class S5 {
  int a;
  S5():a(0) {}
  S5(const S5 &s5):a(s5.a) { }
public:
  S5(int v):a(v) { }
};

S3 h;
#pragma omp threadprivate(h) // expected-error 2 {{threadprivate variables cannot be used in target constructs}}

int main(int argc, char **argv) {
  const int d = 5;
  const int da[5] = { 0 };
  S4 e(4);
  S5 g(5);
  int i;
  int &j = i;
  int *k = &j;
  const int (&l)[5] = da;
  #pragma omp target update from // expected-error {{expected '(' after 'from'}} expected-error {{expected expression}}
  #pragma omp target update from ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update from () // expected-error {{expected expression}}
  #pragma omp target update from (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update from (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update from (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  #pragma omp target update from (argc)
  #pragma omp target update from (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp target update from (a, b, c, d, f) // expected-error {{incomplete type 'S1' where a complete type is required}} expected-error 2 {{type 'S2' is not mappable to target}}
  #pragma omp target update from (argv[1])
  #pragma omp target update from(ba) // expected-error 2 {{type 'S2' is not mappable to target}}
  #pragma omp target update from(ca)
  #pragma omp target update from(da)
  #pragma omp target update from(S2::S2s)
  #pragma omp target update from(S2::S2sc)
  #pragma omp target update from(e, g)
  #pragma omp target update from(h) // expected-note {{used here}}
  #pragma omp target update from(k), from(k[:10]) // expected-error {{variable can appear only once in OpenMP 'target update' construct}} expected-note {{used here}}
  foo();
  #pragma omp target update from(da)
  #pragma omp target update from(da[:4])
  foo();

  #pragma omp target update to // expected-error {{expected '(' after 'to'}} expected-error {{expected expression}}
  #pragma omp target update to ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update to () // expected-error {{expected expression}}
  #pragma omp target update to (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update to (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp target update to (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  #pragma omp target update to (argc)
  #pragma omp target update to (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp target update to (a, b, c, d, f) // expected-error {{incomplete type 'S1' where a complete type is required}} expected-error 2 {{type 'S2' is not mappable to target}}
  #pragma omp target update to (argv[1])
  #pragma omp target update to(ba) // expected-error 2 {{type 'S2' is not mappable to target}}
  #pragma omp target update to(ca)
  #pragma omp target update to(da)
  #pragma omp target update to(S2::S2s)
  #pragma omp target update to(S2::S2sc)
  #pragma omp target update to(e, g)
  #pragma omp target update to(h) // expected-note {{used here}}
  #pragma omp target update to(k), to(k[:10]) // expected-error {{variable can appear only once in OpenMP 'target update' construct}} expected-note {{used here}}
  foo();
  #pragma omp target update to(da)
  #pragma omp target update to(da[:4])
  foo();
  #pragma omp target update to(da[:4]) from(da) // expected-error {{variable can appear only once in OpenMP 'target update' construct}} expected-note {{used here}}
  return 0;
}
