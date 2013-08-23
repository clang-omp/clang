// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

void foo() {
}

bool foobool(int argc) {
  return argc;
}

struct S1; // expected-note {{declared here}}
extern S1 a;
class S2 {
  mutable int a;
public:
  S2():a(0) { }
  S2(S2 &s2):a(s2.a) { }
};
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
#pragma omp threadprivate(h) // expected-note {{defined as threadprivate or thread local}}

int main(int argc, char **argv) {
  const int d = 5;
  const int da[5] = { 0 };
  S4 e(4);
  S5 g(5);
  int i;
  int &j = i;
  #pragma omp parallel
  #pragma omp task shared // expected-error {{expected '(' after 'shared'}} expected-error {{expected expression}}
  #pragma omp task shared ( // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp task shared () // expected-error {{expected expression}}
  #pragma omp task shared (argc // expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp task shared (argc, // expected-error {{expected expression}} expected-error {{expected ')'}} expected-note {{to match this '('}}
  #pragma omp task shared (argc > 0 ? argv[1] : argv[2]) // expected-error {{expected variable name}}
  #pragma omp task shared (argc)
  #pragma omp task shared (S1) // expected-error {{'S1' does not refer to a value}}
  #pragma omp task shared (a, b, c, d, f)
  #pragma omp task shared (argv[1]) // expected-error {{expected variable name}}
  #pragma omp task shared(ba)
  #pragma omp task shared(ca)
  #pragma omp task shared(da)
  #pragma omp task shared(e, g)
  #pragma omp task shared(h) // expected-error {{threadprivate or thread local variable cannot be shared}}
  #pragma omp task private(i), shared(i) // expected-error {{private variable cannot be shared}} expected-note {{defined as private}}
  foo();
  #pragma omp parallel
  #pragma omp task private(i)
  #pragma omp task shared(i)
  #pragma omp task shared(j)
  foo();

  return 0;
}
