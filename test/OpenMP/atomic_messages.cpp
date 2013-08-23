// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

struct S {
  int s;
  S operator+=(const S &s) { return *this; }
} s1, s2, s3;

int main() {
  int a, b, c;
  int arr[5];
  
  #pragma omp atomic
  ; // expected-error {{expected expression statement for '#pragma omp atomic update'}}
  #pragma omp atomic untied // expected-error {{unexpected OpenMP clause 'untied' in directive '#pragma omp atomic'}}
  ++a;
  #pragma omp atomic unknown // expected-warning {{extra tokens at the end of '#pragma omp atomic' are ignored}}
  ++a;
  #pragma omp atomic read write update capture // expected-error {{directive '#pragma omp atomic' cannot contain more than one 'read', 'write', 'update' or 'capture' clause}}
  ++a;
  #pragma omp atomic (name) // expected-warning {{extra tokens at the end of '#pragma omp atomic' are ignored}}
  ++a;
  #pragma omp atomic read
  a = b;
  #pragma omp atomic read seq_cst
  a = b;
  #pragma omp atomic read
  s1 = s2; //expected-error {{statement form is not allowed for '#pragma omp atomic read'}}
  #pragma omp atomic read
  a = b + 5; //expected-error {{statement form is not allowed for '#pragma omp atomic read'}}
  #pragma omp atomic read seq_cst
  a = a; //expected-error {{statement form is not allowed for '#pragma omp atomic read'}}
  #pragma omp atomic read
  ++a; //expected-error {{statement form is not allowed for '#pragma omp atomic read'}}

  #pragma omp atomic write
  a = b + 5;
  #pragma omp atomic write seq_cst
  a = b;
  #pragma omp atomic write
  s1 = s2; //expected-error {{statement form is not allowed for '#pragma omp atomic write'}}
  #pragma omp atomic write seq_cst
  a = a; //expected-error {{statement form is not allowed for '#pragma omp atomic write'}}
  #pragma omp atomic write
  ++a; //expected-error {{statement form is not allowed for '#pragma omp atomic write'}}

  #pragma omp atomic
  ++a;
  #pragma omp atomic update
  b++;
  #pragma omp atomic seq_cst
  a -= b;
  #pragma omp atomic update seq_cst
  a = b & a;
  #pragma omp atomic update seq_cst
  a += b & b;
  #pragma omp atomic
  s1 += s2; //expected-error {{statement form is not allowed for '#pragma omp atomic update'}}
  #pragma omp atomic
  a %= b; //expected-error {{statement form is not allowed for '#pragma omp atomic update'}}
  #pragma omp atomic update
  a = a & int(a); //expected-error {{statement form is not allowed for '#pragma omp atomic update'}}
  #pragma omp atomic seq_cst
  a = b && a; //expected-error {{statement form is not allowed for '#pragma omp atomic update'}}
  #pragma omp atomic update seq_cst
  {a--;} //expected-error {{expected expression statement for '#pragma omp atomic update'}}

  #pragma omp atomic capture
  b = ++a;
  #pragma omp atomic capture
  a = b++;
  #pragma omp atomic capture seq_cst
  c = a -= b;
  #pragma omp atomic capture seq_cst
  arr[1] = a = b & a;
  #pragma omp atomic capture seq_cst
  arr[1] = arr[1] &= b; //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture seq_cst
  arr[1] = b &= arr[1]; //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  s3 = s1 += s2; //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  c = a %= b; //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  c = a = a & int(a); //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture seq_cst
  c = a = b && a; //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  {c = b; ++b;}
  #pragma omp atomic capture
  {a = b++ + a; c = a;}
  #pragma omp atomic capture seq_cst
  {c = a; a -= b;}
  #pragma omp atomic capture seq_cst
  {a = b & a; c = a;}
  #pragma omp atomic capture seq_cst
  {a -= b + b; c = a;}
  #pragma omp atomic capture
  {s3 = s1; s1 += s2;} //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  {a %= b; c = a;} //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture
  {c = a; a = a & int(a); c = a;} //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture seq_cst
  {a = b && a; c = a;} //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}
  #pragma omp atomic capture seq_cst
  {a = b & c; c = a;} //expected-error {{statement form is not allowed for '#pragma omp atomic capture'}}

  return 0;
}

int foo() {
  L1:
    foo();
  #pragma omp atomic
  { // expected-error {{expected expression statement for '#pragma omp atomic update'}}
    foo();
    goto L1; // expected-error {{use of undeclared label 'L1'}}
  }
  goto L2; // expected-error {{use of undeclared label 'L2'}}
  #pragma omp atomic read
  { // expected-error {{expected expression statement for '#pragma omp atomic read'}}
    foo();
    L2:
    foo();
  }

  return 0;
}
