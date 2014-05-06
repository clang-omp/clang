// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

void foo() {
}

#pragma omp teams // expected-error {{unexpected OpenMP directive '#pragma omp teams'}}

int main(int argc, char **argv) {
  #pragma omp target
  #pragma omp teams
  foo();
  #pragma omp target
  {
  #pragma omp teams
  foo();
  }
  #pragma omp target
  {
    foo();
  #pragma omp teams // expected-error {{the teams construct must be the only construct inside of target region}}
  foo();
  }
  #pragma omp target
  #pragma omp teams unknown() // expected-warning {{extra tokens at the end of '#pragma omp teams' are ignored}}
  foo();
  L1:
    foo();
  #pragma omp target
  #pragma omp teams
  ;
  #pragma omp target
  #pragma omp teams
  {
    goto L1; // expected-error {{use of undeclared label 'L1'}}
    argc++;
  }

  for (int i = 0; i < 10; ++i) {
    switch(argc) {
     case (0):
      #pragma omp target
      #pragma omp teams
      {
        foo();
        break; // expected-error {{'break' statement not in loop or switch statement}}
        continue; // expected-error {{'continue' statement not in loop statement}}
      }
      default:
       break;
    }
  }
  #pragma omp target
  #pragma omp teams default(none)
  ++argc; // expected-error {{variable 'argc' must have explicitly specified data sharing attributes}}

  return 0;
}
