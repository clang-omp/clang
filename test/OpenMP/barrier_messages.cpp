// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int foo();

int main(int argc, char **argv) {
  #pragma omp barrier
  ;
  #pragma omp barrier untied // expected-error {{unexpected OpenMP clause 'untied' in directive '#pragma omp barrier'}}
  #pragma omp barrier unknown // expected-warning {{extra tokens at the end of '#pragma omp barrier' are ignored}}
  if (argc)
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  if (argc) {
    #pragma omp barrier
  }
  while (argc)
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  while (argc) {
    #pragma omp barrier
  }
  do
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  while (argc);
  do {
    #pragma omp barrier
  }
  while (argc);
  switch (argc)
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  switch (argc)
    case 1:
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  switch (argc)
    case 1: {
    #pragma omp barrier
    }
  switch (argc) {
    #pragma omp barrier
  case 1:
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
    break;
  default: {
    #pragma omp barrier
    }
    break;
  }
  for (;;)
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  for (;;) {
    #pragma omp barrier
  }
  label:
    #pragma omp barrier // expected-error {{'#pragma omp barrier' cannot be immediate substatement}}
  label1: {
    #pragma omp barrier
  }
  #pragma omp for
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside a worksharing region}}
  }
  #pragma omp sections
  {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside a worksharing region}}
  }
  #pragma omp single
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside a worksharing region}}
  }
  #pragma omp task
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside explicit task region}}
  }
  #pragma omp master
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside a master region}}
  }
  #pragma omp critical
  for (int i = 0; i < 10; ++i) {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside a critical region}}
  }
  #pragma omp for ordered
  for (int i = 0; i < 10; ++i)
  #pragma omp ordered
  {
    foo();
    #pragma omp barrier // expected-error {{region cannot be closely nested inside an ordered region}}
  }

  return 0;
}
