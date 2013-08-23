// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int main(int argc, char **argv) {
  #pragma omp taskwait
  ;
  #pragma omp taskwait untied // expected-error {{unexpected OpenMP clause 'untied' in directive '#pragma omp taskwait'}}
  #pragma omp taskwait unknown // expected-warning {{extra tokens at the end of '#pragma omp taskwait' are ignored}}
  if (argc)
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  if (argc) {
    #pragma omp taskwait
  }
  while (argc)
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  while (argc) {
    #pragma omp taskwait
  }
  do
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  while (argc);
  do {
    #pragma omp taskwait
  }
  while (argc);
  switch (argc)
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  switch (argc)
    case 1:
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  switch (argc)
    case 1: {
    #pragma omp taskwait
    }
  switch (argc) {
    #pragma omp taskwait
  case 1:
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
    break;
  default: {
    #pragma omp taskwait
    }
    break;
  }
  for (;;)
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  for (;;) {
    #pragma omp taskwait
  }
  label:
    #pragma omp taskwait // expected-error {{'#pragma omp taskwait' cannot be immediate substatement}}
  label1: {
    #pragma omp taskwait
  }

  return 0;
}
