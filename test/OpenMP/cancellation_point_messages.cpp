// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int main(int argc, char **argv) {
  #pragma omp cancellation // expected-error {{expected an OpenMP directive}}
  #pragma omp cancellation point // expected-error {{expected 'parallel', 'sections', 'for' or 'taskgroup' construct type}}
  ;
  #pragma omp cancellation point parallel untied // expected-error {{unexpected OpenMP clause 'untied' in directive '#pragma omp cancellation point'}}
  #pragma omp cancellation point unknown // expected-error {{expected 'parallel', 'sections', 'for' or 'taskgroup' construct type}}
  #pragma omp cancellation point sections ( // expected-warning {{extra tokens at the end of '#pragma omp cancellation point' are ignored}}
  #pragma omp cancellation point for, ) // expected-warning {{extra tokens at the end of '#pragma omp cancellation point' are ignored}}
  #pragma omp cancellation point taskgroup () // expected-warning {{extra tokens at the end of '#pragma omp cancellation point' are ignored}}
  #pragma omp cancellation point parallel, if // expected-warning {{extra tokens at the end of '#pragma omp cancellation point' are ignored}}
  if (argc)
    #pragma omp cancellation point for // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  if (argc) {
  #pragma omp taskgroup
  #pragma omp task
    #pragma omp parallel
    {
    #pragma omp cancellation point taskgroup // expected-error {{region cannot be closely nested inside a parallel region}}
    }
  }
  #pragma omp parallel
  #pragma omp taskgroup
    {
    #pragma omp cancellation point taskgroup // expected-error {{region cannot be closely nested inside a taskgroup region}}
    }
  #pragma omp parallel
    {
    #pragma omp cancellation point for // expected-error {{region cannot be closely nested inside a parallel region}}
    }
  #pragma omp task
    {
    #pragma omp cancellation point sections // expected-error {{region cannot be closely nested inside explicit task region}}
    }
  #pragma omp sections
    {
    #pragma omp cancellation point parallel // expected-error {{region cannot be closely nested inside a worksharing region}}
    }
  while (argc)
    #pragma omp cancellation point for// expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  while (argc) {
    #pragma omp cancellation point sections
  }
  do
    #pragma omp cancellation point parallel // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  while (argc);
  do {
    #pragma omp cancellation point taskgroup
  }
  while (argc);
  switch (argc)
    #pragma omp cancellation point parallel // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  switch (argc)
    case 1:
    #pragma omp cancellation point sections // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  switch (argc)
    case 1: {
    #pragma omp cancellation point for
    }
  switch (argc) {
    #pragma omp cancellation point taskgroup
  case 1:
    #pragma omp cancellation point parallel // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
    break;
  default: {
    #pragma omp cancellation point sections
    }
    break;
  }
  for (;;)
    #pragma omp cancellation point for // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  for (;;) {
    #pragma omp cancellation point taskgroup
  }
  label:
    #pragma omp cancellation point parallel // expected-error {{'#pragma omp cancellation point' cannot be immediate substatement}}
  label1: {
    #pragma omp cancellation point sections
  }

  return 0;
}
