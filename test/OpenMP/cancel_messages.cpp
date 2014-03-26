// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int main(int argc, char **argv) {
  #pragma omp cancel // expected-error {{expected 'parallel', 'sections', 'for' or 'taskgroup' construct type}}
  ;
  #pragma omp cancel parallel untied // expected-error {{unexpected OpenMP clause 'untied' in directive '#pragma omp cancel'}}
  #pragma omp cancel unknown // expected-error {{expected 'parallel', 'sections', 'for' or 'taskgroup' construct type}}
  #pragma omp cancel sections ( // expected-warning {{extra tokens at the end of '#pragma omp cancel' are ignored}}
  #pragma omp cancel for, ) // expected-warning {{extra tokens at the end of '#pragma omp cancel' are ignored}}
  #pragma omp cancel taskgroup () // expected-warning {{extra tokens at the end of '#pragma omp cancel' are ignored}}
  #pragma omp cancel parallel, if // expected-error {{expected expression}} expected-error {{expected '(' after 'if'}}
  #pragma omp cancel sections if(argc,argv) // expected-warning {{extra tokens at the end of '#pragma omp cancel' are ignored}} expected-error {{expected ')'}} expected-note{{to match this '('}}
  if (argc)
    #pragma omp cancel for, if (argc // expected-error {{'#pragma omp cancel' cannot be immediate substatement}} expected-error {{expected ')'}} expected-note{{to match this '('}}
  if (argc) {
  #pragma omp taskgroup
  #pragma omp task
    #pragma omp parallel
    {
    #pragma omp cancel taskgroup // expected-error {{region cannot be closely nested inside a parallel region}}
    }
  }
  #pragma omp parallel
  #pragma omp taskgroup
    {
    #pragma omp cancel taskgroup // expected-error {{region cannot be closely nested inside a taskgroup region}}
    }
  #pragma omp parallel
    {
    #pragma omp cancel for // expected-error {{region cannot be closely nested inside a parallel region}}
    }
  #pragma omp task
    {
    #pragma omp cancel sections // expected-error {{region cannot be closely nested inside explicit task region}}
    }
  #pragma omp sections
    {
    #pragma omp cancel parallel // expected-error {{region cannot be closely nested inside a worksharing region}}
    }
  while (argc)
    #pragma omp cancel for// expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  while (argc) {
    #pragma omp cancel sections
  }
  do
    #pragma omp cancel parallel // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  while (argc);
  do {
    #pragma omp cancel taskgroup
  }
  while (argc);
  switch (argc)
    #pragma omp cancel parallel // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  switch (argc)
    case 1:
    #pragma omp cancel sections // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  switch (argc)
    case 1: {
    #pragma omp cancel for
    }
  switch (argc) {
    #pragma omp cancel taskgroup
  case 1:
    #pragma omp cancel parallel // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
    break;
  default: {
    #pragma omp cancel sections
    }
    break;
  }
  for (;;)
    #pragma omp cancel for // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  for (;;) {
    #pragma omp cancel taskgroup
  }
  label:
    #pragma omp cancel parallel // expected-error {{'#pragma omp cancel' cannot be immediate substatement}}
  label1: {
    #pragma omp cancel sections
  }

  return 0;
}
