// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 -o - %s

#pragma omp // expected-error {{expected an OpenMP directive}}
#pragma omp unknown_directive // expected-error {{expected an OpenMP directive}}

void foo() {
#pragma omp // expected-error {{expected an OpenMP directive}}
#pragma omp unknown_directive // expected-error {{expected an OpenMP directive}}
#pragma omp parallel unknown_clause // expected-warning {{extra tokens at the end of '#pragma omp parallel' are ignored}}
#pragma omp parallel ordered // expected-error {{unexpected OpenMP clause 'ordered' in directive '#pragma omp parallel'}}
#pragma omp for unknown_clause // expected-warning {{extra tokens at the end of '#pragma omp for' are ignored}}
for (int i = 0; i < 1; ++i) ++i;
#pragma omp for default(none) // expected-error {{unexpected OpenMP clause 'default' in directive '#pragma omp for'}}
for (int i = 0; i < 1; ++i) ++i;
foo();
}
