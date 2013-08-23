// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -ferror-limit 100 %s

int main() {
  int i;
  goto label; // expected-error {{use of undeclared label 'label'}}
  #pragma omp for
  for (i = 0; i < 100; ++i) {
    label: ++i;
  }
}
