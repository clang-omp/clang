///
/// Perform preliminary check of map clause codegen
///

/// Check the codegen
// RUN:   %clang -S -emit-llvm -O0 -fopenmp -target powerpc64-linux -omptargets=powerpc64-ibm-linux-gnu %s 2>&1
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-HOST -input-file=target_map_codegen.ll %s

double a[32];
double b[32];
double c[32];


void foo(double A[32], double B[32], double C[32]){
  int i;

  // CHK-CODEGEN-HOST: [[MT:@.mapped_types[0-9]*]] = private constant [3 x i32] [i32 1, i32 1, i32 3]
  // CHK-CODEGEN-HOST: [[MP:%[a-zA-Z0-9_\.]+]] = alloca i8*, i32 3
  // CHK-CODEGEN-HOST: [[MS:%[a-zA-Z0-9_\.]+]] = alloca i32, i32 3

  // CHK-CODEGEN-HOST: [[P0:%[0-9]+]] = getelementptr inbounds i8** [[MP]], i32 0
  // CHK-CODEGEN-HOST: [[S0:%[0-9]+]] = getelementptr inbounds i32* [[MS]], i32 0
  // CHK-CODEGEN-HOST: store i8* bitcast (double* getelementptr inbounds ([32 x double]* @a, i32 0, i64 2) to i8*), i8** [[P0]]
  // CHK-CODEGEN-HOST: store i32 trunc (i64 sub (i64 ptrtoint (double* getelementptr inbounds ([32 x double]* @a, i64 1, i64 0) to i64), i64 ptrtoint (double* getelementptr inbounds ([32 x double]* @a, i32 0, i64 2) to i64)) to i32), i32* [[S0]]

  // CHK-CODEGEN-HOST: [[P1:%[0-9]+]] = getelementptr inbounds i8** [[MP]], i32 1
  // CHK-CODEGEN-HOST: [[S1:%[0-9]+]] = getelementptr inbounds i32* [[MS]], i32 1
  // CHK-CODEGEN-HOST: store i8* bitcast ([32 x double]* @b to i8*), i8** [[P1]]
  // CHK-CODEGEN-HOST: store i32 256, i32* [[S1]]
  // CHK-CODEGEN-HOST: [[P2:%[0-9]+]] = getelementptr inbounds i8** [[MP]], i32 2
  // CHK-CODEGEN-HOST: [[S2:%[0-9]+]] = getelementptr inbounds i32* [[MS]], i32 2
  // CHK-CODEGEN-HOST: store i8* bitcast ([32 x double]* @c to i8*), i8** [[P2]]
  // CHK-CODEGEN-HOST: store i32 256, i32* [[S2]]

  // CHK-CODEGEN-HOST: call void @__kmpc_target_data_begin(i32 1, i32 3, i8** [[MP]], i32* [[MS]], i32* getelementptr inbounds ([3 x i32]* [[MT]], i32 0, i32 0))
  // CHK-CODEGEN-HOST: call i32 @__kmpc_target(
  // CHK-CODEGEN-HOST: call void @__kmpc_target_data_end(i32 1, i32 3, i8** [[MP]], i32* [[MS]], i32* getelementptr inbounds ([3 x i32]* [[MT]], i32 0, i32 0))

#pragma omp target map(to: a[2:30], b) map(tofrom: c) device(1)
  for(i=2; i<32; ++i)
  {
    C[i] += A[i] * B[i] + a[i];
  }

  return;
}

int main(int argc, char *argv[]){
  foo(a, b, c);
  return 0;
}
