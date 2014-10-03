///
/// Perform several offloading tests for the driver and codegen of OpenMP target
/// regions
///

/// Check whether an invalid OpenMP target is specified:
// RUN:   %clang -### -fopenmp -omptargets=aaa-bbb-ccc-ddd %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-INVALID-TARGET %s
// CHK-INVALID-TARGET: error: OpenMP target is invalid: 'aaa-bbb-ccc-ddd'

/// Check error for empty -omptargets
// RUN:   %clang -### -fopenmp -omptargets=  %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-EMPTY-OMPTARGETS %s
// CHK-EMPTY-OMPTARGETS: warning: joined argument expects additional value: '-omptargets='

/// Check whether we are using a target whose toolchain was not prepared to
/// to support offloading:
// RUN:   %clang -### -fopenmp -omptargets=x86_64-apple-darwin %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-NO-SUPPORT %s
// CHK-NO-SUPPORT: error: Toolchain for target 'x86_64-apple-darwin' is not supporting OpenMP offloading.

/// Target independent check of the commands passed to each tool when using
/// valid OpenMP targets
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-COMMANDS %s
//
// Host front-end command
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-module-id=[[ID:[0-9a-f]+_[0-9a-f]+]]"
// Target1 commands
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-module-id=[[ID]]" "-triple" "nvptx64-nvidia-cuda" "-E"
// CHK-COMMANDS: "-target-cpu" "sm_20"
// CHK-COMMANDS: "-o" "[[T1PP:.+]].i"
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-module-id=[[ID]]" "-triple" "nvptx64-nvidia-cuda" "-S"
// CHK-COMMANDS: "-target-cpu" "sm_20"
// CHK-COMMANDS: "-o" "[[T1ASM:.+]].s" "-x" "cpp-output" "[[T1PP]].i"
// CHK-COMMANDS: ptxas" "-o" "[[T1OBJ:.+]].o" "-c" "-arch" "sm_20" "[[T1ASM]].s"
// CHK-COMMANDS: cp" "[[T1OBJ]].o" "[[T1CBIN:.+]].cubin"
// CHK-COMMANDS: nvlink" "-o" "[[T1LIB:.+]].so" "-arch" "sm_20" "[[T1CBIN]].cubin"

// Target2 command
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-module-id=[[ID]]" "-triple" "powerpc64-ibm-linux-gnu" "-E"
// CHK-COMMANDS: "-target-cpu" "ppc64"
// CHK-COMMANDS: "-o" "[[T2PP:.+]].i"
// CHK-COMMANDS: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-omp-module-id=[[ID]]" "-triple" "powerpc64-ibm-linux-gnu" "-S"
// CHK-COMMANDS: "-target-cpu" "ppc64"
// CHK-COMMANDS: "-o" "[[T2ASM:.+]].s" "-x" "cpp-output" "[[T2PP]].i"
// CHK-COMMANDS: as" "-a64" "-mppc64" "-many" "-o" "[[T2OBJ:.+]].o" "[[T2ASM]].s"
// CHK-COMMANDS: ld" "--eh-frame-hdr" "-m" "elf64ppc" "-shared" "-o" "[[T2LIB:.+]].so" {{.*}} "[[T2OBJ]].o"

// Final linking command
// CHK-COMMANDS: ld" {{.*}} "-o" "a.out"  {{.*}}  "[[HOSTOBJ:.+]].o" "-liomp5" "-lomptarget" {{.*}} "-T" "[[LKSCRIPT:.+]].lk"

/// Check frontend require module ID
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-triple" "powerpc64-ibm-linux-gnu" %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MODULEID %s
// RUN:   not %clang_cc1 "-fopenmp" "-omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda" "-omp-target-mode" "-triple" "nvptx64-nvidia-cuda" %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-MODULEID %s
// CHK-MODULEID: error: A module ID is required to enable OpenMP target code generation. Use '-omp-module-id=ID'.

/// Check the subtarget detection
// RUN:   %clang -### -fopenmp -target powerpc64-linux -omptargets=nvptx64sm_35-nvidia-cuda %s 2>&1 \
// RUN:   | FileCheck -check-prefix=CHK-SUBTARGET %s
// CHK-SUBTARGET: clang{{.*}}" "-cc1" "-fopenmp" "-omptargets=nvptx64sm_35-nvidia-cuda" "-omp-target-mode" "-omp-module-id=[[ID:[0-9a-f]+_[0-9a-f]+]]" "-triple" "nvptx64sm_35-nvidia-cuda" "-E"
// CHK-SUBTARGET: "-target-cpu" "sm_35"
// CHK-SUBTARGET: "-o" "[[T1PP:.+]].i"

/// Check the codegen
// RUN:   %clang -S -emit-llvm -O0 -fopenmp -target powerpc64-linux -omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda %s 2>&1
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-HOST -input-file=target_driver_and_codegen.ll %s
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-TARGET1 -input-file=target_driver_and_codegen.tgt-nvptx64-nvidia-cuda.ll %s
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-TARGET2 -input-file=target_driver_and_codegen.tgt-powerpc64-ibm-linux-gnu.ll %s


// CHK-CODEGEN-HOST:   @__omptgt__img_start_[[T1:.*]] = external constant i8
// CHK-CODEGEN-HOST:   @__omptgt__img_end_[[T1]] = external constant i8
// CHK-CODEGEN-HOST:   @__omptgt__img_start_[[T2:.*]] = external constant i8
// CHK-CODEGEN-HOST:   @__omptgt__img_end_[[T2]] = external constant i8
// CHK-CODEGEN-HOST:   @__omptgt__device_images = internal constant [2 x { i8*, i8* }] [
// CHK-CODEGEN-HOST:   { i8*, i8* } { i8* @__omptgt__img_start_[[T1]], i8* @__omptgt__img_end_[[T1]] },
// CHK-CODEGEN-HOST:   { i8*, i8* } { i8* @__omptgt__img_start_[[T2]], i8* @__omptgt__img_end_[[T2]] }]
// CHK-CODEGEN-HOST:   @__omptgt__host_entries_begin = external constant { i8*, i32 }
// CHK-CODEGEN-HOST:   @__omptgt__host_entries_end = external constant { i8*, i32 }
// CHK-CODEGEN-HOST:   @__omptgt__target_regions_descriptor =
// CHK-CODEGEN-HOST:   internal constant { i32, { i8*, i8* }*, { i8*, i32 }*, { i8*, i32 }* } {
// CHK-CODEGEN-HOST:   i32 2,
// CHK-CODEGEN-HOST:   { i8*, i8* }* getelementptr inbounds ([2 x { i8*, i8* }]* @__omptgt__device_images, i32 0, i32 0),
// CHK-CODEGEN-HOST:   { i8*, i32 }* @__omptgt__host_entries_begin,
// CHK-CODEGEN-HOST:   { i8*, i32 }* @__omptgt__host_entries_end }

/// Argument sizes (int32)
// CHK-CODEGEN-HOST:   [[S1:@.tgt_sizes[0-9]*]] = private constant [1 x i32] [i32 4]
/// Map types (to-from)
// CHK-CODEGEN-HOST:   [[M1:@.tgt_types[0-9]*]] = private constant [1 x i32] [i32 3]
/// Dummy host pointer (we do not use the function pointer because we do not
/// want to prevent inlining
// CHK-CODEGEN-HOST:   @__omptgt__host_ptr_0 = internal constant i8 0
// CHK-CODEGEN-HOST:   @__omptgt__[[ID:[0-9a-f]+_[0-9a-f]+]]_0_hst_entry =
// CHK-CODEGEN-HOST:   constant { i8*, i32 } { i8* @__omptgt__host_ptr_0, i32 0 },
// CHK-CODEGEN-HOST:   section ".openmptgt_host_entries"


/// Argument sizes (int32)
// CHK-CODEGEN-HOST:   [[S2:@.tgt_sizes[0-9]*]] = private constant [1 x i32] [i32 4]
/// Map types (to-from)
// CHK-CODEGEN-HOST:   [[M2:@.tgt_types[0-9]*]] = private constant [1 x i32] [i32 3]
/// Dummy host pointer (we do not use the function pointer because we do not
/// want to prevent inlining
// CHK-CODEGEN-HOST:   @__omptgt__host_ptr_1 = internal constant i8 0
// CHK-CODEGEN-HOST:   @__omptgt__[[ID]]_1_hst_entry =
// CHK-CODEGEN-HOST:   constant { i8*, i32 } { i8* @__omptgt__host_ptr_1, i32 0 },
// CHK-CODEGEN-HOST:   section ".openmptgt_host_entries"

#pragma omp declare target
// CHK-CODEGEN-HOST:    define signext i32 @tdouble
// CHK-CODEGEN-TARGET1: define i32 @tdouble
// CHK-CODEGEN-TARGET2: define signext i32 @tdouble
int tdouble(int a){
  return 2*a;
}
// CHK-CODEGEN-HOST: define signext i32 @tsquare
// CHK-CODEGEN-TARGET1: define i32 @tsquare
// CHK-CODEGEN-TARGET2: define signext i32 @tsquare
int tsquare(int a){
  return a*a;
}
// CHK-CODEGEN-HOST: define signext i32 @tadd
// CHK-CODEGEN-TARGET1: define i32 @tadd
// CHK-CODEGEN-TARGET2: define signext i32 @tadd
int tadd(int a){
  return a+a;
}
#pragma omp end declare target

int foo(int a){
  // CHK-CODEGEN-HOST: define signext i32 @foo
  // CHK-CODEGEN-HOST: call void @__kmpc_register_lib({ i32, { i8*, i8* }*, { i8*, i32 }*, { i8*, i32 }* }* @__omptgt__target_regions_descriptor)
  int i;

  // CHK-CODEGEN-HOST: [[RET1:%[a-zA-Z0-9_\.]+]] = call i32 @__kmpc_target(
  // CHK-CODEGEN-HOST: i32 [[DEVICEID1:0]],
  // CHK-CODEGEN-HOST: i8* @__omptgt__host_ptr_0,
  // CHK-CODEGEN-HOST: i32 [[ARGNUM1:[0-9]+]],
  // CHK-CODEGEN-HOST: i8** [[DATA1:%[a-zA-Z0-9_\.]+]],
  // CHK-CODEGEN-HOST: i32* getelementptr inbounds ([1 x i32]* [[S1]], i32 0, i32 0),
  // CHK-CODEGEN-HOST: i32* getelementptr inbounds ([1 x i32]* [[M1]], i32 0, i32 0))
  // CHK-CODEGEN-HOST: [[CMP1:%[0-9]+]] = icmp eq i32 [[RET1]], 0
  // CHK-CODEGEN-HOST: br i1 [[CMP1]], label %[[OFFSUCCESS1:[a-zA-Z0-9_\.]+]], label %[[OFFFAIL1:[a-zA-Z0-9_\.]+]]

  // CHK-CODEGEN-HOST: [[OFFFAIL1]]{{.*}}
  // CHK-CODEGEN-HOST: call void @__omptgt__[[ID]]_0(i32* %{{.*}})
  // CHK-CODEGEN-HOST: br label %[[OFFSUCCESS1]]

  // CHK-CODEGEN-HOST: [[OFFSUCCESS1]]{{.*}}

  // CHK-CODEGEN-TARGET1: define void @__omptgt__[[ID:[0-9a-f]+_[0-9a-f]+]]_nvptx64_nvidia_cuda_0
  // CHK-CODEGEN-TARGET2: define void @__omptgt__[[ID:[0-9a-f]+_[0-9a-f]+]]_powerpc64_ibm_linux_gnu_0
#pragma omp target
  {
    a += tsquare(a);
  }

  for(i=0; i<5; ++i){
    // CHK-CODEGEN-HOST: [[RET2:%[a-zA-Z0-9_\.]+]] = call i32 @__kmpc_target(
    // CHK-CODEGEN-HOST: i32 [[DEVICEID2:0]],
    // CHK-CODEGEN-HOST: i8* @__omptgt__host_ptr_1,
    // CHK-CODEGEN-HOST: i32 [[ARGNUM2:[0-9]+]],
    // CHK-CODEGEN-HOST: i8** [[DATA2:%[a-zA-Z0-9_\.]+]],
    // CHK-CODEGEN-HOST: i32* getelementptr inbounds ([1 x i32]* [[S2]], i32 0, i32 0),
    // CHK-CODEGEN-HOST: i32* getelementptr inbounds ([1 x i32]* [[M2]], i32 0, i32 0))
    // CHK-CODEGEN-HOST: [[CMP2:%[0-9]+]] = icmp eq i32 [[RET2]], 0
    // CHK-CODEGEN-HOST: br i1 [[CMP2]], label %[[OFFSUCCESS2:[a-zA-Z0-9_\.]+]], label %[[OFFFAIL2:[a-zA-Z0-9_\.]+]]

    // CHK-CODEGEN-HOST: [[OFFFAIL2]]{{.*}}
    // CHK-CODEGEN-HOST: call void @__omptgt__[[ID]]_1(i32* %{{.*}})
    // CHK-CODEGEN-HOST: br label %[[OFFSUCCESS2]]

    // CHK-CODEGEN-HOST: [[OFFSUCCESS2]]{{.*}}

    // CHK-CODEGEN-TARGET1: define void @__omptgt__[[ID]]_nvptx64_nvidia_cuda_1
    // CHK-CODEGEN-TARGET2: define void @__omptgt__[[ID]]_powerpc64_ibm_linux_gnu_1
#pragma omp target
    {
      a -= tdouble(a) + tadd(a);
    }
  }

  return a;
}

int main(int argc, char *argv[]){
  return foo(argc);
}
