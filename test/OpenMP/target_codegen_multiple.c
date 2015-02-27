///
/// Perform codegen tests for OpenMP offloading when using multiple target
/// types, in this case powerpc64 and nvptx64 targets. The host is a powerpc64.
///

/// Check the codegen
// RUN:   %clang -S -emit-llvm -O0 -fopenmp -target powerpc64-linux -omptargets=powerpc64-ibm-linux-gnu,nvptx64-nvidia-cuda %s 2>&1
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-HOST -input-file=target_codegen_multiple.ll %s
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-TARGET1 -input-file=target_codegen_multiple.ll.tgt-powerpc64-ibm-linux-gnu %s
// RUN:   FileCheck -check-prefix=CHK-CODEGEN-TARGET2 -input-file=target_codegen_multiple.ll.tgt-nvptx64-nvidia-cuda %s


// Entries and map type of the global variable
// CHK-CODEGEN-HOST: @GV = global i32 123
// CHK-CODEGEN-HOST: @__omptgt__gbl__0_[[ID:[0-9a-f]+_[0-9a-f]+]]__entry_name = internal constant [3 x i8] c"GV\00"
// CHK-CODEGEN-HOST: @__omptgt__gbl__0_[[ID]]__entry = constant { i8*, i8*, i64 } { i8* bitcast (i32* @GV to i8*),
// CHK-CODEGEN-HOST:   i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__0_[[ID]]__entry_name, i32 0, i32 0), i64 4
// CHK-CODEGEN-HOST: }, section ".openmptgt_host_entries", align 1

// CHK-CODEGEN-TARGET1: @__omptgt__gbl__0_[[ID:[0-9a-f]+_[0-9a-f]+]]__entry_name = internal constant [3 x i8] c"GV\00"
// CHK-CODEGEN-TARGET1: @__omptgt__gbl__0_[[ID]]__entry = constant { i8*, i8*, i64 } { i8* bitcast (i32* @GV to i8*),
// CHK-CODEGEN-TARGET1:   i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__0_[[ID]]__entry_name, i32 0, i32 0), i64 4
// CHK-CODEGEN-TARGET1: }, section ".openmptgt_host_entries", align 1

// Entries and map type of 1st target region
// CHK-CODEGEN-HOST: @__omptgt__0_[[ID]]__entry_name =
// CHK-CODEGEN-HOST: internal constant [{{[1-9][0-9]*}} x i8] c"__omptgt__0_[[ID]]_\00"
// CHK-CODEGEN-HOST: @__omptgt__0_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CHK-CODEGEN-HOST: i8* bitcast (void (i32*)* @__omptgt__0_[[ID]]_ to i8*), 
// CHK-CODEGEN-HOST: i8* getelementptr inbounds ([{{[1-9][0-9]*}} x i8]* @__omptgt__0_[[ID]]__entry_name, i32 0, i32 0), i64 0
// CHK-CODEGEN-HOST: }, section ".openmptgt_host_entries", align 1
// CHK-CODEGEN-HOST: [[M1:@.tgt_types[0-9]*]] = private constant [1 x i32] [i32 3]

// CHK-CODEGEN-TARGET1: @__omptgt__0_[[ID]]__entry_name =
// CHK-CODEGEN-TARGET1: internal constant [{{[1-9][0-9]*}} x i8] c"__omptgt__0_[[ID]]_\00"
// CHK-CODEGEN-TARGET1: @__omptgt__0_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CHK-CODEGEN-TARGET1: i8* bitcast (void (i32*)* @__omptgt__0_[[ID]]_ to i8*), 
// CHK-CODEGEN-TARGET1: i8* getelementptr inbounds ([{{[1-9][0-9]*}} x i8]* @__omptgt__0_[[ID]]__entry_name, i32 0, i32 0), i64 0
// CHK-CODEGEN-TARGET1: }, section ".openmptgt_host_entries", align 1

// Entries and map type of 2nd target region
// CHK-CODEGEN-HOST: @__omptgt__1_[[ID]]__entry_name = 
// CHK-CODEGEN-HOST: internal constant [{{[1-9][0-9]*}} x i8] c"__omptgt__1_[[ID]]_\00"
// CHK-CODEGEN-HOST: @__omptgt__1_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CHK-CODEGEN-HOST: i8* bitcast (void (i32*)* @__omptgt__1_[[ID]]_ to i8*), 
// CHK-CODEGEN-HOST: i8* getelementptr inbounds ([{{[1-9][0-9]*}} x i8]* @__omptgt__1_[[ID]]__entry_name, i32 0, i32 0), i64 0
// CHK-CODEGEN-HOST: }, section ".openmptgt_host_entries", align 1
// CHK-CODEGEN-HOST: [[M2:@.tgt_types[0-9]*]] = private constant [1 x i32] [i32 3]

// CHK-CODEGEN-TARGET1: @__omptgt__1_[[ID]]__entry_name = 
// CHK-CODEGEN-TARGET1: internal constant [{{[1-9][0-9]*}} x i8] c"__omptgt__1_[[ID]]_\00"
// CHK-CODEGEN-TARGET1: @__omptgt__1_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CHK-CODEGEN-TARGET1: i8* bitcast (void (i32*)* @__omptgt__1_[[ID]]_ to i8*), 
// CHK-CODEGEN-TARGET1: i8* getelementptr inbounds ([{{[1-9][0-9]*}} x i8]* @__omptgt__1_[[ID]]__entry_name, i32 0, i32 0), i64 0
// CHK-CODEGEN-TARGET1: }, section ".openmptgt_host_entries", align 1

// Host entries range defined by the linker
// CHK-CODEGEN-HOST: @__omptgt__host_entries_begin = external constant { i8*, i8*, i64 }
// CHK-CODEGEN-HOST: @__omptgt__host_entries_end = external constant { i8*, i8*, i64 }

// Targets image start and end - set by the linker
// CHK-CODEGEN-HOST: @__omptgt__img_start_[[T1:powerpc64_ibm_linux_gnu]] = external constant i8
// CHK-CODEGEN-HOST: @__omptgt__img_end_[[T1]] = external constant i8
// CHK-CODEGEN-HOST: @__omptgt__img_start_[[T2:nvptx64_nvidia_cuda]] = external constant i8
// CHK-CODEGEN-HOST: @__omptgt__img_end_[[T2]] = external constant i8

// CHK-CODEGEN-HOST: @__omptgt__device_images = internal constant [2 x { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }] [
// CHK-CODEGEN-HOST: { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* } { i8* @__omptgt__img_start_[[T1]], i8* @__omptgt__img_end_[[T1]],
// CHK-CODEGEN-HOST: { i8*, i8*, i64 }* @__omptgt__host_entries_begin, { i8*, i8*, i64 }* @__omptgt__host_entries_end },
// CHK-CODEGEN-HOST: { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* } { i8* @__omptgt__img_start_[[T2]], i8* @__omptgt__img_end_[[T2]],
// CHK-CODEGEN-HOST: { i8*, i8*, i64 }* @__omptgt__host_entries_begin, { i8*, i8*, i64 }* @__omptgt__host_entries_end }]

// Target descriptor consists of the number of entries, s pointer to an array of images
// and the begin and end pointer of the entries range
// CHK-CODEGEN-HOST: @__omptgt__target_regions_descriptor = 
// CHK-CODEGEN-HOST: internal constant { i32, { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* } {
// CHK-CODEGEN-HOST: i32 2, 
// CHK-CODEGEN-HOST: { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }* getelementptr inbounds ([2 x { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }]* @__omptgt__device_images, i32 0, i32 0),
// CHK-CODEGEN-HOST: { i8*, i8*, i64 }* @__omptgt__host_entries_begin, { i8*, i8*, i64 }* @__omptgt__host_entries_end }


// The register lib caller is implemented as a global constructor
// CHK-CODEGEN-HOST: @llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] 
// CHK-CODEGEN-HOST: [{ i32, void ()*, i8* } { i32 0, void ()* @_GLOBAL__A_000000_OPENMP_TGT, i8* null }]


// Target functions
#pragma omp declare target
// CHK-CODEGEN-HOST:    define signext i32 @tdouble
// CHK-CODEGEN-TARGET1: define signext i32 @tdouble
// CHK-CODEGEN-TARGET2: define i32 @tdouble

int tdouble(int a){
  return 2*a;
}
// CHK-CODEGEN-HOST: define signext i32 @tsquare
// CHK-CODEGEN-TARGET1: define signext i32 @tsquare
// CHK-CODEGEN-TARGET2: define i32 @tsquare

int tsquare(int a){
  return a*a;
}
// CHK-CODEGEN-HOST: define signext i32 @tadd
// CHK-CODEGEN-TARGET1: define signext i32 @tadd
// CHK-CODEGEN-TARGET2: define i32 @tadd

int tadd(int a){
  return a+a;
}

int GV = 123;

#pragma omp end declare target

int foo(int a){
  // CHK-CODEGEN-HOST-LABEL: define signext i32 @foo
  int i;
  
  // Start codegen for 1st target region
  // Fill the base pointers, pointers and size arrays
  // CHK-CODEGEN-HOST:  %[[BP1:[a-zA-Z0-9_\.]+]] = alloca i8*
  // CHK-CODEGEN-HOST:  %[[P1:[a-zA-Z0-9_\.]+]] = alloca i8*
  // CHK-CODEGEN-HOST:  %[[S1:[a-zA-Z0-9_\.]+]] = alloca i64
  // CHK-CODEGEN-HOST:  %[[EBP1:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i8** %[[BP1]], i32 0
  // CHK-CODEGEN-HOST:  %[[EP1:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i8** %[[P1]], i32 0
  // CHK-CODEGEN-HOST:  %[[ES1:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i64* %[[S1]], i32 0
  // CHK-CODEGEN-HOST:  store i8* {{.*}}, i8** %[[EBP1]]
  // CHK-CODEGEN-HOST:  store i8* {{.*}}, i8** %[[EP1]]
  // CHK-CODEGEN-HOST:  store i64 4, i64* %[[ES1]]
  
  // Transfer execution to the target
  // CHK-CODEGEN-HOST:  %[[FAILOFF1:[a-zA-Z0-9_\.]+]] = call i32 @__tgt_target(
  // Device ID
  // CHK-CODEGEN-HOST:  i32 -1,
  // CHK-CODEGEN-HOST:  i8* bitcast (void (i32*)* @__omptgt__0_[[ID]]_ to i8*),
  // Num of arguments for the target region
  // CHK-CODEGEN-HOST:  i32 1, 
  // CHK-CODEGEN-HOST:  i8** %[[BP1]], i8** %[[P1]], i64* %[[S1]],
  // CHK-CODEGEN-HOST:  i32* getelementptr inbounds ([1 x i32]* [[M1]], i32 0, i32 0))
  
  // Call host version if offloading failed
  // CHK-CODEGEN-HOST:  %[[CMP1:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[FAILOFF1]], 0
  // CHK-CODEGEN-HOST:  br i1 %[[CMP1]], label %{{.*}}, label %[[FAILOFFLAB1:[a-zA-Z0-9_\.]+]]
  // CHK-CODEGEN-HOST:  {{:?}}[[FAILOFFLAB1]]{{:?}}
  // CHK-CODEGEN-HOST:  call void @__omptgt__0_[[ID]]_(i32*

  // CHK-CODEGEN-TARGET1: define void @__omptgt__0_[[ID:[0-9a-f]+_[0-9a-f]+]]_
  // CHK-CODEGEN-TARGET2: define void @__omptgt__0_[[ID:[0-9a-f]+_[0-9a-f]+]]_
#pragma omp target
  {
    GV = a += tsquare(a);
  }

  for(i=0; i<5; ++i){
    // Start codegen for 2nd target region
    // Fill the base pointers, pointers and size arrays
    // CHK-CODEGEN-HOST:  %[[BP2:[a-zA-Z0-9_\.]+]] = alloca i8*
    // CHK-CODEGEN-HOST:  %[[P2:[a-zA-Z0-9_\.]+]] = alloca i8*
    // CHK-CODEGEN-HOST:  %[[S2:[a-zA-Z0-9_\.]+]] = alloca i64
    // CHK-CODEGEN-HOST:  %[[EBP2:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i8** %[[BP2]], i32 0
    // CHK-CODEGEN-HOST:  %[[EP2:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i8** %[[P2]], i32 0
    // CHK-CODEGEN-HOST:  %[[ES2:[a-zA-Z0-9_\.]+]] = getelementptr inbounds i64* %[[S2]], i32 0
    // CHK-CODEGEN-HOST:  store i8* {{.*}}, i8** %[[EBP2]]
    // CHK-CODEGEN-HOST:  store i8* {{.*}}, i8** %[[EP2]]
    // CHK-CODEGEN-HOST:  store i64 4, i64* %[[ES2]]
  
    // Transfer execution to the target
    // CHK-CODEGEN-HOST:  %[[FAILOFF2:[a-zA-Z0-9_\.]+]] = call i32 @__tgt_target(
    // Device ID
    // CHK-CODEGEN-HOST:  i32 -1,
    // CHK-CODEGEN-HOST:  i8* bitcast (void (i32*)* @__omptgt__1_[[ID]]_ to i8*),
    // Num of arguments for the target region
    // CHK-CODEGEN-HOST:  i32 1, 
    // CHK-CODEGEN-HOST:  i8** %[[BP2]], i8** %[[P2]], i64* %[[S2]],
    // CHK-CODEGEN-HOST:  i32* getelementptr inbounds ([1 x i32]* [[M2]], i32 0, i32 0))
  
    // Call host version if offloading failed
    // CHK-CODEGEN-HOST:  %[[CMP2:[a-zA-Z0-9_\.]+]] = icmp eq i32 %[[FAILOFF2]], 0
    // CHK-CODEGEN-HOST:  br i1 %[[CMP2]], label %{{.*}}, label %[[FAILOFFLAB2:[a-zA-Z0-9_\.]+]]
    // CHK-CODEGEN-HOST:  {{:?}}[[FAILOFFLAB2]]{{:?}}
    // CHK-CODEGEN-HOST:  call void @__omptgt__1_[[ID]]_(i32*

    // CHK-CODEGEN-TARGET1: define void @__omptgt__1_[[ID]]_
    // CHK-CODEGEN-TARGET2: define void @__omptgt__1_[[ID]]_
#pragma omp target
    {
      a -= tdouble(a) + tadd(a) + GV;
    }
  }

  return a;
}

int main(int argc, char *argv[]){
  return foo(argc);
}

// The target descriptor registration happens in a global initializer
// CHK-CODEGEN-HOST-LABEL: @__omptgt__register_lib()
// CHK-CODEGEN-HOST: section ".text.startup"
// CHK-CODEGEN-HOST: call void @__tgt_register_lib({ i32, { i8*, i8*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }*, { i8*, i8*, i64 }*, { i8*, i8*, i64 }* }* @__omptgt__target_regions_descriptor)
  

// CHK-CODEGEN-HOST-LABEL: @_GLOBAL__A_000000_OPENMP_TGT() 
// CHK-CODEGEN-HOST: section ".text.startup"
// CHK-CODEGEN-HOST: call void @__omptgt__register_lib()

