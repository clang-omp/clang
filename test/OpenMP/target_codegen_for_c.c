///
/// Perform several offloading codegen tests
///

///##############################################
///
/// Test where map clause uses array sections
///
///##############################################
#ifdef TT1
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT1 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK1 -input-file=target_codegen_for_c.ll %s

// The map types in #target data are 'to' for B and 'from' for A
// CK1:     [[MT1:@.mapped_types[0-9]*]] = private constant [2 x i32] [
// CK1-DAG: i32 1
// CK1-DAG: i32 2
// CK1: ]
// the map types in #target are 'alloc' for A and B (they were mapped before) and 'to' for C
// CK1:     [[MT2:@.tgt_types[0-9]*]] = private constant [3 x i32] [
// CK1-DAG: i32 0
// CK1-DAG: i32 0
// CK1-DAG: i32 1
// CK1: ]

// CK1-LABEL: @T1
void T1(){

  double A[32];
  double B[32]; 
  double C[32];
  
  // Base pointer, start and end address of B map
  // CK1:     [[BB:%.*]] = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 0
  // CK1:     [[SB:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 0
  // CK1:     [[ETB:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 31
  // CK1:     [[EB:%.*]] = getelementptr inbounds double* [[ETB]], i64 1
  // CK1:     [[SBINT:%.*]] = ptrtoint double* [[SB]] to i64
  // CK1:     [[EBINT:%.*]] = ptrtoint double* [[EB]] to i64
  // CK1:     [[SIZEB:%.*]] = sub i64 [[EBINT]], [[SBINT]]
  
  // Base pointer, start and end address of A map
  // CK1:     [[BA:%.*]] = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 0
  // CK1:     [[SA:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 2
  // CK1:     [[ETA:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 31
  // CK1:     [[EA:%.*]] = getelementptr inbounds double* [[ETA]], i64 1
  // CK1:     [[SAINT:%.*]] = ptrtoint double* [[SA]] to i64
  // CK1:     [[EAINT:%.*]] = ptrtoint double* [[EA]] to i64
  // CK1:     [[SIZEA:%.*]] = sub i64 [[EAINT]], [[SAINT]]
  
  // Store the pointers and sizes in the arrays passed to the runtime in order to create
  // the data environment
  // CK1:     [[BPTRS:%.*]] = alloca i8*, i32 2
  // CK1:     [[PPTRS:%.*]] = alloca i8*, i32 2
  // CK1:     [[SIZES:%.*]] = alloca i64, i32 2
  // CK1:     [[EBPTRS0:%.*]] = getelementptr inbounds i8** [[BPTRS]], i32 0
  // CK1:     [[EPPTRS0:%.*]] = getelementptr inbounds i8** [[PPTRS]], i32 0
  // CK1:     [[ESIZES0:%.*]] = getelementptr inbounds i64* [[SIZES]], i32 0
  // CK1:     store i8* {{.*}}, i8** [[EBPTRS0]]
  // CK1:     store i8* {{.*}}, i8** [[EPPTRS0]]
  // CK1:     store i64 {{.*}}, i64* [[ESIZES0]]
  // CK1:     [[EBPTRS1:%.*]] = getelementptr inbounds i8** [[BPTRS]], i32 1
  // CK1:     [[EPPTRS1:%.*]] = getelementptr inbounds i8** [[PPTRS]], i32 1
  // CK1:     [[ESIZES1:%.*]] = getelementptr inbounds i64* [[SIZES]], i32 1
  // CK1:     store i8* {{.*}}, i8** [[EBPTRS1]]
  // CK1:     store i8* {{.*}}, i8** [[EPPTRS1]]
  // CK1:     store i64 {{.*}}, i64* [[ESIZES1]]

  // Begin data environment
  // CK1:     call void @__tgt_target_data_begin(i32 1, i32 2, i8** [[BPTRS]], i8** [[PPTRS]], i64* [[SIZES]], i32* getelementptr inbounds ([2 x i32]* [[MT1]], i32 0, i32 0))
  
  #pragma omp target data map(to: B[0:32]) map(from: A[2:30]) device(1)
  {
    // Base pointer, start and end address of C map
    // CK1:     [[BC:%.*]] = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 0
    // CK1:     [[SC:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 2
    // CK1:     [[ETC:%.*]]  = getelementptr inbounds [32 x double]* %{{.*}}, i32 0, i64 31
    // CK1:     [[EC:%.*]] = getelementptr inbounds double* [[ETC]], i64 1
    // CK1:     [[SCINT:%.*]] = ptrtoint double* [[SC]] to i64
    // CK1:     [[ECINT:%.*]] = ptrtoint double* [[EC]] to i64
    // CK1:     [[SIZEC:%.*]] = sub i64 [[ECINT]], [[SCINT]]
  
    // Store the pointers and sizes in the arrays passed to the runtime in order to transfer
    // execution to the device
    // CK1:     [[TGT_BPTRS:%.*]] = alloca i8*, i32 3
    // CK1:     [[TGT_PPTRS:%.*]] = alloca i8*, i32 3
    // CK1:     [[TGT_SIZES:%.*]] = alloca i64, i32 3
    // CK1:     [[TGT_EBPTRS0:%.*]] = getelementptr inbounds i8** [[TGT_BPTRS]], i32 0
    // CK1:     [[TGT_EPPTRS0:%.*]] = getelementptr inbounds i8** [[TGT_PPTRS]], i32 0
    // CK1:     [[TGT_ESIZES0:%.*]] = getelementptr inbounds i64* [[TGT_SIZES]], i32 0
    // CK1:     store i8* {{.*}}, i8** [[TGT_EBPTRS0]]
    // CK1:     store i8* {{.*}}, i8** [[TGT_EPPTRS0]]
    // CK1:     store i64 {{.*}}, i64* [[TGT_ESIZES0]]
    // CK1:     [[TGT_EBPTRS1:%.*]] = getelementptr inbounds i8** [[TGT_BPTRS]], i32 1
    // CK1:     [[TGT_EPPTRS1:%.*]] = getelementptr inbounds i8** [[TGT_PPTRS]], i32 1
    // CK1:     [[TGT_ESIZES1:%.*]] = getelementptr inbounds i64* [[TGT_SIZES]], i32 1
    // CK1:     store i8* {{.*}}, i8** [[TGT_EBPTRS1]]
    // CK1:     store i8* {{.*}}, i8** [[TGT_EPPTRS1]]
    // CK1:     store i64 {{.*}}, i64* [[TGT_ESIZES1]]
    // CK1:     [[TGT_EBPTRS2:%.*]] = getelementptr inbounds i8** [[TGT_BPTRS]], i32 2
    // CK1:     [[TGT_EPPTRS2:%.*]] = getelementptr inbounds i8** [[TGT_PPTRS]], i32 2
    // CK1:     [[TGT_ESIZES2:%.*]] = getelementptr inbounds i64* [[TGT_SIZES]], i32 2
    // CK1:     store i8* {{.*}}, i8** [[TGT_EBPTRS2]]
    // CK1:     store i8* {{.*}}, i8** [[TGT_EPPTRS2]]
    // CK1:     store i64 {{.*}}, i64* [[TGT_ESIZES2]]
    
    // Call runtime to launch target execution
    // CK1:     call i32 @__tgt_target(i32 1, i8* {{.*}}, i32 3, i8** [[TGT_BPTRS]], i8** [[TGT_PPTRS]], i64* [[TGT_SIZES]], i32* getelementptr inbounds ([3 x i32]* [[MT2]], i32 0, i32 0))
    #pragma omp target map(to: C[2:30]) device(1)
    for (int i=2; i<32; ++i){
      A[i] = B[i] * C[i];
    }
    
  // Close data environment
  // CK1:     call void @__tgt_target_data_end(i32 1, i32 2, i8** [[BPTRS]], i8** [[PPTRS]], i64* [[SIZES]], i32* getelementptr inbounds ([2 x i32]* [[MT1]], i32 0, i32 0))
  }
  return;
}

#endif

///##############################################
///
/// Test that uses pointers in the map clause
///
///##############################################
#ifdef TT2
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT2 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK2 -input-file=target_codegen_for_c.ll %s

// The map types in #target data are
// to | from          -> &A[0]
// pointer | to | from-> pA
// CK2:     [[MT1:@.mapped_types[0-9]*]] = private constant [2 x i32] [
// CK2-DAG: i32 3
// CK2-DAG: i32 35
// CK2: ]

// the map types in #target are:
// from             -> i
// to | from        -> M
// alloc            -> &A[0] (already mapped)
// alloc            -> pA (already mapped)
// from             -> &B[0]
// pointer | to     -> pB
// CK2:     [[MT2:@.tgt_types[0-9]*]] = private constant [6 x i32] [
// CK2-DAG: i32 2
// CK2-DAG: i32 3
// CK2-DAG: i32 0
// CK2-DAG: i32 0
// CK2-DAG: i32 2
// CK2-DAG: i32 33
// CK2: ]

// CK2-LABEL: @T2
void T2(){
  const int M = 32;
  
  int A[64];
  int B[32];
  int *pA;
  int *pB;
  int i;
        
  #pragma omp target data map(A[0:M], pA[0:M])
  {
    #pragma omp target map(from:B,i) map(to:pB[0:M])
    {
      for (i=0; i<M; ++i){
        ++A[i];
        --pA[i];
        B[i] = pB[i] - A[i]*pA[i];
      }
    }
  }

}

#endif

///##############################################
///
/// Test with multiple declare target regions
///
///##############################################

#ifdef TT3
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT3 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK3 -input-file=target_codegen_for_c.ll %s

// Verify if the order and sizes of the entries is consistent to what we have in the 
// source code

// CK3: @GA = common global [20 x i32] zeroinitializer
// CK3: @__omptgt__gbl__[[G0:[0-9]+]]_[[ID:[0-9a-f]+_[0-9a-f]+]]__entry_name = internal constant [3 x i8] c"GA\00"
// CK3: @__omptgt__gbl__[[G0]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast ([20 x i32]* @GA to i8*), 
// CK3: i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__[[G0]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 80 }, section ".openmptgt_host_entries"

// CK3: @GB = global [10 x i32] [i32 9, i32 8, i32 7, i32 6, i32 5, i32 4, i32 3, i32 2, i32 1, i32 0]
// CK3: @__omptgt__gbl__[[G1:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"GB\00"
// CK3: @__omptgt__gbl__[[G1]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast ([10 x i32]* @GB to i8*), 
// CK3: i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__[[G1]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 40 }, section ".openmptgt_host_entries"

// CK3: @GC = common global i32 0
// CK3: @__omptgt__gbl__[[G2:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"GC\00"
// CK3: @__omptgt__gbl__[[G2]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (i32* @GC to i8*), 
// CK3: i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__[[G2]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 4 }, section ".openmptgt_host_entries"

// CK3: @GD = global i32 5
// CK3: @__omptgt__gbl__[[G3:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"GD\00"
// CK3: @__omptgt__gbl__[[G3]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (i32* @GD to i8*), 
// CK3: i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__[[G3]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 4 }, section ".openmptgt_host_entries"

// CK3: @__omptgt__[[E0:[0-9]+]]_[[ID]]__entry_name = internal constant [{{[1-9][0-9]+}} x i8] c"__omptgt__[[E0]]_[[ID]]_\00"
// CK3: @__omptgt__[[E0]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (void (i32*, [100 x i32]*)* @__omptgt__[[E0]]_[[ID]]_ to i8*), 
// CK3: i8* getelementptr inbounds ([{{[1-9][0-9]+}} x i8]* @__omptgt__[[E0]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 0 }, section ".openmptgt_host_entries"

// CK3: @__omptgt__[[E1:[0-9]+]]_[[ID]]__entry_name = internal constant [{{[1-9][0-9]+}} x i8] c"__omptgt__[[E1]]_[[ID]]_\00"
// CK3: @__omptgt__[[E1]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (void (i32*, [100 x i32]*)* @__omptgt__[[E1]]_[[ID]]_ to i8*), 
// CK3: i8* getelementptr inbounds ([{{[1-9][0-9]+}} x i8]* @__omptgt__[[E1]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 0 }, section ".openmptgt_host_entries"

// CK3: @GE = global i32 6
// CK3: @__omptgt__gbl__[[G4:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"GE\00"
// CK3: @__omptgt__gbl__[[G4]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (i32* @GE to i8*), 
// CK3: i8* getelementptr inbounds ([3 x i8]* @__omptgt__gbl__[[G4]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 4 }, section ".openmptgt_host_entries"

// CK3: @__omptgt__[[E2:[0-9]+]]_[[ID]]__entry_name = internal constant [{{[1-9][0-9]+}} x i8] c"__omptgt__[[E2]]_[[ID]]_\00"
// CK3: @__omptgt__[[E2]]_[[ID]]__entry = constant { i8*, i8*, i64 } {
// CK3: i8* bitcast (void (i32*)* @__omptgt__2_[[ID]]_ to i8*), 
// CK3: i8* getelementptr inbounds ([{{[1-9][0-9]+}} x i8]* @__omptgt__[[E2]]_[[ID]]__entry_name, i32 0, i32 0), 
// CK3: i64 0 }, section ".openmptgt_host_entries"

#pragma omp declare target
int GA[20];
int GB[10] = {9,8,7,6,5,4,3,2,1,0};
int GC;
int GD = 5;
#pragma omp end declare target

void foo();

void bar(){
	int A[100];
	int i;
	
  foo();
	
  #pragma omp target map(A[45:10])
  {
    for (i=45; i<55; ++i){
      A[i] += i * GB[i-45];
      GA[i-45] = 3*A[i];
      GA[i-45+10] = 7*A[i];
      GD -= A[i];
    }  
    GC = GD*5;
  }
  
  foo();
  
  #pragma omp target map(A)
  {
    for (i=0; i<100; ++i)
      A[i] += GA[i%20] + 5*GC + 3*GD;
  }
}

#pragma omp declare target
int GE = 6;
#pragma omp end declare target

void foo(){
  int i;

  #pragma omp target
  {
    ++GE;
    for (i=0; i<20; ++i)
      GA[i] += GE;
  }

}

#endif

///##############################################
///
/// Example that uses host globals inside target
/// regions
///
///##############################################
#ifdef TT4
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT4 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK4 -input-file=target_codegen_for_c.ll %s

// Verify the globals are created without the logic usually codegen for when globals are
// in #declare target

// CK4:     @IF = global i32 0
// CK4-NOT: @__omptgt__gbl__
// CK4:     @GBL1 = global i32 123
// CK4-NOT: @__omptgt__gbl__
// CK4:     @GBL3 = global i32 0
// CK4-NOT: @__omptgt__gbl__
// CK4:     @GBL2 = common global i32 0
// CK4-NOT: @__omptgt__gbl__

int IF=0;
int GBL1=123;
int GBL2;
int GBL3=0;

// We should have 5 values being mapped. The 4 globals and at local.
// FIXME: @IF does not need to be mapped...
// CK4: @.tgt_types = private constant [5 x i32] [i32 3, i32 3, i32 3, i32 3, i32 3]

// CK4-LABEL: @foo 
void foo ()
{
  int at = 0;

  // Make sure the if clause is properly controlled by the global IF
  // CK4: [[IFVAL:%[a-zA-Z0-9_\.]+]] = load i32* @IF, align 4
  // CK4: [[CMP:%[a-zA-Z0-9_\.]+]]  = icmp ne i32 [[IFVAL]], 0
  // CK4: br i1 [[CMP]], label %[[IFTRUE:[a-zA-Z0-9_\.]+]], label %[[IFFALSE:[a-zA-Z0-9_\.]+]]
 
  // Verify the 5 arguments are used by the target region
  // CK4: {{:?}}[[IFTRUE]]{{:?}}
  // CK4: call i32 @__tgt_target(i32 -1,
  // CK4: i8* bitcast (void (i32*, i32*, i32*, i32*, i32*)* @__omptgt__0_[[ID:[0-9a-f]+_[0-9a-f]+]]_ to i8*), 
  // CK4: i32 5, 
  // CK4: i8** %{{[^,]*}}, i8** %{{[^,]*}}, i64* %{{[^,]*}},
  // CK4: i32* getelementptr inbounds ([5 x i32]* @.tgt_types, i32 0, i32 0))
  
  #pragma omp target if (IF!=0)
  {
    ++at;
    at *= GBL1 + GBL2;
    ++GBL3;
  }
  
  // Check the call to the host version in case the offloading failed
  // CK4: call void @__omptgt__0_[[ID]]_(i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}})
  
  // Check the call to the host version in case the if clause value is false
  // CK4: {{:?}}[[IFFALSE]]{{:?}}
  // CK4: call void @__omptgt__0_[[ID]]_(i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}}, i32* %{{[^,]*}})
}


#endif

///##############################################
///
/// Test where mapped arrays have several dimensions
///
///##############################################
#ifdef TT5
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT5 -O0 -S -emit-llvm %s 2>&1 
// RUN:   FileCheck -check-prefix=CK5 -input-file=target_codegen_for_c.ll %s

#define N 64
#define D0 (2)
#define D1 (2)
#define D2 (3)
#define D3 (4)
#define D4 (6*N)

// CK5-LABEL: @foo
void foo ()
{
  int i0, i1, i2, i3, i4, i5, i6;

  double a[D0][D1][D2][D3][D4];
  double b[D0][D1][D2][D3][D4];
  
  // Verify we get the right size for the multi-dimensional arrays in the map
  
  // We have 7 elements being mapped 2 arrays and 5 integers
  // CK5: [[BPTRS:%[a-zA-Z0-9_\.]+]] = alloca i8*, i32 7
  // CK5: [[PPTRS:%[a-zA-Z0-9_\.]+]] = alloca i8*, i32 7
  // CK5: [[SIZES:%[a-zA-Z0-9_\.]+]] = alloca i64, i32 7
  
  // We need to have two maps whose size is 2 x 2 x 3 x 4 x 6xN x sizeof(double) = 147456
  // and 5 maps whose size is sizeof(int)
  // CK5-DAG: [[S1:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 147456, i64* %{{[0-9]+}}
  // CK5-DAG: [[S2:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 147456, i64* %{{[0-9]+}}
  // CK5-DAG: [[S3:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 4, i64* %{{[0-9]+}}
  // CK5-DAG: [[S4:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 4, i64* %{{[0-9]+}}
  // CK5-DAG: [[S5:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 4, i64* %{{[0-9]+}}
  // CK5-DAG: [[S6:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 4, i64* %{{[0-9]+}}
  // CK5-DAG: [[S7:%[0-9]+]] = getelementptr inbounds i64* [[SIZES]], i32 {{[0-9]+}}
  // CK5-DAG: store i64 4, i64* %{{[0-9]+}}


  #pragma omp target map(tofrom:a,b)
  {
    for (i0 = 0 ; i0 < D0 ; i0++)
    for (i1 = 0 ; i1 < D1 ; i1++)
    for (i2 = 0 ; i2 < D2 ; i2++)
    for (i3 = 0 ; i3 < D3 ; i3++)
    for (i4 = 0 ; i4 < D4 ; i4++)
    {
      a[i0][i1][i2][i3][i4] += \
        b[i0][i1][i2][i3][i4];
    }
  }
}

#endif

///##############################################
///
/// Test target update directive
///
///##############################################

#ifdef TT6
// RUN:   %clang -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT6 -O0 -S -emit-llvm %s 2>&1 
// RUN:   FileCheck -check-prefix=CK6 -input-file=target_codegen_for_c.ll %s

#pragma omp declare target
int GA[10] = {9,8,7,6,5,4,3,2,1,0};
int GB[10] = {90,80,70,60,50,40,30,20,10,0};
#pragma omp end declare target

// Update map types is 'to' for GA and 'from' for GB 
// CK6: @.update_types = private constant [2 x i32] [i32 1, i32 2]

// CK6-LABEL: @foo
void foo(){
	int A[10];
	int i;
	
	// 1st target region shouldn't have any arguments
	// CK6: call i32 @__tgt_target(i32 -1, i8* {{[^,]*}}, i32 0, i8** null, i8** null, i64* null, i32* null)
  // CK6: call void @__omptgt__0_[[ID:[0-9a-f]+_[0-9a-f]+]]_()
 
	#pragma omp target
  {
    GA[0] = 90;
    GA[1] = 80;
    GA[2] = 70;
  }

  GA[0] = 900;
  GA[1] = 800;
  GA[2] = 700;

  GB[0] = 9000;
  GB[1] = 8000;
  GB[2] = 7000;

  // Verify information passed to the runtime
  // CK6: [[BPTRS:%.*]] = alloca i8*, i32 2
  // CK6: [[PPTRS:%.*]] = alloca i8*, i32 2
  // CK6: [[SIZES:%.*]] = alloca i64, i32 2

  // CK6: [[B1:%[0-9]+]] = getelementptr inbounds i8** [[BPTRS]], i32 0
  // CK6: [[P1:%[0-9]+]] = getelementptr inbounds i8** [[PPTRS]], i32 0
  // CK6: store i8* bitcast ([10 x i32]* @GA to i8*), i8** [[B1]]
  // CK6: store i8* bitcast ([10 x i32]* @GA to i8*), i8** [[P1]]
  
  // CK6: [[B2:%[0-9]+]] = getelementptr inbounds i8** [[BPTRS]], i32 1
  // CK6: [[P2:%[0-9]+]] = getelementptr inbounds i8** [[PPTRS]], i32 1
  // CK6: store i8* bitcast ([10 x i32]* @GB to i8*), i8** [[B2]]
  // CK6: store i8* bitcast (i32* getelementptr inbounds ([10 x i32]* @GB, i32 0, i64 1) to i8*), i8** [[P2]]
  
  // CK6: call void @__tgt_target_data_update(i32 -1, i32 2, i8** [[BPTRS]], i8** [[PPTRS]], i64* [[SIZES]], i32* getelementptr inbounds ([2 x i32]* @.update_types, i32 0, i32 0))
  
  #pragma omp target update to(GA[0:1]) from(GB[1:1])
  
  // Second target region has two arguments: A and i
  // CK6: call i32 @__tgt_target(i32 -1, i8* {{.*}}, i32 2, i8** {{[^,]*}}, i8** {{[^,]*}}, i64* {{[^,]*}}, i32* getelementptr inbounds ([2 x i32]* @.tgt_types, i32 0, i32 0))
  // CK6: call void @__omptgt__1_[[ID]]_({{[^,]*}}, {{[^,]*}})
  #pragma omp target
  {
    for (i=0; i<10; ++i)
      A[i] = GA[i];
  }

}
#endif
