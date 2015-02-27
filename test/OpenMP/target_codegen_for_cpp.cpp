///
/// Perform several offloading codegen tests for C++
///

///##############################################
///
/// Test using member ctors/dtors and routines
///
///##############################################
#ifdef TT1
// RUN:   %clangxx -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT1 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK1 -input-file=target_codegen_for_cpp.ll %s
// RUN:   FileCheck -check-prefix=CK1-TGT -input-file=target_codegen_for_cpp.ll.tgt-nvptx64sm_35-nvidia-cuda %s

// Make sure we get the right order of target globals and destructors. Usually we will get
// - the global
// - the name of the global 
// - the corresponding openmp entry
// - the dtor name (if any)
// - the correspondent dtor openmp entry (if any)

// CK1: @C = global
// CK1-TGT: @C = global
// CK1: @__omptgt__gbl__[[G0:[0-9]+]]_[[ID:[0-9a-f]+_[0-9a-f]+]]__entry_name = internal constant [2 x i8] c"C\00"
// CK1: @__omptgt__gbl__[[G0]]_[[ID]]__entry

// CK1: @D = global
// CK1-TGT: @D = global
// CK1: @__omptgt__gbl__[[G1:[0-9]+]]_[[ID]]__entry_name = internal constant [2 x i8] c"D\00"
// CK1: @__omptgt__gbl__[[G1]]_[[ID]]__entry

// CK1: @YC = global
// CK1-TGT: @YC = global
// CK1: @__omptgt__gbl__[[G2:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"YC\00"
// CK1: @__omptgt__gbl__[[G2]]_[[ID]]__entry
// CK1: @__omptgt__[[E0:[0-9]+]]_[[ID]]__entry_name
// CK1: @__omptgt__[[E0]]_[[ID]]__entry = constant { i8*, i8*, i64 } { i8* bitcast (void (i8*)* @__omptgt__dtor_caller{{.*}} to i8*)

// CK1: @YD = global
// CK1-TGT: @YD = global
// CK1: @__omptgt__gbl__[[G3:[0-9]+]]_[[ID]]__entry_name = internal constant [3 x i8] c"YD\00"
// CK1: @__omptgt__gbl__[[G3]]_[[ID]]__entry = 
// CK1: @__omptgt__[[E1:[0-9]+]]_[[ID]]__entry_name
// CK1: @__omptgt__[[E1]]_[[ID]]__entry = constant { i8*, i8*, i64 } { i8* bitcast (void (i8*)* @__omptgt__dtor_caller{{.*}} to i8*)

// Check entry that calls the global initializers
// CK1: @__omptgt__{{[0-9]+}}_[[ID]]__entry = constant { i8*, i8*, i64 } { i8* bitcast (void ()* @_GLOBAL__sub_I_target_codegen_for_cpp{{.*}} to i8*)

// Check that global ctors have the the target registraton and global initializers
// CK1: @llvm.global_ctors = appending global [2 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } 
// CK1: { i32 0, void ()* @_GLOBAL__A_000000_OPENMP_TGT, i8* null }
// CK1: { i32 65535, void ()* @_GLOBAL__sub_I_target_codegen_for_cpp{{.*}}, i8* null }

#define N 64
#pragma omp declare target

struct MyClass{
  int A[N];
  int B[N];

  MyClass &add(MyClass &op){
    for (int i=0; i<N; ++i){      
      A[i] += op.A[i];
      B[i] += op.B[i];
    }
    return *this;
  }
  
  MyClass &add(int (&op)[N]){
    for (int i=0; i<N; ++i){      
      A[i] += op[i];
      B[i] += op[i];
    }
    return *this;
  }
  
  bool eq(MyClass &op){
    for (int i=0; i<N; ++i)
      if (op.A[i] != A[i] || op.B[i] != B[i])
        return false;
        
    return true;
  }

  MyClass() {
    for (int i=0; i<N; ++i){      
      A[i] = i;
      B[i] = i*1000;
    }
  }
};

// Ctor of C
// CK1: define internal void @__cxx_global_var_init
// CK1: call void @_ZN7MyClassC2Ev({{.*}} @C)
// CK1: define {{.*}} void @_ZN7MyClassC2Ev
MyClass C;

// Ctor of D
// CK1: define internal void @__cxx_global_var_init
// CK1: call void @_ZN7MyClassC2Ev({{.*}} @D)
MyClass D;

struct YourClass{
  MyClass &A;

  YourClass(MyClass &a) : A(a) {}
  ~YourClass() {A.add(A);}
};

// Ctor and Dtor - Dtor is registered to the POSIX call atexit(), one registers the host Dtor the other the target's
// Ctors and Dtors are called with a single team and thread
// CK1: define internal void @__cxx_global_var_init
// CK1: call void @_ZN9YourClassC2ER7MyClass({{.*}} @YC, {{.*}} @C)
// CK1: call i32 @__cxa_atexit({{.*}}@_ZN9YourClassD2Ev{{.*}}@YC
// CK1: call i32 @__cxa_atexit({{.*}}@__omptgt__dtor_caller{{.*}}@YC
// CK1: define {{.*}} void @_ZN9YourClassC2ER7MyClass
// CK1: define {{.*}} void @_ZN9YourClassD2Ev
// CK1: define internal void @__omptgt__dtor_caller
// CK1: call i32 @__tgt_target_teams(i32 -3, {{.*}}@__omptgt__dtor_caller{{.*}}, i32 0, i8** null, i8** null, i64* null, i32* null, i32 1, i32 1)
YourClass YC(C);

// CK1: define internal void @__cxx_global_var_init
// CK1: call void @_ZN9YourClassC2ER7MyClass({{.*}} @YD, {{.*}} @D)
// CK1: call i32 @__cxa_atexit({{.*}}@_ZN9YourClassD2Ev{{.*}}@YD
// CK1: call i32 @__cxa_atexit({{.*}}@__omptgt__dtor_caller{{.*}}@YD
// CK1: define internal void @__omptgt__dtor_caller
// CK1: call i32 @__tgt_target_teams(i32 -3, {{.*}}@__omptgt__dtor_caller{{.*}}, i32 0, i8** null, i8** null, i64* null, i32* null, i32 1, i32 1)
YourClass YD(D);


#pragma omp end declare target 	

int ARGC;

// CK1-LABEL: @_Z4testv()
void test(){
  const int n = N;

  int AA[N];
  // The constructors of the local vars have to be called
  // CK1: call void @_ZN7MyClassC2Ev(
  // CK1: call void @_ZN7MyClassC2Ev(
  // CK1: call void @_ZN7MyClassC2Ev(
  MyClass H, T;
  MyClass HC;

  for (int i = 0 ; i < N ; i++){
    AA[i] = ARGC*i;
  }
  
  {
    YourClass HYC(H);
    H.add(AA).add(HC).add(HC).add(C).add(D);
    
// We use three arguments for the target region: T, AA and HC
// CK1: call i32 @__tgt_target(i32 -1, {{.*}}, i32 3,
  
    #pragma omp target
    {
      YourClass TYC(T);
      MyClass TC;
      T.add(AA).add(HC).add(TC).add(C).add(D);
    }
  }
}

// We need to have the add routines defined 
// CK1: define {{.*}} @_ZN7MyClass3addERA64_i
// CK1: define {{.*}} @_ZN7MyClass3addERS_(

// We need to have the functions that register the target  info and to the global initialization 
// registered with the llvm constructors
// CK1: define internal void @_GLOBAL__A_000000_OPENMP_TGT() section ".text.startup" {
// CK1: call void @__omptgt__register_lib()

// CK1: define internal void @_GLOBAL__sub_I_target_codegen_for_cpp{{.*}}() section ".text.startup" {
// CK1: call void @__cxx_global_var_init
// CK1: call void @__cxx_global_var_init1
// CK1: call void @__cxx_global_var_init2
// CK1: call void @__cxx_global_var_init3
// CK1: call i32 @__tgt_target_teams(i32 -2, {{.*}}, i32 0, i8** null, i8** null, i64* null, i32* null, i32 1, i32 1)

// Target also needs to call all the initializers
// CK1-TGT: define internal void @__omptgt__[[E0:[0-9]+]]_[[ID:[0-9a-f]+_[0-9a-f]+]]_() {
// CK1-TGT: call void @__cxx_global_var_init()
// CK1-TGT: call void @__cxx_global_var_init1()
// CK1-TGT: call void @__cxx_global_var_init2()
// CK1-TGT: call void @__cxx_global_var_init3()

#endif

///##############################################
///
/// Example where target regions are used inside
/// template and static functions
///
///##############################################
#ifdef TT2
// RUN:   %clangxx -fopenmp -target powerpc64le-ibm-linux-gnu -omptargets=nvptx64sm_35-nvidia-cuda \
// RUN:   -DTT2 -O0 -S -emit-llvm %s 2>&1
// RUN:   FileCheck -check-prefix=CK2 -input-file=target_codegen_for_cpp.ll %s

template <typename T>
T foo ( T a)
{
  T b;

  #pragma omp target
  {
    b = a+a;
  }
  
  return b;
}

static int bar(int a){
  int b;
  
  #pragma omp target
  {
    b = a+a;
  }
  
  return b;
}

// CH2-LABEL :@_Z4testi
int test (int a)
{
  // template and static function are generated when they are used.
  // all functions must launch a target region.
  //CK2: define {{.*}} i32 @_Z3fooIiET_S0_(i32
  //CK2: call i32 @__tgt_target
  a = foo<int>(a);

  double b = (double)a;

  //CK2: define {{.*}} double @_Z3fooIdET_S0_(double %a
  //CK2: call i32 @__tgt_target
  b = foo<double>(b);

  a = (int)b;

  //CK2: define {{.*}} i32 @_ZL3bari(i32
  //CK2: call i32 @__tgt_target
  a = bar(a);

  return a;
}

#endif

