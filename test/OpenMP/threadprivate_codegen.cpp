// RUN: %clang_cc1 -triple x86_64-apple-macos10.7.0 -verify -fopenmp -emit-llvm -o - %s | FileCheck %s
// expected-no-diagnostics

// CHECK: gs = internal global %class.S zeroinitializer
// CHECK: gs1 = internal global %class.S zeroinitializer
// CHECK: gs.cache. = common global i8** null
// CHECK: arr_x = global [5 x [10 x %class.S]] zeroinitializer
// CHECK: arr_x.cache. = common global i8** null
// CHECK: s1 = internal global %class.S zeroinitializer
// CHECK: s1.cache. = common global i8** null

class S{
  public:
  int a;
  S() : a(0) {}
  S(int a) : a(a) {}
  S(const S& s) {a = 12 + s.a;}
};

static S gs(5), gs1(5);
#pragma omp threadprivate(gs)
// CHECK: define internal i8* @__kmpc_ctor_{{[a-zA-Z0-9_]+}}gs(i8*)
// CHECK: %.addr = alloca i8*
// CHECK: store i8* %0, i8** %.addr
// CHECK: %{{.*}} = bitcast i8* %0 to %class.S*
// CHECK: call void @{{[a-zA-Z0-9_]+}}(%class.S* %{{.*}}, i32 5)
// CHECK: ret i8* %0
// CHECK: define internal void @__omp_threadprivate_{{[a-zA-Z0-9_]+}}gs()
// CHECK: %retval = alloca i8*
// CHECK: call void @__kmpc_threadprivate_register({ i32, i32, i32, i32, i8* }* %.__kmpc_ident_t.2., i8* bitcast (%class.S* @{{[a-zA-Z0-9_]+}}gs to i8*), i8* (i8*)* @__kmpc_ctor_{{[a-zA-Z0-9_]+}}gs, i8* (i8*, i8*)* null, i8* (i8*)* null)
// CHECK: ret void
S arr_x[5][10];
#pragma omp threadprivate(arr_x)
// CHECK: define internal i8* @__kmpc_ctor_{{.+}}arr_x(i8*)
// CHECK: %.addr = alloca i8*
// CHECK: store i8* %0, i8** %.addr
// CHECK: {{.*}} = bitcast i8* %0 to {{.*}}%class.S]]*
// CHECK: call void @{{.+}}(%class.S*
// CHECK: ret i8* %0
// CHECK: define internal void @__omp_threadprivate_vec_{{.*}}arr_x()
// CHECK: %{{.*}} = alloca i8*
// CHECK: call void @__kmpc_threadprivate_register({ i32, i32, i32, i32, i8* }* %.__kmpc_ident_t.2., i8* bitcast ([5 x [10 x %class.S]]* @{{[a-zA-Z0-9_]*}}arr_x to i8*), i8* (i8*)* @__kmpc_ctor_{{[a-zA-Z0-9_]*}}arr_x, i8* (i8*, i8*)* null, i8* (i8*)* null)
// CHECK: ret void

// CHECK: define i32 @main(i32 %argc, i8** %argv)
int main(int argc, char **argv) {
  static S s1(gs.a);
// CHECK: %{{[0-9]+}} = call i32 @__kmpc_global_thread_num({ i32, i32, i32, i32, i8* }* %.__kmpc_ident_t.2.)
// CHECK-NEXT: store i32 %{{[0-9]+}}, i32* %.__kmpc_global_thread_num.
// CHECK: call i32 @__cxa_guard_acquire
// CHECK: = call i8* @__kmpc_threadprivate_cached({ i32, i32, i32, i32, i8* }* %.__kmpc_ident_t.2., i32 %.gtid., i8* bitcast (%class.S* @{{[a-zA-Z0-9_]+}}gs to i8*), i32 4, i8*** @{{[a-zA-Z0-9_]+}}gs.cache.)
// CHECK: call void @__cxa_guard_release
  #pragma omp threadprivate(s1)
// CHECK: = call i8* @__kmpc_threadprivate_cached({ i32, i32, i32, i32, i8* }* %.__kmpc_ident_t.2., i32 %.gtid.1, i8* bitcast (%class.S* @{{[a-zA-Z0-9_]+}}s1 to i8*), i32 4, i8*** @{{[a-zA-Z0-9_]+}}s1.cache.)
  return s1.a;
}
// CHECK: }
// CHECK: define internal void @_GLOBAL__I_a()
// CHECK: call void @__cxx_global_var_init()
// CHECK: call void @__cxx_global_var_init1()
// CHECK: call void @__omp_threadprivate_{{[a-zA-Z0-9_]+}}gs()
// CHECK: call void @__cxx_global_var_init2()
// CHECK: call void @__omp_threadprivate_vec_arr_x()
// CHECK: ret void

