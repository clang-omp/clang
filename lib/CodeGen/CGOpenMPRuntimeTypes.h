//===- CGOpenMPRuntimeTypes.h - Interface to OpenMP Runtime Types - C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This provides the set of types used by the OpenMP runtime
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_CODEGEN_OPENMPRUNTIMETYPES_H
#define CLANG_CODEGEN_OPENMPRUNTIMETYPES_H

#include "CodeGenModule.h"
#include "CodeGenFunction.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"

namespace {
struct ident_t {};
enum sched_type {};
enum kmp_proc_bind_t {};
enum target_size_t {};
enum target_intptr_t {};
typedef void (*kmpc_micro)(int32_t *global_tid, int32_t *bound_tid, ...);
typedef void(__kmpc_fork_call)(ident_t *loc, int32_t argc, kmpc_micro microtask,
                               void *);
typedef void(__kmpc_push_num_threads)(ident_t *loc, int32_t global_tid,
                                      int32_t num_threads);
typedef void(__kmpc_push_proc_bind)(ident_t *loc, int32_t global_tid,
                                    kmp_proc_bind_t proc_bind);
typedef void(__kmpc_push_num_teams)(ident_t *loc, int32_t global_tid,
                                    int32_t num_teams, int32_t num_threads);
typedef void(__kmpc_fork_teams)(ident_t *loc, int32_t argc,
                                kmpc_micro microtask, ...);
// const int KMP_PROC_BIND_FALSE = 0;
// const int KMP_PROC_BIND_TRUE = 1;
const int KMP_PROC_BIND_MASTER = 2;
const int KMP_PROC_BIND_CLOSE = 3;
const int KMP_PROC_BIND_SPREAD = 4;
// const int KMP_PROC_BIND_DISABLED = 5;
// const int KMP_PROC_BIND_INTEL = 6;
// const int KMP_PROC_BIND_DEFAULT = 7;
const int KMP_IDENT_BARRIER_EXPL = 0x20;
const int KMP_IDENT_BARRIER_IMPL = 0x0040;
const int KMP_IDENT_BARRIER_IMPL_FOR = 0x0040;
const int KMP_IDENT_BARRIER_IMPL_SECTIONS = 0x00C0;
const int KMP_IDENT_BARRIER_IMPL_SINGLE = 0x0140;
typedef int32_t(__kmpc_cancel_barrier)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_barrier)(ident_t *loc, int32_t global_tid);
const int KMP_CANCEL_NOREQ = 0;
const int KMP_CANCEL_PARALLEL = 1;
const int KMP_CANCEL_LOOP = 2;
const int KMP_CANCEL_SECTIONS = 3;
const int KMP_CANCEL_TASKGROUP = 4;
typedef int32_t(__kmpc_cancellationpoint)(ident_t *loc, int32_t global_tid,
                                          int32_t cncl_kind);
typedef int32_t(__kmpc_cancel)(ident_t *loc, int32_t global_tid,
                               int32_t cncl_kind);
typedef int32_t kmp_critical_name[8];
typedef int32_t(__kmpc_omp_taskyield)(ident_t *loc, int32_t global_tid,
                                      int32_t end_part);
typedef int32_t(__kmpc_omp_taskwait)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_flush)(ident_t *loc, ...);
typedef int32_t(__kmpc_master)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_end_master)(ident_t *loc, int32_t global_tid);
typedef int32_t(__kmpc_single)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_end_single)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_critical)(ident_t *loc, int32_t global_tid,
                              kmp_critical_name *lck);
typedef void(__kmpc_end_critical)(ident_t *loc, int32_t global_tid,
                                  kmp_critical_name *lck);
typedef void(__kmpc_ordered)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_end_ordered)(ident_t *loc, int32_t global_tid);
typedef void (*kmp_copy_func)(void *lhs_data, void *rhs_data);
typedef void(__kmpc_copyprivate)(ident_t *loc, int32_t global_tid,
                                 target_size_t cpy_size, void *cpy_data,
                                 kmp_copy_func cpy_func, int32_t didit);
typedef void (*kmp_reduce_func)(void *lhs_data, void *rhs_data);
typedef int32_t(__kmpc_reduce_nowait)(ident_t *loc, int32_t global_tid,
                                      int32_t num_vars,
                                      target_size_t reduce_size,
                                      void *reduce_data,
                                      kmp_reduce_func reduce_func,
                                      kmp_critical_name *lck);
typedef void(__kmpc_end_reduce_nowait)(ident_t *loc, int32_t global_tid,
                                       kmp_critical_name *lck);
typedef int32_t(__kmpc_reduce)(ident_t *loc, int32_t global_tid,
                               int32_t num_vars, target_size_t reduce_size,
                               void *reduce_data, kmp_reduce_func reduce_func,
                               kmp_critical_name *lck);
typedef void(__kmpc_end_reduce)(ident_t *loc, int32_t global_tid,
                                kmp_critical_name *lck);
const int KMP_IDENT_ATOMIC_REDUCE = 0x10;
typedef void(__kmpc_atomic_start)();
typedef void(__kmpc_atomic_end)();
typedef void(__kmpc_dispatch_init_4)(ident_t *loc, int32_t global_tid,
                                     sched_type schedule, int32_t lb,
                                     int32_t ub, int32_t st, int32_t chunk);
typedef void(__kmpc_dispatch_init_4u)(ident_t *loc, int32_t global_tid,
                                      sched_type schedule, uint32_t lb,
                                      uint32_t ub, uint32_t st, uint32_t chunk);
typedef void(__kmpc_dispatch_init_8)(ident_t *loc, int32_t global_tid,
                                     sched_type schedule, int64_t lb,
                                     int64_t ub, int64_t st, int64_t chunk);
typedef void(__kmpc_dispatch_init_8u)(ident_t *loc, int32_t global_tid,
                                      sched_type schedule, uint64_t lb,
                                      uint64_t ub, uint64_t st, uint64_t chunk);
typedef int(__kmpc_dispatch_next_4)(ident_t *loc, int32_t global_tid,
                                    int32_t *p_last, int32_t *p_lb,
                                    int32_t *p_ub, int32_t *p_st);
typedef int(__kmpc_dispatch_next_4u)(ident_t *loc, int32_t global_tid,
                                     int32_t *p_last, uint32_t *p_lb,
                                     uint32_t *p_ub, int32_t *p_st);
typedef int(__kmpc_dispatch_next_8)(ident_t *loc, int32_t global_tid,
                                    int32_t *p_last, int64_t *p_lb,
                                    int64_t *p_ub, int64_t *p_st);
typedef int(__kmpc_dispatch_next_8u)(ident_t *loc, int32_t global_tid,
                                     int32_t *p_last, uint64_t *p_lb,
                                     uint64_t *p_ub, int64_t *p_st);
typedef void(__kmpc_dispatch_fini_4)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_dispatch_fini_4u)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_dispatch_fini_8)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_dispatch_fini_8u)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_for_static_init_4)(ident_t *loc, int32_t global_tid,
                                       int32_t schedule, int32_t *pliter,
                                       int32_t *plb, int32_t *pub, int32_t *pst,
                                       int32_t incr, int32_t chunk);
typedef void(__kmpc_for_static_init_4u)(ident_t *loc, int32_t global_tid,
                                        int32_t schedule, int32_t *pliter,
                                        int32_t *plb, int32_t *pub,
                                        int32_t *pst, int32_t incr,
                                        int32_t chunk);
typedef void(__kmpc_for_static_init_8)(ident_t *loc, int32_t global_tid,
                                       int32_t schedule, int32_t *pliter,
                                       int64_t *plb, int64_t *pub, int64_t *pst,
                                       int64_t incr, int64_t chunk);
typedef void(__kmpc_for_static_init_8u)(ident_t *loc, int32_t global_tid,
                                        int32_t schedule, int32_t *pliter,
                                        int64_t *plb, int64_t *pub,
                                        int64_t *pst, int64_t incr,
                                        int64_t chunk);
typedef void(__kmpc_for_static_fini)(ident_t *loc, int32_t global_tid);
const int KMP_SCH_STATIC_CHUNKED = 33;
const int KMP_SCH_STATIC = 34;
const int KMP_SCH_DYNAMIC_CHUNKED = 35;
const int KMP_SCH_GUIDED_CHUNKED = 36;
const int KMP_SCH_RUNTIME = 37;
const int KMP_SCH_AUTO = 38;
const int KMP_ORD_STATIC_CHUNKED = 65;
// const int KMP_ORD_STATIC = 66;
// const int KMP_ORD_DYNAMIC_CHUNKED = 67;
// const int KMP_ORD_GUIDED_CHUNKED = 68;
// const int KMP_ORD_RUNTIME = 69;
// const int KMP_ORD_AUTO = 70;
const int KMP_NM_STATIC_CHUNKED = 161;
// const int KMP_NM_STATIC = 162;
// const int KMP_NM_DYNAMIC_CHUNKED = 163;
// const int KMP_NM_GUIDED_CHUNKED = 164;
// const int KMP_NM_RUNTIME = 165;
// const int KMP_NM_AUTO = 166;
const int KMP_NM_ORD_STATIC_CHUNKED = 193;
// const int KMP_NM_ORD_STATIC = 194;
// const int KMP_NM_ORD_DYNAMIC_CHUNKED = 195;
// const int KMP_NM_ORD_GUIDED_CHUNKED = 196;
// const int KMP_NM_ORD_RUNTIME = 197;
// const int KMP_NM_ORD_AUTO = 198;
const int KMP_SCH_DEFAULT = KMP_SCH_STATIC;
const int SCH_ORD = KMP_ORD_STATIC_CHUNKED - KMP_SCH_STATIC_CHUNKED;
const int SCH_NM = KMP_NM_STATIC_CHUNKED - KMP_SCH_STATIC_CHUNKED;
const int SCH_NM_ORD = KMP_NM_ORD_STATIC_CHUNKED - KMP_SCH_STATIC_CHUNKED;
const int KMP_SCH_DISTRIBUTE_STATIC_CHUNKED = 91;
const int KMP_SCH_DISTRIBUTE_STATIC = 92;
typedef int32_t (*kmp_routine_entry_t)(int32_t, void *);
struct kmp_task_t {};
const int OMP_TASK_UNTIED = 0;
const int OMP_TASK_TIED = 1;
const int OMP_TASK_FINAL = 2;
const int OMP_TASK_DESTRUCTORS_THUNK = 8;
const int OMP_TASK_CURRENT_QUEUED = 1;
struct kmp_depend_info_t {};
const unsigned char IN = 1;
const unsigned char OUT = 2;
const unsigned char INOUT = 3;
typedef int32_t(__kmpc_omp_task_with_deps)(ident_t *loc, int32_t gtid,
                                           kmp_task_t *task, int32_t ndeps,
                                           kmp_depend_info_t *dep_list,
                                           int32_t ndeps_noalias,
                                           kmp_depend_info_t *noalias_dep_list);
typedef void(__kmpc_omp_wait_deps)(ident_t *loc, int32_t gtid, int32_t ndeps,
                                   kmp_depend_info_t *dep_list,
                                   int32_t ndeps_noalias,
                                   kmp_depend_info_t *noalias_dep_list);
typedef kmp_task_t *(__kmpc_omp_task_alloc)(ident_t *loc, int32_t gtid,
                                            int32_t flags,
                                            target_size_t sizeof_kmp_task_t,
                                            target_size_t sizeof_shareds,
                                            kmp_routine_entry_t task_entry);
typedef void(__kmpc_omp_task_begin_if0)(ident_t *loc, int32_t gtid,
                                        kmp_task_t *task);
typedef void(__kmpc_omp_task_complete_if0)(ident_t *loc, int32_t gtid,
                                           kmp_task_t *task);
typedef int32_t(__kmpc_omp_task_parts)(ident_t *loc, int32_t gtid,
                                       kmp_task_t *task);
typedef void(__kmpc_taskgroup)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_end_taskgroup)(ident_t *loc, int32_t global_tid);

// OMP runtime functions used within clang
typedef int32_t(omp_get_num_threads)();
typedef int32_t(omp_get_num_teams)();


// Target-NVPTX specific functions, we may need to move these elsewhere
typedef void(__kmpc_kernel_init)();
typedef int(__kmpc_kernel_prepare_parallel)();
typedef void(__kmpc_kernel_parallel)();
typedef void(__kmpc_kernel_end_parallel)();
typedef void(__kmpc_serialized_parallel)(ident_t *loc, int32_t global_tid);
typedef void(__kmpc_end_serialized_parallel)(ident_t *loc, int32_t global_tid);

//struct target_size_t {};
typedef void *(*kmpc_ctor)(void *);
typedef void *(*kmpc_cctor)(void *, void *);
typedef void *(*kmpc_dtor)(void *);
typedef void(__kmpc_threadprivate_register)(ident_t *loc, void *data,
                                            kmpc_ctor ctor, kmpc_cctor cctor,
                                            kmpc_dtor dtor);
typedef int32_t(__kmpc_global_thread_num)(ident_t *loc);
typedef void *(__kmpc_threadprivate_cached)(ident_t *loc, int32_t global_tid,
                                            void *data, target_size_t size,
                                            void ***cache);

const unsigned OMP_TGT_MAPTYPE_ALLOC   = 0x0000;
const unsigned OMP_TGT_MAPTYPE_TO      = 0x0001;
const unsigned OMP_TGT_MAPTYPE_FROM    = 0x0002;
const unsigned OMP_TGT_MAPTYPE_ALWAYS  = 0x0004;
const unsigned OMP_TGT_MAPTYPE_RELEASE = 0x0008;
const unsigned OMP_TGT_MAPTYPE_DELETE  = 0x0018;
const unsigned OMP_TGT_MAPTYPE_POINTER = 0x0020;
const unsigned OMP_TGT_MAPTYPE_EXTRA   = 0x0040;

struct __tgt_offload_entry{
  void      *addr;       // Pointer to the offload entry info (function or global)
  char      *name;       // Name of the function or global
  int64_t    size;       // Size of the entry info (0 if it a function)
};

struct __tgt_device_image{
  void   *ImageStart;       // Pointer to the target code start
  void   *ImageEnd;         // Pointer to the target code end
  // We also add the host entries to the device image, as it may be useful
  // for the target runtime to have access to that information
  __tgt_offload_entry  *EntriesBegin;   // Begin of the table with all the entries
  __tgt_offload_entry  *EntriesEnd;     // End of the table with all the entries (non inclusive)
};

struct __tgt_bin_desc{
  int32_t              NumDevices;     // Number of devices supported
  __tgt_device_image   *DeviceImages;   // Arrays of device images (one per device)
  __tgt_offload_entry  *EntriesBegin;   // Begin of the table with all the entries
  __tgt_offload_entry  *EntriesEnd;     // End of the table with all the entries (non inclusive)
};

typedef void(__tgt_register_lib)(__tgt_bin_desc* desc);

typedef int32_t(__tgt_target)(int32_t device_id, void *host_addr,
    int32_t num_args, void** args_base, void** args, int64_t *args_size,
    int32_t *args_maptype);

typedef int32_t(__tgt_target_teams)(int32_t device_id, void *host_addr,
    int32_t num_args, void** args_base, void** args, int64_t *args_size,
    int32_t *args_maptype, int32_t num_teams, int32_t thread_limit);

typedef void(__tgt_target_data_begin)(int32_t device_id, int32_t num_args,
    void** args_base, void** args, int64_t *args_size, int32_t *args_maptype);

typedef void(__tgt_target_data_end)(int32_t device_id, int32_t num_args,
    void** args_base, void** args, int64_t *args_size, int32_t *args_maptype);

typedef void(__tgt_target_data_update)(int32_t device_id, int32_t num_args,
    void** args_base, void** args, int64_t *args_size, int32_t *args_maptype);
}

namespace llvm {
/// Specializations of llvm::TypeBuilder for:
///   ident_t
template <bool X> class TypeBuilder<ident_t, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_1
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // flags
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_2
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_3
        TypeBuilder<llvm::types::i<8> *, X>::get(C), // psource
        NULL);
  }
  enum { reserved_1, flags, reserved_2, reserved_3, psource };
};
///   ident_t
template <bool X> class TypeBuilder<kmp_task_t, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
        TypeBuilder<void *, X>::get(C),              // shareds
        TypeBuilder<kmp_routine_entry_t, X>::get(C), // routine
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // part_id
        TypeBuilder<kmp_routine_entry_t, X>::get(C), // destructors
        TypeBuilder<llvm::types::i<32>, X>::get(C),  // firstprivate_locker
        NULL);
  }
  enum { shareds, routine, part_id, destructors, firstprivate_locker };
};
template <typename R, typename A1, typename A2, typename A3, typename A4,
          typename A5, typename A6, typename A7, bool cross>
class TypeBuilder<R(A1, A2, A3, A4, A5, A6, A7), cross> {
public:
  static FunctionType *get(LLVMContext &Context) {
    Type *params[] = {
        TypeBuilder<A1, cross>::get(Context),
        TypeBuilder<A2, cross>::get(Context),
        TypeBuilder<A3, cross>::get(Context),
        TypeBuilder<A4, cross>::get(Context),
        TypeBuilder<A5, cross>::get(Context),
        TypeBuilder<A6, cross>::get(Context),
        TypeBuilder<A7, cross>::get(Context),
    };
    return FunctionType::get(TypeBuilder<R, cross>::get(Context), params,
                             false);
  }
};
template <typename R, typename A1, typename A2, typename A3, typename A4,
          typename A5, typename A6, typename A7, typename A8, typename A9,
          bool cross>
class TypeBuilder<R(A1, A2, A3, A4, A5, A6, A7, A8, A9), cross> {
public:
  static FunctionType *get(LLVMContext &Context) {
    Type *params[] = {
        TypeBuilder<A1, cross>::get(Context),
        TypeBuilder<A2, cross>::get(Context),
        TypeBuilder<A3, cross>::get(Context),
        TypeBuilder<A4, cross>::get(Context),
        TypeBuilder<A5, cross>::get(Context),
        TypeBuilder<A6, cross>::get(Context),
        TypeBuilder<A7, cross>::get(Context),
        TypeBuilder<A8, cross>::get(Context),
        TypeBuilder<A9, cross>::get(Context),
    };
    return FunctionType::get(TypeBuilder<R, cross>::get(Context), params,
                             false);
  }
};
template <typename R, typename A1, typename A2, typename A3, typename A4,
          typename A5, typename A6, bool cross>
class TypeBuilder<R(A1, A2, A3, A4, A5, A6), cross> {
public:
  static FunctionType *get(LLVMContext &Context) {
    Type *params[] = {
        TypeBuilder<A1, cross>::get(Context),
        TypeBuilder<A2, cross>::get(Context),
        TypeBuilder<A3, cross>::get(Context),
        TypeBuilder<A4, cross>::get(Context),
        TypeBuilder<A5, cross>::get(Context),
        TypeBuilder<A6, cross>::get(Context),
    };
    return FunctionType::get(TypeBuilder<R, cross>::get(Context), params,
                             false);
  }
};
template <typename R, typename A1, typename A2, typename A3, typename A4,
          typename A5, typename A6, typename A7, typename A8, bool cross>
class TypeBuilder<R(A1, A2, A3, A4, A5, A6, A7, A8), cross> {
public:
  static FunctionType *get(LLVMContext &Context) {
    Type *params[] = {
        TypeBuilder<A1, cross>::get(Context),
        TypeBuilder<A2, cross>::get(Context),
        TypeBuilder<A3, cross>::get(Context),
        TypeBuilder<A4, cross>::get(Context),
        TypeBuilder<A5, cross>::get(Context),
        TypeBuilder<A6, cross>::get(Context),
        TypeBuilder<A7, cross>::get(Context),
        TypeBuilder<A8, cross>::get(Context),
    };
    return FunctionType::get(TypeBuilder<R, cross>::get(Context), params,
                             false);
  }
};
template <bool X> class TypeBuilder<sched_type, X> {
public:
  static IntegerType *get(LLVMContext &C) {
    return TypeBuilder<llvm::types::i<32>, X>::get(C);
  }
};
template <bool X> class TypeBuilder<kmp_proc_bind_t, X> {
public:
  static IntegerType *get(LLVMContext &C) {
    return TypeBuilder<llvm::types::i<32>, X>::get(C);
  }
};

//struct LLVContextCGM {
//  llvm::LLVMContext &VMContext;
//  CodeGenModule *CGM;
//  LLVContextCGM(llvm::LLVMContext &VMContext, CodeGenModule *CGM)
//      : VMContext(VMContext), CGM(CGM) {}
//};

typedef llvm::TypeBuilder<kmp_task_t, false> TaskTBuilder;
typedef llvm::TypeBuilder<kmp_proc_bind_t, false> ProcBindTBuilder;
typedef llvm::TypeBuilder<ident_t, false> IdentTBuilder;

///// Specializations of llvm::TypeBuilder for:
/////   ident_t
//template <bool X> class TypeBuilder<ident_t, X> {
//public:
//  static StructType *get(LLVMContext &C) {
//    return StructType::get(
//        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_1
//        TypeBuilder<llvm::types::i<32>, X>::get(C),  // flags
//        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_2
//        TypeBuilder<llvm::types::i<32>, X>::get(C),  // reserved_3
//        TypeBuilder<llvm::types::i<8> *, X>::get(C), // psource
//        NULL);
//  }
//  enum {
//    reserved_1,
//    flags,
//    reserved_2,
//    reserved_3,
//    psource
//  };
//};

template <bool X> class TypeBuilder<__tgt_offload_entry, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
    	// Pointer to the offload entry info (function or global)
        TypeBuilder<llvm::types::i<8>*, X>::get(C),
      // Name of the function or global
        TypeBuilder<llvm::types::i<8>*, X>::get(C),
		  // Size of the entry info (0 if it a function)
        TypeBuilder<llvm::types::i<64>, X>::get(C),
        NULL);
  }
  enum {
    address,
    name,
    size
  };
};
template <bool X> class TypeBuilder<__tgt_device_image, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
    // Pointer to the target code start
        TypeBuilder<llvm::types::i<8>*, X>::get(C),
    // Pointer to the target code end
        TypeBuilder<llvm::types::i<8>*, X>::get(C),
    // Begin of the table with all the entries
        TypeBuilder<__tgt_offload_entry*, X>::get(C),
    // End of the table with all the entries (non inclusive)
        TypeBuilder<__tgt_offload_entry*, X>::get(C),
        NULL);
  }
  enum {
    image_start,
    image_end,
    entries_begin,
    entries_end
  };
};
template <bool X> class TypeBuilder<__tgt_bin_desc, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
    	// Number of devices supported
        TypeBuilder<llvm::types::i<32>,   X>::get(C),
		// Arrays of device images (one per device)
        TypeBuilder<__tgt_device_image*,  X>::get(C),
		// Begin of the table with all the entries
        TypeBuilder<__tgt_offload_entry*, X>::get(C),
		// End of the table with all the entries (non inclusive)
        TypeBuilder<__tgt_offload_entry*, X>::get(C),
        NULL);
  }
  enum {
    num_devices,
    device_images,
    entries_begin,
    entries_end
  };
};
}

#endif
