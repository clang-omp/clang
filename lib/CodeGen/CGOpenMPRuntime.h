//===---- CGOpenMPRuntime.h - Interface to OpenMP Runtimes ------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This provides an abstract class for OpenMP code generation.  Concrete
// subclasses of this implement code generation for specific OpenMP
// runtime libraries.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_CODEGEN_OPENMPRUNTIME_H
#define CLANG_CODEGEN_OPENMPRUNTIME_H

#include "clang/AST/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "CodeGenModule.h"
#include "CodeGenFunction.h"

namespace llvm {
class AllocaInst;
class CallInst;
class GlobalVariable;
class Constant;
class Function;
class Module;
class StructLayout;
class FunctionType;
class StructType;
class Type;
class Value;
} // namespace llvm

namespace clang {
namespace CodeGen {

class CodeGenFunction;
class CodeGenModule;

#define DEFAULT_EMIT_OPENMP_DECL(name)       \
  virtual llvm::Constant* Get_##name();

/// Implements runtime-specific code generation functions.
class CGOpenMPRuntime {
public:
  /// \brief Values for bit flags used in the ident_t to describe the fields.
  /// All enumeric elements are named and described in accordance with the code
  /// from http://llvm.org/svn/llvm-project/openmp/trunk/runtime/src/kmp.h
  enum OpenMPLocationFlags {
    /// \brief Use trampoline for internal microtask.
    OMP_IDENT_IMD = 0x01,
    /// \brief Use c-style ident structure.
    OMP_IDENT_KMPC = 0x02,
    /// \brief Atomic reduction option for kmpc_reduce.
    OMP_ATOMIC_REDUCE = 0x10,
    /// \brief Explicit 'barrier' directive.
    OMP_IDENT_BARRIER_EXPL = 0x20,
    /// \brief Implicit barrier in code.
    OMP_IDENT_BARRIER_IMPL = 0x40,
    /// \brief Implicit barrier in 'for' directive.
    OMP_IDENT_BARRIER_IMPL_FOR = 0x40,
    /// \brief Implicit barrier in 'sections' directive.
    OMP_IDENT_BARRIER_IMPL_SECTIONS = 0xC0,
    /// \brief Implicit barrier in 'single' directive.
    OMP_IDENT_BARRIER_IMPL_SINGLE = 0x140
  };
  enum OpenMPRTLFunction {
    // Call to void __kmpc_fork_call(ident_t *loc, kmp_int32 argc, kmpc_micro
    // microtask, ...);
    OMPRTL__kmpc_fork_call,
    // Call to kmp_int32 kmpc_global_thread_num(ident_t *loc);
    OMPRTL__kmpc_global_thread_num
  };

protected:
  CodeGenModule &CGM;

  /// \brief Default const ident_t object used for initialization of all other
  /// ident_t objects.
  llvm::Constant *DefaultOpenMPPSource;
  /// \brief Map of flags and corrsponding default locations.
  typedef llvm::DenseMap<unsigned, llvm::Value *> OpenMPDefaultLocMapTy;
  OpenMPDefaultLocMapTy OpenMPDefaultLocMap;
  llvm::Value *GetOrCreateDefaultOpenMPLocation(OpenMPLocationFlags Flags);
  /// \brief Describes ident structure that describes a source location.
  /// All descriptions are taken from
  /// http://llvm.org/svn/llvm-project/openmp/trunk/runtime/src/kmp.h
  /// Original structure:
  /// typedef struct ident {
  ///    kmp_int32 reserved_1;   /**<  might be used in Fortran;
  ///                                  see above  */
  ///    kmp_int32 flags;        /**<  also f.flags; KMP_IDENT_xxx flags;
  ///                                  KMP_IDENT_KMPC identifies this union
  ///                                  member  */
  ///    kmp_int32 reserved_2;   /**<  not really used in Fortran any more;
  ///                                  see above */
  ///#if USE_ITT_BUILD
  ///                            /*  but currently used for storing
  ///                                region-specific ITT */
  ///                            /*  contextual information. */
  ///#endif /* USE_ITT_BUILD */
  ///    kmp_int32 reserved_3;   /**< source[4] in Fortran, do not use for
  ///                                 C++  */
  ///    char const *psource;    /**< String describing the source location.
  ///                            The string is composed of semi-colon separated
  //                             fields which describe the source file,
  ///                            the function and a pair of line numbers that
  ///                            delimit the construct.
  ///                             */
  /// } ident_t;
  enum IdentFieldIndex {
    /// \brief might be used in Fortran
    IdentField_Reserved_1,
    /// \brief OMP_IDENT_xxx flags; OMP_IDENT_KMPC identifies this union member.
    IdentField_Flags,
    /// \brief Not really used in Fortran any more
    IdentField_Reserved_2,
    /// \brief Source[4] in Fortran, do not use for C++
    IdentField_Reserved_3,
    /// \brief String describing the source location. The string is composed of
    /// semi-colon separated fields which describe the source file, the function
    /// and a pair of line numbers that delimit the construct.
    IdentField_PSource
  };
  llvm::StructType *IdentTy;
  /// \brief Map for Sourcelocation and OpenMP runtime library debug locations.
  typedef llvm::DenseMap<unsigned, llvm::Value *> OpenMPDebugLocMapTy;
  OpenMPDebugLocMapTy OpenMPDebugLocMap;
  /// \brief The type for a microtask which gets passed to __kmpc_fork_call().
  /// Original representation is:
  /// typedef void (kmpc_micro)(kmp_int32 global_tid, kmp_int32 bound_tid,...);
  llvm::FunctionType *Kmpc_MicroTy;
  /// \brief Map of local debug location and functions.
  typedef llvm::DenseMap<llvm::Function *, llvm::Value *> OpenMPLocMapTy;
  OpenMPLocMapTy OpenMPLocMap;
  /// \brief Map of local gtid and functions.
  typedef llvm::DenseMap<llvm::Function *, llvm::Value *> OpenMPGtidMapTy;
  OpenMPGtidMapTy OpenMPGtidMap;

  // Number of target regions processed so far
  unsigned NumTargetRegions;

  // Set of all functions that register target libraries
  llvm::SmallSet<const llvm::Function*, 32> FunctionsWithTargetRegistry;

  // Target regions descriptor for the current compilation unit
  llvm::Constant *TargetRegionsDescriptor;

public:

  // Returns the number of target regions processed so far
  unsigned getNumOfProcessedTargetRegions(){ return NumTargetRegions; }

  // Incremented number of processed target regions
  void incNumOfProcessedTargetRegions(){ ++NumTargetRegions; }

  // Get and Incremented number of processed target regions
  unsigned getAndIncNumOfProcessedTargetRegions(){ return NumTargetRegions++; }

  // Mark function as using target registry
  void setFunctionRegisterTarget(const llvm::Function *F){
    FunctionsWithTargetRegistry.insert(F);
  }

  // Return true if the function registers a target
  bool getFunctionRegisterTarget(const llvm::Function *F){
    return FunctionsWithTargetRegistry.count(F);
  }


  enum EAtomicOperation {
    OMP_Atomic_add,
    OMP_Atomic_sub,
    OMP_Atomic_mul,
    OMP_Atomic_div,
    OMP_Atomic_andb,
    OMP_Atomic_shl,
    OMP_Atomic_shr,
    OMP_Atomic_orb,
    OMP_Atomic_xor,
    OMP_Atomic_andl,
    OMP_Atomic_orl,
    OMP_Atomic_max,
    OMP_Atomic_min,
    OMP_Atomic_eqv,
    OMP_Atomic_neqv,
    OMP_Atomic_rd,
    OMP_Atomic_wr,
    OMP_Atomic_swp,
    OMP_Atomic_assign,
    OMP_Atomic_invalid
  };
  
  explicit CGOpenMPRuntime(CodeGenModule &CGM);
  virtual ~CGOpenMPRuntime() {}

  /// \brief Cleans up references to the objects in finished function.
  /// \param CGF Reference to finished CodeGenFunction.
  ///
  void FunctionFinished(CodeGenFunction &CGF);

  /// \brief Emits object of ident_t type with info for source location.
  /// \param CGF Reference to current CodeGenFunction.
  /// \param Loc Clang source location.
  /// \param Flags Flags for OpenMP location.
  ///
  llvm::Value *
  EmitOpenMPUpdateLocation(CodeGenFunction &CGF, SourceLocation Loc,
                           OpenMPLocationFlags Flags = OMP_IDENT_KMPC);

  /// \brief Generates global thread number value.
  /// \param CGF Reference to current CodeGenFunction.
  /// \param Loc Clang source location.
  ///
  llvm::Value *GetOpenMPGlobalThreadNum(CodeGenFunction &CGF,
                                        SourceLocation Loc);

  /// \brief Returns pointer to ident_t type;
  llvm::Type *getIdentTyPointerTy();

  /// \brief Returns pointer to kmpc_micro type;
  llvm::Type *getKmpc_MicroPointerTy();

  /// \brief Returns specified OpenMP runtime function.
  /// \param Function OpenMP runtime function.
  /// \return Specified function.
  llvm::Constant *CreateRuntimeFunction(OpenMPRTLFunction Function);

  DEFAULT_EMIT_OPENMP_DECL(fork_call)
  DEFAULT_EMIT_OPENMP_DECL(push_num_threads)
  DEFAULT_EMIT_OPENMP_DECL(push_proc_bind)
  DEFAULT_EMIT_OPENMP_DECL(fork_teams)
  DEFAULT_EMIT_OPENMP_DECL(push_num_teams)
  DEFAULT_EMIT_OPENMP_DECL(cancel_barrier)
  DEFAULT_EMIT_OPENMP_DECL(barrier)
  DEFAULT_EMIT_OPENMP_DECL(cancellationpoint)
  DEFAULT_EMIT_OPENMP_DECL(cancel)
  DEFAULT_EMIT_OPENMP_DECL(omp_taskyield)
  DEFAULT_EMIT_OPENMP_DECL(omp_taskwait)
  DEFAULT_EMIT_OPENMP_DECL(flush)
  DEFAULT_EMIT_OPENMP_DECL(master)
  DEFAULT_EMIT_OPENMP_DECL(end_master)
  DEFAULT_EMIT_OPENMP_DECL(single)
  DEFAULT_EMIT_OPENMP_DECL(end_single)
  DEFAULT_EMIT_OPENMP_DECL(critical)
  DEFAULT_EMIT_OPENMP_DECL(end_critical)
  DEFAULT_EMIT_OPENMP_DECL(ordered)
  DEFAULT_EMIT_OPENMP_DECL(end_ordered)
  DEFAULT_EMIT_OPENMP_DECL(end_reduce_nowait)
  DEFAULT_EMIT_OPENMP_DECL(end_reduce)
  DEFAULT_EMIT_OPENMP_DECL(atomic_start)
  DEFAULT_EMIT_OPENMP_DECL(atomic_end)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_init_4)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_init_4u)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_init_8)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_init_8u)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_next_4)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_next_4u)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_next_8)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_next_8u)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_fini_4)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_fini_4u)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_fini_8)
  DEFAULT_EMIT_OPENMP_DECL(dispatch_fini_8u)
  DEFAULT_EMIT_OPENMP_DECL(for_static_init_4)
  DEFAULT_EMIT_OPENMP_DECL(for_static_init_4u)
  DEFAULT_EMIT_OPENMP_DECL(for_static_init_8)
  DEFAULT_EMIT_OPENMP_DECL(for_static_init_8u)
  DEFAULT_EMIT_OPENMP_DECL(for_static_fini)
  DEFAULT_EMIT_OPENMP_DECL(omp_task_begin_if0)
  DEFAULT_EMIT_OPENMP_DECL(omp_task_complete_if0)
  DEFAULT_EMIT_OPENMP_DECL(omp_task_parts)
  DEFAULT_EMIT_OPENMP_DECL(taskgroup)
  DEFAULT_EMIT_OPENMP_DECL(end_taskgroup)
  DEFAULT_EMIT_OPENMP_DECL(register_lib)
  DEFAULT_EMIT_OPENMP_DECL(target)
  DEFAULT_EMIT_OPENMP_DECL(target_data_begin)
  DEFAULT_EMIT_OPENMP_DECL(target_data_end)

  DEFAULT_EMIT_OPENMP_DECL(threadprivate_register)
  DEFAULT_EMIT_OPENMP_DECL(global_thread_num)

  virtual llvm::Type *getKMPDependInfoType();

  // Special processing for __kmpc_copyprivate
  // DEFAULT_GET_OPENMP_FUNC(copyprivate)
  virtual llvm::Constant *Get_copyprivate();
  // Special processing for __kmpc_reduce_nowait
  // DEFAULT_GET_OPENMP_FUNC(reduce_nowait)
  virtual llvm::Constant * Get_reduce_nowait();
  // Special processing for __kmpc_reduce
  // DEFAULT_GET_OPENMP_FUNC(reduce)
  virtual llvm::Constant *Get_reduce();
  // Special processing for __kmpc_omp_task_alloc
  // DEFAULT_GET_OPENMP_FUNC(omp_task_alloc)
  virtual llvm::Constant * Get_omp_task_alloc();
  // Special processing for __kmpc_omp_task_with_deps
  // DEFAULT_GET_OPENMP_FUNC(omp_task_with_deps)
  virtual llvm::Constant * Get_omp_task_with_deps();
  // Special processing for __kmpc_omp_wait_deps
  // DEFAULT_GET_OPENMP_FUNC(omp_wait_deps)
  virtual llvm::Constant * Get_omp_wait_deps();

  // Special processing for __kmpc_threadprivate_cached
  // DEFAULT_GET_OPENMP_FUNC(threadprivate_cached)
  virtual llvm::Constant *  Get_threadprivate_cached();


  virtual QualType GetAtomicType(CodeGenFunction &CGF, QualType QTy);
  virtual llvm::Value *GetAtomicFuncGeneral(CodeGenFunction &CGF, QualType QTyRes,
                                           QualType QTyIn, EAtomicOperation Aop,
                                           bool Capture, bool Reverse);
  virtual llvm::Value *GetAtomicFunc(CodeGenFunction &CGF, QualType QTy,
      OpenMPReductionClauseOperator Op);


  /// Implement some target dependent transformation for the target region
  /// outlined function
  ///
  virtual void PostProcessTargetFunction(const Decl *D,
                                          llvm::Function *F,
                                          const CGFunctionInfo &FI);

  /// \brief Creates a structure with the location info for Intel OpenMP RTL.
  virtual llvm::Value *CreateIntelOpenMPRTLLoc(SourceLocation Loc,
      CodeGenFunction &CGF, unsigned Flags = 0x02);
  /// \brief Creates call to "__kmpc_global_thread_num(ident_t *loc)" OpenMP
  /// RTL function.
  virtual llvm::Value *CreateOpenMPGlobalThreadNum(SourceLocation Loc,
      CodeGenFunction &CGF);
  /// \brief Checks if the variable is OpenMP threadprivate and generates code
  /// for threadprivate variables.
  /// \return 0 if the variable is not threadprivate, or new address otherwise.
  virtual llvm::Value *CreateOpenMPThreadPrivateCached(const VarDecl *VD,
                                               SourceLocation Loc,
                                               CodeGenFunction &CGF,
                                               bool NoCast = false);

  /// \brief  Return a string with the mangled name of a target region for
  /// the given module
  ///
  std::string GetOffloadEntryMangledName(llvm::Triple TargetTriple);

  /// \brief  Return the target regions descriptor or a create a new
  /// one if if does not exist
  ///
  llvm::Constant* GetTargetRegionsDescriptor();

  /// \brief  Return host pointer for the current target regions. This creates
  /// the offload entry for the target region
  ///
  llvm::Constant* GetHostPtrForCurrentTargetRegion();

  /// \brief Return a pointer to the device image begin
  ///
  llvm::Constant* GetDeviceImageBeginPointer(llvm::Triple TargetTriple);

  /// \brief Return a pointer to the device image end
  ///
  llvm::Constant* GetDeviceImageEndPointer(llvm::Triple TargetTriple);
};

/// \brief Returns an implementation of the OpenMP RT for a given target
CGOpenMPRuntime *CreateOpenMPRuntime(CodeGenModule &CGM);

} // namespace CodeGen
} // namespace clang

#endif
