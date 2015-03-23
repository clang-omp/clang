//===----- CGOpenMPRuntime.h - Interface to OpenMP Runtimes ---------*- C++ -*-===//
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

#include "CGOpenMPRuntimeTypes.h"
#include "CGOpenMPRuntime.h"
#include "CodeGenFunction.h"
#include "clang/AST/Decl.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace clang;
using namespace CodeGen;

#define DEBUG_TYPE "CGOpenMPRuntime"




// Register global declaration as required for OpenMP Target offloading
void CGOpenMPRuntime::registerEntryDeclaration(const Decl *D ){
  EntriesList.push_back(nullptr);
  EntriesListMap[D] = std::prev(EntriesList.end());
}

// Register global initializer for OpenMP Target offloading
void CGOpenMPRuntime::registerTargetGlobalInitializer(const llvm::Constant *D){
  TargetGlobalInitializers.insert(D);
}

// Return true if D is a global initializer for OpenMP Target offloading
bool CGOpenMPRuntime::isTargetGlobalInitializer(const llvm::Constant *D){
  return TargetGlobalInitializers.count(D) != 0;
}

// Return true if the current module has global initializers
bool CGOpenMPRuntime::hasTargetGlobalInitializers(){
  return !TargetGlobalInitializers.empty();
}

// Return true if this declaration corresponds to an offloading entry
bool CGOpenMPRuntime::isEntryDeclaration(const Decl *D){
  return EntriesListMap.find(D) != EntriesListMap.end();
}

// Mark value as requiring to be moved to global memory
void CGOpenMPRuntime::setRequiresSharedVersion(llvm::Value *V){
  ValuesToBeInSharedMemory.insert(V);
}

// Register the global value created for a given global declaration that
// is used in OpenMP offloading code
void
CGOpenMPRuntime::registerGlobalVarForEntryDeclaration(const Decl *D,
                                                      llvm::GlobalVariable *GV){
  EntriesListMapTy::iterator it = EntriesListMap.find(D);

  // No declaration was registered before?
  if (it == EntriesListMap.end()){
    registerEntryDeclaration(D);
    it = EntriesListMap.find(D);
    assert(it != EntriesListMap.end()
        && "We should already have a entry in the list of entry points!");
    return;
  }

  *(it->second) = GV;
}

// Return the first global value that comes after the one associated with
// the requested declaration by reference. Return true if the requested
// declaration is registered and false otherwise.
llvm::GlobalVariable *
CGOpenMPRuntime::getNextGlobalVarForEntryDeclaration(const Decl *D){

  if (!D)
    return nullptr;

  EntriesListMapTy::iterator it = EntriesListMap.find(D);

  if (it == EntriesListMap.end())
    return nullptr;

  // Locate the following global value that is defined
  for (EntriesListTy::iterator i = std::next(it->second),
                               e = EntriesList.end(); i!=e; ++i){
    if (*i){
      return *i;
    }
  }

  return nullptr;
}

// Return a pair of Function/host entry for a given directive with target
llvm::Function*
CGOpenMPRuntime::getEntryForDirectiveWithTarget(const Decl *D){

  TargetDirectiveToEntriesMapTy::iterator it =
      TargetDirectiveToEntriesMap.find(D);

  if (it != TargetDirectiveToEntriesMap.end())
    return it->second;

  return nullptr;
}

// Register a function and host entry for a given diretive with target
void
CGOpenMPRuntime::registerEntryForDirectiveWithTarget(
                                                const Decl *D,
                                                llvm::Function *F){
  TargetDirectiveToEntriesMap[D] = F;
}


CGOpenMPRuntime::CGOpenMPRuntime(CodeGenModule &CGM)
    : CGM(CGM), DefaultOpenMPPSource(nullptr), NumTargetRegions(0),
      NumTargetGlobals(0), TargetRegionsDescriptor(nullptr) {
  IdentTy = llvm::StructType::create(
      "ident_t", CGM.Int32Ty /* reserved_1 */, CGM.Int32Ty /* flags */,
      CGM.Int32Ty /* reserved_2 */, CGM.Int32Ty /* reserved_3 */,
      CGM.Int8PtrTy /* psource */, NULL);
  // Build void (*kmpc_micro)(kmp_int32 *global_tid, kmp_int32 *bound_tid,...)
  llvm::Type *MicroParams[] = {llvm::PointerType::getUnqual(CGM.Int32Ty),
                               llvm::PointerType::getUnqual(CGM.Int32Ty)};
  Kmpc_MicroTy = llvm::FunctionType::get(CGM.VoidTy, MicroParams, true);
}

llvm::Value *
CGOpenMPRuntime::GetOrCreateDefaultOpenMPLocation(OpenMPLocationFlags Flags) {
  llvm::Value *Entry = OpenMPDefaultLocMap.lookup(Flags);
  if (!Entry) {
    if (!DefaultOpenMPPSource) {
      // Initialize default location for psource field of ident_t structure of
      // all ident_t objects. Format is ";file;function;line;column;;".
      // Taken from
      // http://llvm.org/svn/llvm-project/openmp/trunk/runtime/src/kmp_str.c
      DefaultOpenMPPSource =
          CGM.GetAddrOfConstantCString(";unknown;unknown;0;0;;");
      DefaultOpenMPPSource =
          llvm::ConstantExpr::getBitCast(DefaultOpenMPPSource, CGM.Int8PtrTy);
    }
    llvm::GlobalVariable *DefaultOpenMPLocation = cast<llvm::GlobalVariable>(
        CGM.CreateRuntimeVariable(IdentTy, ".kmpc_default_loc.addr"));
    DefaultOpenMPLocation->setUnnamedAddr(true);
    DefaultOpenMPLocation->setConstant(true);
    DefaultOpenMPLocation->setLinkage(llvm::GlobalValue::PrivateLinkage);

    llvm::Constant *Zero = llvm::ConstantInt::get(CGM.Int32Ty, 0, true);
    llvm::Constant *Values[] = {Zero,
                                llvm::ConstantInt::get(CGM.Int32Ty, Flags),
                                Zero, Zero, DefaultOpenMPPSource};
    llvm::Constant *Init = llvm::ConstantStruct::get(IdentTy, Values);
    DefaultOpenMPLocation->setInitializer(Init);
    return DefaultOpenMPLocation;
  }
  return Entry;
}

llvm::Value *CGOpenMPRuntime::EmitOpenMPUpdateLocation(
    CodeGenFunction &CGF, SourceLocation Loc, OpenMPLocationFlags Flags) {
  // If no debug info is generated - return global default location.
  if (CGM.getCodeGenOpts().getDebugInfo() == CodeGenOptions::NoDebugInfo ||
      Loc.isInvalid())
    return GetOrCreateDefaultOpenMPLocation(Flags);

  assert(CGF.CurFn && "No function in current CodeGenFunction.");

  llvm::Value *LocValue = nullptr;
  OpenMPLocMapTy::iterator I = OpenMPLocMap.find(CGF.CurFn);
  if (I != OpenMPLocMap.end()) {
    LocValue = I->second;
  } else {
    // Generate "ident_t .kmpc_loc.addr;"
    llvm::AllocaInst *AI = CGF.CreateTempAlloca(IdentTy, ".kmpc_loc.addr");
    AI->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(IdentTy));
    OpenMPLocMap[CGF.CurFn] = AI;
    LocValue = AI;

    CGBuilderTy::InsertPointGuard IPG(CGF.Builder);
    CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
    CGF.Builder.CreateMemCpy(LocValue, GetOrCreateDefaultOpenMPLocation(Flags),
                             llvm::ConstantExpr::getSizeOf(IdentTy),
                             CGM.PointerAlignInBytes);
  }

  // char **psource = &.kmpc_loc_<flags>.addr.psource;
  llvm::Value *PSource =
      CGF.Builder.CreateConstInBoundsGEP2_32(LocValue, 0, IdentField_PSource);

  auto OMPDebugLoc = OpenMPDebugLocMap.lookup(Loc.getRawEncoding());
  if (OMPDebugLoc == nullptr) {
    SmallString<128> Buffer2;
    llvm::raw_svector_ostream OS2(Buffer2);
    // Build debug location
    PresumedLoc PLoc = CGF.getContext().getSourceManager().getPresumedLoc(Loc);
    OS2 << ";" << PLoc.getFilename() << ";";
    if (const FunctionDecl *FD =
            dyn_cast_or_null<FunctionDecl>(CGF.CurFuncDecl)) {
      OS2 << FD->getQualifiedNameAsString();
    }
    OS2 << ";" << PLoc.getLine() << ";" << PLoc.getColumn() << ";;";
    OMPDebugLoc = CGF.Builder.CreateGlobalStringPtr(OS2.str());
    OpenMPDebugLocMap[Loc.getRawEncoding()] = OMPDebugLoc;
  }
  // *psource = ";<File>;<Function>;<Line>;<Column>;;";
  CGF.Builder.CreateStore(OMPDebugLoc, PSource);

  return LocValue;
}

llvm::Value *CGOpenMPRuntime::GetOpenMPGlobalThreadNum(CodeGenFunction &CGF,
                                                       SourceLocation Loc) {
  assert(CGF.CurFn && "No function in current CodeGenFunction.");

  llvm::Value *GTid = nullptr;
  OpenMPGtidMapTy::iterator I = OpenMPGtidMap.find(CGF.CurFn);
  if (I != OpenMPGtidMap.end()) {
    GTid = I->second;
  } else {
    // Generate "int32 .kmpc_global_thread_num.addr;"
    CGBuilderTy::InsertPointGuard IPG(CGF.Builder);
    CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
    llvm::Value *Args[] = {EmitOpenMPUpdateLocation(CGF, Loc)};
    GTid = CGF.EmitRuntimeCall(
        CreateRuntimeFunction(OMPRTL__kmpc_global_thread_num), Args);
    OpenMPGtidMap[CGF.CurFn] = GTid;
  }
  return GTid;
}

void CGOpenMPRuntime::FunctionFinished(CodeGenFunction &CGF) {
  assert(CGF.CurFn && "No function in current CodeGenFunction.");
  if (OpenMPGtidMap.count(CGF.CurFn))
    OpenMPGtidMap.erase(CGF.CurFn);
  if (OpenMPLocMap.count(CGF.CurFn))
    OpenMPLocMap.erase(CGF.CurFn);
}

llvm::Type *CGOpenMPRuntime::getIdentTyPointerTy() {
  return llvm::PointerType::getUnqual(IdentTy);
}

llvm::Type *CGOpenMPRuntime::getKmpc_MicroPointerTy() {
  return llvm::PointerType::getUnqual(Kmpc_MicroTy);
}

llvm::Constant *
CGOpenMPRuntime::CreateRuntimeFunction(OpenMPRTLFunction Function) {
  llvm::Constant *RTLFn = nullptr;
  switch (Function) {
  case OMPRTL__kmpc_fork_call: {
    // Build void __kmpc_fork_call(ident_t *loc, kmp_int32 argc, kmpc_micro
    // microtask, ...);
    llvm::Type *TypeParams[] = {getIdentTyPointerTy(), CGM.Int32Ty,
                                getKmpc_MicroPointerTy()};
    llvm::FunctionType *FnTy =
        llvm::FunctionType::get(CGM.VoidTy, TypeParams, true);
    RTLFn = CGM.CreateRuntimeFunction(FnTy, "__kmpc_fork_call");
    break;
  }
  case OMPRTL__kmpc_global_thread_num: {
    // Build kmp_int32 __kmpc_global_thread_num(ident_t *loc);
    llvm::Type *TypeParams[] = {getIdentTyPointerTy()};
    llvm::FunctionType *FnTy =
        llvm::FunctionType::get(CGM.Int32Ty, TypeParams, false);
    RTLFn = CGM.CreateRuntimeFunction(FnTy, "__kmpc_global_thread_num");
    break;
  }
  }
  return RTLFn;
}

#define OPENMPRTL_FUNC(name) Get_##name()
#define OPENMPRTL_ATOMIC_FUNC(QTy, Op) GetAtomicFunc(CGF, QTy, Op)
#define OPENMPRTL_ATOMIC_FUNC_GENERAL(QTyRes, QTyIn, Aop, Capture, Reverse)    \
  GetAtomicFuncGeneral(CGF, QTyRes, QTyIn, Aop, Capture, Reverse)

#define DEFAULT_EMIT_OPENMP_FUNC(name)                                         \
  llvm::Constant* CGOpenMPRuntime::Get_##name(){                               \
    return CGM.CreateRuntimeFunction(                                          \
            llvm::TypeBuilder<__kmpc_##name, false>::get(CGM.getLLVMContext()),\
            "__kmpc_" #name);                                                  \
  }

#define DEFAULT_EMIT_OPENMP_FUNC_TARGET(name)                                         \
  llvm::Constant* CGOpenMPRuntime::Get_##name(){                               \
    return CGM.CreateRuntimeFunction(                                          \
            llvm::TypeBuilder<__tgt_##name, false>::get(CGM.getLLVMContext()),\
            "__tgt_" #name);                                                  \
  }

///===---------------
///
/// Default OpenMP Runtime Implementation
///
///===---------------

DEFAULT_EMIT_OPENMP_FUNC(fork_call)
DEFAULT_EMIT_OPENMP_FUNC(push_num_threads)
DEFAULT_EMIT_OPENMP_FUNC(push_proc_bind)
DEFAULT_EMIT_OPENMP_FUNC(fork_teams)
DEFAULT_EMIT_OPENMP_FUNC(push_num_teams)
DEFAULT_EMIT_OPENMP_FUNC(cancel_barrier)
DEFAULT_EMIT_OPENMP_FUNC(barrier)
DEFAULT_EMIT_OPENMP_FUNC(cancellationpoint)
DEFAULT_EMIT_OPENMP_FUNC(cancel)
DEFAULT_EMIT_OPENMP_FUNC(omp_taskyield)
DEFAULT_EMIT_OPENMP_FUNC(omp_taskwait)
DEFAULT_EMIT_OPENMP_FUNC(flush)
DEFAULT_EMIT_OPENMP_FUNC(master)
DEFAULT_EMIT_OPENMP_FUNC(end_master)
DEFAULT_EMIT_OPENMP_FUNC(single)
DEFAULT_EMIT_OPENMP_FUNC(end_single)
DEFAULT_EMIT_OPENMP_FUNC(critical)
DEFAULT_EMIT_OPENMP_FUNC(end_critical)
DEFAULT_EMIT_OPENMP_FUNC(ordered)
DEFAULT_EMIT_OPENMP_FUNC(end_ordered)
DEFAULT_EMIT_OPENMP_FUNC(end_reduce_nowait)
DEFAULT_EMIT_OPENMP_FUNC(end_reduce)
DEFAULT_EMIT_OPENMP_FUNC(atomic_start)
DEFAULT_EMIT_OPENMP_FUNC(atomic_end)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_init_4)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_init_4u)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_init_8)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_init_8u)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_next_4)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_next_4u)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_next_8)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_next_8u)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_fini_4)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_fini_4u)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_fini_8)
DEFAULT_EMIT_OPENMP_FUNC(dispatch_fini_8u)
DEFAULT_EMIT_OPENMP_FUNC(for_static_init_4)
DEFAULT_EMIT_OPENMP_FUNC(for_static_init_4u)
DEFAULT_EMIT_OPENMP_FUNC(for_static_init_8)
DEFAULT_EMIT_OPENMP_FUNC(for_static_init_8u)
DEFAULT_EMIT_OPENMP_FUNC(for_static_fini)
DEFAULT_EMIT_OPENMP_FUNC(omp_task_begin_if0)
DEFAULT_EMIT_OPENMP_FUNC(omp_task_complete_if0)
DEFAULT_EMIT_OPENMP_FUNC(omp_task_parts)
DEFAULT_EMIT_OPENMP_FUNC(taskgroup)
DEFAULT_EMIT_OPENMP_FUNC(end_taskgroup)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(register_lib)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(target)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(target_teams)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(target_data_begin)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(target_data_end)
DEFAULT_EMIT_OPENMP_FUNC_TARGET(target_data_update)

DEFAULT_EMIT_OPENMP_FUNC(threadprivate_register)
DEFAULT_EMIT_OPENMP_FUNC(global_thread_num)

DEFAULT_EMIT_OPENMP_FUNC(kernel_init)
DEFAULT_EMIT_OPENMP_FUNC(kernel_prepare_parallel)
DEFAULT_EMIT_OPENMP_FUNC(kernel_parallel)
DEFAULT_EMIT_OPENMP_FUNC(kernel_end_parallel)

DEFAULT_EMIT_OPENMP_FUNC(serialized_parallel)
DEFAULT_EMIT_OPENMP_FUNC(end_serialized_parallel)

// Special processing for __kmpc_copyprivate
// DEFAULT_GET_OPENMP_FUNC(copyprivate)
llvm::Constant *CGOpenMPRuntime::Get_copyprivate() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), CGM.SizeTy, llvm::TypeBuilder<
          void *, false>::get(C),
      llvm::TypeBuilder<kmp_reduce_func, false>::get(C), llvm::TypeBuilder<
          int32_t, false>::get(C) };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<void, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_copyprivate");
}

// Special processing for __kmpc_reduce_nowait
// DEFAULT_GET_OPENMP_FUNC(reduce_nowait)
llvm::Constant *CGOpenMPRuntime::Get_reduce_nowait() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), llvm::TypeBuilder<int32_t,
          false>::get(C), CGM.SizeTy, llvm::TypeBuilder<void *, false>::get(C),
      llvm::TypeBuilder<kmp_copy_func, false>::get(C), llvm::TypeBuilder<
          kmp_critical_name *, false>::get(C) };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<int32_t, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_reduce_nowait");
}
// Special processing for __kmpc_reduce
// DEFAULT_GET_OPENMP_FUNC(reduce)
llvm::Constant *CGOpenMPRuntime::Get_reduce() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), llvm::TypeBuilder<int32_t,
          false>::get(C), CGM.SizeTy, llvm::TypeBuilder<void *, false>::get(C),
      llvm::TypeBuilder<kmp_copy_func, false>::get(C), llvm::TypeBuilder<
          kmp_critical_name *, false>::get(C) };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<int32_t, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_reduce");
}
// Special processing for __kmpc_omp_task_alloc
// DEFAULT_GET_OPENMP_FUNC(omp_task_alloc)
llvm::Constant *CGOpenMPRuntime::Get_omp_task_alloc() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), llvm::TypeBuilder<int32_t,
          false>::get(C), CGM.SizeTy, CGM.SizeTy, llvm::TypeBuilder<
          kmp_routine_entry_t, false>::get(C) };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<kmp_task_t *, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_omp_task_alloc");
}
llvm::Type *CGOpenMPRuntime::getKMPDependInfoType() {
  llvm::Type *Ty = CGM.OpenMPSupport.getKMPDependInfoType();
  if (Ty)
    return Ty;
  IdentifierInfo *II = &CGM.getContext().Idents.get("__kmp_depend_info_t");
  DeclContext *DC = CGM.getContext().getTranslationUnitDecl();
  RecordDecl *RD = RecordDecl::Create(CGM.getContext(), TTK_Struct, DC,
      SourceLocation(), SourceLocation(), II);
  RD->startDefinition();
  DC->addHiddenDecl(RD);
  II = &CGM.getContext().Idents.get("base_addr");
  FieldDecl *FD = FieldDecl::Create(CGM.getContext(), RD, SourceLocation(),
      SourceLocation(), II, CGM.getContext().getIntPtrType(),
      CGM.getContext().getTrivialTypeSourceInfo(
          CGM.getContext().getIntPtrType(), SourceLocation()), 0, false,
      ICIS_NoInit);
  FD->setAccess(AS_public);
  RD->addDecl(FD);
  II = &CGM.getContext().Idents.get("len");
  FD = FieldDecl::Create(CGM.getContext(), RD, SourceLocation(),
      SourceLocation(), II, CGM.getContext().getSizeType(),
      CGM.getContext().getTrivialTypeSourceInfo(CGM.getContext().getSizeType(),
          SourceLocation()), 0, false, ICIS_NoInit);
  FD->setAccess(AS_public);
  RD->addDecl(FD);
  II = &CGM.getContext().Idents.get("flags");
  FD = FieldDecl::Create(CGM.getContext(), RD, SourceLocation(),
      SourceLocation(), II, CGM.getContext().BoolTy,
      CGM.getContext().getTrivialTypeSourceInfo(CGM.getContext().BoolTy,
          SourceLocation()), 0, false, ICIS_NoInit);
  FD->setAccess(AS_public);
  RD->addDecl(FD);
  RD->completeDefinition();
  QualType QTy = CGM.getContext().getRecordType(RD);
  Ty = CGM.getTypes().ConvertTypeForMem(QTy);
  CGM.OpenMPSupport.setKMPDependInfoType(Ty,
      CGM.getContext().getTypeAlignInChars(QTy).getQuantity());
  return Ty;
}
// Special processing for __kmpc_omp_task_with_deps
// DEFAULT_GET_OPENMP_FUNC(omp_task_with_deps)
llvm::Constant *CGOpenMPRuntime::Get_omp_task_with_deps() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), llvm::TypeBuilder<kmp_task_t *,
          false>::get(C), llvm::TypeBuilder<int32_t, false>::get(C),
      getKMPDependInfoType()->getPointerTo(),
      llvm::TypeBuilder<int32_t, false>::get(C),
      getKMPDependInfoType()->getPointerTo() };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<int32_t, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_omp_task_with_deps");
}
// Special processing for __kmpc_omp_wait_deps
// DEFAULT_GET_OPENMP_FUNC(omp_wait_deps)
llvm::Constant *CGOpenMPRuntime::Get_omp_wait_deps() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C), llvm::TypeBuilder<int32_t,
          false>::get(C), getKMPDependInfoType()->getPointerTo(),
      llvm::TypeBuilder<int32_t, false>::get(C),
      getKMPDependInfoType()->getPointerTo() };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<void, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_omp_wait_deps");
}

// Special processing for __kmpc_threadprivate_cached
// DEFAULT_GET_OPENMP_FUNC(threadprivate_cached)
llvm::Constant *CGOpenMPRuntime::Get_threadprivate_cached() {
  llvm::LLVMContext &C = CGM.getLLVMContext();
  llvm::Type *Params[] = { llvm::TypeBuilder<ident_t *, false>::get(C),
      llvm::TypeBuilder<int32_t, false>::get(C),
      llvm::TypeBuilder<void *, false>::get(C), CGM.SizeTy, llvm::TypeBuilder<
          void ***, false>::get(C) };

  llvm::FunctionType *FT = llvm::FunctionType::get(
      llvm::TypeBuilder<void *, false>::get(C), Params, false);
  return CGM.CreateRuntimeFunction(FT, "__kmpc_threadprivate_cached");
}

QualType CGOpenMPRuntime::GetAtomicType(CodeGenFunction &CGF, QualType QTy) {
  if (!QTy->isArithmeticType())
    return QualType();
  if (QTy->isRealFloatingType())
    // CGF.ConvertTypeForMem(QTy->getCanonicalTypeUnqualified());
    return QTy->getCanonicalTypeUnqualified();
  uint64_t TySize = CGF.getContext().getTypeSize(QTy);
  if (CGF.getContext().getTypeSize(CGF.getContext().CharTy) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedCharTy : CGF.getContext().SignedCharTy;
  else if (CGF.getContext().getTypeSize(CGF.getContext().ShortTy) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedShortTy : CGF.getContext().ShortTy;
  else if (CGF.getContext().getTypeSize(CGF.getContext().IntTy) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedIntTy : CGF.getContext().IntTy;
  else if (CGF.getContext().getTypeSize(CGF.getContext().LongTy) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedLongTy : CGF.getContext().LongTy;
  else if (CGF.getContext().getTypeSize(CGF.getContext().LongLongTy) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedLongLongTy : CGF.getContext().LongLongTy;
  else if (CGF.getContext().getTypeSize(CGF.getContext().Int128Ty) == TySize)
    return
        QTy->isUnsignedIntegerOrEnumerationType() ?
            CGF.getContext().UnsignedInt128Ty : CGF.getContext().Int128Ty;
  return QualType();
}

llvm::Value *CGOpenMPRuntime::GetAtomicFuncGeneral(CodeGenFunction &CGF,
    QualType QTyRes, QualType QTyIn, CGOpenMPRuntime::EAtomicOperation Aop,
    bool Capture, bool Reverse) {
  SmallString<40> Str;
  llvm::raw_svector_ostream OS(Str);

  if (QTyRes.isVolatileQualified() || QTyIn.isVolatileQualified())
    return 0;

  int64_t TySize =
      CGF.CGM.GetTargetTypeStoreSize(
    		  CGF.ConvertTypeForMem(QTyRes)).getQuantity();
  if (QTyRes->isRealFloatingType()) {
    OS << "__kmpc_atomic_float";
    if (TySize != 4 && TySize != 8 && TySize != 10 && TySize != 16)
      return 0;
  } else if (QTyRes->isScalarType()) {
    OS << "__kmpc_atomic_fixed";
    if (TySize != 1 && TySize != 2 && TySize != 4 && TySize != 8)
      return 0;
  } else
    return 0;
  OS << TySize;
  switch (Aop) {
  case OMP_Atomic_orl:
    OS << "_orl";
    break;
  case OMP_Atomic_orb:
    OS << "_orb";
    break;
  case OMP_Atomic_andl:
    OS << "_andl";
    break;
  case OMP_Atomic_andb:
    OS << "_andb";
    break;
  case OMP_Atomic_xor:
    OS << "_xor";
    break;
  case OMP_Atomic_sub:
    OS << "_sub";
    break;
  case OMP_Atomic_add:
    OS << "_add";
    break;
  case OMP_Atomic_mul:
    OS << "_mul";
    break;
  case OMP_Atomic_div:
    if (QTyRes->hasUnsignedIntegerRepresentation() || QTyRes->isPointerType()) {
      if (!CGF.getContext().hasSameType(QTyIn, QTyRes))
        return 0;
      OS << "u";
    }
    OS << "_div";
    break;
  case OMP_Atomic_min:
    OS << "_min";
    break;
  case OMP_Atomic_max:
    OS << "_max";
    break;
  case OMP_Atomic_shl:
    OS << "_shl";
    break;
  case OMP_Atomic_shr:
    if (QTyRes->hasUnsignedIntegerRepresentation() || QTyRes->isPointerType()) {
      if (!CGF.getContext().hasSameType(QTyIn, QTyRes))
        return 0;
      OS << "u";
    }
    OS << "_shr";
    break;
  case OMP_Atomic_wr:
    OS << "_wr";
    break;
  case OMP_Atomic_rd:
    OS << "_rd";
    break;
  case OMP_Atomic_assign:
    return 0;
  case OMP_Atomic_invalid:
  default:
    llvm_unreachable("Unknown atomic operation.");
  }
  if (Capture) {
    OS << "_cpt";
    if (!CGF.getContext().hasSameType(QTyIn, QTyRes))
      return 0;
  }
  if (Reverse
      && (Aop == OMP_Atomic_sub || Aop == OMP_Atomic_div
          || Aop == OMP_Atomic_shr || Aop == OMP_Atomic_shl)) {
    OS << "_rev";
    if (!CGF.getContext().hasSameType(QTyIn, QTyRes))
      return 0;
  }
  int64_t TyInSize = CGF.CGM.GetTargetTypeStoreSize(
      CGF.ConvertTypeForMem(QTyIn)).getQuantity();
  if (!CGF.getContext().hasSameType(QTyIn, QTyRes)) {
    if (QTyRes->isScalarType() && QTyIn->isRealFloatingType() && TyInSize == 8)
      OS << "_float8";
    else
      return 0;
  }
  SmallVector<llvm::Type *, 5> Params;
  Params.push_back(
      llvm::TypeBuilder<ident_t, false>::get(
    		  CGF.CGM.getLLVMContext())->getPointerTo());
  Params.push_back(CGF.Int32Ty);
  llvm::Type *Ty = CGF.ConvertTypeForMem(GetAtomicType(CGF, QTyRes));
  Params.push_back(Ty->getPointerTo());
  if (Aop != OMP_Atomic_rd)
    Params.push_back(CGF.ConvertTypeForMem(GetAtomicType(CGF, QTyIn)));
  if (Capture) {
    Params.push_back(CGF.Int32Ty);
  }
  llvm::Type *RetTy = CGF.VoidTy;
  if (Capture || Aop == OMP_Atomic_rd)
    RetTy = Ty;
  llvm::FunctionType *FunTy = llvm::FunctionType::get(RetTy, Params, false);
  return CGF.CGM.CreateRuntimeFunction(FunTy, OS.str());
}

llvm::Value *CGOpenMPRuntime::GetAtomicFunc(CodeGenFunction &CGF, QualType QTy,
    OpenMPReductionClauseOperator Op) {

  if (QTy.isVolatileQualified())
    return 0;

  EAtomicOperation Aop = OMP_Atomic_invalid;
  switch (Op) {
  case OMPC_REDUCTION_or:
    Aop = OMP_Atomic_orl;
    break;
  case OMPC_REDUCTION_bitor:
    Aop = OMP_Atomic_orb;
    break;
  case OMPC_REDUCTION_and:
    Aop = OMP_Atomic_andl;
    break;
  case OMPC_REDUCTION_bitand:
    Aop = OMP_Atomic_andb;
    break;
  case OMPC_REDUCTION_bitxor:
    Aop = OMP_Atomic_xor;
    break;
  case OMPC_REDUCTION_sub:
    Aop = OMP_Atomic_add;
    break;
  case OMPC_REDUCTION_add:
    Aop = OMP_Atomic_add;
    break;
  case OMPC_REDUCTION_mult:
    Aop = OMP_Atomic_mul;
    break;
  case OMPC_REDUCTION_min:
    Aop = OMP_Atomic_min;
    break;
  case OMPC_REDUCTION_max:
    Aop = OMP_Atomic_max;
    break;
  case OMPC_REDUCTION_custom:
    return 0;
  case OMPC_REDUCTION_unknown:
  case NUM_OPENMP_REDUCTION_OPERATORS:
    llvm_unreachable("Unknown reduction operation.");
  }
  return GetAtomicFuncGeneral(CGF, QTy, QTy, Aop, false, false);
}

/// This is a hook to enable postprocessing of the module.
void CGOpenMPRuntime::PostProcessModule(CodeGenModule &CGM) {

  if (CGM.getLangOpts().OpenMPTargetMode
      && CGM.getLangOpts().OpenMPTargetIRDump)
  CGM.getModule().dump();
  if (!CGM.getLangOpts().OpenMPTargetMode
      && CGM.getLangOpts().OpenMPHostIRDump)
    CGM.getModule().dump();
}

void CGOpenMPRuntime::PostProcessTargetFunction(const Decl *D,
                                        llvm::Function *F,
                                        const CGFunctionInfo &FI){
  CGM.SetInternalFunctionAttributes(D, F, FI);

}
void CGOpenMPRuntime::PostProcessTargetFunction(llvm::Function *F){}

static llvm::Value *GEP(CGBuilderTy &B, llvm::Value *Base, int field) {
  return B.CreateConstInBoundsGEP2_32(Base, 0, field);
}

static void StoreField(CGBuilderTy &B, llvm::Value *Val, llvm::Value *Dst,
    int field) {
  B.CreateStore(Val, GEP(B, Dst, field));
}

llvm::Value *CGOpenMPRuntime::CreateIntelOpenMPRTLLoc(SourceLocation Loc,
    CodeGenFunction &CGF, unsigned Flags) {
  llvm::Value *Tmp;
  // ident_t tmp;
  llvm::AllocaInst *AI = 0;
  llvm::BasicBlock &EntryBB = CGF.CurFn->getEntryBlock();
  std::string VarName = ".__kmpc_ident_t." + llvm::utostr(Flags) + ".";
  std::string DefaultLoc = ".omp.default.loc.";
  std::string DefaultConstName = DefaultLoc + llvm::utostr(Flags) + ".";
  llvm::Value *DefaultString;
  if (!(DefaultString = CGM.getModule().getNamedValue(DefaultLoc))) {
    DefaultString = CGF.Builder.CreateGlobalString(";unknown;unknown;0;0;;",
        DefaultLoc);
  }
  for (llvm::BasicBlock::iterator I = EntryBB.begin(), E = EntryBB.end();
      I != E; ++I)
    if (I->getName().startswith(VarName)) {
      AI = cast<llvm::AllocaInst>(I);
      break;
    }
  if (!AI) {
    llvm::StructType *StTy = llvm::IdentTBuilder::get(CGM.getLLVMContext());
    AI = CGF.CreateTempAlloca(StTy, VarName);
    AI->setAlignment(CGM.PointerAlignInBytes);
    CGBuilderTy::InsertPoint SavedIP = CGF.Builder.saveIP();
    assert(SavedIP.isSet() && "No insertion point is set!");
    CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
    llvm::Value *DefaultVal;
    if (!(DefaultVal = CGM.getModule().getNamedValue(DefaultConstName))) {
      llvm::Constant *Zero = CGF.Builder.getInt32(0);
      llvm::Value *Args[] = { Zero, Zero };
      llvm::Constant *Values[] = { Zero, CGF.Builder.getInt32(Flags), Zero,
          Zero, cast<llvm::Constant>(
              CGF.Builder.CreateInBoundsGEP(DefaultString, Args)) };
      llvm::Constant *Init = llvm::ConstantStruct::get(StTy,
          llvm::makeArrayRef(Values));
      llvm::GlobalVariable *ConstVar = new llvm::GlobalVariable(CGM.getModule(),
          StTy, true, llvm::GlobalValue::PrivateLinkage, Init,
          DefaultConstName);
      ConstVar->setUnnamedAddr(true);
      DefaultVal = ConstVar;
    }
    CGF.Builder.CreateMemCpy(AI, DefaultVal,
        llvm::ConstantExpr::getSizeOf(StTy), CGM.PointerAlignInBytes);
    CGF.Builder.restoreIP(SavedIP);
  }
  Tmp = AI;
  if (CGM.getCodeGenOpts().getDebugInfo() != CodeGenOptions::NoDebugInfo
      && Loc.isValid()) {
    PresumedLoc PLoc = CGM.getContext().getSourceManager().getPresumedLoc(Loc);
    std::string Res = ";";
    Res += PLoc.getFilename();
    Res += ";";
    if (const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(
        CGF.CurFuncDecl)) {
      Res += FD->getQualifiedNameAsString();
    }
    Res += ";";
    Res += llvm::utostr(PLoc.getLine()) + ";" + llvm::utostr(PLoc.getColumn())
        + ";;";
    // tmp.psource = ";file;func;line;col;;";
    StoreField(CGF.Builder, CGF.Builder.CreateGlobalStringPtr(Res), Tmp,
        llvm::IdentTBuilder::psource);
  } else if (CGM.getCodeGenOpts().getDebugInfo()
      != CodeGenOptions::NoDebugInfo) {
    llvm::Value *Zero = CGF.Builder.getInt32(0);
    llvm::Value *Args[] = { Zero, Zero };
    StoreField(CGF.Builder, CGF.Builder.CreateInBoundsGEP(DefaultString, Args),
        Tmp, llvm::IdentTBuilder::psource);
  }
  return Tmp;
}

llvm::Value *CGOpenMPRuntime::CreateOpenMPGlobalThreadNum(SourceLocation Loc,
    CodeGenFunction &CGF) {
  llvm::BasicBlock &EntryBB = CGF.CurFn->getEntryBlock();
  for (llvm::BasicBlock::iterator I = EntryBB.begin(), E = EntryBB.end();
      I != E; ++I)
    if (I->getName().startswith(".__kmpc_global_thread_num."))
      return CGF.Builder.CreateLoad(I, ".gtid.");
  llvm::AllocaInst *AI = CGF.CreateTempAlloca(CGM.Int32Ty,
      ".__kmpc_global_thread_num.");
  AI->setAlignment(4);
  CGBuilderTy::InsertPoint SavedIP = CGF.Builder.saveIP();
  assert(SavedIP.isSet() && "No insertion point is set!");
  CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
  llvm::Value *IdentT = CreateIntelOpenMPRTLLoc(Loc, CGF);
  llvm::Value *Res = CGF.EmitRuntimeCall(OPENMPRTL_FUNC(global_thread_num),
      llvm::makeArrayRef<llvm::Value *>(&IdentT, 1));
  CGF.Builder.CreateStore(Res, AI);
  CGF.Builder.restoreIP(SavedIP);
  return CGF.Builder.CreateLoad(AI, ".gtid.");
}

llvm::Value *CGOpenMPRuntime::CreateOpenMPThreadPrivateCached(const VarDecl *VD,
    SourceLocation Loc, CodeGenFunction &CGF, bool NoCast) {
  if (CGM.OpenMPSupport.hasThreadPrivateVar(VD)) {
    llvm::Type *VDTy = CGM.getTypes().ConvertTypeForMem(VD->getType());
    llvm::PointerType *PTy = llvm::PointerType::get(VDTy,
        CGM.getContext().getTargetAddressSpace(VD->getType()));
    CharUnits SZ = CGM.GetTargetTypeStoreSize(VDTy);
    std::string VarCache = CGM.getMangledName(VD).str() + ".cache.";

    llvm::Value *Args[] = { CreateIntelOpenMPRTLLoc(Loc, CGF),
        CreateOpenMPGlobalThreadNum(Loc, CGF), CGF.Builder.CreateBitCast(
            VD->isStaticLocal() ?
                CGM.getStaticLocalDeclAddress(VD) : CGM.GetAddrOfGlobal(VD),
            CGM.Int8PtrTy), llvm::ConstantInt::get(CGF.SizeTy,
            SZ.getQuantity()), CGM.getModule().getNamedValue(VarCache) };
    llvm::Value *Call = CGF.EmitRuntimeCall(
        OPENMPRTL_FUNC(threadprivate_cached), Args);
    if (NoCast)
      return Call;
    return CGF.Builder.CreateBitCast(Call, PTy);
  }
  return 0;
}

/// Remove dashes and other strange characters from the target triple
/// as they may cause some problems for the external symbols
static std::string LegalizeTripleString(llvm::Triple TargetTriple) {

  const std::string &TS = TargetTriple.getTriple();
  std::string S;
  llvm::raw_string_ostream OS(S);

  for (unsigned i = 0; i < TS.size(); ++i) {
    unsigned char c = (unsigned char) TS[i];

    if (c >= 'a' && c <= 'z') {
      OS << c;
      continue;
    }
    if (c >= 'A' && c <= 'Z') {
      OS << c;
      continue;
    }
    if (c >= '0' && c <= '9') {
      OS << c;
      continue;
    }
    if (c == '_' || c == '-') {
      OS << '_';
      continue;
    }

    OS << llvm::format("%02x", (unsigned) c);
  }

  return OS.str();
}

// These are hooks for NVPTX backend: nothing is generated for other backends
void CGOpenMPRuntime::GenerateTargetControlLoop(SourceLocation Loc,
    CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::GenerateFinishLabelSetting(SourceLocation Loc,
    CodeGenFunction &CGF, bool prevIsParallel) {
}

void CGOpenMPRuntime::GenerateNextLabel (CodeGenFunction &CGF,
    bool prevIsParallel, bool nextIsParallel) {
}

void CGOpenMPRuntime::GenerateIfMaster (SourceLocation Loc, CapturedStmt *CS,
    CodeGenFunction &CGF) {
}

StringRef CGOpenMPRuntime::RenameStandardFunction (StringRef name) {
  return name;
}

void CGOpenMPRuntime::SelectActiveThreads (CodeGenFunction &CGF) {
}

llvm::Value * CGOpenMPRuntime::CallParallelRegionPrepare(CodeGenFunction &CGF) {
  return 0;
}

void CGOpenMPRuntime::CallParallelRegionStart(CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::CallParallelRegionEnd(CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::CallSerializedParallelStart(CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::CallSerializedParallelEnd(CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::StartParallelRegionInTarget (CodeGenFunction &CGF) {
}

void CGOpenMPRuntime::EndParallelRegionInTarget (CodeGenFunction &CGF){
}

void CGOpenMPRuntime::SupportCritical (const OMPCriticalDirective &S,
    CodeGenFunction &CGF, llvm::Function * CurFn, llvm::GlobalVariable *Lck) {
}

void CGOpenMPRuntime::EmitNativeBarrier(CodeGenFunction &CGF) {
}

bool CGOpenMPRuntime::IsNestedParallel () {
  return false;
}

void CGOpenMPRuntime::StartNewTargetRegion() {
}

llvm::Value *
CGOpenMPRuntime::AllocateThreadLocalInfo(CodeGenFunction & CGF) {
	return 0;
}

llvm::Value *
CGOpenMPRuntime::GetNextIdIncrement(CodeGenFunction &CGF,
      bool IsStaticSchedule, const Expr * ChunkSize, llvm::Value * Chunk,
      llvm::Type * IdxTy, QualType QTy, llvm::Value * Idx,
      OpenMPDirectiveKind Kind, OpenMPDirectiveKind SKind, llvm::Value * PSt) {
  return 0;
}

bool CGOpenMPRuntime::requiresMicroTaskForTeams(){
  return true;
}
bool CGOpenMPRuntime::requiresMicroTaskForParallel(){
  return true;
}



///===---------------
///
/// Generate target regions descriptor
///
///===---------------

/// Return a pointer to the device image begin.
///
llvm::Constant* CGOpenMPRuntime::GetDeviceImageBeginPointer(
                                                    llvm::Triple TargetTriple){
  return new llvm::GlobalVariable(
          CGM.getModule(),
          CGM.Int8Ty,
          true,
          llvm::GlobalValue::ExternalLinkage,
          0,
          Twine("__omptgt__img_start_")
          + Twine(LegalizeTripleString(TargetTriple)));
}

/// Return a pointer to the device image end.
///
llvm::Constant* CGOpenMPRuntime::GetDeviceImageEndPointer(
                                                    llvm::Triple TargetTriple){
  return new llvm::GlobalVariable(
          CGM.getModule(),
          CGM.Int8Ty,
          true,
          llvm::GlobalValue::ExternalLinkage,
          0,
          Twine("__omptgt__img_end_")
          + Twine(LegalizeTripleString(TargetTriple)));
}

/// Return a string with the mangled name of a target region for the given
/// module and target region index
///
std::string CGOpenMPRuntime::GetOffloadEntryMangledName(
    llvm::Triple TargetTriple) {

  std::string S;
  llvm::raw_string_ostream OS(S);

  // append the module unique region index
  OS << "__omptgt__"
     << NumTargetRegions << '_'
     << CGM.getLangOpts().OMPModuleUniqueID << '_';

  return OS.str();
}

/// Return a string with the mangled name of a global host entry for the given
/// module
///
std::string CGOpenMPRuntime::GetTargetGlobalMangledName(
    llvm::Triple TargetTriple) {

  std::string S;
  llvm::raw_string_ostream OS(S);

  // append the module unique region index
  OS << "__omptgt__gbl__"
     << NumTargetGlobals << '_'
     << CGM.getLangOpts().OMPModuleUniqueID << '_';

  return OS.str();
}

/// Return the target regions descriptor or a create a new
/// one if if does not exist
///
llvm::Constant* CGOpenMPRuntime::GetTargetRegionsDescriptor(){

  // If we created the target regions descriptor before, just return it
  if (TargetRegionsDescriptor)
    return TargetRegionsDescriptor;

  assert(!CGM.getLangOpts().OpenMPTargetMode
      && "Generating offload descriptor for target code??");

  llvm::LLVMContext &C = CGM.getModule().getContext();
  llvm::Module &M = CGM.getModule();

  //Get list of devices we care about
  const std::vector<llvm::Triple> &Devices = CGM.getLangOpts().OMPTargetTriples;

  assert(Devices.size()
      && "No devices specified while running in target mode??");

  //Type of target regions descriptor
  llvm::StructType *DescTy = llvm::TypeBuilder<__tgt_bin_desc, true>::get(C);
  //Type of device image
  llvm::StructType *DevTy = llvm::TypeBuilder<__tgt_device_image, true>::get(C);
  //Type of offload entry
  llvm::StructType *EntryTy = llvm::TypeBuilder<__tgt_offload_entry, true>::get(C);

  //No devices: return a null pointer
  if (Devices.empty())
    return llvm::ConstantExpr::getBitCast(
        llvm::Constant::getNullValue(llvm::Type::getInt8PtrTy(C)),
        DescTy->getPointerTo());

  //Create the external vars that will point to the begin and end of the
  //host entries section.
  //
  // FIXME: The names of these globals need to be consistent with the linker.
  // Maybe make the runtime class to return these strings

  llvm::GlobalVariable *HostEntriesBegin = new llvm::GlobalVariable(
      M, EntryTy, true, llvm::GlobalValue::ExternalLinkage, 0,
      "__omptgt__host_entries_begin");
  llvm::GlobalVariable *HostEntriesEnd = new llvm::GlobalVariable(
      M, EntryTy, true, llvm::GlobalValue::ExternalLinkage, 0,
      "__omptgt__host_entries_end");

  //Create all device images
  llvm::SmallVector<llvm::Constant*,4> DeviceImagesEntires;

  for (unsigned i=0; i<Devices.size(); ++i){
    llvm::Constant *Dev = llvm::ConstantStruct::get(DevTy,
        CGM.getOpenMPRuntime().GetDeviceImageBeginPointer(Devices[i]),
        CGM.getOpenMPRuntime().GetDeviceImageEndPointer(Devices[i]),
        HostEntriesBegin, HostEntriesEnd, nullptr);
    DeviceImagesEntires.push_back(Dev);
  }

  //Create device images global array
  llvm::ArrayType *DeviceImagesInitTy =
      llvm::ArrayType::get(DevTy,DeviceImagesEntires.size());
  llvm::Constant *DeviceImagesInit = llvm::ConstantArray::get(
      DeviceImagesInitTy,DeviceImagesEntires);

  llvm::GlobalVariable *DeviceImages = new llvm::GlobalVariable(
      M,
      DeviceImagesInitTy,
      true,
      llvm::GlobalValue::InternalLinkage,
      DeviceImagesInit,
      "__omptgt__device_images");

  //This is a Zero array to be used in the creation of the constant expressions
  llvm::Constant *Index[] = { llvm::Constant::getNullValue(CGM.Int32Ty),
                              llvm::Constant::getNullValue(CGM.Int32Ty)};

  //Create the target region descriptor:
  // - number of devices
  // - pointer to the devices array
  // - begin of host entries point
  // - end of host entries point
  llvm::Constant *TargetRegionsDescriptorInit = llvm::ConstantStruct::get(
      DescTy,
      llvm::ConstantInt::get(CGM.Int32Ty, Devices.size()),
      llvm::ConstantExpr::getGetElementPtr(DeviceImages,Index),
      HostEntriesBegin, HostEntriesEnd, nullptr);

  TargetRegionsDescriptor = new llvm::GlobalVariable(
        M,
        DescTy,
        true,
        llvm::GlobalValue::InternalLinkage,
        TargetRegionsDescriptorInit,
        "__omptgt__target_regions_descriptor");

  return TargetRegionsDescriptor;

}

/// Return host pointer for the current target regions. This creates
/// the offload entry for the target region.
///
void CGOpenMPRuntime::CreateHostPtrForCurrentTargetRegionTD(const Decl *D,
                                                          llvm::Function *Fn){

  llvm::LLVMContext &C = CGM.getModule().getContext();
  llvm::Module &M = CGM.getModule();

  // FIXME: we probably can set this to null as the entries creation is done
  // right after the outlined target function is created. Therefore, it is
  // always okay to add it to the end of the global variables list.
  llvm::GlobalVariable *Successor = getNextGlobalVarForEntryDeclaration(D);

  std::string Name = GetOffloadEntryMangledName(llvm::Triple());

  // Create the unique host pointer for a target region. We do not use the
  // outlined function address in the host so that it can be inlined by the
  // optimizer if appropriate.
  // In the offloading scheme, the content being pointed by this pointer is not
  // relevant. Nevertheless, we fill this content with a string that
  // correspond to the entries' name. This information can be
  // useful for some targets to expedite the runtime look-up of the entries
  // in the target image. In order to use this information the target OpenMP
  // codegen class should encode the host entries in his image.
  //
  // However, for the target code we use the function pointer since it can be
  // used to more quickly load the target functions by the runtime if it can
  // rely on the order of the entries.

  llvm::Constant *FuncPtr = llvm::ConstantExpr::getBitCast(Fn, CGM.VoidPtrTy);
  llvm::Constant *StrPtrInit = llvm::ConstantDataArray::getString(C,Name);

  llvm::GlobalVariable *Str = new llvm::GlobalVariable(
      M,
      StrPtrInit->getType(),
      true,
      llvm::GlobalValue::InternalLinkage,
      StrPtrInit,
      Twine(Name) + Twine("_entry_name"),
      Successor);


  llvm::Constant *StrPtr = llvm::ConstantExpr::getBitCast(Str,CGM.Int8PtrTy);

  // Create the entry struct
  // - pointer
  // - name
  // - size - we assume size zero for functions

  // Type of the entry
  llvm::StructType *EntryTy = llvm::TypeBuilder<__tgt_offload_entry, true>::get(C);

  llvm::Constant *EntryInit = llvm::ConstantStruct::get(EntryTy, FuncPtr, StrPtr,
          llvm::ConstantInt::get(CGM.Int64Ty, 0), NULL);

  llvm::GlobalVariable *Entry = new llvm::GlobalVariable(
      M,
      EntryTy,
      true,
      llvm::GlobalValue::ExternalLinkage,
      EntryInit,
      Twine(Name) + Twine("_entry"),
      Successor);

  // The entry has to be created in the section the linker expects it to be
  Entry->setSection(".openmptgt_host_entries");
  // We can't have any padding between symbols, so we need to have 1-byte
  // alignment
  Entry->setAlignment(1);

  // Record the GV associated with the provided declaration or the generated
  // entry if no GV was produced.
  registerGlobalVarForEntryDeclaration(D,Str);
  registerEntryForDirectiveWithTarget(D, Fn);

  return;
}
void CGOpenMPRuntime::CreateHostPtrForCurrentTargetRegion(const Decl *D,
                                                          llvm::Function *Fn){
  CreateHostPtrForCurrentTargetRegionTD(D,Fn);
  ++NumTargetRegions;
}

/// Creates the host entry for a given global and places it in the entries
/// reserved section
///
void CGOpenMPRuntime::CreateHostEntryForTargetGlobalTD(const Decl *D,
                                                     llvm::GlobalVariable* GV,
                                                     llvm::GlobalVariable*Succ){

  // If this is not an entry declaration we do not need to do anything
  if (!isEntryDeclaration(D))
    return;

  llvm::LLVMContext &C = CGM.getModule().getContext();
  llvm::Module &M = CGM.getModule();

  std::string Name = GetTargetGlobalMangledName(llvm::Triple());

  llvm::Constant *StrPtrInit =
      llvm::ConstantDataArray::getString(C,GV->getName());

  llvm::GlobalVariable *Str = new llvm::GlobalVariable(
      M,
      StrPtrInit->getType(),
      true,
      llvm::GlobalValue::InternalLinkage,
      StrPtrInit,
      Twine(Name) + Twine("_entry_name"),
      Succ);

  llvm::Constant *StrPtr = llvm::ConstantExpr::getBitCast(Str,CGM.Int8PtrTy);

  // Create the entry struct
  // - pointer
  // - name
  // - size - we get the size of the global based on the datalayout

  // Type of the entry
  llvm::StructType *EntryTy = llvm::TypeBuilder<__tgt_offload_entry, true>::get(C);

  llvm::Constant *EntryInit = llvm::ConstantStruct::get(EntryTy,
          llvm::ConstantExpr::getBitCast(GV,CGM.VoidPtrTy),
          StrPtr,
          llvm::ConstantInt::get(
              CGM.Int64Ty,
              CGM.getDataLayout().getTypeStoreSize(
                  GV->getType()->getPointerElementType())),
          NULL);

  llvm::GlobalVariable *Entry = new llvm::GlobalVariable(
      M,
      EntryTy,
      true,
      llvm::GlobalValue::ExternalLinkage,
      EntryInit,
      Twine(Name) + Twine("_entry"),
      Succ);

  // The entry has to be created in the section the linker expects it to be
  Entry->setSection(".openmptgt_host_entries");
  // We can't have any padding between symbols, so we need to have 1-byte
  // alignment
  Entry->setAlignment(1);

  // Record the new entry associated with the provided declaration
  registerGlobalVarForEntryDeclaration(D,GV);

  return;
}
void CGOpenMPRuntime::CreateHostEntryForTargetGlobal(const Decl *D,
                                                     llvm::GlobalVariable* GV,
                                                     llvm::GlobalVariable*Succ){
  CreateHostEntryForTargetGlobalTD(D,GV,Succ);
  ++NumTargetGlobals;
}

llvm::Value * CGOpenMPRuntime::Get_omp_get_num_threads() {
    return CGM.CreateRuntimeFunction(
       llvm::TypeBuilder<omp_get_num_threads, false>::get(
           CGM.getLLVMContext()), "omp_get_num_threads");
  }

llvm::Value * CGOpenMPRuntime::Get_omp_get_num_teams() {
    return CGM.CreateRuntimeFunction(
       llvm::TypeBuilder<omp_get_num_teams, false>::get(
           CGM.getLLVMContext()), "omp_get_num_teams");
  }


///===---------------
///
/// NVPTX OpenMP Runtime Implementation
///
///===---------------

/// Target specific runtime hacks
class CGOpenMPRuntime_NVPTX: public CGOpenMPRuntime {

  StringRef ArchName;

  // this is the identifier of a master thread, either in a block, warp or
  // entire grid, for each dimension (e.g. threadIdx.x, y and z)
  unsigned MASTER_ID;

  // type of thread local info (will be stored in loc variable)
  llvm::StructType *LocalThrTy;

  // Master and others label used by the master to control execution of threads
  // in same team
  llvm::GlobalVariable * masterLabelShared;
  llvm::GlobalVariable * othersLabelShared;

  // region labels associated to basic blocks and id generator
  std::vector<llvm::BasicBlock *> regionLabelMap;
  unsigned nextId;

  // there is one case in the following switch for each parallel and sequential
  // region
  llvm::SwitchInst * InspectorExecutorSwitch;

  // Starting and ending blocks for control-loop
  llvm::BasicBlock * startControl;
  llvm::BasicBlock * endControl;

  // finished is private to each thread and controls ends of control-loop
  llvm::AllocaInst * finishedVar;

  // this is private to each thread and is assigned at each iteration of
  // the control-loop to either masterLabelSahred or othersLabelShared
  llvm::AllocaInst * nextLabelVar;

  // first region in the control-loop (assume it is sequential)
  // and last one
  llvm::BasicBlock * sequentialStartBlock;
  llvm::BasicBlock * checkFinished;

  // labels that need to be shared amongst basic blocks
  int idleLabel;
  int finishedBlockLabel;

  // only one parallel region is currently activated as parallel in nvptx,
  // the others are just serialized (use a stack)
  typedef llvm::SmallVector<bool, 16> NestedParallelStackTy;
  NestedParallelStackTy NestedParallelStack;

  llvm::GlobalVariable * getMasterLabelShared () const
     {return masterLabelShared;}

   void setMasterLabelShared (llvm::GlobalVariable * _masterLabelShared) {
     masterLabelShared = _masterLabelShared;
   }

   llvm::GlobalVariable * getOthersLabelShared () const {
     return othersLabelShared;
   }

   void setOthersLabelShared (llvm::GlobalVariable * _othersLabelShared) {
     othersLabelShared = _othersLabelShared;
   }

   // Return basic block corresponding to label
   llvm::BasicBlock * getBasicBlockByLabel (unsigned label) const {
     return regionLabelMap[label];
   }

   // Return a reference to the entire regionLabelMap
   std::vector<llvm::BasicBlock *>& getRegionLabelMap () {
     return regionLabelMap;
   }

   llvm::BasicBlock * getEndControlBlock () const { return endControl; }

   llvm::BasicBlock * getCheckFinished () const { return checkFinished; }

   llvm::BasicBlock * getSequentialStartBlock () const {
     return sequentialStartBlock;
   }

  llvm::Function * Get_num_teams() {
    return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
        llvm::Intrinsic::nvvm_read_ptx_sreg_nctaid_x);
  }
  llvm::Function * Get_team_num() {
    return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
        llvm::Intrinsic::nvvm_read_ptx_sreg_ctaid_x);
  }
  llvm::Function * Get_num_threads() {
    return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
        llvm::Intrinsic::nvvm_read_ptx_sreg_ntid_x);
  }
  llvm::Function * Get_thread_num() {
    return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
        llvm::Intrinsic::nvvm_read_ptx_sreg_tid_x);
  }

  llvm::Function * Get_syncthreads () {
    return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
          llvm::Intrinsic::nvvm_barrier0);
  }

  // generate llvm.nvvm.ptr.gen.to.local.*
  llvm::Function * Get_ConvGenericPtrToLocal (llvm::Type * convType) {
    llvm::Type * args [] = {convType, convType};
      return llvm::Intrinsic::getDeclaration(&CGM.getModule(),
            llvm::Intrinsic::nvvm_ptr_gen_to_global, makeArrayRef(args));
    }

  // TODO: replace std vector with a working map and use index!!
  int AddNewRegionLabel (llvm::BasicBlock * bb) {
    regionLabelMap.push_back(bb);
    return nextId++;
  }

  int AddNewRegionLabelAndSwitchCase (llvm::BasicBlock * bb,
      CodeGenFunction &CGF) {
    regionLabelMap.push_back(bb);

    // TODO: make sure that the CGF is set to the proper block...if it is needed
    InspectorExecutorSwitch->addCase(CGF.Builder.getInt32(nextId), bb);

    return nextId++;
  }

  bool NextOnParallelStack () {
      return NestedParallelStack.back();
  }

  void PushNewParallelRegion (bool IsParallel) {
    NestedParallelStack.push_back(IsParallel);
  }

  bool PopParallelRegion () {
    bool cont = NextOnParallelStack();
    NestedParallelStack.pop_back();
    return cont;
  }

  bool IsNestedParallel () {
    return NextOnParallelStack();
  }

  // For NVTPX the control loop is generated when a target construct is found
  void GenerateTargetControlLoop(SourceLocation Loc,
      CodeGenFunction &CGF) {

    CGBuilderTy &Bld = CGF.Builder;

    // 32 bits should be enough to represent the number of basic
    // blocks in a target region
    llvm::IntegerType *VarTy = CGM.Int32Ty;

    // 3 is shared memory address space for nvptx backend
    llvm::GlobalVariable * masterLabelShared = new llvm::GlobalVariable(
        CGM.getModule(), VarTy, false,
        llvm::GlobalValue::CommonLinkage,
        llvm::Constant::getNullValue(Bld.getInt32Ty()), "masterLabel", 0,
        llvm::GlobalVariable::NotThreadLocal, SHARED_ADDRESS_SPACE, false);

    llvm::GlobalVariable * othersLabelShared = new llvm::GlobalVariable(
        CGM.getModule(), VarTy, false,llvm::GlobalValue::CommonLinkage,
        //llvm::GlobalValue::PrivateLinkage,
        llvm::Constant::getNullValue(Bld.getInt32Ty()), "othersLabel", 0,
        llvm::GlobalVariable::NotThreadLocal, SHARED_ADDRESS_SPACE, false);

    setMasterLabelShared(masterLabelShared);
    setOthersLabelShared(othersLabelShared);

    // first create all basic blocks in the initial right order
    // initialize labels (master only)
    llvm::BasicBlock * initLabelsBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".init.labels", CGF.CurFn);

    llvm::BasicBlock * fallThroughInitMaster = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".fall.through.init.master", CGF.CurFn);

    startControl = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".start.control", CGF.CurFn);

    llvm::BasicBlock * masterNextLabelBlock = llvm::BasicBlock::Create(
          CGM.getLLVMContext(), ".master.next.label", CGF.CurFn);

    llvm::BasicBlock * othersNextLabelBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".others.next.label", CGF.CurFn);

    llvm::BasicBlock * switchBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".switch", CGF.CurFn);

    // start by adding a first sequential region (may be empty) and the idle
    // case and initialize labels to execute them

    sequentialStartBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".seq.start", CGF.CurFn);

    llvm::BasicBlock * idleBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".idle", CGF.CurFn);

    llvm::BasicBlock * finishedBlock = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".finished", CGF.CurFn);

    checkFinished = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".check.finish.control.loop", CGF.CurFn);

    endControl = llvm::BasicBlock::Create(
        CGM.getLLVMContext(), ".end.control", CGF.CurFn);

    // convention: 0 is always IDLE (first time we add, ID is 0)
    idleLabel = AddNewRegionLabel (idleBlock);
    int firstSeqLabel = AddNewRegionLabel (sequentialStartBlock);
    finishedBlockLabel = AddNewRegionLabel (finishedBlock);

    assert (idleLabel == 0 &&
        "switch region in control loop not empty at initialization");

    // Initialization of labels by master *before* control-loop
    llvm::Value * callThreadNum = Bld.CreateCall(Get_thread_num());
    llvm::Value * amIMasterCond = Bld.CreateICmpEQ(callThreadNum,
           Bld.getInt32(MASTER_ID), "amIMaster");

    Bld.CreateCondBr(amIMasterCond, initLabelsBlock, fallThroughInitMaster);

    Bld.SetInsertPoint(initLabelsBlock);

    Bld.CreateStore (Bld.getInt32(firstSeqLabel), getMasterLabelShared ());
    Bld.CreateStore (Bld.getInt32(idleLabel), getOthersLabelShared ());


    Bld.CreateBr(fallThroughInitMaster);

    Bld.SetInsertPoint(fallThroughInitMaster);

    Bld.CreateCall(Get_syncthreads());

    nextLabelVar = Bld.CreateAlloca(Bld.getInt32Ty(), Bld.getInt32(1),
        "nextLabel");

    llvm::Value * nextLabelLocal = Bld.CreateAddrSpaceCast(nextLabelVar,
        llvm::Type::getInt32PtrTy(Bld.getContext(), 5), "nextLabelVarPtrLocal");

    // build the actual control loop: this can be a do {} while because
    // we will always execute at least one case of the switch
    // (i.e. finished case)

    // finished boolean controlling the while: create and init to false
    finishedVar =
          Bld.CreateAlloca(Bld.getInt1Ty(), Bld.getInt32(1), "finished");

    // create pointer to local for finishedVar
    llvm::Value * finishedVarLocal = Bld.CreateAddrSpaceCast(finishedVar,
            llvm::Type::getInt1PtrTy(Bld.getContext(), 5), "finishedVarPtrLocal");

    Bld.CreateStore(Bld.getFalse(), finishedVarLocal);

    Bld.CreateBr(startControl);

    Bld.SetInsertPoint(startControl);

    // main control-loop contains: synchronization,
    // if-master for nextLabel allocation & selection
    Bld.CreateCall(Get_syncthreads());

    llvm::Value * callThreadNumInLoop = Bld.CreateCall(Get_thread_num());
    llvm::Value * amIMasterCondInLoop = Bld.CreateICmpEQ(callThreadNumInLoop,
           Bld.getInt32(MASTER_ID), "amIMaster");


    Bld.CreateCondBr(amIMasterCondInLoop, masterNextLabelBlock,
        othersNextLabelBlock);

    // set next label for master
    Bld.SetInsertPoint(masterNextLabelBlock);

    Bld.CreateStore(Bld.CreateLoad(masterLabelShared), nextLabelLocal);

    Bld.CreateBr(switchBlock);

    // set next label for all the others
    Bld.SetInsertPoint(othersNextLabelBlock);

    Bld.CreateStore(Bld.CreateLoad(othersLabelShared), nextLabelLocal);

    Bld.CreateBr(switchBlock);

    Bld.SetInsertPoint(switchBlock);

    // initialize switch command in switchBlock and add initial cases
    // further cases will be added later in addNewRegionLabel

    // TODO: room for improvement is hint number of cases expected and
    // set high probability for idle case
    InspectorExecutorSwitch = Bld.CreateSwitch(
        Bld.CreateLoad(nextLabelLocal), checkFinished);


    InspectorExecutorSwitch->addCase(Bld.getInt32(idleLabel), idleBlock);
    InspectorExecutorSwitch->addCase(Bld.getInt32(firstSeqLabel),
        sequentialStartBlock);
    InspectorExecutorSwitch->addCase(Bld.getInt32(finishedBlockLabel),
        finishedBlock);

    // fill up idle block with just a branch to check finished
    Bld.SetInsertPoint(idleBlock);

    Bld.CreateBr(checkFinished);

    // fill up the finished block
    Bld.SetInsertPoint(finishedBlock);

    Bld.CreateStore ( Bld.getTrue(), finishedVarLocal);

    // jump to check block of control loop
    Bld.CreateBr(checkFinished);

    Bld.SetInsertPoint(checkFinished);
    llvm::Value * finishedLoad = Bld.CreateLoad(finishedVarLocal);
    llvm::Value * isFinished = Bld.CreateICmpEQ(finishedLoad, Bld.getTrue());

    // go to end if finishedVar was set to true
    Bld.CreateCondBr(isFinished, endControl, startControl);

    // the switch is generated once we will know all blocks in it
    // for now, set code generation at first sequential block
    Bld.SetInsertPoint(sequentialStartBlock);

    // first thing: initialize the state of the OpenMP rt library on the GPU
    CGF.EmitRuntimeCall(OPENMPRTL_FUNC(kernel_init));
  }

  // \brief For NVTPX generate label setting when closing
  // a target region
   void GenerateFinishLabelSetting(SourceLocation Loc,
       CodeGenFunction &CGF, bool prevIsParallel) {
     CGBuilderTy &Bld = CGF.Builder;

     // Master selects the next labels for everyone
     // if we are generating code in a parallel region,
     // we need to select the master only
     llvm::BasicBlock * nextLabelsBlock;
     llvm::BasicBlock * fallThroughNextLabelsMaster;
     if (prevIsParallel) {
       nextLabelsBlock = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".next.labels", CGF.CurFn);

       fallThroughNextLabelsMaster = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".fall.through.next.labels.master", CGF.CurFn);

       llvm::Value * callThreadNum = Bld.CreateCall(Get_thread_num());
       llvm::Value * amIMasterCond = Bld.CreateICmpEQ(callThreadNum,
              Bld.getInt32(MASTER_ID), "amIMaster");

       Bld.CreateCondBr(amIMasterCond, nextLabelsBlock,
           fallThroughNextLabelsMaster);

       Bld.SetInsertPoint(nextLabelsBlock);
     }

     Bld.CreateStore (Bld.getInt32(finishedBlockLabel), getMasterLabelShared ());
     Bld.CreateStore (Bld.getInt32(finishedBlockLabel), getOthersLabelShared ());

     // again, only done if previous region was a parallel one
     if (prevIsParallel) {
       Bld.CreateBr(fallThroughNextLabelsMaster);

       Bld.SetInsertPoint(fallThroughNextLabelsMaster);
     }

     // close current block to check if we are done with the control loop
     Bld.CreateBr(checkFinished);

     // after setting the finish label we can jump to generate code in the end
     // block because we will only have a return in there and this is specific
     // to NVPTX only
     llvm::BasicBlock * endBlock = getEndControlBlock();

     CGF.Builder.SetInsertPoint(endBlock);
   }

   void GenerateNextLabel (CodeGenFunction &CGF, bool prevIsParallel,
       bool nextIsParallel) {

     CGBuilderTy &Bld = CGF.Builder;

     // create new basic block for next region, get a new label for it
     // and add it to the switch
     const std::string nextRegionName = nextIsParallel? ".par.reg" : ".seq.reg";
     llvm::BasicBlock * nextRegionBlock = llvm::BasicBlock::Create(
         CGM.getLLVMContext(), nextRegionName, CGF.CurFn);

     int nextLabel = AddNewRegionLabel (nextRegionBlock);
     InspectorExecutorSwitch->addCase(Bld.getInt32(nextLabel), nextRegionBlock);

     // Master selects the next labels for everyone
     // if we are generating code in a parallel region,
     // we need to select the master only
     llvm::BasicBlock * nextLabelsBlock;
     llvm::BasicBlock * fallThroughNextLabelsMaster;
     if (prevIsParallel) {
       nextLabelsBlock = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".next.labels", CGF.CurFn);

       fallThroughNextLabelsMaster = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".fall.through.next.labels.master", CGF.CurFn);

       llvm::Value * callThreadNumInLoop = Bld.CreateCall(Get_thread_num());
       llvm::Value * amIMasterCondInLoop = Bld.CreateICmpEQ(callThreadNumInLoop,
              Bld.getInt32(MASTER_ID), "amIMaster");

       Bld.CreateCondBr(amIMasterCondInLoop, nextLabelsBlock,
           fallThroughNextLabelsMaster);

       Bld.SetInsertPoint(nextLabelsBlock);
     }

     // if next is sequential, non-master need to go to idle
     Bld.CreateStore (Bld.getInt32(nextLabel), getMasterLabelShared ());

     if (nextIsParallel)
       Bld.CreateStore (Bld.getInt32(nextLabel), getOthersLabelShared ());
     else
       Bld.CreateStore (Bld.getInt32(idleLabel), getOthersLabelShared ());

     // again, only done if previous region was a parallel one
     if (prevIsParallel) {
       Bld.CreateBr(fallThroughNextLabelsMaster);

       Bld.SetInsertPoint(fallThroughNextLabelsMaster);
     }

     // no need to wait for master, we are going back to the main
     // synchronization point at the beginning of the control loop
     Bld.CreateBr(checkFinished);

     // start inserting new region statements into next switch case
     Bld.SetInsertPoint(nextRegionBlock);
   }

   void GenerateIfMaster (SourceLocation Loc, CapturedStmt *CS,
        CodeGenFunction &CGF)
  {
    CGBuilderTy &Bld = CGF.Builder;

    llvm::BasicBlock * ifMasterBlock = llvm::BasicBlock::Create(
             CGM.getLLVMContext(), ".if.master", CGF.CurFn);

    llvm::BasicBlock * fallThroughMaster = llvm::BasicBlock::Create(
                CGM.getLLVMContext(), ".fall.through.master", CGF.CurFn);

    llvm::Value * callThreadNum = Bld.CreateCall(Get_thread_num());
    llvm::Value * amIMasterCond = Bld.CreateICmpEQ(callThreadNum,
          Bld.getInt32(MASTER_ID), "amIMaster");

    Bld.CreateCondBr(amIMasterCond, ifMasterBlock, fallThroughMaster);

    Bld.SetInsertPoint(ifMasterBlock);

    CGF.EmitStmt(CS->getCapturedStmt());

    Bld.CreateBr(fallThroughMaster);

    Bld.SetInsertPoint(fallThroughMaster);

    Bld.CreateCall(Get_syncthreads());
  }

   llvm::StringMap<StringRef> stdFuncs;

   StringRef RenameStandardFunction (StringRef name) {

     // Fill up hashmap entries lazily
     if (stdFuncs.empty()) {

       // Trigonometric functions
       stdFuncs.GetOrCreateValue("cos", "__nv_cos");
       stdFuncs.GetOrCreateValue("sin", "__nv_sin");
       stdFuncs.GetOrCreateValue("tan", "__nv_tan");
       stdFuncs.GetOrCreateValue("acos", "__nv_acos");
       stdFuncs.GetOrCreateValue("asin", "__nv_asin");
       stdFuncs.GetOrCreateValue("atan", "__nv_atan");
       stdFuncs.GetOrCreateValue("atan2", "__nv_atan2");

       stdFuncs.GetOrCreateValue("cosf", "__nv_cosf");
       stdFuncs.GetOrCreateValue("sinf", "__nv_sinf");
       stdFuncs.GetOrCreateValue("tanf", "__nv_tanf");
       stdFuncs.GetOrCreateValue("acosf", "__nv_acosf");
       stdFuncs.GetOrCreateValue("asinf", "__nv_asinf");
       stdFuncs.GetOrCreateValue("atanf", "__nv_atanf");
       stdFuncs.GetOrCreateValue("atan2f", "__nv_atan2f");

       // Hyperbolic functions
       stdFuncs.GetOrCreateValue("cosh", "__nv_cosh");
       stdFuncs.GetOrCreateValue("sinh", "__nv_sinh");
       stdFuncs.GetOrCreateValue("tanh", "__nv_tanh");
       stdFuncs.GetOrCreateValue("acosh", "__nv_acosh");
       stdFuncs.GetOrCreateValue("asinh", "__nv_asinh");
       stdFuncs.GetOrCreateValue("atanh", "__nv_atanh");

       stdFuncs.GetOrCreateValue("coshf", "__nv_coshf");
       stdFuncs.GetOrCreateValue("sinhf", "__nv_sinhf");
       stdFuncs.GetOrCreateValue("tanhf", "__nv_tanhf");
       stdFuncs.GetOrCreateValue("acoshf", "__nv_acoshf");
       stdFuncs.GetOrCreateValue("asinhf", "__nv_asinhf");
       stdFuncs.GetOrCreateValue("atanhf", "__nv_atanhf");

       // Exponential and logarithm functions
       stdFuncs.GetOrCreateValue("exp", "__nv_exp");
       stdFuncs.GetOrCreateValue("frexp", "__nv_frexp");
       stdFuncs.GetOrCreateValue("ldexp", "__nv_ldexp");
       stdFuncs.GetOrCreateValue("log", "__nv_log");
       stdFuncs.GetOrCreateValue("log10", "__nv_log10");
       stdFuncs.GetOrCreateValue("modf", "__nv_modf");
       stdFuncs.GetOrCreateValue("exp2", "__nv_exp2");
       stdFuncs.GetOrCreateValue("expm1", "__nv_expm1");
       stdFuncs.GetOrCreateValue("ilogb", "__nv_ilogb");
       stdFuncs.GetOrCreateValue("log1p", "__nv_log1p");
       stdFuncs.GetOrCreateValue("log2", "__nv_log2");
       stdFuncs.GetOrCreateValue("logb", "__nv_logb");
       stdFuncs.GetOrCreateValue("scalbn", "__nv_scalbn");
//     map.GetOrCreateValue("scalbln", "");

       stdFuncs.GetOrCreateValue("expf", "__nv_exp");
       stdFuncs.GetOrCreateValue("frexpf", "__nv_frexpf");
       stdFuncs.GetOrCreateValue("ldexpf", "__nv_ldexpf");
       stdFuncs.GetOrCreateValue("logf", "__nv_logf");
       stdFuncs.GetOrCreateValue("log10f", "__nv_log10f");
       stdFuncs.GetOrCreateValue("modff", "__nv_modff");
       stdFuncs.GetOrCreateValue("exp2f", "__nv_exp2f");
       stdFuncs.GetOrCreateValue("expm1f", "__nv_expm1f");
       stdFuncs.GetOrCreateValue("ilogbf", "__nv_ilogbf");
       stdFuncs.GetOrCreateValue("log1pf", "__nv_log1pf");
       stdFuncs.GetOrCreateValue("log2f", "__nv_log2f");
       stdFuncs.GetOrCreateValue("logbf", "__nv_logbf");
       stdFuncs.GetOrCreateValue("scalbnf", "__nv_scalbnf");
//     map.GetOrCreateValue("scalblnf", "");

       // Power functions
       stdFuncs.GetOrCreateValue("pow", "__nv_pow");
       stdFuncs.GetOrCreateValue("sqrt", "__nv_sqrt");
       stdFuncs.GetOrCreateValue("cbrt", "__nv_cbrt");
       stdFuncs.GetOrCreateValue("hypot", "__nv_hypot");

       stdFuncs.GetOrCreateValue("powf", "__nv_powf");
       stdFuncs.GetOrCreateValue("sqrtf", "__nv_sqrtf");
       stdFuncs.GetOrCreateValue("cbrtf", "__nv_cbrtf");
       stdFuncs.GetOrCreateValue("hypotf", "__nv_hypotf");

       // Error and gamma functions
       stdFuncs.GetOrCreateValue("erf", "__nv_erf");
       stdFuncs.GetOrCreateValue("erfc", "__nv_erfc");
       stdFuncs.GetOrCreateValue("tgamma", "__nv_tgamma");
       stdFuncs.GetOrCreateValue("lgamma", "__nv_lgamma");

       stdFuncs.GetOrCreateValue("erff", "__nv_erff");
       stdFuncs.GetOrCreateValue("erfcf", "__nv_erfcf");
       stdFuncs.GetOrCreateValue("tgammaf", "__nv_tgammaf");
       stdFuncs.GetOrCreateValue("lgammaf", "__nv_lgammaf");

       // Rounding and remainder functions
       stdFuncs.GetOrCreateValue("ceil", "__nv_ceil");
       stdFuncs.GetOrCreateValue("floor", "__nv_floor");
       stdFuncs.GetOrCreateValue("fmod", "__nv_fmod");
       stdFuncs.GetOrCreateValue("trunc", "__nv_trunc");
       stdFuncs.GetOrCreateValue("round", "__nv_round");
       stdFuncs.GetOrCreateValue("lround", "__nv_lround");
       stdFuncs.GetOrCreateValue("llround", "__nv_llround");
       stdFuncs.GetOrCreateValue("rint", "__nv_rint");
       stdFuncs.GetOrCreateValue("lrint", "__nv_lrint");
       stdFuncs.GetOrCreateValue("llrint", "__nv_llrint");
       stdFuncs.GetOrCreateValue("nearbyint", "__nv_nearbyint");
       stdFuncs.GetOrCreateValue("remainder", "__nv_remainder");
       stdFuncs.GetOrCreateValue("remquo", "__nv_remquo");

       stdFuncs.GetOrCreateValue("ceilf", "__nv_ceilf");
       stdFuncs.GetOrCreateValue("floorf", "__nv_floorf");
       stdFuncs.GetOrCreateValue("fmodf", "__nv_fmodf");
       stdFuncs.GetOrCreateValue("truncf", "__nv_truncf");
       stdFuncs.GetOrCreateValue("roundf", "__nv_roundf");
       stdFuncs.GetOrCreateValue("lroundf", "__nv_lroundf");
       stdFuncs.GetOrCreateValue("llroundf", "__nv_llroundf");
       stdFuncs.GetOrCreateValue("rintf", "__nv_rintf");
       stdFuncs.GetOrCreateValue("lrintf", "__nv_lrintf");
       stdFuncs.GetOrCreateValue("llrintf", "__nv_llrintf");
       stdFuncs.GetOrCreateValue("nearbyintf", "__nv_nearbyintf");
       stdFuncs.GetOrCreateValue("remainderf", "__nv_remainderf");
       stdFuncs.GetOrCreateValue("remquof", "__nv_remquof");

       // Floating-point manipulation functions
       stdFuncs.GetOrCreateValue("copysign", "__nv_copysign");
       stdFuncs.GetOrCreateValue("nan", "__nv_nan");
       stdFuncs.GetOrCreateValue("nextafter", "__nv_nextafter");
//     map.GetOrCreateValue("nexttoward", "");

       stdFuncs.GetOrCreateValue("copysignf", "__nv_copysignf");
       stdFuncs.GetOrCreateValue("nanf", "__nv_nanf");
       stdFuncs.GetOrCreateValue("nextafterf", "__nv_nextafterf");
//     map.GetOrCreateValue("nexttowardf", "");

       // Minimum, maximu,, difference functions
       stdFuncs.GetOrCreateValue("fdim", "__nv_fdim");
       stdFuncs.GetOrCreateValue("fmax", "__nv_fmax");
       stdFuncs.GetOrCreateValue("fmin", "__nv_fmin");

       stdFuncs.GetOrCreateValue("fdimf", "__nv_fdimf");
       stdFuncs.GetOrCreateValue("fmaxf", "__nv_fmaxf");
       stdFuncs.GetOrCreateValue("fminf", "__nv_fminf");

       // Other functions
       stdFuncs.GetOrCreateValue("fabs", "__nv_fabs");
       stdFuncs.GetOrCreateValue("abs", "__nv_abs");
       stdFuncs.GetOrCreateValue("fma", "__nv_fma");

       stdFuncs.GetOrCreateValue("fabsf", "__nv_fabsf");
       stdFuncs.GetOrCreateValue("absf", "__nv_absf");
       stdFuncs.GetOrCreateValue("fmaf", "__nv_fmaf");
     }

     // If callee is standard function, change its name
     StringRef match =  stdFuncs.lookup(name);
     if (!match.empty()) {
       return match;
     }

     return name;
   }

   void SelectActiveThreads (CodeGenFunction &CGF) {

     // this is only done when in non nested parallel region
     // because in a nested parallel region there is a single thread and
     // we don't need to check
     bool CurrentIsNested = PopParallelRegion();

     // if we are in the first level, the previous position is set to false
     if (!NestedParallelStack.back()) {
     CGBuilderTy &Bld = CGF.Builder;

       // call omp_get_num_threads
     llvm::Value * NumThreads = Bld.CreateCall(Get_omp_get_num_threads());
       llvm::Value * callThreadNum = Bld.CreateCall(Get_thread_num());

       llvm::BasicBlock * IfInExcess = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".if.in.excess", CGF.CurFn);

       llvm::BasicBlock * NotInExcess = llvm::BasicBlock::Create(
           CGM.getLLVMContext(), ".not.in.excess", CGF.CurFn);

       llvm::Value * AmIInExcess = Bld.CreateICmpUGE(callThreadNum, NumThreads);
       Bld.CreateCondBr(AmIInExcess, IfInExcess, NotInExcess);

       // if it is in excess, just go back to syncthreads
       Bld.SetInsertPoint(IfInExcess);
       Bld.CreateBr(checkFinished);

       // else, do the parallel
       Bld.SetInsertPoint(NotInExcess);
     }

     PushNewParallelRegion(CurrentIsNested);
   }

   llvm::Value * CallParallelRegionPrepare(CodeGenFunction &CGF) {
     llvm::Value * call = CGF.EmitRuntimeCall(OPENMPRTL_FUNC(
         kernel_prepare_parallel));
     return call;
   }

   void CallParallelRegionStart(CodeGenFunction &CGF) {
       CGF.EmitRuntimeCall(OPENMPRTL_FUNC(kernel_parallel));
   }

   void CallParallelRegionEnd(CodeGenFunction &CGF) {
        CGF.EmitRuntimeCall(OPENMPRTL_FUNC(kernel_end_parallel));
   }

   void CallSerializedParallelStart(CodeGenFunction &CGF) {
     llvm::Value *RealArgs[] = {
         CreateIntelOpenMPRTLLoc(clang::SourceLocation(), CGF, 0),
         CreateOpenMPGlobalThreadNum(clang::SourceLocation(), CGF)};
       CGF.EmitRuntimeCall(OPENMPRTL_FUNC(serialized_parallel), RealArgs);
   }

   void CallSerializedParallelEnd(CodeGenFunction &CGF) {
     llvm::Value *RealArgs[] = {
         CreateIntelOpenMPRTLLoc(clang::SourceLocation(), CGF, 0),
         CreateOpenMPGlobalThreadNum(clang::SourceLocation(), CGF)};
     CGF.EmitRuntimeCall(OPENMPRTL_FUNC(end_serialized_parallel),
         RealArgs);
   }

   // the following two functions deal with nested parallelism
   // by calling the appropriate codegen functions above
   void StartParallelRegionInTarget (CodeGenFunction &CGF)
   {
     if (!NestedParallelStack.back()) { // not already in a parallel region
       CallParallelRegionPrepare(CGF);
       CGM.getOpenMPRuntime().GenerateNextLabel(CGF, false, true);
       CallParallelRegionStart(CGF);
     } else { // nested parallel region: serialize!
       CallSerializedParallelStart (CGF);
     }

     PushNewParallelRegion(true);
   }

   void EndParallelRegionInTarget (CodeGenFunction &CGF)
   {
     // we need to inspect the previous layer to understand what type
     // of end we need
     PopParallelRegion();
     // check if we are in a nested parallel region
     if (!NestedParallelStack.back()) { // not nested parallel
       CallParallelRegionEnd(CGF);
       CGM.getOpenMPRuntime().GenerateNextLabel(CGF, true, false);
     } else { // nested parallel region: close serialize
       CallSerializedParallelEnd (CGF);
     }
   }

   void SupportCritical (const OMPCriticalDirective &S, CodeGenFunction &CGF,
       llvm::Function * CurFn, llvm::GlobalVariable *Lck) {
     CGBuilderTy &Builder = CGF.Builder;
     llvm::Value *Loc = CreateIntelOpenMPRTLLoc(S.getLocStart(), CGF, 0);


       //  OPENMPRTL_LOC(S.getLocStart(), CGF);
     llvm::Value *GTid = Builder.CreateCall(Get_thread_num());
     llvm::Value *RealArgs[] = { Loc, GTid, Lck };

     llvm::BasicBlock * preLoopBlock = Builder.GetInsertBlock();
     llvm::BasicBlock * criticalLoopBlock =
         llvm::BasicBlock::Create(CGM.getLLVMContext(), ".critical.loop",
             CurFn);
       llvm::BasicBlock * criticalExecBlock =
           llvm::BasicBlock::Create(CGM.getLLVMContext(), ".critical.exec",
               CurFn);
       llvm::BasicBlock * criticalSkipBlock =
           llvm::BasicBlock::Create(CGM.getLLVMContext(), ".critical.skip");
       llvm::BasicBlock * criticalLoopEndBlock =
           llvm::BasicBlock::Create(CGM.getLLVMContext(), ".critical.loop.end");
       llvm::Value *laneIndex = llvm::CastInst::CreateZExtOrBitCast(
           Builder.CreateAnd(GTid,0x1f),llvm::Type::getInt64Ty(
               CGM.getLLVMContext()),"laneIndex",preLoopBlock);
       Builder.CreateBr(criticalLoopBlock);
       Builder.SetInsertPoint(criticalLoopBlock);
       llvm::PHINode *loopiv = Builder.CreatePHI(llvm::Type::getInt64Ty(
           CGM.getLLVMContext()),2,"critical_loop_iv");
       llvm::Value *init = llvm::ConstantInt::get(llvm::Type::getInt64Ty(
           CGM.getLLVMContext()),0);
       loopiv->addIncoming(init,preLoopBlock);
       llvm::Value *myturn = Builder.CreateICmpEQ(laneIndex,loopiv,"myturn");
       Builder.CreateCondBr(myturn,criticalExecBlock,criticalSkipBlock);
       Builder.SetInsertPoint(criticalExecBlock);
       CGF.EmitRuntimeCall(OPENMPRTL_FUNC(critical), RealArgs);
       CGF.EmitOMPCapturedBodyHelper(S);
       CGF.EmitRuntimeCall(OPENMPRTL_FUNC(end_critical), RealArgs);
       Builder.CreateBr(criticalSkipBlock);
       CurFn->getBasicBlockList().push_back(criticalSkipBlock);
       Builder.SetInsertPoint(criticalSkipBlock);
       llvm::Value *bump = llvm::ConstantInt::get(llvm::Type::getInt64Ty(CGM.getLLVMContext()),1);
       llvm::Value *bumpedIv = Builder.CreateAdd(loopiv,bump,"bumpediv");
       loopiv->addIncoming(bumpedIv,criticalSkipBlock);
       //llvm::Value *limit = llvm::ConstantInt::get(llvm::Type::getInt64Ty(CGM.getLLVMContext()),32);
       //llvm::Value *finished = Builder.CreateICmpULT(bumpedIv,limit,"finished");
       llvm::Value *limit = llvm::ConstantInt::get(llvm::Type::getInt64Ty(CGM.getLLVMContext()),31);
       llvm::Value *finished = Builder.CreateICmpULT(limit,bumpedIv,"finished");
       Builder.CreateCondBr(finished,criticalLoopEndBlock,criticalLoopBlock);
       CurFn->getBasicBlockList().push_back(criticalLoopEndBlock);
       Builder.SetInsertPoint(criticalLoopEndBlock);
   }

   void EmitNativeBarrier(CodeGenFunction &CGF) {
     CGBuilderTy &Bld = CGF.Builder;

     Bld.CreateCall(Get_syncthreads());
   }

   void StartNewTargetRegion() {
     // reset some class instance variables for a new target region
     masterLabelShared = 0;
     othersLabelShared = 0;
     regionLabelMap.clear();
     nextId = 0;
     InspectorExecutorSwitch = 0;
     startControl = 0;
     endControl = 0;
     finishedVar = 0;
     nextLabelVar = 0;
     checkFinished = 0;
     sequentialStartBlock = 0;
     NestedParallelStack.clear();
     PushNewParallelRegion(false); // we start in a sequential region
   }

public:
   unsigned SHARED_ADDRESS_SPACE = 3;

   CGOpenMPRuntime_NVPTX(CodeGenModule &CGM) :
      CGOpenMPRuntime(CGM), ArchName(CGM.getTarget().getTriple().getArchName()),
      MASTER_ID(0), masterLabelShared(0), othersLabelShared(0), nextId(0),
      InspectorExecutorSwitch(0), startControl(0), endControl(0),
      finishedVar(0), nextLabelVar(0), sequentialStartBlock(0),
      checkFinished(0), idleLabel(0), finishedBlockLabel(-1) {

	  LocalThrTy = llvm::StructType::create(
	        "local_thr_info",
			CGM.Int32Ty /* priv */,
			CGM.Int32Ty /* current_event */,
	        CGM.Int32Ty /* eventsNumber */,
			CGM.Int32Ty /* chunk_warp */,
	        CGM.Int32Ty /* num_iterations */,
			NULL);
  }

  /// Implement some target dependent transformation for the target region
  /// outlined function
  ///
  virtual void PostProcessTargetFunction(const Decl *D,
                                          llvm::Function *F,
                                          const CGFunctionInfo &FI){

    CGOpenMPRuntime::PostProcessTargetFunction(D, F, FI);
    PostProcessTargetFunction(F);
  }

  virtual void PostProcessTargetFunction(llvm::Function *F) {

    CGOpenMPRuntime::PostProcessTargetFunction(F);

    // No further post processing required if we are not in target mode
    if (!CGM.getLangOpts().OpenMPTargetMode)
      return;

    llvm::Module &M = CGM.getModule();
    llvm::LLVMContext &C = M.getContext();

    // Get "nvvm.annotations" metadata node
    llvm::NamedMDNode *MD = M.getOrInsertNamedMetadata("nvvm.annotations");

    // Create !{<func-ref>, metadata !"kernel", i32 1} node
    llvm::SmallVector<llvm::Value *, 3> MDVals;
    MDVals.push_back(F);
    MDVals.push_back(llvm::MDString::get(C, "kernel"));
    MDVals.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(C), 1));

    // Append metadata to nvvm.annotations
    MD->addOperand(llvm::MDNode::get(C, MDVals));
  }

  void PostProcessPrintfs(llvm::Module &M) {
    llvm::Function *PrintfFunc = FindPrintfFunction(M);

    if (PrintfFunc == nullptr) {
      return;
    }

    llvm::Function *VprintfFunc = InsertVprintfDeclaration(M);
    const llvm::DataLayout *DL = M.getDataLayout();

    // Go over all the uses of printf in the module. The iteration pattern here
    // (increment the iterator immediately after grabbing the current
    // instruction) is required to allow this loop to remove the actual uses
    // and still keep running over all of them properly.
    for (llvm::Value::use_iterator UI = PrintfFunc->use_begin(),
                                   UE = PrintfFunc->use_end();
         UI != UE;) {
      llvm::CallInst *Call = dyn_cast<llvm::CallInst>(UI->getUser());
      if (!Call) {
        llvm::report_fatal_error(
            "Only 'call' uses of 'printf' are allowed for NVPTX");
      }
      UI++;

      // First compute the buffer size required to hold all the formatting
      // arguments, and create the buffer with an alloca.
      // Note: the first argument is the formatting string - its validity is
      // verified by the frontend.
      unsigned BufSize = 0;
      for (unsigned I = 1, IE = Call->getNumArgOperands(); I < IE; ++I) {
        llvm::Value *Operand = Call->getArgOperand(I);
        BufSize = DL->RoundUpAlignment(
            BufSize, DL->getPrefTypeAlignment(Operand->getType()));
        BufSize += DL->getTypeAllocSize(Call->getArgOperand(I)->getType());
      }

      llvm::Type *Int32Ty = llvm::Type::getInt32Ty(M.getContext());
      llvm::Value *BufferPtr = nullptr;

      if (BufSize == 0) {
        // If no arguments, pass an empty buffer as the second argument to
        // vprintf.
        BufferPtr = new llvm::AllocaInst(llvm::Type::getInt8Ty(M.getContext()),
                                         llvm::ConstantInt::get(Int32Ty,
                                                                BufSize),
                                         "buf_for_vprintf_args", Call);
      } else {
        // Create the buffer to hold all the arguments. Align it to the
        // preferred alignment of the first object going into the buffer.
        // Note: if BufSize > 0, we know there's at least one object so
        // getArgOperand(1) is safe.
        unsigned AlignOfFirst =
            DL->getPrefTypeAlignment(Call->getArgOperand(1)->getType());
        BufferPtr = new llvm::AllocaInst(llvm::Type::getInt8Ty(M.getContext()),
                                         llvm::ConstantInt::get(Int32Ty,
                                                                BufSize),
                                         AlignOfFirst,
                                         "buf_for_vprintf_args", Call);

        // Each argument is placed into the buffer as follows:
        // 1. GEP is used to compute an offset into the buffer
        // 2. Bitcast to convert the buffer pointer to the correct type
        // 3. Store into that location
        unsigned Offset = 0;
        for (unsigned I = 1, IE = Call->getNumArgOperands(); I < IE; ++I) {
          llvm::Value *Operand = Call->getArgOperand(I);
          Offset = DL->RoundUpAlignment(
              Offset, DL->getPrefTypeAlignment(Operand->getType()));

          llvm::GetElementPtrInst *GEP = llvm::GetElementPtrInst::Create(
              BufferPtr, llvm::ConstantInt::get(Int32Ty, Offset), "", Call);

          llvm::BitCastInst *Cast =
              new llvm::BitCastInst(
                  GEP, Operand->getType()->getPointerTo(), "", Call);
          new llvm::StoreInst(Operand, Cast, false,
                        DL->getPrefTypeAlignment(Operand->getType()), Call);

          Offset += DL->getTypeAllocSize(Operand->getType());
        }
      }

      // Generate the alternative call to vprintf and replace the original.
      llvm::Value *VprintfArgs[] = {Call->getArgOperand(0), BufferPtr};
      llvm::CallInst *VprintfCall =
          llvm::CallInst::Create(VprintfFunc, VprintfArgs, "", Call);

      Call->replaceAllUsesWith(VprintfCall);
      Call->eraseFromParent();
    }    
  }

  llvm::Function *FindPrintfFunction(llvm::Module &M) {
    // Looking for a declaration of a function named "printf". If this function
    // is *defined* in the module, bail out.
    llvm::Function *PrintfFunc = M.getFunction("printf");
    if (!PrintfFunc || !PrintfFunc->isDeclaration())
      return nullptr;

    // So this is just a declaration. If so, it must match what we expect from
    // printf; otherwise, it's an error.
    llvm::FunctionType *FT = PrintfFunc->getFunctionType();

    if (FT->getNumParams() == 1 && FT->isVarArg() &&
        FT->getReturnType() == llvm::Type::getInt32Ty(M.getContext()) &&
        FT->getParamType(0) == llvm::Type::getInt8PtrTy(M.getContext())) {
      return PrintfFunc;
    } else {
      llvm::report_fatal_error(
          "Found printf in module but it has an invalid type");
      return nullptr;
    }
  }

  llvm::Function *InsertVprintfDeclaration(llvm::Module &M) {
    if (M.getFunction("vprintf") != nullptr) {
      llvm::report_fatal_error(
          "It is illegal to declare vprintf with C linkage");
    }

    // Create a declaration for vprintf with the proper type and insert it into
    // the module.
    llvm::Type *ArgTypes[] = {llvm::Type::getInt8PtrTy(M.getContext()),
                              llvm::Type::getInt8PtrTy(M.getContext())};
    llvm::FunctionType *VprintfFuncType =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(
            M.getContext()), ArgTypes, false);

    return llvm::Function::Create(VprintfFuncType,
                                  llvm::GlobalVariable::ExternalLinkage,
                                  "vprintf", &M);
  }

  virtual llvm::Value *CreateIntelOpenMPRTLLoc(SourceLocation Loc,
      CodeGenFunction &CGF, unsigned Flags) {
    //The Loc struct is not used by the target therefore we do not perform
    //any initialization

    return CGF.CreateTempAlloca(
  		llvm::IdentTBuilder::get(CGM.getLLVMContext()));
  }

  virtual llvm::Value *CreateOpenMPGlobalThreadNum(SourceLocation Loc,
      CodeGenFunction &CGF) {

    //FIXME: Not sure this is what we want, I am computing global thread ID
    //as blockID*BlockSize * threadID

    llvm::Value *BId = CGF.Builder.CreateCall(Get_team_num(), "blockid");
    llvm::Value *BSz = CGF.Builder.CreateCall(Get_num_threads(), "blocksize");
    llvm::Value *TId = CGF.Builder.CreateCall(Get_thread_num(), "threadid");

    return CGF.Builder.CreateAdd(CGF.Builder.CreateMul(BId, BSz), TId, "gid");
  }

  llvm::Value * GetNextIdIncrement(CodeGenFunction &CGF,
		  bool IsStaticSchedule, const Expr * ChunkSize, llvm::Value * Chunk,
		  llvm::Type * IdxTy, QualType QTy, llvm::Value * Idx,
		  OpenMPDirectiveKind Kind, OpenMPDirectiveKind SKind, llvm::Value * PSt) {


	  CGBuilderTy &Builder = CGF.Builder;
      llvm::Value *NextIdx;

	  // when distribute contains a parallel for, each distribute iteration
	  // executes "stride" instructions of the innermost for
	  switch (Kind) {
	  case OMPD_distribute_parallel_for:
	  case OMPD_distribute_parallel_for_simd:
	  case OMPD_teams_distribute_parallel_for:
	  case OMPD_teams_distribute_parallel_for_simd:
	  case OMPD_target_teams_distribute_parallel_for:
	  case OMPD_target_teams_distribute_parallel_for_simd:
	    if (SKind == OMPD_distribute) {
	      llvm::Value * Stride = Builder.CreateLoad(PSt);
	      NextIdx = Builder.CreateAdd(
	          Idx, Stride, ".next.idx.", false,
	          QTy->isSignedIntegerOrEnumerationType());
	      break;
	    } // else is default
	  default:
		NextIdx = Builder.CreateAdd(
					Idx, llvm::ConstantInt::get(IdxTy, 1), ".next.idx.", false,
					QTy->isSignedIntegerOrEnumerationType());
	  }

	  assert (NextIdx && "NextIdx variable not set");

	  return NextIdx;
  }

  // Insert the overload of the default kmpc calls' implementation here, e.g.:
  //
  // TARGET_EMIT_OPENMP_FUNC(
  //    <name of the kmpc call> ,
  //    <body of the function generation - Fn is the current function and Bld
  //    is the builder for the the entry basic block>

  // ...or specialize it by hand as we do f for fork_call and fork_teams
  llvm::Constant* Get_fork_call(){
    llvm::Function *Fn = cast<llvm::Function>(CGM.CreateRuntimeFunction(
            llvm::TypeBuilder<__kmpc_fork_call, false>::get(
            		CGM.getLLVMContext()),
					(Twine("__kmpc_",ArchName) + Twine("fork_call")).str()));

    llvm::BasicBlock *EntryBB =
      llvm::BasicBlock::Create(CGM.getLLVMContext(), "entry", Fn);
    CGBuilderTy Bld(EntryBB);
    {
      assert(Fn->arg_size() == 4 && "Unexpected number of arguments");

	  // the helper function is inlined - it is just a function call
	  Fn->setLinkage(llvm::GlobalValue::InternalLinkage);
	  Fn->addFnAttr(llvm::Attribute::AlwaysInline);

	  llvm::Function::arg_iterator arg = Fn->arg_begin();
	  std::advance(arg, 2);  // get to the function we need calling
	  llvm::Value *BitCastedFunction = arg;
	  std::advance(arg, 1); // arguments of function to be called
	  llvm::Value *FunctionArgs = arg;

	  SmallVector<llvm::Type *, 3> FnArgTypes;
	  FnArgTypes.push_back(CGM.Int32Ty->getPointerTo());
	  FnArgTypes.push_back(CGM.Int32Ty->getPointerTo());
	  FnArgTypes.push_back(CGM.Int8Ty->getPointerTo());
	  llvm::FunctionType * FnTy =
		  llvm::FunctionType::get(CGM.VoidTy, FnArgTypes, false);

	  llvm::AllocaInst * gtidEmpty = Bld.CreateAlloca(
		  Bld.getInt32Ty());
	  llvm::AllocaInst * boundEmpty = Bld.CreateAlloca(
						Bld.getInt32Ty());

	  llvm::Value * BitCastedBackFunction = Bld.CreateBitCast(
		  BitCastedFunction, FnTy->getPointerTo());

	  llvm::Value * BitCastedArgs = Bld.CreateBitCast(FunctionArgs,
		  CGM.Int8Ty->getPointerTo());

	  // For target nvptx we pass 0s as global thread id and thread id
	  // these values can be retrieved from the thread's own state instead
	  // of having them in the function parameters
	  llvm::Value * RealArgs[] = { gtidEmpty, boundEmpty, BitCastedArgs };

	  // emit a call to the microtask function using the passed args
	  Bld.CreateCall(BitCastedBackFunction, makeArrayRef(RealArgs));

	  // Unset the number of threads required by the parallel region at the end
      llvm::Function *UnsetFn = cast<llvm::Function>(CGM.CreateRuntimeFunction(
              llvm::TypeBuilder<__kmpc_unset_num_threads, false>::get(
              		CGM.getLLVMContext()),
  					"__kmpc_unset_num_threads"));
      Bld.CreateCall(UnsetFn);

	  Bld.CreateRetVoid();
    }
    return Fn;
  }

  llvm::Constant* Get_fork_teams(){
    llvm::Function *Fn = cast<llvm::Function>(CGM.CreateRuntimeFunction(
            llvm::TypeBuilder<__kmpc_fork_teams, false>::get(
            		CGM.getLLVMContext()),
					(Twine("__kmpc_",ArchName) + Twine("fork_teams")).str()));
    llvm::BasicBlock *EntryBB =
      llvm::BasicBlock::Create(CGM.getLLVMContext(), "entry", Fn);
    CGBuilderTy Bld(EntryBB);
    {
      assert(Fn->arg_size() == 4 && "Unexpected number of arguments");

      // the helper function is inlined - it is just a function call
      Fn->setLinkage(llvm::GlobalValue::InternalLinkage);
      Fn->addFnAttr(llvm::Attribute::AlwaysInline);

      llvm::Function::arg_iterator arg = Fn->arg_begin();
      std::advance(arg, 2);  // get to the function we need calling
      llvm::Value *BitCastedFunction = arg;
      std::advance(arg, 1); // arguments of function to be called
      llvm::Value *FunctionArgs = arg;

      SmallVector<llvm::Type *, 3> FnArgTypes;
      FnArgTypes.push_back(CGM.Int32Ty->getPointerTo());
      FnArgTypes.push_back(CGM.Int32Ty->getPointerTo());
      FnArgTypes.push_back(CGM.Int8Ty->getPointerTo());
      llvm::FunctionType * FnTy =
    		  llvm::FunctionType::get(CGM.VoidTy, FnArgTypes, false);

      llvm::AllocaInst * gtidEmpty = Bld.CreateAlloca(
    		  Bld.getInt32Ty());
      llvm::AllocaInst * boundEmpty = Bld.CreateAlloca(
    		  Bld.getInt32Ty());

      llvm::Value * BitCastedBackFunction = Bld.CreateBitCast(
    		  BitCastedFunction, FnTy->getPointerTo());

      llvm::Value * BitCastedArgs = Bld.CreateBitCast(FunctionArgs,
    		  CGM.Int8Ty->getPointerTo());

      // For target nvptx we pass 0s as global thread id and thread id
      // these values can be retrieved from the thread's own state
      // instead of having them in the function parameters
      llvm::Value * RealArgs[] = {gtidEmpty, boundEmpty, BitCastedArgs};

      // emit a call to the microtask function using the passed args
      Bld.CreateCall(BitCastedBackFunction, makeArrayRef(RealArgs));
      Bld.CreateRetVoid();
    }
    return Fn;
  }

  llvm::Value * AllocateThreadLocalInfo(CodeGenFunction & CGF) {
	    CGBuilderTy &Bld = CGF.Builder;

	    return Bld.CreateAlloca(LocalThrTy);
  }

  // these are run-time functions which are only exposed by the gpu library
  typedef void(__kmpc_unset_num_threads)();

  bool requiresMicroTaskForTeams(){
    return false;
  }
  bool requiresMicroTaskForParallel(){
    return false;
  }

  /// NVPTX targets cannot take advantage of the entries ordering to retrieve
  /// symbols, therefore we need to rely on names. We are currently failing
  /// if this target is being used as host because the linker cannot combine
  /// the entries in the same section as desired and do not generate any symbols
  /// in target mode (we just can't use them)

  void CreateHostPtrForCurrentTargetRegionTD(const Decl *D, llvm::Function *Fn){

    if (CGM.getLangOpts().OpenMPTargetMode)
      return;

    llvm_unreachable("This target cannot be used as OpenMP host");
  }

  void CreateHostEntryForTargetGlobalTD(const Decl *D, llvm::GlobalVariable* GV,
                                      llvm::GlobalVariable*Succ){

    if (CGM.getLangOpts().OpenMPTargetMode)
      return;

    llvm_unreachable("This target cannot be used as OpenMP host");
  }

  /// This is a hook to enable postprocessing of the module. By default this
  /// only does the creation of globals from local variables due to data sharing
  /// constraints
  void PostProcessModule(CodeGenModule &CGM) {

    if (ValuesToBeInSharedMemory.size()){
      // We need local data of each thread to be shared with others if that is
      // required by the application. Therefore we create a static table where
      // that data is going to live instead of each thread stack. The current
      // problem is that we do not know how many threads we are going to have
      // so we need to generate a table that is big enough to accommodate the
      // maximum possible number of threads. We may want to do this dynamically
      // so we use only the memory we need, but it should hurt performance a lot.
      // FIXME: Try to use an approach that use less memory but does not hurt
      // performance.

      // Each time a thread needs shared data it needs to pick it from the table
      // row of its lane master. We currently support only the block master but
      // should be able to expand this soon once SIMD support is included in the
      // control loop.
      // FIXME: Get right row from the lane master.

      // FIXME: There used to be a bug in NVPTX backend that is causing the
      // struct fields codegen to be wrong. We seem to have fix that. However
      // I'm still leaving the old code here just in case we find any issue.

#define NVPTX_GEP_BUG_IS_FIXED
#ifdef NVPTX_GEP_BUG_IS_FIXED
      // Get the type associated with each ID (the table row)
      llvm::SmallVector<llvm::Type*, 16> RowTys;
      for (auto L : ValuesToBeInSharedMemory){

        llvm::AllocaInst *AIL = cast<llvm::AllocaInst>(L);
        llvm::Type *Ty = AIL->getAllocatedType();
        RowTys.push_back(Ty);
      }

      llvm::StructType *RowTy = llvm::StructType::create(RowTys);

      // Create the table, assume we have a maximum of 1024 threads.
      llvm::ArrayType *TblTy = llvm::ArrayType::get(RowTy,1024);

      llvm::GlobalVariable *Tbl = new llvm::GlobalVariable(
                CGM.getModule(),
                TblTy,
                false,llvm::GlobalValue::InternalLinkage,
                llvm::Constant::getNullValue(TblTy),
                Twine("__omptgt__shared_data_tbl_"), 0,
                llvm::GlobalVariable::NotThreadLocal,
                SHARED_ADDRESS_SPACE, false);

      // FIXME: We need to add support for the computation of the lane master here,
      // for now we assume we only have a block master whose ID by default is 0
      llvm::Value *LaneMasterID = llvm::ConstantInt::get(CGM.Int32Ty, 0, true);

      // Replace the alloca with the right element of the table.
      llvm::Value *Zero = llvm::ConstantInt::get(CGM.Int32Ty, 0, true);
      int Idx = 0;
      for (auto L : ValuesToBeInSharedMemory){
        llvm::AllocaInst *AIL = cast<llvm::AllocaInst>(L);

        llvm::Value *Row = LaneMasterID;
        llvm::Value *Col = llvm::ConstantInt::get(CGM.Int32Ty, Idx++, true);

        llvm::Value *Comp[] = {Zero,Row,Col};
        llvm::Constant *Addr =
            llvm::ConstantExpr::getGetElementPtr(Tbl,Comp,true);

        llvm::PointerType *Ty = cast<llvm::PointerType>(Addr->getType());
        llvm::Type *FixedTy = llvm::PointerType::get(Ty->getElementType(), 0);
        Addr = llvm::ConstantExpr::getAddrSpaceCast(Addr,FixedTy);

        AIL->replaceAllUsesWith(Addr);
        AIL->eraseFromParent();
      }
#else
      for (auto L : ValuesToBeInSharedMemory){
        llvm::AllocaInst *AIL = cast<llvm::AllocaInst>(L);
        llvm::PointerType *Ty = cast<llvm::PointerType>(AIL->getType());

        llvm::Constant *Addr = new llvm::GlobalVariable(
                  CGM.getModule(),
                  Ty->getElementType(),
                  false,llvm::GlobalValue::InternalLinkage,
                  llvm::Constant::getNullValue(Ty->getElementType()),
                  Twine("__omptgt__sh_") + Twine(AIL->getName()), 0,
                  llvm::GlobalVariable::NotThreadLocal,
                  SHARED_ADDRESS_SPACE, false);

        Addr = llvm::ConstantExpr::getAddrSpaceCast(Addr,Ty);
        AIL->replaceAllUsesWith(Addr);
        AIL->eraseFromParent();
      }
#endif
    }


    CGOpenMPRuntime::PostProcessModule(CGM);

    // Process printf calls
    PostProcessPrintfs(CGM.getModule());
  }
}; // class CGOpenMPRuntime_NVPTX

///===---------------
///
/// Create runtime for the target used in the Module
///
///===---------------

CGOpenMPRuntime *CodeGen::CreateOpenMPRuntime(CodeGenModule &CGM) {

  switch (CGM.getTarget().getTriple().getArch()) {
  default:
    return new CGOpenMPRuntime(CGM);
  case llvm::Triple::nvptx:
  case llvm::Triple::nvptx64:
    return new CGOpenMPRuntime_NVPTX(CGM);
  }

}
