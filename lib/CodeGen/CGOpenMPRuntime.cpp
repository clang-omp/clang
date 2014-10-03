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
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace clang;
using namespace CodeGen;

CGOpenMPRuntime::CGOpenMPRuntime(CodeGenModule &CGM)
    : CGM(CGM), DefaultOpenMPPSource(nullptr), NumTargetRegions(0),
      TargetRegionsDescriptor(nullptr) {
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

#define DEFAULT_EMIT_OPENMP_FUNC(name) \
  llvm::Constant* CGOpenMPRuntime::Get_##name(){                                    \
    return CGM.CreateRuntimeFunction(                                               \
            llvm::TypeBuilder<__kmpc_##name, false>::get(CGM.getLLVMContext()),     \
            "__kmpc_" #name);                                                       \
  }

// Convenience macro to overload the default kmpc call with something more
// appropriate for a target
#define TARGET_EMIT_OPENMP_FUNC(name, body)                                         \
  llvm::Constant* Get_##name(){                                                     \
    llvm::Function *Fn = cast<llvm::Function>(CGM.CreateRuntimeFunction(            \
            llvm::TypeBuilder<__kmpc_##name, false>::get(CGM.getLLVMContext()),     \
            (Twine("__kmpc_",ArchName) + Twine(#name)).str()));    \
    llvm::BasicBlock *EntryBB = llvm::BasicBlock::Create(CGM.getLLVMContext(), "entry", Fn); \
    CGBuilderTy Bld(EntryBB); \
    {             \
      body        \
    }             \
    return Fn;    \
  }               \


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
DEFAULT_EMIT_OPENMP_FUNC(register_lib)
DEFAULT_EMIT_OPENMP_FUNC(target)
DEFAULT_EMIT_OPENMP_FUNC(target_data_begin)
DEFAULT_EMIT_OPENMP_FUNC(target_data_end)

DEFAULT_EMIT_OPENMP_FUNC(threadprivate_register)
DEFAULT_EMIT_OPENMP_FUNC(global_thread_num)

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
    return QTy->getCanonicalTypeUnqualified(); // CGF.ConvertTypeForMem(QTy->getCanonicalTypeUnqualified());
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
      CGF.CGM.GetTargetTypeStoreSize(CGF.ConvertTypeForMem(QTyRes)).getQuantity();
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
      llvm::TypeBuilder<ident_t, false>::get(CGF.CGM.getLLVMContext())->getPointerTo());
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

void CGOpenMPRuntime::PostProcessTargetFunction(const Decl *D,
                                        llvm::Function *F,
                                        const CGFunctionInfo &FI){
  CGM.SetInternalFunctionAttributes(D, F, FI);

}

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

/// Return a pointer to the device image begin
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

/// Return a pointer to the device image begin
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

///===---------------
///
/// NVPTX OpenMP Runtime Implementation
///
///===---------------

/// Target specific runtime hacks
class CGOpenMPRuntime_NVPTX: public CGOpenMPRuntime {

  StringRef ArchName;

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

public:
  CGOpenMPRuntime_NVPTX(CodeGenModule &CGM) :
      CGOpenMPRuntime(CGM), ArchName(CGM.getTarget().getTriple().getArchName()) {
  }

  /// Implement some target dependent transformation for the target region
  /// outlined function
  ///
  virtual void PostProcessTargetFunction(const Decl *D,
                                          llvm::Function *F,
                                          const CGFunctionInfo &FI){

    CGOpenMPRuntime::PostProcessTargetFunction(D, F, FI);

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

  virtual llvm::Value *CreateIntelOpenMPRTLLoc(SourceLocation Loc,
      CodeGenFunction &CGF, unsigned Flags) {
    //The Loc struct is not used by the target therefore we do not perfrom
    //any initialization
    return CGF.Builder.CreateAlloca(
        llvm::IdentTBuilder::get(CGM.getLLVMContext()), CGF.Builder.getInt32(1));
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

  // Insert the overload of the default kmpc calls' implementation here, e.g.:
  //
  // TARGET_EMIT_OPENMP_FUNC(
  //    <name of the kmpc call> ,
  //    <body of the function generation - Fn is the current function and Bld
  //    is the builder for the the entry basic block>
};

///===---------------
///
/// Generate target regions descriptor
///
///===---------------
/// Return a string with the mangled name of a target region for the given module
/// and target region index
///
std::string CGOpenMPRuntime::GetOffloadEntryMangledName(
    llvm::Triple TargetTriple) {

  std::string S;
  llvm::raw_string_ostream OS(S);

  OS << "__omptgt__" << CGM.getLangOpts().OMPModuleUniqueID << "_";

  // if we are in target mode append the target triple to the mangled name
  if (!TargetTriple.getTriple().empty())
    OS << LegalizeTripleString(TargetTriple) << '_';

  // append the module unique region index
  OS << NumTargetRegions;

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

  //Create all device images
  llvm::SmallVector<llvm::Constant*,4> DeviceImagesEntires;

  for (unsigned i=0; i<Devices.size(); ++i){
    llvm::Constant *Dev = llvm::ConstantStruct::get(DevTy,
        CGM.getOpenMPRuntime().GetDeviceImageBeginPointer(Devices[i]),
        CGM.getOpenMPRuntime().GetDeviceImageEndPointer(Devices[i]),NULL);
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

  //This is a Zero array to be used in the creation of the constant expressions
  llvm::Constant *Zero = llvm::Constant::getNullValue(
      llvm::TypeBuilder<llvm::types::i<32>, true>::get(C));
  llvm::SmallVector<llvm::Constant*, 2> ZeroZero;
  ZeroZero.push_back(Zero);
  ZeroZero.push_back(Zero);

  //Create the target region descriptor:
  // - number of devices
  // - pointer to the devices array
  // - begin of host entries point
  // - end of host entries point
  llvm::Constant *TargetRegionsDescriptorInit = llvm::ConstantStruct::get(
      DescTy,
      llvm::ConstantInt::get(
            llvm::TypeBuilder<llvm::types::i<32>, true>::get(C),
            Devices.size()),
      llvm::ConstantExpr::getGetElementPtr(DeviceImages,ZeroZero),
      HostEntriesBegin, HostEntriesEnd, NULL);

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
llvm::Constant* CGOpenMPRuntime::GetHostPtrForCurrentTargetRegion(){

  llvm::LLVMContext &C = CGM.getModule().getContext();
  llvm::Module &M = CGM.getModule();

  // Create the unique host pointer for a target region, currently is a constant
  // to a null variable (the content of the variable does not matter, only its
  // address). We do not use the outlined function address so that it can be
  // during optimization
  llvm::GlobalVariable *HostPtr = new llvm::GlobalVariable(
      M,
      llvm::Type::getInt8Ty(C),
      true,
      llvm::GlobalValue::InternalLinkage,
      llvm::Constant::getNullValue(llvm::Type::getInt8Ty(C)),
      Twine("__omptgt__host_ptr_") + Twine(NumTargetRegions) );

  // Create the entry struct
  // - pointer
  // - size - we assume size zero for functions

  // Type of the entry
  llvm::StructType *EntryTy = llvm::TypeBuilder<__tgt_offload_entry, true>::get(C);

  llvm::Constant *EntryInit = llvm::ConstantStruct::get(EntryTy, HostPtr,
          llvm::ConstantInt::get(
                llvm::TypeBuilder<llvm::types::i<32>, true>::get(C), 0), NULL);

  llvm::GlobalVariable *Entry = new llvm::GlobalVariable(
      M,
      EntryTy,
      true,
      llvm::GlobalValue::ExternalLinkage,
      EntryInit,
      Twine(GetOffloadEntryMangledName(llvm::Triple())) + Twine("_hst_entry"));

  // The entry has to be created in the section the linker expects it to be
  Entry->setSection(".openmptgt_host_entries");

  return HostPtr;
}

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
