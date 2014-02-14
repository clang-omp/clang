//===--- CGDecl.cpp - Emit LLVM Code for declarations ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This contains code to emit Decl nodes as LLVM code.
//
//===----------------------------------------------------------------------===//

#include "CodeGenFunction.h"
#include "CGDebugInfo.h"
#include "CGOpenCLRuntime.h"
#include "CodeGenModule.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/CharUnits.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
using namespace clang;
using namespace CodeGen;

namespace {
struct ident_t{};
typedef void *(*kmpc_ctor)(void *);
typedef void *(*kmpc_cctor)(void *, void *);
typedef void *(*kmpc_dtor)(void *);
typedef void (__kmpc_threadprivate_register)(ident_t *loc, void *data,
                                             kmpc_ctor ctor, kmpc_cctor cctor,
                                             kmpc_dtor dtor);
typedef int32_t (__kmpc_global_thread_num)(ident_t *loc);
typedef void *(__kmpc_threadprivate_cached)(ident_t *loc, int32_t global_tid, void *data,
                                            int32_t size, void ***cache);
}

namespace llvm {
/// Specializations of llvm::TypeBuilder for:
///   ident_t
template <bool X>
class TypeBuilder<ident_t, X> {
public:
  static StructType *get(LLVMContext &C) {
    return StructType::get(
      TypeBuilder<llvm::types::i<32>,   X>::get(C), // reserved_1
      TypeBuilder<llvm::types::i<32>,   X>::get(C), // flags
      TypeBuilder<llvm::types::i<32>,   X>::get(C), // reserved_2
      TypeBuilder<llvm::types::i<32>,   X>::get(C), // reserved_3
      TypeBuilder<llvm::types::i<8>*,   X>::get(C), // psource
      NULL);
  }
  enum {
    reserved_1,
    flags,
    reserved_2,
    reserved_3,
    psource
  };
};
}

#define OPENMPRTL_FUNC(name) Get__kmpc_##name(this)

#define DEFAULT_GET_OPENMP_FUNC(name)                                           \
static llvm::Value *Get__kmpc_##name(clang::CodeGen::CodeGenModule *CGM) {      \
   return CGM->CreateRuntimeFunction(                                           \
      llvm::TypeBuilder<__kmpc_##name, false>::get(CGM->getLLVMContext()),      \
      "__kmpc_"#name);                                                          \
}

DEFAULT_GET_OPENMP_FUNC(threadprivate_register)
DEFAULT_GET_OPENMP_FUNC(threadprivate_cached)
DEFAULT_GET_OPENMP_FUNC(global_thread_num)

typedef llvm::TypeBuilder<ident_t, false> IdentTBuilder;

static llvm::Value *GEP(CGBuilderTy &B, llvm::Value *Base, int field) {
  return B.CreateConstInBoundsGEP2_32(Base, 0, field);
}

static void StoreField(CGBuilderTy &B, llvm::Value *Val,
                       llvm::Value *Dst, int field) {
  B.CreateStore(Val, GEP(B, Dst, field));
}

//static llvm::Value *LoadField(CGBuilderTy &B, llvm::Value *Src, int field) {
//  return B.CreateLoad(GEP(B, Src, field));
//}

llvm::Value *CodeGenModule::CreateIntelOpenMPRTLLoc(SourceLocation Loc,
                                                    CodeGenFunction &CGF,
                                                    unsigned Flags) {
  llvm::Value *Tmp;
  // ident_t tmp;
  llvm::AllocaInst *AI = 0;
  llvm::BasicBlock &EntryBB = CGF.CurFn->getEntryBlock();
  std::string VarName = ".__kmpc_ident_t." + llvm::utostr(Flags) + ".";
  std::string DefaultLoc = ".omp.default.loc.";
  std::string DefaultConstName = DefaultLoc + llvm::utostr(Flags) + ".";
  llvm::Value *DefaultString;
  if (!(DefaultString = TheModule.getNamedValue(DefaultLoc))) {
    DefaultString = CGF.Builder.CreateGlobalString(";unknown;unknown;0;0;;", DefaultLoc);
  }
  for (llvm::BasicBlock::iterator I = EntryBB.begin(),
                                  E = EntryBB.end();
       I != E; ++I)
    if (I->getName().startswith(VarName)) {
      AI = cast<llvm::AllocaInst>(I);
      break;
    }
  if (!AI) {
    llvm::StructType *StTy = IdentTBuilder::get(getLLVMContext());
    AI = CGF.CreateTempAlloca(StTy, VarName);
    AI->setAlignment(PointerAlignInBytes);
    CGBuilderTy::InsertPoint SavedIP = CGF.Builder.saveIP();
    assert (SavedIP.isSet() && "No insertion point is set!");
    CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
    llvm::Value *DefaultVal;
    if (!(DefaultVal = TheModule.getNamedValue(DefaultConstName))) {
      llvm::Constant *Zero = CGF.Builder.getInt32(0);
      llvm::Value *Args[] = {Zero, Zero};
      llvm::Constant *Values[] = {Zero,
                                  CGF.Builder.getInt32(Flags),
                                  Zero,
                                  Zero,
                                  cast<llvm::Constant>(CGF.Builder.CreateInBoundsGEP(DefaultString,
                                                                                     Args))};
      llvm::Constant *Init =
        llvm::ConstantStruct::get(StTy, llvm::makeArrayRef(Values));
      llvm::GlobalVariable *ConstVar =
        new llvm::GlobalVariable(TheModule, StTy,
                                 true, llvm::GlobalValue::PrivateLinkage, Init,
                                 DefaultConstName);
      ConstVar->setUnnamedAddr(true);
      DefaultVal = ConstVar;
    }
    CGF.Builder.CreateMemCpy(AI, DefaultVal, llvm::ConstantExpr::getSizeOf(StTy),
                             PointerAlignInBytes);
    CGF.Builder.restoreIP(SavedIP);
  }
  Tmp = AI;
  if (CodeGenOpts.getDebugInfo() != CodeGenOptions::NoDebugInfo && Loc.isValid()) {
    PresumedLoc PLoc =
       getContext().getSourceManager().getPresumedLoc(Loc);
    std::string Res = ";";
    Res += PLoc.getFilename();
    Res += ";";
    if (const FunctionDecl *FD =
                  dyn_cast_or_null<FunctionDecl>(CGF.CurFuncDecl)) {
      Res += FD->getQualifiedNameAsString();
    }
    Res += ";";
    Res += llvm::utostr(PLoc.getLine()) + ";" +
           llvm::utostr(PLoc.getColumn()) + ";;";
    // tmp.psource = ";file;func;line;col;;";
    StoreField(CGF.Builder, CGF.Builder.CreateGlobalStringPtr(Res),
               Tmp, IdentTBuilder::psource);
  } else if (CodeGenOpts.getDebugInfo() != CodeGenOptions::NoDebugInfo) {
    llvm::Value *Zero = CGF.Builder.getInt32(0);
    llvm::Value *Args[] = {Zero, Zero};
    StoreField(CGF.Builder, CGF.Builder.CreateInBoundsGEP(DefaultString, Args),
               Tmp, IdentTBuilder::psource);
  }
  return Tmp;
}

llvm::Value *CodeGenModule::CreateOpenMPGlobalThreadNum(SourceLocation Loc,
                                                        CodeGenFunction &CGF) {
  llvm::BasicBlock &EntryBB = CGF.CurFn->getEntryBlock();
  for (llvm::BasicBlock::iterator I = EntryBB.begin(),
                                  E = EntryBB.end();
       I != E; ++I)
    if (I->getName().startswith(".__kmpc_global_thread_num."))
      return CGF.Builder.CreateLoad(I, ".gtid.");
  llvm::AllocaInst *AI = CGF.CreateTempAlloca(Int32Ty, ".__kmpc_global_thread_num.");
  AI->setAlignment(4);
  CGBuilderTy::InsertPoint SavedIP = CGF.Builder.saveIP();
  assert (SavedIP.isSet() && "No insertion point is set!");
  CGF.Builder.SetInsertPoint(CGF.AllocaInsertPt);
  llvm::Value *IdentT = CreateIntelOpenMPRTLLoc(Loc, CGF);
  llvm::Value *Res =
         CGF.EmitRuntimeCall(OPENMPRTL_FUNC(global_thread_num),
                             llvm::makeArrayRef<llvm::Value *>(&IdentT, 1));
  CGF.Builder.CreateStore(Res, AI);
  CGF.Builder.restoreIP(SavedIP);
  return CGF.Builder.CreateLoad(AI, ".gtid.");
}

llvm::Value *CodeGenModule::CreateOpenMPThreadPrivateCached(
                                                      const VarDecl *VD,
                                                      SourceLocation Loc,
                                                      CodeGenFunction &CGF,
                                                      bool NoCast) {
  if (OpenMPSupport.hasThreadPrivateVar(VD)) {
    llvm::Type *VDTy = getTypes().ConvertTypeForMem(VD->getType());
    llvm::PointerType *PTy =
      llvm::PointerType::get(VDTy,
                             getContext().getTargetAddressSpace(VD->getType()));
    CharUnits SZ = GetTargetTypeStoreSize(VDTy);
    std::string VarCache = getMangledName(VD).str() + ".cache.";

    llvm::Value *Args[] = {CreateIntelOpenMPRTLLoc(Loc, CGF),
                           CreateOpenMPGlobalThreadNum(Loc, CGF),
                           CGF.Builder.CreateBitCast(VD->isStaticLocal() ? 
                                                           getStaticLocalDeclAddress(VD) :
                                                           GetAddrOfGlobal(VD),
                                                     Int8PtrTy),
                           llvm::ConstantInt::get(Int32Ty, SZ.getQuantity()),
                           GetGlobalValue(VarCache)};
    llvm::Value *Call =
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(threadprivate_cached), Args);
    if (NoCast)
      return Call;
    return CGF.Builder.CreateBitCast(Call, PTy);
  }
  return 0;
}

void CodeGenModule::EmitOMPThreadPrivate(const VarDecl *VD, const Expr *TPE) {
  // Create cache memory for threadprivate variable void **Var.cache;
  std::string VarCache = getMangledName(VD).str() + ".cache.";
  llvm::GlobalVariable *GV;
  if (!(GV = dyn_cast_or_null<llvm::GlobalVariable>(GetGlobalValue(VarCache)))) {
    llvm::GlobalVariable *GV =
      cast<llvm::GlobalVariable>(CreateRuntimeVariable(Int8PtrPtrTy, VarCache));
    GV->setInitializer(llvm::Constant::getNullValue(Int8PtrPtrTy));
    GV->setLinkage(llvm::GlobalValue::CommonLinkage);
  }
  // Do not define constructors/destructors for declaration, they are defined
  // for definitions.
  if (!VD->isLocalVarDecl() && !getContext().DeclMustBeEmitted(VD)) return;
  llvm::Value *Val = VD->isStaticLocal() ? getStaticLocalDeclAddress(VD) : GetAddrOfGlobal(VD);
  bool isArray = false;
  const Type *TypePtr = VD->getType().getCanonicalType().getTypePtr();
  while (TypePtr->isArrayType()) {
    isArray = true;
    TypePtr = TypePtr->getArrayElementTypeNoTypeQual();
  }
  CXXRecordDecl *Ty = TypePtr->getAsCXXRecordDecl();
  if (isArray && Ty) {
    // void __omp_threadprivate_Var();
    llvm::Value *Ctor, *CCtor, *Dtor;
    llvm::Function *InitFn;
    CreateOpenMPArrCXXInit(VD, Ty, InitFn, Ctor, CCtor, Dtor);
    if (InitFn) {
      CodeGenFunction CGF(*this);
      FunctionArgList ArgList;
      CGF.StartFunction(GlobalDecl(), getContext().VoidPtrTy, InitFn,
                        getTypes().arrangeNullaryFunction(),
                        ArgList, SourceLocation());
      // ident_t tmp;
      llvm::Value *Tmp = CreateIntelOpenMPRTLLoc(TPE->getExprLoc(), CGF);
      llvm::Value *Args[5] = {
                        Tmp,
                        CGF.Builder.CreateBitCast(Val,
                                                  CGF.Builder.getInt8PtrTy()),
                        Ctor, CCtor, Dtor};
      // __kmpc_threadprivate_register(&tmp, &var, ctor, cctor, dtor);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(threadprivate_register), Args);
      CGF.FinishFunction();
      CXXGlobalInits.push_back(InitFn);
    }
  }
  else if (Ty) {
    // void __omp_threadprivate_Var();
    llvm::Value *Ctor, *CCtor, *Dtor;
    llvm::Function *InitFn;
    CreateOpenMPCXXInit(VD, Ty, InitFn, Ctor, CCtor, Dtor);
    if (InitFn) {
      CodeGenFunction CGF(*this);
      FunctionArgList ArgList;
      CGF.StartFunction(GlobalDecl(), getContext().VoidPtrTy, InitFn,
                        getTypes().arrangeNullaryFunction(),
                        ArgList, SourceLocation());
      // ident_t tmp;
      llvm::Value *Tmp = CreateIntelOpenMPRTLLoc(TPE->getExprLoc(), CGF);
      llvm::Value *Args[5] = {
                        Tmp,
                        CGF.Builder.CreateBitCast(Val,
                                                  CGF.Builder.getInt8PtrTy()),
                        Ctor, CCtor, Dtor};
      // __kmpc_threadprivate_register(&tmp, &var, ctor, cctor, dtor);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(threadprivate_register), Args);
      CGF.FinishFunction();
      CXXGlobalInits.push_back(InitFn);
    }
  }
}

void CodeGenModule::EmitOMPThreadPrivate(const OMPThreadPrivateDecl *D) {
  for (OMPThreadPrivateDecl::varlist_const_iterator I = D->varlist_begin(),
                                                    E = D->varlist_end();
       I != E; ++I) {
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    OpenMPSupport.addThreadPrivateVar(VD, *I);
    EmitOMPThreadPrivate(VD, *I);
  }
}

void CodeGenModule::EmitOMPDeclareReduction(const OMPDeclareReductionDecl *D) {
  for (OMPDeclareReductionDecl::datalist_const_iterator I = D->datalist_begin(),
                                                        E = D->datalist_end();
       I != E; ++I) {
    if (!I->CombinerFunction || !I->InitFunction) continue;
    Decl *D = cast<DeclRefExpr>(I->CombinerFunction)->getDecl();
    EmitGlobal(cast<FunctionDecl>(D));
    D = cast<DeclRefExpr>(I->InitFunction)->getDecl();
    EmitGlobal(cast<FunctionDecl>(D));
  }
}

void CodeGenModule::EmitOMPDeclareSimd(const OMPDeclareSimdDecl *D) {
  // 1) Emit function, extract FunctionDecl, Function, CGFunctionInfo.
  // 2) Prepare input (Groups) for metadata generation.
  // 3) Do the metadata generation -- call EmitVectorVariantsMetadata.

  // Make sure that the function is emitted.
  const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(D->getFunction());

  if (!FD) {
    return;
  }

  EmitGlobal(FD);

  const CGFunctionInfo &FI = getTypes().arrangeGlobalDeclaration(FD);
  llvm::FunctionType *Ty = getTypes().GetFunctionType(FI);
  llvm::Function *Fn = cast<llvm::Function>(GetAddrOfFunction(FD, Ty));

  if (FD->isVariadic())
    return;

  if (!Fn)
    return;

  // Prepare input for the metadata emission.
  GroupMap Groups;
  static unsigned key = 0;

  for (OMPDeclareSimdDecl::simd_variants_const_iterator
       I = D->simd_variants_begin(),
       E = D->simd_variants_end(); I != E; ++I) {
    Groups.FindAndConstruct(++key);
    unsigned BeginIdx = I->BeginIdx;
    unsigned EndIdx = I->EndIdx;
    for (unsigned Idx = BeginIdx; Idx < EndIdx; ++Idx) {
      OMPDeclareSimdDecl::clauses_const_iterator J = D->clauses_begin() + Idx;
      if (!*J)
        continue;
      if (isa<OMPInBranchClause>(*J)) {
        Groups[key].Mask.push_back(1);
      }
      else if (isa<OMPNotInBranchClause>(*J)) {
        Groups[key].Mask.push_back(0);
      }
      else if (OMPSimdlenClause *C = dyn_cast<OMPSimdlenClause>(*J)) {
        const Expr *LengthExpr = C->getSimdlen();
        assert(isa<IntegerLiteral>(LengthExpr) && "integer literal expected");
        unsigned VLen = cast<IntegerLiteral>(LengthExpr)->getValue().getZExtValue();
        Groups[key].VecLength.push_back(VLen);
      }
      else if (OMPLinearClause *C = dyn_cast<OMPLinearClause>(*J)) {
        const Expr *StepExpr = C->getStep();
        int Step = 0;
        if (const IntegerLiteral *IL =
            dyn_cast_or_null<IntegerLiteral>(StepExpr)) {
          Step = IL->getValue().getZExtValue();
        }
        else {
          Step = 1;
        }
        for (OMPLinearClause::varlist_const_iterator I = C->varlist_begin(),
            E = C->varlist_end(); I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          const std::string Name = DRE->getDecl()->getDeclName().getAsString();
          Groups[key].setLinear(Name, "", Step);
        }
      }
      else if (OMPAlignedClause *C = dyn_cast<OMPAlignedClause>(*J)) {
        const Expr *AlignExpr = C->getAlignment();
        int Align = 0;
        if (const IntegerLiteral *IL =
            dyn_cast_or_null<IntegerLiteral>(AlignExpr)) {
          Align = IL->getValue().getZExtValue();
        }
        for (OMPAlignedClause::varlist_const_iterator I = C->varlist_begin(),
            E = C->varlist_end(); I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          const std::string Name = DRE->getDecl()->getDeclName().getAsString();
          Groups[key].setAligned(Name, Align);
        }
      }
      else if (OMPUniformClause *C = dyn_cast<OMPUniformClause>(*J)) {
        for (OMPUniformClause::varlist_const_iterator I = C->varlist_begin(),
            E = C->varlist_end(); I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          Groups[key].setUniform(DRE->getDecl()->getDeclName().getAsString());
        }
      }
      else {
        llvm_unreachable("Unknown clause on 'omp declare simd' directive");
      }
    }
  }

  EmitVectorVariantsMetadata(FI, FD, Fn, Groups);
}

