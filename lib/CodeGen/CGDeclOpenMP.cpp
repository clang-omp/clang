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
#include "CGOpenMPRuntimeTypes.h"
#include "CGOpenMPRuntime.h"
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

#define OPENMPRTL_FUNC(name) OpenMPRuntime->Get_##name()

void CodeGenModule::EmitOMPThreadPrivate(const VarDecl *VD, const Expr *TPE) {
  // Create cache memory for threadprivate variable void **Var.cache;
  std::string VarCache = getMangledName(VD).str() + ".cache.";
  llvm::GlobalVariable *GV;
  if (!(GV =
            dyn_cast_or_null<llvm::GlobalVariable>(GetGlobalValue(VarCache)))) {
    llvm::GlobalVariable *GV = cast<llvm::GlobalVariable>(
        CreateRuntimeVariable(Int8PtrPtrTy, VarCache));
    GV->setInitializer(llvm::Constant::getNullValue(Int8PtrPtrTy));
    GV->setLinkage(llvm::GlobalValue::CommonLinkage);
  }
  // Do not define constructors/destructors for declaration, they are defined
  // for definitions.
  if (!VD->isLocalVarDecl() && !getContext().DeclMustBeEmitted(VD))
    return;
  llvm::Value *Val =
      VD->isStaticLocal() ? getStaticLocalDeclAddress(VD) : GetAddrOfGlobal(VD);
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
                        getTypes().arrangeNullaryFunction(), ArgList,
                        SourceLocation());
      // ident_t tmp;
      llvm::Value *Tmp = OpenMPRuntime->CreateIntelOpenMPRTLLoc(TPE->getExprLoc(), CGF);
      llvm::Value *Args1[] = { Tmp };
      // __kmpc_global_thread_num(&tmp);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(global_thread_num), Args1);
      llvm::Value *Args[5] = { Tmp, CGF.Builder.CreateBitCast(
                                        Val, CGF.Builder.getInt8PtrTy()),
                               Ctor, CCtor, Dtor };
      // __kmpc_threadprivate_register(&tmp, &var, ctor, cctor, dtor);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(threadprivate_register), Args);
      CGF.FinishFunction();
      CXXGlobalInits.push_back(InitFn);
      // If we are generating code for OpenMP and we are inside a declare target
      // region we need to register the initializer so we can properly generate
      // the device initialization
      if (OpenMPRuntime && OpenMPSupport.getTargetDeclare())
        OpenMPRuntime->registerTargetGlobalInitializer(InitFn);
    }
  } else if (Ty) {
    // void __omp_threadprivate_Var();
    llvm::Value *Ctor, *CCtor, *Dtor;
    llvm::Function *InitFn;
    CreateOpenMPCXXInit(VD, Ty, InitFn, Ctor, CCtor, Dtor);
    if (InitFn) {
      CodeGenFunction CGF(*this);
      FunctionArgList ArgList;
      CGF.StartFunction(GlobalDecl(), getContext().VoidPtrTy, InitFn,
                        getTypes().arrangeNullaryFunction(), ArgList,
                        SourceLocation());
      // ident_t tmp;
      llvm::Value *Tmp = OpenMPRuntime->CreateIntelOpenMPRTLLoc(TPE->getExprLoc(), CGF);
      llvm::Value *Args1[] = { Tmp };
      // __kmpc_global_thread_num(&tmp);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(global_thread_num), Args1);
      llvm::Value *Args[5] = { Tmp, CGF.Builder.CreateBitCast(
                                        Val, CGF.Builder.getInt8PtrTy()),
                               Ctor, CCtor, Dtor };
      // __kmpc_threadprivate_register(&tmp, &var, ctor, cctor, dtor);
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(threadprivate_register), Args);
      CGF.FinishFunction();
      CXXGlobalInits.push_back(InitFn);
      // If we are generating code for OpenMP and we are inside a declare target
      // region we need to register the initializer so we can properly generate
      // the device initialization
      if (OpenMPRuntime && OpenMPSupport.getTargetDeclare())
        OpenMPRuntime->registerTargetGlobalInitializer(InitFn);
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
    if (!I->CombinerFunction || !I->InitFunction)
      continue;
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
           E = D->simd_variants_end();
       I != E; ++I) {
    Groups.FindAndConstruct(++key);
    unsigned BeginIdx = I->BeginIdx;
    unsigned EndIdx = I->EndIdx;
    for (unsigned Idx = BeginIdx; Idx < EndIdx; ++Idx) {
      OMPDeclareSimdDecl::clauses_const_iterator J = D->clauses_begin() + Idx;
      if (!*J)
        continue;
      if (isa<OMPInBranchClause>(*J)) {
        Groups[key].Mask.push_back(1);
      } else if (isa<OMPNotInBranchClause>(*J)) {
        Groups[key].Mask.push_back(0);
      } else if (OMPSimdlenClause *C = dyn_cast<OMPSimdlenClause>(*J)) {
        const Expr *LengthExpr = C->getSimdlen();
        assert(isa<IntegerLiteral>(LengthExpr) && "integer literal expected");
        unsigned VLen =
            cast<IntegerLiteral>(LengthExpr)->getValue().getZExtValue();
        Groups[key].VecLength.push_back(VLen);
      } else if (OMPLinearClause *C = dyn_cast<OMPLinearClause>(*J)) {
        const Expr *StepExpr = C->getStep();
        int Step = 0;
        if (const IntegerLiteral *IL =
                dyn_cast_or_null<IntegerLiteral>(StepExpr)) {
          Step = IL->getValue().getZExtValue();
        } else {
          Step = 1;
        }
        for (OMPLinearClause::varlist_const_iterator I = C->varlist_begin(),
                                                     E = C->varlist_end();
             I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          const std::string Name = DRE->getDecl()->getDeclName().getAsString();
          Groups[key].setLinear(Name, "", Step);
        }
      } else if (OMPAlignedClause *C = dyn_cast<OMPAlignedClause>(*J)) {
        const Expr *AlignExpr = C->getAlignment();
        int Align = 0;
        if (const IntegerLiteral *IL =
                dyn_cast_or_null<IntegerLiteral>(AlignExpr)) {
          Align = IL->getValue().getZExtValue();
        }
        for (OMPAlignedClause::varlist_const_iterator I = C->varlist_begin(),
                                                      E = C->varlist_end();
             I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          const std::string Name = DRE->getDecl()->getDeclName().getAsString();
          Groups[key].setAligned(Name, Align);
        }
      } else if (OMPUniformClause *C = dyn_cast<OMPUniformClause>(*J)) {
        for (OMPUniformClause::varlist_const_iterator I = C->varlist_begin(),
                                                      E = C->varlist_end();
             I != E; ++I) {
          const DeclRefExpr *DRE = cast<DeclRefExpr>(*I);
          Groups[key].setUniform(DRE->getDecl()->getDeclName().getAsString());
        }
      } else {
        llvm_unreachable("Unknown clause on 'omp declare simd' directive");
      }
    }
  }

  EmitVectorVariantsMetadata(FI, FD, Fn, Groups);
}

void CodeGenModule::EmitOMPDeclareTarget(const OMPDeclareTargetDecl *D) {

  // Create a region for the declare target so the the codegen knows
  // that is a valid region for a target
  OpenMPSupport.startOpenMPRegion(false);
  OpenMPSupport.setTargetDeclare(true);

  for (DeclContext::decl_iterator I = D->decls_begin(), E = D->decls_end();
       I != E; ++I) {
    if (const VarDecl *VD = dyn_cast<VarDecl>(*I))
      if (VD->getTemplateSpecializationKind() != TSK_ExplicitSpecialization &&
          VD->getTemplateSpecializationKind() != TSK_Undeclared)
        continue;

    // Inform the runtime this declaration is in a declare target region so it
    // knows how to order the entries in the module
    getOpenMPRuntime().registerEntryDeclaration(*I);
    EmitTopLevelDecl(*I);
  }

  OpenMPSupport.endOpenMPRegion();
}
