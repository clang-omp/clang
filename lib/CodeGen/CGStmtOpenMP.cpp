//===--- CGStmtOpenMP.cpp - Emit LLVM Code for declarations ---------------===//
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
#include "TargetInfo.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Frontend/CodeGenOptions.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IR/CallSite.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include <string>
using namespace clang;
using namespace CodeGen;

namespace {
// Getters for fields of the loop-like directives. We may want to add a
// common parent to all the loop-like directives to get rid of these.
//

static bool isLoopDirective(const OMPExecutableDirective *ED) {
  return isa<OMPForDirective>(ED) || isa<OMPParallelForDirective>(ED)
      || isa<OMPParallelForSimdDirective>(ED) || isa<OMPSimdDirective>(ED)
      || isa<OMPForSimdDirective>(ED) || isa<OMPDistributeDirective>(ED)
      || isa<OMPDistributeSimdDirective>(ED)
      || isa<OMPDistributeParallelForDirective>(ED)
      || isa<OMPDistributeParallelForSimdDirective>(ED)
      || isa<OMPTeamsDistributeParallelForDirective>(ED)
      || isa<OMPTeamsDistributeParallelForSimdDirective>(ED)
      || isa<OMPTargetTeamsDistributeParallelForDirective>(ED)
      || isa<OMPTargetTeamsDistributeParallelForSimdDirective>(ED)
      || isa<OMPTeamsDistributeDirective>(ED)
      || isa<OMPTeamsDistributeSimdDirective>(ED)
      || isa<OMPTargetTeamsDistributeDirective>(ED)
      || isa<OMPTargetTeamsDistributeSimdDirective>(ED);
}

static bool isParallelDirective(const OMPExecutableDirective *ED) {
  return isa<OMPParallelDirective>(ED) || isa<OMPParallelForDirective>(ED)
      || isa<OMPParallelForSimdDirective>(ED)
      || isa<OMPParallelSectionsDirective>(ED);
}

static const Expr *getInitFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getInit();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getInit();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getInit();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const Expr *getFinalFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getFinal();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getFinal();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getFinal();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const Expr *
getNewIterVarFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getNewIterVar();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getNewIterVar();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getNewIterVar();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const Expr *
getNewIterEndFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getNewIterEnd();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getNewIterEnd();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const Expr *
getLowerBoundFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getLowerBound();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getLowerBound();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getLowerBound();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getLowerBound();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getLowerBound();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getLowerBound();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const Expr *
getUpperBoundFromLoopDirective(const OMPExecutableDirective *ED) {
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getUpperBound();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getUpperBound();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getUpperBound();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getUpperBound();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getUpperBound();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getUpperBound();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static const ArrayRef<Expr *> getCountersFromLoopDirective(
    const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getCounters();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getCounters();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getCounters();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static unsigned getCollapsedNumberFromLoopDirective(
    const OMPExecutableDirective *ED) {
  if (const OMPForDirective *D = dyn_cast<OMPForDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPParallelForDirective *D = dyn_cast<OMPParallelForDirective>(
      ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPParallelForSimdDirective *D = dyn_cast<
      OMPParallelForSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPSimdDirective *D = dyn_cast<OMPSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPForSimdDirective *D = dyn_cast<OMPForSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPDistributeDirective *D = dyn_cast<OMPDistributeDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPDistributeSimdDirective *D =
      dyn_cast<OMPDistributeSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPDistributeParallelForDirective *D = dyn_cast<
      OMPDistributeParallelForDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPDistributeParallelForSimdDirective *D = dyn_cast<
      OMPDistributeParallelForSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTargetTeamsDistributeParallelForDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTargetTeamsDistributeParallelForSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeParallelForSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTeamsDistributeDirective *D = dyn_cast<
      OMPTeamsDistributeDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTeamsDistributeSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTargetTeamsDistributeDirective *D = dyn_cast<
      OMPTargetTeamsDistributeDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  if (const OMPTargetTeamsDistributeSimdDirective *D = dyn_cast<
      OMPTargetTeamsDistributeSimdDirective>(ED)) {
    return D->getCollapsedNumber();
  }
  assert(0 && "bad loop directive");
  return 0;
}

static bool IsAllowedClause(OpenMPClauseKind CKind,
    ArrayRef<OpenMPDirectiveKind> DKinds) {
  for (ArrayRef<OpenMPDirectiveKind>::const_iterator I = DKinds.begin(), E =
      DKinds.end(); I != E; ++I) {
    if (isAllowedClauseForDirective(*I, CKind))
      return true;
  }
  return false;
}
}

#define OPENMPRTL_FUNC(name) CGM.getOpenMPRuntime().Get_##name()
#define OPENMPRTL_ATOMIC_FUNC(QTy, Op) CGM.getOpenMPRuntime().GetAtomicFunc(*this, QTy, Op)
#define OPENMPRTL_ATOMIC_FUNC_GENERAL(QTyRes, QTyIn, Aop, Capture, Reverse)    \
    CGM.getOpenMPRuntime().GetAtomicFuncGeneral(*this, QTyRes, QTyIn, Aop, Capture, Reverse)
#define OPENMPRTL_ATOMICTYPE(CGF, QTy) \
    CGM.getOpenMPRuntime().GetAtomicType(CGF, QTy)
#define OPENMPRTL_LOC(SLoc, CGF)    \
    CGM.getOpenMPRuntime().CreateIntelOpenMPRTLLoc(SLoc, CGF)
#define OPENMPRTL_LOCFLAGS(SLoc, CGF, Flags)    \
    CGM.getOpenMPRuntime().CreateIntelOpenMPRTLLoc(SLoc, CGF, Flags)
#define OPENMPRTL_THREADNUM(SLoc, CGF)    \
    CGM.getOpenMPRuntime().CreateOpenMPGlobalThreadNum(SLoc, CGF)
#define OPENMPRTL_THREADPVTCACHED(VD, Loc, CGF, NoCast) \
    CGM.getOpenMPRuntime().CreateOpenMPThreadPrivateCached(VD,Loc,CGF,NoCast)
#define OPENMPRTL_DINFOTY \
    CGM.getOpenMPRuntime().getKMPDependInfoType()

static void EmitCancelArgs(CodeGenFunction &CGF,
    OpenMPDirectiveKind ConstructType, SourceLocation SLoc, llvm::Value *&Loc,
    llvm::Value *&GTid, llvm::Value *&Kind) {
  Loc = CGF.OPENMPRTL_LOC(SLoc, CGF);
  GTid = CGF.OPENMPRTL_THREADNUM(SLoc, CGF);
  int CKind = KMP_CANCEL_NOREQ;
  switch (ConstructType) {
  case OMPD_parallel:
    CKind = KMP_CANCEL_PARALLEL;
    break;
  case OMPD_for:
    CKind = KMP_CANCEL_LOOP;
    break;
  case OMPD_sections:
    CKind = KMP_CANCEL_SECTIONS;
    break;
  case OMPD_taskgroup:
    CKind = KMP_CANCEL_TASKGROUP;
    break;
  default:
    llvm_unreachable("Unknown construct type in cancel directive");
    break;
  }
  Kind = CGF.Builder.getInt32(CKind);
}

static void EmitCancellationPoint(CodeGenFunction &CGF, SourceLocation Loc,
    ArrayRef<llvm::Value *> Args, llvm::BasicBlock *ExitBB,
    llvm::BasicBlock *ContBB, CodeGenFunction::JumpDest FinalBB =
        CodeGenFunction::JumpDest()) {
  CodeGenModule &CGM = CGF.CGM;
  llvm::Value *CallRes = CGF.Builder.CreateIsNotNull(
      CGF.EmitRuntimeCall(OPENMPRTL_FUNC(cancellationpoint), Args));
  CGF.Builder.CreateCondBr(CallRes, ExitBB, ContBB);
  if (FinalBB.isValid()) {
    CGF.EmitBlock(ExitBB);
    CGF.EmitOMPCancelBarrier(Loc, KMP_IDENT_BARRIER_IMPL, true);
    CGF.EmitBranchThroughCleanup(FinalBB);
    CGF.EmitBlock(ContBB);
  }
}

namespace {
/// \brief RAII object that save current insert position and then restores it.
class BuilderInsertPositionRAII {
  CGBuilderTy &Builder;
  CGBuilderTy::InsertPoint SavedIP;

public:
  BuilderInsertPositionRAII(CGBuilderTy &Builder,
      llvm::Instruction *NewInsertPoint) :
      Builder(Builder), SavedIP(Builder.saveIP()) {
    assert(SavedIP.isSet() && "No insertion point is set!");
    Builder.SetInsertPoint(NewInsertPoint);
  }
  ~BuilderInsertPositionRAII() {
    Builder.restoreIP(SavedIP);
  }
};

/// \brief RAII object for OpenMP region.
class OpenMPRegionRAII {
public:
  enum OMPRegionTypes {
    // The default OpenMP region: capture into the provided context
    OMPRegionType_Default,
    // A target region initialization: initialize the capture statement cache
    // but not the context
    OMPRegionType_TargetInit,
    // A target region: create and initialize capture statement and everything
    // else the requires the context
    OMPRegionType_Target,
    // A region where the captured declarations need to be specially handled
    // so they can be shared accross threads (placed in shared memory address
    // spaces)
    OMPRegionType_Shared
  };

private:
  CodeGenFunction &CGF;

  bool OwnsCaptureStmtInfo;
  bool isTargetRegion;

  // store mappings between local variables and shared variables for
  // team-master only regions (for NVPTX)
  SmallVector<const VarDecl *, 8> MappingDecls;
  SmallVector<llvm::Value*, 8> MappingVals;

public:
  OpenMPRegionRAII(CodeGenFunction &CGF, const CapturedStmt &CS,
      llvm::Value *Context = nullptr,
      OMPRegionTypes RegionType = OMPRegionType_Default) :
      CGF(CGF), isTargetRegion(false) {

    switch (RegionType){
    default:
      isTargetRegion = false;
      assert(!CGF.CapturedStmtInfo &&
          "Capture statement already exists for region??");
      OwnsCaptureStmtInfo = true;
      CGF.InitOpenMPFunction(Context, CS);
      break;
    case OMPRegionType_TargetInit:
      isTargetRegion = true;
      assert(!CGF.CapturedStmtInfo &&
          "Capture statement already exists for region??");
      OwnsCaptureStmtInfo = true;
      CGF.InitOpenMPTargetFunction(CS, false);
      break;
    case OMPRegionType_Target:
      isTargetRegion = true;
      assert(CGF.CapturedStmtInfo &&
          "Capture statement does not exist for region??");
      OwnsCaptureStmtInfo = false;
      CGF.InitOpenMPTargetFunction(CS, true);
      break;
    case OMPRegionType_Shared:
      isTargetRegion = false;
      // We need to understand whether the captured information for the current
      // function was generated before or if it is going to be generated for this
      // region.
      OwnsCaptureStmtInfo = CGF.CapturedStmtInfo == nullptr;

      CGF.InitOpenMPSharedizeParameters (CS, MappingDecls,
          MappingVals);
      break;
    }
  }
  ~OpenMPRegionRAII() {

    if (isTargetRegion)
      CGF.CGM.getOpenMPRuntime().clearTargetCapturedGlobal();

    assert( MappingDecls.size() == MappingVals.size()
         && "Inconsistent sizes for map information");

    // Update the capture statment cache to the values it had before entering
    // the OpenMP region
    for (unsigned i=0; i<MappingDecls.size(); ++i)
      CGF.CapturedStmtInfo->addCachedVar(MappingDecls[i],
                                         MappingVals[i]);

    if (OwnsCaptureStmtInfo)
      delete CGF.CapturedStmtInfo;
  }
};

static void SetFirstprivateInsertPt(CodeGenFunction &CGF) {
  if (CGF.FirstprivateInsertPt) {
    llvm::Instruction *Ptr = CGF.FirstprivateInsertPt;
    CGF.FirstprivateInsertPt = 0;
    Ptr->eraseFromParent();
  }
  llvm::Value *Undef = llvm::UndefValue::get(CGF.Int32Ty);
  CGF.FirstprivateInsertPt = new llvm::BitCastInst(Undef, CGF.Int32Ty, "",
      CGF.Builder.GetInsertBlock());

}

static void EmitFirstprivateInsert(CodeGenFunction &CGF, SourceLocation Loc) {
  if (CGF.FirstprivateInsertPt) {
    BuilderInsertPositionRAII PosRAII(CGF.Builder, CGF.FirstprivateInsertPt);
    CGF.EmitOMPBarrier(Loc, KMP_IDENT_BARRIER_IMPL);
  }
}
}

static llvm::GlobalVariable *CreateRuntimeVariable(CodeGenModule &CGM,
    StringRef MangledName, llvm::Type *Ty) {
  llvm::PointerType *PtrTy = llvm::PointerType::getUnqual(Ty);
  unsigned AddrSpace = PtrTy->getAddressSpace();
  return new llvm::GlobalVariable(CGM.getModule(), Ty, false,
      llvm::GlobalValue::PrivateLinkage, llvm::Constant::getNullValue(Ty),
      MangledName, 0, llvm::GlobalVariable::NotThreadLocal, AddrSpace);
}

void CodeGenFunction::EmitOMPBarrier(SourceLocation L, unsigned Flags) {
	// replace this with appropriate overridable call to OpenMPRuntime object
  if (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
      || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64) {
    CGM.getOpenMPRuntime().EmitNativeBarrier(*this);
  } else {
    EmitOMPCallWithLocAndTidHelper(OPENMPRTL_FUNC(barrier), L, Flags);
  }
}

void CodeGenFunction::EmitOMPCancelBarrier(SourceLocation L, unsigned Flags,
    bool IgnoreResult) {
  if (OMPCancelMap.empty()) {
    EmitOMPBarrier(L, Flags);
  } else {
    llvm::Value *CallRes = EmitOMPCallWithLocAndTidHelper(
    OPENMPRTL_FUNC(cancel_barrier), L, Flags);
    if (!IgnoreResult) {
      JumpDest FinalBB;
      if (OMPCancelMap.count(OMPD_for))
        FinalBB = OMPCancelMap[OMPD_for];
      else if (OMPCancelMap.count(OMPD_sections))
        FinalBB = OMPCancelMap[OMPD_sections];
      else if (OMPCancelMap.count(OMPD_parallel))
        FinalBB = OMPCancelMap[OMPD_parallel];
      else
        FinalBB = OMPCancelMap[OMPD_taskgroup];

      llvm::BasicBlock *ExitBB = createBasicBlock("omp.cancel_barrier.exit");
      llvm::BasicBlock *ContBB = createBasicBlock(
          "omp.cancel_barrier.continue");
      llvm::Value *Cond = Builder.CreateIsNotNull(CallRes);
      Builder.CreateCondBr(Cond, ExitBB, ContBB);
      EmitBlock(ExitBB);
      EmitBranchThroughCleanup(FinalBB);
      EmitBlock(ContBB);
    }
  }
}

void CodeGenFunction::EmitOMPDirectiveWithParallel(OpenMPDirectiveKind DKind,
    ArrayRef<OpenMPDirectiveKind> SKinds, const OMPExecutableDirective &S) {

  if (CGM.getOpenMPRuntime().requiresMicroTaskForParallel())
    EmitOMPDirectiveWithParallelMicrotask(DKind, SKinds, S);
  else
    EmitOMPDirectiveWithParallelNoMicrotask(DKind, SKinds, S);

}

void CodeGenFunction::EmitOMPDirectiveWithParallelNoMicrotask(
    OpenMPDirectiveKind DKind, ArrayRef<OpenMPDirectiveKind> SKinds,
    const OMPExecutableDirective &S) {

  // Generate shared args for captured stmt.
  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());

  // Are we generating code for a target?
  bool isTargetMode = CGM.getLangOpts().OpenMPTargetMode;

  assert(
      !(isTargetMode && CGM.getLangOpts().OMPTargetTriples.empty())
          && "Are we in target mode and no targets were specified??");

  OpenMPRegionRAII OMPRegion(*this, *CS, nullptr,
                             OpenMPRegionRAII::OMPRegionType_Shared);

  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(true);
  CGM.OpenMPSupport.setMergeable(false);
  CGM.OpenMPSupport.setOrdered(false);
  CGM.OpenMPSupport.setScheduleChunkSize(KMP_SCH_DEFAULT, 0);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitInitOMPClause(*(*I), S);

  if (isTargetMode)
  {
    CGM.getOpenMPRuntime().StartParallelRegionInTarget(*this);
  }

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitAfterInitOMPClause(*(*I), S);

  // CodeGen for clauses (call start).
  {
   	for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I
          && (!IsAllowedClause((*I)->getClauseKind(), SKinds)
              || (*I)->getClauseKind() == OMPC_firstprivate))
        EmitPreOMPClause(*(*I), S);

   	// in nvptx backend, exclude threads in excess (only when in non-nested
   	// parallelism)
   	if (isTargetMode)
   	  CGM.getOpenMPRuntime().SelectActiveThreads(*this);

    switch (DKind) {
    case OMPD_parallel:
      EmitStmt(CS->getCapturedStmt());
      break;
    case OMPD_parallel_sections:
      EmitOMPSectionsDirective(DKind, OMPD_sections, S);
      break;
    case OMPD_parallel_for:
      EmitOMPDirectiveWithLoop(DKind, OMPD_for, S);
      break;
    case OMPD_parallel_for_simd:
      EmitOMPDirectiveWithLoop(DKind, OMPD_for_simd, S);
      break;
    default:
      break;
    }
    EnsureInsertPoint();

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
        EmitPostOMPClause(*(*I), S);

    // CodeGen for clauses (closing steps).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
        EmitCloseOMPClause(*(*I), S);
  }

  EnsureInsertPoint();
  // Implicit barrier for simple parallel region only.
  // Others (combined) directives already has implicit barriers.
  if (DKind == OMPD_parallel) {
    EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL);
  }

  EmitFirstprivateInsert(*this, S.getLocStart());

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitFinalOMPClause(*(*I), S);

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();

  if (isTargetMode) {
    CGM.getOpenMPRuntime().EndParallelRegionInTarget(*this);
  }
}

void CodeGenFunction::EmitOMPDirectiveWithParallelMicrotask(
    OpenMPDirectiveKind DKind, ArrayRef<OpenMPDirectiveKind> SKinds,
    const OMPExecutableDirective &S) {

  // Are we generating code for a target?
  bool isTargetMode = CGM.getLangOpts().OpenMPTargetMode;

  assert(
      !(isTargetMode && CGM.getLangOpts().OMPTargetTriples.empty())
          && "Are we in target mode and no targets were specified??");

  if (isTargetMode) {
    CGM.getOpenMPRuntime().GenerateNextLabel(*this, false, true);
  }

  // Generate shared args for captured stmt.
  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());
  llvm::Value *Arg = GenerateCapturedStmtArgument(*CS);

  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(true);
  CGM.OpenMPSupport.setMergeable(false);
  CGM.OpenMPSupport.setOrdered(false);
  CGM.OpenMPSupport.setScheduleChunkSize(KMP_SCH_DEFAULT, 0);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitInitOMPClause(*(*I), S);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitAfterInitOMPClause(*(*I), S);

  // ptxas does not allow names with dots: specialize
  std::string microTaskName;
  if (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
      || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64)
    microTaskName = "omp_microtask";
  else
    microTaskName = ".omp_microtask.";
  IdentifierInfo *Id = &getContext().Idents.get(microTaskName);
  QualType PtrIntTy = getContext().getPointerType(getContext().IntTy);
  SmallVector<QualType, 4> FnArgTypes;
  FnArgTypes.push_back(PtrIntTy);
  FnArgTypes.push_back(PtrIntTy);
  FnArgTypes.push_back(getContext().VoidPtrTy);
  FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpecType = EST_BasicNoexcept;
  QualType FnTy = getContext().getFunctionType(getContext().VoidTy, FnArgTypes,
      EPI);
  TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(FnTy,
      SourceLocation());
  FunctionDecl *FD = FunctionDecl::Create(getContext(),
      getContext().getTranslationUnitDecl(), CS->getLocStart(),
      SourceLocation(), Id, FnTy, TI, SC_Static, false, false, false);
  TypeSourceInfo *PtrIntTI = getContext().getTrivialTypeSourceInfo(PtrIntTy,
      SourceLocation());
  TypeSourceInfo *PtrVoidTI = getContext().getTrivialTypeSourceInfo(
      getContext().VoidPtrTy, SourceLocation());
  ParmVarDecl *Arg1 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, PtrIntTy, PtrIntTI, SC_Auto, 0);
  ParmVarDecl *Arg2 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, PtrIntTy, PtrIntTI, SC_Auto, 0);
  ParmVarDecl *Arg3 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, getContext().VoidPtrTy, PtrVoidTI, SC_Auto, 0);
  CodeGenFunction CGF(CGM, true);
  const CGFunctionInfo &FI = getTypes().arrangeFunctionDeclaration(FD);
  llvm::Function *Fn = llvm::Function::Create(getTypes().GetFunctionType(FI),
      llvm::GlobalValue::PrivateLinkage, FD->getName(), &CGM.getModule());
  CGM.SetInternalFunctionAttributes(CurFuncDecl, Fn, FI);
  FunctionArgList FnArgs;
  FnArgs.push_back(Arg1);
  FnArgs.push_back(Arg2);
  FnArgs.push_back(Arg3);
  CGF.OpenMPRoot = OpenMPRoot ? OpenMPRoot : this;
  CGF.StartFunction(FD, getContext().VoidTy, Fn, FI, FnArgs, SourceLocation());

  CGF.OMPCancelMap[OMPD_parallel] = CGF.ReturnBlock;

  CGF.Builder.CreateLoad(CGF.GetAddrOfLocalVar(Arg1),
      ".__kmpc_global_thread_num.");

  // Emit call to the helper function.
  llvm::Value *Arg3Val = CGF.Builder.CreateLoad(CGF.GetAddrOfLocalVar(Arg3),
      "arg3");
  QualType QTy = getContext().getRecordType(CS->getCapturedRecordDecl());
  llvm::Type *ConvertedType =
      CGF.getTypes().ConvertTypeForMem(QTy)->getPointerTo();
  llvm::Value *RecArg = CGF.Builder.CreatePointerCast(Arg3Val, ConvertedType,
      "(anon)arg3");

  // CodeGen for clauses (call start).
  {
    OpenMPRegionRAII OMPRegion(CGF, *CS, RecArg);
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I
          && (!IsAllowedClause((*I)->getClauseKind(), SKinds)
              || (*I)->getClauseKind() == OMPC_firstprivate))
        CGF.EmitPreOMPClause(*(*I), S);

    switch (DKind) {
    case OMPD_parallel:
      CGF.EmitStmt(CS->getCapturedStmt());
      break;
    case OMPD_parallel_sections:
      CGF.EmitOMPSectionsDirective(DKind, OMPD_sections, S);
      break;
    case OMPD_parallel_for:
      CGF.EmitOMPDirectiveWithLoop(DKind, OMPD_for, S);
      break;
    case OMPD_parallel_for_simd:
      CGF.EmitOMPDirectiveWithLoop(DKind, OMPD_for_simd, S);
      break;
    default:
      break;
    }
    CGF.EnsureInsertPoint();

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
        CGF.EmitPostOMPClause(*(*I), S);

    // CodeGen for clauses (closing steps).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
        CGF.EmitCloseOMPClause(*(*I), S);
  }

  CGF.EnsureInsertPoint();
  // Implicit barrier for simple parallel region only.
  // Others (combined) directives already has implicit barriers.
  if (DKind == OMPD_parallel) {
    CGF.EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL);
  }

  EmitFirstprivateInsert(CGF, S.getLocStart());

  CGF.FinishFunction();

  // CodeGen for "omp parallel {Associated statement}".
  {
    RunCleanupsScope MainBlock(*this);

    llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
    llvm::Type *KmpcMicroTy = llvm::TypeBuilder<kmpc_micro, false>::get(
        getLLVMContext());
    llvm::Value *RealArgs[] = { Loc, Builder.getInt32(2),
        CGF.Builder.CreateBitCast(Fn, KmpcMicroTy, "(kmpc_micro_ty)helper"),
        Builder.CreateBitCast(Arg, CGM.VoidPtrTy) };
    // __kmpc_fork_call(&loc, argc/*2*/, microtask, arg);
    EmitRuntimeCall(OPENMPRTL_FUNC(fork_call), makeArrayRef(RealArgs));
  }

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && !IsAllowedClause((*I)->getClauseKind(), SKinds))
      EmitFinalOMPClause(*(*I), S);

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();

  if (isTargetMode)
    CGM.getOpenMPRuntime().GenerateNextLabel(*this, true, false);

}

/// Generate an instructions for '#pragma omp parallel' directive.
void CodeGenFunction::EmitOMPParallelDirective(const OMPParallelDirective &S) {
  EmitOMPDirectiveWithParallel(OMPD_parallel, OMPD_unknown, S);
}

/// Generate an instructions for '#pragma omp parallel for' directive.
void CodeGenFunction::EmitOMPParallelForDirective(
    const OMPParallelForDirective &S) {
  EmitOMPDirectiveWithParallel(OMPD_parallel_for, OMPD_for, S);
}

/// Generate an instructions for '#pragma omp parallel for simd' directive.
void CodeGenFunction::EmitOMPParallelForSimdDirective(
    const OMPParallelForSimdDirective &S) {
  EmitOMPDirectiveWithParallel(OMPD_parallel_for_simd, OMPD_for_simd, S);
}

/// Generate an instructions for '#pragma omp parallel sections' directive.
void CodeGenFunction::EmitOMPParallelSectionsDirective(
    const OMPParallelSectionsDirective &S) {
  EmitOMPDirectiveWithParallel(OMPD_parallel_sections, OMPD_sections, S);
}

/// Generate instruction for OpenMP loop-like directives.
void CodeGenFunction::EmitOMPDirectiveWithLoop(OpenMPDirectiveKind DKind,
    OpenMPDirectiveKind SKind, const OMPExecutableDirective &S) {

  // Several Simd-specific vars are declared here.
  // OMPD_distribute_parallel_for_simd is not included because it separates to
  // OMPD_distribute and OMPD_parallel_for_simd directives intentionally and
  // HasSimd is processed for OMPD_parallel_for_simd part.
  bool HasSimd = DKind == OMPD_parallel_for_simd || DKind == OMPD_for_simd
      || DKind == OMPD_distribute_simd || DKind == OMPD_teams_distribute_simd
      || DKind == OMPD_target_teams_distribute_simd;
  CGPragmaOmpSimd SimdWrapper(&S);
  llvm::Function *BodyFunction = 0;
  bool SeparateLastIter = false;
  LValue CapStruct;

  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(false);
  CGM.OpenMPSupport.setNoWait(false);
  CGM.OpenMPSupport.setMergeable(true);
  CGM.OpenMPSupport.setOrdered(false);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);

  bool IsDistributeLoop = DKind == OMPD_distribute
      || DKind == OMPD_distribute_simd || DKind == OMPD_distribute_parallel_for
      || DKind == OMPD_distribute_parallel_for_simd
      || DKind == OMPD_teams_distribute_parallel_for
      || DKind == OMPD_teams_distribute_parallel_for_simd
      || DKind == OMPD_target_teams_distribute_parallel_for
      || DKind == OMPD_target_teams_distribute_parallel_for_simd
      || DKind == OMPD_teams_distribute || DKind == OMPD_teams_distribute_simd
      || DKind == OMPD_target_teams_distribute
      || DKind == OMPD_target_teams_distribute_simd;
  int Schedule = KMP_SCH_DEFAULT;
  if (!IsDistributeLoop) {
    bool Ordered = CGM.OpenMPSupport.getOrdered();
    bool Merge = CGM.OpenMPSupport.getMergeable();
    int Offset = 0;
    if (Ordered && Merge)
      Offset = SCH_ORD;
    else if (!Ordered && !Merge)
      Offset = SCH_NM;
    else if (Ordered && !Merge)
      Offset = SCH_NM_ORD;
    Schedule += Offset;
  } else {
    Schedule = KMP_SCH_DISTRIBUTE_STATIC;
  }
  CGM.OpenMPSupport.setScheduleChunkSize(Schedule, 0);

  llvm::BasicBlock *PrecondEndBB = createBasicBlock("omp.loop.precond_end");
  {
    RunCleanupsScope ExecutedScope(*this);
    // CodeGen for clauses (call start).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
        EmitPreOMPClause(*(*I), S);

    const Expr *ChunkSize;
    CGM.OpenMPSupport.getScheduleChunkSize(Schedule, ChunkSize);
    OpenMPDirectiveKind Kind = S.getDirectiveKind();
    bool IsComplexParallelLoop = Kind == OMPD_distribute_parallel_for
        || Kind == OMPD_distribute_parallel_for_simd
        || Kind == OMPD_teams_distribute_parallel_for
        || Kind == OMPD_teams_distribute_parallel_for_simd
        || Kind == OMPD_target_teams_distribute_parallel_for
        || Kind == OMPD_target_teams_distribute_parallel_for_simd;
    bool IsInnerLoopGen = IsComplexParallelLoop && DKind != Kind;
    bool IsStaticSchedule = Schedule == KMP_SCH_STATIC_CHUNKED
        || Schedule == KMP_SCH_STATIC
        || Schedule == KMP_SCH_DISTRIBUTE_STATIC_CHUNKED
        || Schedule == KMP_SCH_DISTRIBUTE_STATIC;
    // CodeGen for "omp for {Associated statement}".
    {
      llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
      llvm::Value *GTid =
      OPENMPRTL_THREADNUM(S.getLocStart(), *this);
      const Expr *IterVar = getNewIterVarFromLoopDirective(&S);
      QualType QTy = IterVar->getType();
      uint64_t TypeSize = 32;
      if (getContext().getTypeSize(QTy) > TypeSize)
        TypeSize = 64;
      bool isSigned = true;
      if (QTy->hasUnsignedIntegerRepresentation())
        isSigned = false;
      llvm::Type *VarTy = TypeSize == 32 ? Int32Ty : Int64Ty;
      llvm::Value *LB = 0;
      llvm::Value *UB = 0;
      llvm::Value *GlobalUB = 0;
      // Generate loop for inner 'for' directive
      if (IsInnerLoopGen) {
        LB = EmitScalarExpr(getLowerBoundFromLoopDirective(&S));
        UB = EmitScalarExpr(getUpperBoundFromLoopDirective(&S));
      } else {
        LB = llvm::Constant::getNullValue(VarTy);
        UB = EmitScalarExpr(getNewIterEndFromLoopDirective(&S));
      }
      GlobalUB = UB;

      llvm::AllocaInst *GlobalUBStack = CreateMemTemp(
              getNewIterEndFromLoopDirective(&S)->getType(), "global.ub.stack");
      Builder.CreateStore(UB, GlobalUBStack);

#ifdef DEBUG
      llvm::AllocaInst *DebugUB = CreateMemTemp(
          getNewIterEndFromLoopDirective(&S)->getType(), "debug.ub");
      Builder.CreateStore(UB, DebugUB);
#endif
      UB = Builder.CreateIntCast(UB, VarTy, isSigned);
      llvm::Value *Chunk;
      if (ChunkSize) {
        Chunk = EmitScalarExpr(ChunkSize);
        Chunk = Builder.CreateIntCast(Chunk, VarTy, true);
      } else {
        Chunk = llvm::Constant::getNullValue(VarTy);
      }
      llvm::BasicBlock *EndBB = createBasicBlock("omp.loop.end");
      llvm::BasicBlock *OMPLoopBB = 0; // createBasicBlock("omp.loop.begin");
      llvm::AllocaInst *PLast = CreateTempAlloca(Int32Ty, "last");
      PLast->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(Int32Ty));
      InitTempAlloca(PLast,
          IsStaticSchedule ? Builder.getInt32(1) : Builder.getInt32(0));
      llvm::AllocaInst *PLB = CreateTempAlloca(VarTy, "lb");
      PLB->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(VarTy));
      Builder.CreateStore(LB, PLB);
      llvm::AllocaInst *PUB = CreateTempAlloca(VarTy, "ub");
      PUB->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(VarTy));
      Builder.CreateStore(UB, PUB);
      llvm::AllocaInst *PSt = CreateTempAlloca(VarTy, "st");
      PSt->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(VarTy));
      InitTempAlloca(PSt,
          TypeSize == 32 ? Builder.getInt32(1) : Builder.getInt64(1));
      llvm::AllocaInst *Private = CreateMemTemp(QTy, ".idx.");
      llvm::Type *IdxTy =
          cast<llvm::PointerType>(Private->getType())->getElementType();
      llvm::BasicBlock *MainBB;
      llvm::BasicBlock *FiniBB = 0;

      const Stmt *Body = S.getAssociatedStmt();
      ArrayRef<Expr *> Arr = getCountersFromLoopDirective(&S);
      if (const CapturedStmt *CS = dyn_cast_or_null<CapturedStmt>(Body))
        Body = CS->getCapturedStmt();
      const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(IterVar)->getDecl());
      CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
      for (unsigned I = 0; I < getCollapsedNumberFromLoopDirective(&S); ++I) {
        RunCleanupsScope InitScope(*this);
        const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(Arr[I])->getDecl());
        bool SkippedContainers = false;
        while (!SkippedContainers) {
          if (const AttributedStmt *AS = dyn_cast_or_null<AttributedStmt>(Body))
            Body = AS->getSubStmt();
          else if (const CompoundStmt *CS = dyn_cast_or_null<CompoundStmt>(
              Body)) {
            if (CS->size() != 1) {
              SkippedContainers = true;
            } else {
              Body = CS->body_back();
            }
          } else
            SkippedContainers = true;
        }
        const ForStmt *For = dyn_cast_or_null<ForStmt>(Body);
        Body = For->getBody();
        if (CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD))
          continue;
        QualType QTy = Arr[I]->getType();
        llvm::Value *Private = CreateMemTemp(QTy, CGM.getMangledName(VD) + ".private.");
        CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
        llvm::BasicBlock *PrecondBB = createBasicBlock("omp.loop.precond");
        if (isa<DeclStmt>(For->getInit()))
          EmitAnyExprToMem(VD->getAnyInitializer(), Private,
                           VD->getType().getQualifiers(),
                           /*IsInitializer=*/true);
        else
          EmitStmt(For->getInit());
        EmitBranchOnBoolExpr(For->getCond(), PrecondBB, PrecondEndBB, 0);
        EmitBlock(PrecondBB);
      }

      if (IsStaticSchedule) {
        llvm::Value *RealArgs[] = { Loc, GTid, Builder.getInt32(Schedule),
            PLast, PLB, PUB, PSt,
                TypeSize == 32 ? Builder.getInt32(1) : Builder.getInt64(1),
            Chunk };
        if (TypeSize == 32 && isSigned)
        EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_4), RealArgs);
        else if (TypeSize == 32 && !isSigned)
          EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_4u), RealArgs);
        else if (TypeSize == 64 && isSigned)
          EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_8), RealArgs);
        else
          EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_8u), RealArgs);
        OMPLoopBB = createBasicBlock("omp.loop.begin");
        EmitBlock(OMPLoopBB);
        LB = Builder.CreateLoad(PLB);
        Builder.CreateStore(LB, Private);
        UB = Builder.CreateLoad(PUB);
        llvm::Value *Cond = Builder.CreateICmp(
            isSigned ? llvm::CmpInst::ICMP_SLT : llvm::CmpInst::ICMP_ULT, UB,
            GlobalUB);
        UB = Builder.CreateSelect(Cond, UB, GlobalUB);
        Builder.CreateStore(UB, PUB);
        MainBB = createBasicBlock("omp.loop.main");
        FiniBB = createBasicBlock("omp.loop.fini");
      } else {
        llvm::IntegerType *SchedTy = llvm::TypeBuilder<sched_type, false>::get(
            getLLVMContext());
        llvm::Value *RealArgs[] = { Loc, GTid, llvm::ConstantInt::get(SchedTy,
            Schedule), LB, UB,
            TypeSize == 32 ? Builder.getInt32(1) : Builder.getInt64(1), Chunk };
        // __kmpc_dispatch_init{4, 8}(&loc, gtid, sched_type, lb, ub, st,
        // chunk);
        if (TypeSize == 32 && isSigned)
          EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_init_4), RealArgs);
        else if (TypeSize == 32 && !isSigned)
          EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_init_4u), RealArgs);
        else if (TypeSize == 64 && isSigned)
          EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_init_8), RealArgs);
        else
          EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_init_8u), RealArgs);
        llvm::Value *RealArgsNext[] = { Loc, GTid, PLast, PLB, PUB, PSt };
        OMPLoopBB = createBasicBlock("omp.loop.begin");
        EmitBlock(OMPLoopBB);
        llvm::Value *CallRes;
        if (TypeSize == 32 && isSigned)
          CallRes = EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_next_4),
              RealArgsNext);
        else if (TypeSize == 32 && !isSigned)
          CallRes = EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_next_4u),
              RealArgsNext);
        else if (TypeSize == 64 && isSigned)
          CallRes = EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_next_8),
              RealArgsNext);
        else
          CallRes = EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_next_8u),
              RealArgsNext);
        llvm::BasicBlock *OMPInitBB = createBasicBlock("omp.loop.init");
        llvm::SwitchInst *Switch = Builder.CreateSwitch(
            Builder.CreateIntCast(CallRes, Int32Ty, false), EndBB, 1);
        Switch->addCase(llvm::ConstantInt::get(Int32Ty, 1), OMPInitBB);
        EmitBranch(OMPInitBB);
        EmitBlock(OMPInitBB);
        LB = Builder.CreateLoad(PLB);
        UB = Builder.CreateLoad(PUB);
        Builder.CreateStore(LB, Private);
        MainBB = createBasicBlock("omp.loop.main");
        FiniBB = createBasicBlock("omp.loop.fini");
      }
      if (HasSimd) {
        // Update vectorizer width on the loop stack.
        SeparateLastIter = SimdWrapper.emitSafelen(this);

        if (SeparateLastIter) {
          // Emit the following for the lastprivate vars update:
          //   --UB;
          // It is unclear if putting it under "if (*PLast)" will be
          // more or less efficient, this needs to be investigated.
          UB = Builder.CreateSub(UB, llvm::ConstantInt::get(UB->getType(), 1));
          Builder.CreateStore(UB, PUB);
        }

        // Initialize the captured struct.
        CapStruct = InitCapturedStruct(*SimdWrapper.getAssociatedStmt());
      }

      EmitBranch(MainBB);
      EmitBlock(MainBB);

      if (IsStaticSchedule) {
        LB = Builder.CreateLoad(PLB);
        GlobalUB = Builder.CreateLoad(GlobalUBStack);
        llvm::Value *Cond = Builder.CreateICmp(
            isSigned ? llvm::CmpInst::ICMP_SLE : llvm::CmpInst::ICMP_ULE, LB,
            GlobalUB);
        llvm::BasicBlock *ContBB = createBasicBlock("omp.lb.le.global_ub.");
        Builder.CreateCondBr(Cond, ContBB, EndBB);
        EmitBlock(ContBB);
      }

      if (HasSimd) {
        // Push current LoopInfo onto the LoopStack.
        LoopStack.Push(MainBB);
      }

      {
        RunCleanupsScope ThenScope(*this);
        EmitStmt(getInitFromLoopDirective(&S));
#ifdef DEBUG
        // CodeGen for clauses (call start).
        for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
            S.clauses().end(); I != E; ++I)
          if (const OMPLastPrivateClause *Clause = dyn_cast_or_null<
              OMPLastPrivateClause>(*I)) {
            for (OMPLastPrivateClause::varlist_const_iterator I1 =
                Clause->varlist_begin(), E1 = Clause->varlist_end(); I1 != E1;
                ++I1) {
              const VarDecl *VD = cast<VarDecl>(
                  cast<DeclRefExpr>(*I1)->getDecl());
              if (VD->getName() == "IDX")
                CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
              else if (VD->getName() == "UB")
                CGM.OpenMPSupport.addOpenMPPrivateVar(VD, DebugUB);
              else if (VD->getName() == "LUB")
                CGM.OpenMPSupport.addOpenMPPrivateVar(VD, PUB);
              else if (VD->getName() == "LLB")
                CGM.OpenMPSupport.addOpenMPPrivateVar(VD, PLB);
            }
          }
#endif
        llvm::Value *Idx = Builder.CreateLoad(Private, ".idx.");
        llvm::BasicBlock *UBLBCheckBB = createBasicBlock(
            "omp.lb_ub.check_pass");

        UB = Builder.CreateLoad(PUB);
        llvm::Value *UBLBCheck =
            isSigned ?
                Builder.CreateICmpSLE(Idx, UB, "omp.idx.le.ub") :
                Builder.CreateICmpULE(Idx, UB, "omp.idx.le.ub");
        // llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
        Builder.CreateCondBr(UBLBCheck, UBLBCheckBB, FiniBB);
        EmitBlock(UBLBCheckBB);
        llvm::BasicBlock *ContBlock = createBasicBlock("omp.cont.block");

        BreakContinueStack.push_back(
            BreakContinue(getJumpDestInCurrentScope(EndBB),
                getJumpDestInCurrentScope(ContBlock)));
        if (HasSimd) {
          RunCleanupsScope Scope(*this);
          BodyFunction = EmitSimdFunction(SimdWrapper);
          EmitSIMDForHelperCall(BodyFunction, CapStruct, Private, false);
        } else {
          RunCleanupsScope Scope(*this);
          if (IsInnerLoopGen || !IsComplexParallelLoop) {
            if (SKind == OMPD_for)
              OMPCancelMap[OMPD_for] = getJumpDestInCurrentScope(EndBB);
            EmitStmt(Body);
            OMPCancelMap.erase(OMPD_for);
          } else {
            const Expr *LowerBound = getLowerBoundFromLoopDirective(&S);
            const Expr *UpperBound = getUpperBoundFromLoopDirective(&S);
            EmitStoreOfScalar(Builder.CreateLoad(PLB), EmitLValue(LowerBound));
            EmitStoreOfScalar(Builder.CreateLoad(PUB), EmitLValue(UpperBound));
            // Special codegen for distribute parallel for [simd] constructs
            if (Kind == OMPD_distribute_parallel_for
                || Kind == OMPD_teams_distribute_parallel_for
                || Kind == OMPD_target_teams_distribute_parallel_for)
              EmitOMPDirectiveWithParallel(OMPD_parallel_for, OMPD_for, S);
            else if (Kind == OMPD_distribute_parallel_for_simd
                || Kind == OMPD_teams_distribute_parallel_for_simd
                || Kind == OMPD_target_teams_distribute_parallel_for_simd)
              EmitOMPDirectiveWithParallel(OMPD_parallel_for_simd,
                  OMPD_for_simd, S);
          }
        }
        BreakContinueStack.pop_back();
        EnsureInsertPoint();
        EmitBranch(ContBlock);
        EmitBlock(ContBlock);
        Idx = Builder.CreateLoad(Private, ".idx.");

        // Specialization for NVPTX backend for static schedules with chunk
        // specified: when chunk size > warpSize we need a stride for the
        // innermost loop
        llvm::Value *NextIdx;
        if (CGM.getLangOpts().OpenMPTargetMode
            && (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
                || CGM.getTarget().getTriple().getArch()
                    == llvm::Triple::nvptx64)) {

          NextIdx = CGM.getOpenMPRuntime().GetNextIdIncrement(*this,
              IsStaticSchedule, ChunkSize, Chunk, IdxTy, QTy, Idx, Kind, SKind,
              PSt);

        } else {
          NextIdx = Builder.CreateAdd(Idx, llvm::ConstantInt::get(IdxTy, 1),
              ".next.idx.", false, QTy->isSignedIntegerOrEnumerationType());
        }

        Builder.CreateStore(NextIdx, Private);
        if (!IsStaticSchedule && CGM.OpenMPSupport.getOrdered()) {
          // Emit _dispatch_fini for ordered loops
          llvm::Value *RealArgsFini[] = { Loc, GTid };
          if (TypeSize == 32 && isSigned)
            EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_fini_4), RealArgsFini);
          else if (TypeSize == 32 && !isSigned)
            EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_fini_4u), RealArgsFini);
          else if (TypeSize == 64 && isSigned)
            EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_fini_8), RealArgsFini);
          else
            EmitRuntimeCall(OPENMPRTL_FUNC(dispatch_fini_8u), RealArgsFini);
        }

        EmitBranch(MainBB);
        if (HasSimd) {
          LoopStack.Pop();
        }
        EmitBlock(FiniBB);
        if (IsStaticSchedule && ChunkSize != 0) {
          llvm::Value *St = Builder.CreateLoad(PSt);
          LB = Builder.CreateLoad(PLB);
          LB = Builder.CreateAdd(LB, St);
          Builder.CreateStore(LB, PLB);
          UB = Builder.CreateLoad(PUB);
          UB = Builder.CreateAdd(UB, St);
          Builder.CreateStore(UB, PUB);
        }
        if (SeparateLastIter) {
          // Emit the following for the lastprivate vars update:
          //   call __simd_helper(cs, idx, 1)
          //
          EmitSIMDForHelperCall(BodyFunction, CapStruct, Private, true);
        }
        EmitBranch(!IsStaticSchedule || ChunkSize != 0 ? OMPLoopBB : EndBB);
      }
      EmitBlock(EndBB, true);
      if (IsStaticSchedule) {
    	  llvm::Value *GTidReload =
    	        OPENMPRTL_THREADNUM(S.getLocStart(), *this);

    	  llvm::Value *RealArgsFini[] = { Loc, GTidReload };
        EmitRuntimeCall(OPENMPRTL_FUNC(for_static_fini), RealArgsFini);
      }
      CGM.OpenMPSupport.setLastIterVar(PLast);
    }

    if (!IsDistributeLoop
        && (CGM.OpenMPSupport.hasLastPrivate() || !CGM.OpenMPSupport.getNoWait()))
      EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL_FOR);
    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
        EmitPostOMPClause(*(*I), S);
  }

  // CodeGen for clauses (closing steps).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitCloseOMPClause(*(*I), S);

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitFinalOMPClause(*(*I), S);

  EnsureInsertPoint();

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();

  if (HasSimd) {
    // Emit the final values of 'linear' variables.
    SimdWrapper.emitLinearFinal(*this);
  }
  EmitBlock(PrecondEndBB);
}

/// Generate an instructions for '#pragma omp for' directive.
void CodeGenFunction::EmitOMPForDirective(const OMPForDirective &S) {
  EmitOMPDirectiveWithLoop(OMPD_for, OMPD_for, S);
}

/// Generate an instructions for '#pragma omp distribute' directive.
void CodeGenFunction::EmitOMPDistributeDirective(
    const OMPDistributeDirective &S) {
  CGM.OpenMPSupport.setDistribute(true);
  EmitOMPDirectiveWithLoop(OMPD_distribute, OMPD_distribute, S);
}

/// Generate an instructions for directive with 'teams' region.
void CodeGenFunction::EmitOMPDirectiveWithTeams(OpenMPDirectiveKind DKind,
    OpenMPDirectiveKind SKind, const OMPExecutableDirective &S) {
  if (CGM.getOpenMPRuntime().requiresMicroTaskForTeams())
    EmitOMPDirectiveWithTeamsMicrotask(DKind, SKind, S);
  else
    EmitOMPDirectiveWithTeamsNoMicrotask(DKind, SKind, S);
}

void CodeGenFunction::EmitOMPDirectiveWithTeamsNoMicrotask(
    OpenMPDirectiveKind DKind, OpenMPDirectiveKind SKind,
    const OMPExecutableDirective &S) {
  // Generate shared args for captured stmt.
  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());

  // Init list of private globals in the stack.
  // We do not need to create a new region for teams as it was previously
  // created for directive with target.
  CGM.OpenMPSupport.setMergeable(false);
  CGM.OpenMPSupport.setOrdered(false);
  CGM.OpenMPSupport.setNoWait(true);
  CGM.OpenMPSupport.setScheduleChunkSize(KMP_SCH_DEFAULT, 0);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);

  // CodeGen for clauses (call start).
  {
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        EmitPreOMPClause(*(*I), S);

    switch (DKind) {
    case OMPD_target_teams:
    case OMPD_teams:
      EmitStmt(CS->getCapturedStmt());
      break;
    case OMPD_teams_distribute:
    case OMPD_target_teams_distribute:
      EmitOMPDirectiveWithLoop(OMPD_teams_distribute, OMPD_distribute, S);
      break;
    case OMPD_teams_distribute_simd:
    case OMPD_target_teams_distribute_simd:
      EmitOMPDirectiveWithLoop(OMPD_teams_distribute_simd, OMPD_distribute_simd,
          S);
      break;
    case OMPD_teams_distribute_parallel_for: {
      const OMPTeamsDistributeParallelForDirective &D = cast<
          OMPTeamsDistributeParallelForDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      EmitOMPDirectiveWithLoop(OMPD_teams_distribute_parallel_for,
          OMPD_distribute, S);
      break;
    }
    case OMPD_teams_distribute_parallel_for_simd: {
      const OMPTeamsDistributeParallelForSimdDirective &D = cast<
          OMPTeamsDistributeParallelForSimdDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      EmitOMPDirectiveWithLoop(OMPD_teams_distribute_parallel_for_simd,
          OMPD_distribute, S);
      break;
    }
    case OMPD_target_teams_distribute_parallel_for: {
      const OMPTargetTeamsDistributeParallelForDirective &D = cast<
          OMPTargetTeamsDistributeParallelForDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      EmitOMPDirectiveWithLoop(OMPD_target_teams_distribute_parallel_for,
          OMPD_distribute, S);
      break;
    }
    case OMPD_target_teams_distribute_parallel_for_simd: {
      const OMPTargetTeamsDistributeParallelForSimdDirective &D = cast<
          OMPTargetTeamsDistributeParallelForSimdDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      EmitOMPDirectiveWithLoop(OMPD_target_teams_distribute_parallel_for_simd,
          OMPD_distribute, S);
      break;
    }
    default:
      break;
    }
    EnsureInsertPoint();

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        EmitPostOMPClause(*(*I), S);

    // CodeGen for clauses (closing steps).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        EmitCloseOMPClause(*(*I), S);
  }

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitFinalOMPClause(*(*I), S);

}

void CodeGenFunction::EmitOMPDirectiveWithTeamsMicrotask(
    OpenMPDirectiveKind DKind, OpenMPDirectiveKind SKind,
    const OMPExecutableDirective &S) {
  // Generate shared args for captured stmt.
  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());

  llvm::Value* Arg = 0;
  {
    const RecordDecl *RD = CS->getCapturedRecordDecl();
    QualType RecordTy = getContext().getRecordType(RD);

    // Initialize the captured struct.
    LValue SlotLV = MakeNaturalAlignAddrLValue(
        CreateMemTemp(RecordTy, "agg.captured"), RecordTy);
    Arg = SlotLV.getAddress();

    RecordDecl::field_iterator CurField = RD->field_begin();
    for (CapturedStmt::capture_init_iterator I = CS->capture_init_begin(),
                                             E = CS->capture_init_end();
         I != E; ++I, ++CurField) {

      DeclRefExpr *DE = cast<DeclRefExpr>(*I);

      LValue LV = EmitLValueForFieldInitialization(SlotLV, *CurField);
      LValue LVInit = EmitDeclRefLValue(DE);

      Builder.CreateStore(LVInit.getAddress(), LV.getAddress());
    }
  }

  // Init list of private globals in the stack.
  // We do not need to create a new region for teams as it was previously
  // created for directive with target.
  CGM.OpenMPSupport.setMergeable(false);
  CGM.OpenMPSupport.setOrdered(false);
  CGM.OpenMPSupport.setNoWait(true);
  CGM.OpenMPSupport.setScheduleChunkSize(KMP_SCH_DEFAULT, 0);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);
  Expr *NumTeams = CGM.OpenMPSupport.getNumTeams();
  Expr *ThreadLimit = CGM.OpenMPSupport.getThreadLimit();
  if (NumTeams || ThreadLimit) {
    // __kmpc_push_num_teams(&loc, global_tid, num_threads, thread_limit);
    // ident_t loc = {...};
    llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
    // global_tid = __kmpc_global_thread_num(...);
    llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
    llvm::Value *RealArgs[] = { Loc, GTid,
        NumTeams ? EmitScalarExpr(NumTeams,true) : Builder.getInt32(0),
        ThreadLimit ? EmitScalarExpr(ThreadLimit,true) : Builder.getInt32(0) };
    EmitRuntimeCall(OPENMPRTL_FUNC(push_num_teams), RealArgs);
  }

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);

  // Generate microtask.
  // void .omp_microtask.(int32_t *, int32_t *, void */*AutoGenRecord **/arg3) {
  //  captured_stmt(arg3);
  // }
  IdentifierInfo *Id = &getContext().Idents.get(".omp_microtask.");
  QualType PtrIntTy = getContext().getPointerType(getContext().IntTy);
  SmallVector<QualType, 4> FnArgTypes;
  FnArgTypes.push_back(PtrIntTy);
  FnArgTypes.push_back(PtrIntTy);
  FnArgTypes.push_back(getContext().VoidPtrTy);
  FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpecType = EST_BasicNoexcept;
  QualType FnTy = getContext().getFunctionType(getContext().VoidTy, FnArgTypes,
      EPI);
  TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(FnTy,
      SourceLocation());
  FunctionDecl *FD = FunctionDecl::Create(getContext(),
      getContext().getTranslationUnitDecl(), CS->getLocStart(),
      SourceLocation(), Id, FnTy, TI, SC_Static, false, false, false);
  TypeSourceInfo *PtrIntTI = getContext().getTrivialTypeSourceInfo(PtrIntTy,
      SourceLocation());
  TypeSourceInfo *PtrVoidTI = getContext().getTrivialTypeSourceInfo(
      getContext().VoidPtrTy, SourceLocation());
  ParmVarDecl *Arg1 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, PtrIntTy, PtrIntTI, SC_Auto, 0);
  ParmVarDecl *Arg2 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, PtrIntTy, PtrIntTI, SC_Auto, 0);
  ParmVarDecl *Arg3 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, getContext().VoidPtrTy, PtrVoidTI, SC_Auto, 0);
  CodeGenFunction CGF(CGM, true);
  const CGFunctionInfo &FI = getTypes().arrangeFunctionDeclaration(FD);
  llvm::Function *Fn = llvm::Function::Create(getTypes().GetFunctionType(FI),
      llvm::GlobalValue::PrivateLinkage, FD->getName(), &CGM.getModule());
  CGM.SetInternalFunctionAttributes(CurFuncDecl, Fn, FI);
  llvm::AttributeSet Set = CurFn->getAttributes();
  for (unsigned i = 0; i < Set.getNumSlots(); ++i) {
    if (Set.getSlotIndex(i) == llvm::AttributeSet::FunctionIndex) {
      for (llvm::AttributeSet::iterator I = Set.begin(i), E = Set.end(i);
          I != E; ++I) {
        if (I->isStringAttribute() && I->getKindAsString().startswith("INTEL:"))
          Fn->addFnAttr(I->getKindAsString());
      }
    }
  }
  FunctionArgList FnArgs;
  FnArgs.push_back(Arg1);
  FnArgs.push_back(Arg2);
  FnArgs.push_back(Arg3);
  CGF.OpenMPRoot = OpenMPRoot ? OpenMPRoot : this;
  CGF.StartFunction(FD, getContext().VoidTy, Fn, FI, FnArgs, SourceLocation());
  CGF.Builder.CreateLoad(CGF.GetAddrOfLocalVar(Arg1),
      ".__kmpc_global_thread_num.");

  // Emit call to the helper function.
  llvm::Value *Arg3Val = CGF.Builder.CreateLoad(CGF.GetAddrOfLocalVar(Arg3),
      "arg3");
  QualType QTy = getContext().getRecordType(CS->getCapturedRecordDecl());
  llvm::Type *ConvertedType =
      CGF.getTypes().ConvertTypeForMem(QTy)->getPointerTo();
  llvm::Value *RecArg = CGF.Builder.CreatePointerCast(Arg3Val, ConvertedType,
      "(anon)arg3");

  // CodeGen for clauses (call start).
  {
    OpenMPRegionRAII OMPRegion(CGF, *CS, RecArg);
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        CGF.EmitPreOMPClause(*(*I), S);

    switch (DKind) {
    case OMPD_target_teams:
    case OMPD_teams:
      CGF.EmitStmt(CS->getCapturedStmt());
      break;
    case OMPD_teams_distribute:
    case OMPD_target_teams_distribute:
      CGF.EmitOMPDirectiveWithLoop(OMPD_teams_distribute, OMPD_distribute, S);
      break;
    case OMPD_teams_distribute_simd:
    case OMPD_target_teams_distribute_simd:
      CGF.EmitOMPDirectiveWithLoop(OMPD_teams_distribute_simd, OMPD_distribute_simd,
          S);
      break;
    case OMPD_teams_distribute_parallel_for: {
      const OMPTeamsDistributeParallelForDirective &D = cast<
          OMPTeamsDistributeParallelForDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      CGF.EmitOMPDirectiveWithLoop(OMPD_teams_distribute_parallel_for,
          OMPD_distribute, S);
      break;
    }
    case OMPD_teams_distribute_parallel_for_simd: {
      const OMPTeamsDistributeParallelForSimdDirective &D = cast<
          OMPTeamsDistributeParallelForSimdDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      CGF.EmitOMPDirectiveWithLoop(OMPD_teams_distribute_parallel_for_simd,
          OMPD_distribute, S);
      break;
    }
    case OMPD_target_teams_distribute_parallel_for: {
      const OMPTargetTeamsDistributeParallelForDirective &D = cast<
          OMPTargetTeamsDistributeParallelForDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      CGF.EmitOMPDirectiveWithLoop(OMPD_target_teams_distribute_parallel_for,
          OMPD_distribute, S);
      break;
    }
    case OMPD_target_teams_distribute_parallel_for_simd: {
      const OMPTargetTeamsDistributeParallelForSimdDirective &D = cast<
          OMPTargetTeamsDistributeParallelForSimdDirective>(S);
      assert(D.getLowerBound() && "No lower bound");
      assert(D.getUpperBound() && "No upper bound");
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getLowerBound())->getDecl()));
      CGF.EmitAutoVarDecl(
          *cast<VarDecl>(cast<DeclRefExpr>(D.getUpperBound())->getDecl()));
      CGF.EmitOMPDirectiveWithLoop(OMPD_target_teams_distribute_parallel_for_simd,
          OMPD_distribute, S);
      break;
    }
    default:
      break;
    }
    CGF.EnsureInsertPoint();

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        CGF.EmitPostOMPClause(*(*I), S);

    // CodeGen for clauses (closing steps).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
        CGF.EmitCloseOMPClause(*(*I), S);
  }

  CGF.FinishFunction();

  // CodeGen for "omp parallel {Associated statement}".
  {
    RunCleanupsScope MainBlock(*this);

    llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
    llvm::Type *KmpcMicroTy = llvm::TypeBuilder<kmpc_micro, false>::get(
        getLLVMContext());
    llvm::Value *RealArgs[] =
        { Loc, Builder.getInt32(2), CGF.Builder.CreateBitCast(Fn, KmpcMicroTy,
            "(kmpc_micro_ty)helper"), Arg };
    // __kmpc_fork_teams(&loc, argc/*2*/, microtask, arg);
    EmitRuntimeCall(OPENMPRTL_FUNC(fork_teams), makeArrayRef(RealArgs));
  }

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(OMPD_teams,(*I)->getClauseKind()))
      EmitFinalOMPClause(*(*I), S);

}

/// Generate instructions for directive with 'target' region.
void CodeGenFunction::EmitOMPDirectiveWithTarget(OpenMPDirectiveKind DKind,
    OpenMPDirectiveKind SKind, const OMPExecutableDirective &S) {

  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());

  // Are we generating code for a target?
  bool isTargetMode = CGM.getLangOpts().OpenMPTargetMode;

  assert(
      !(isTargetMode && CGM.getLangOpts().OMPTargetTriples.empty())
          && "Are we in target mode and no targets were specified??");

  // If there are no devices specified we ignore the target directive and just
  // produce regular host code
  if (CGM.getLangOpts().OMPTargetTriples.empty()) {
    EmitStmt(CS->getCapturedStmt());
    return;
  }

  // Determine whether this directive was processed before
  const RecordDecl *RD = CS->getCapturedRecordDecl();
  llvm::Function *Fn =
      CGM.getOpenMPRuntime().getEntryForDirectiveWithTarget(RD);

  CGM.OpenMPSupport.startOpenMPRegion(true);

  // We only need to do the outlining if hasn't already been done
  if (!Fn){

    if (isTargetMode)
      CGM.getOpenMPRuntime().StartNewTargetRegion();

    // Create the target function
    IdentifierInfo *Id = &getContext().Idents.get(
        CGM.getOpenMPRuntime().GetOffloadEntryMangledName(
            (isTargetMode) ? CGM.getTarget().getTriple() : llvm::Triple()));

    SmallVector<QualType, 4> FnArgTypes;
    FunctionArgList FnArgs;

    // Get function type
    for (RecordDecl::field_iterator fb = RD->field_begin(), fe = RD->field_end();
        fb != fe; ++fb) {

      QualType QTy = (*fb)->getType();

      // We don't need to emit types here as they are emitted when we start the
      // outlined function.

      FnArgTypes.push_back(QTy);
    }

    FunctionProtoType::ExtProtoInfo EPI;
    EPI.ExceptionSpecType = EST_BasicNoexcept;
    QualType FnTy = getContext().getFunctionType(getContext().VoidTy, FnArgTypes,
        EPI);

    // Create function declaration
    TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(FnTy,
        SourceLocation());
    FunctionDecl *FD = FunctionDecl::Create(getContext(),
        getContext().getTranslationUnitDecl(), CS->getLocStart(),
        SourceLocation(), Id, FnTy, TI, SC_Static, false, false, false);

    // Create function arguments
    for (RecordDecl::field_iterator fb = RD->field_begin(), fe = RD->field_end();
        fb != fe; ++fb) {
      QualType QTy = (*fb)->getType();
      TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(QTy,
          SourceLocation());
      ParmVarDecl *Arg = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
          SourceLocation(), 0, QTy, TI, SC_Auto, 0);

      FnArgs.push_back(Arg);
    }

    CodeGenFunction CGF(CGM, true);
    const CGFunctionInfo &FI = getTypes().arrangeFunctionDeclaration(FD);
    // The linkage here is going to be overwritten when the attributes are set
    Fn = llvm::Function::Create(getTypes().GetFunctionType(FI),
        llvm::GlobalValue::PrivateLinkage, FD->getName(), &CGM.getModule());

    // Register the new function in the runtime to assure order of the
    // entry points. We use the record declaration as the declaration associated
    // with the entry.
    CGM.getOpenMPRuntime().registerEntryDeclaration(RD);
    CGM.getOpenMPRuntime().CreateHostPtrForCurrentTargetRegion(RD, Fn);

    // PostProcess the function definition for the target and set the function
    // attributes based on the enclosing function
    // but force target functions to external linkage
    CGM.getOpenMPRuntime().PostProcessTargetFunction(CurFuncDecl, Fn, FI);

    if (isTargetMode)
      Fn->setLinkage(llvm::GlobalValue::ExternalLinkage);
    CGF.OpenMPRoot = OpenMPRoot ? OpenMPRoot : this;

    // This will initialize the cache so that any emission done inside
    // StartFunction will work. CGF has to have the right function in order
    // for that to work.
    CGF.CurFn = Fn;
    OpenMPRegionRAII OMPRegion(CGF, *CS, nullptr,
                               OpenMPRegionRAII::OMPRegionType_TargetInit);

    CGF.StartFunction(FD, getContext().VoidTy, Fn, FI, FnArgs, SourceLocation());

    {
      // This will complete the initialization done before
      OpenMPRegionRAII OMPRegion(CGF, *CS, nullptr,
                                 OpenMPRegionRAII::OMPRegionType_Target);

      if (isTargetMode) {
        // generate control loop for nvptx, empty codegen otherwise
        CGM.getOpenMPRuntime().GenerateTargetControlLoop(CS->getLocStart(), CGF);
      }

      // Emit the contents of the target region
      switch (SKind) {
      default:
        llvm_unreachable("Unexpected target directive subkind");
        break;
      case OMPD_teams:
        CGF.EmitOMPDirectiveWithTeams(OMPD_target_teams, OMPD_unknown, S);
        break;
      case OMPD_teams_distribute:
        CGF.EmitOMPDirectiveWithTeams(OMPD_target_teams_distribute, OMPD_distribute,
            S);
        break;
      case OMPD_teams_distribute_parallel_for:
        CGF.EmitOMPDirectiveWithTeams(OMPD_target_teams_distribute_parallel_for,
            OMPD_distribute_parallel_for, S);
        break;
      case OMPD_teams_distribute_parallel_for_simd:
        CGF.EmitOMPDirectiveWithTeams(
            OMPD_target_teams_distribute_parallel_for_simd,
            OMPD_distribute_parallel_for_simd, S);
        break;
      case OMPD_teams_distribute_simd:
        CGF.EmitOMPDirectiveWithTeams(OMPD_target_teams_distribute_simd,
            OMPD_distribute_simd, S);
        break;
      case OMPD_unknown:
        // No subkind, just emit the captured statement as is
        CGF.EmitStmt(CS->getCapturedStmt());
        break;
      }

      if (isTargetMode) {

        // at the end of the target region, set next label as the finishing case
        // note that we need to understand if we are in a sequential or parallel
        // region
        CGM.getOpenMPRuntime().GenerateFinishLabelSetting(CS->getLocStart(), CGF,
            false); //FIXME change false to prevIsParallel?
      }
    }
    CGF.FinishFunction();
  }

  // If we are generating code for the host, we need to emit the runtime calls
  if (!isTargetMode) {

    // Codegen target clauses init, this currently include
    // - device clause
    // - map clause
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_target,(*I)->getClauseKind()))
        EmitInitOMPClause(*(*I), S);

    // Codegen the arguments for the target region (used by runtime call and
    // host version). This has to be codegen before the clauses because it has
    // to be used in the if-false basic block created by the if clause. It also
    // has to be done after the init phase so we  have the map clause
    // information.
    llvm::SmallVector<llvm::Value*, 8> RealArgBasePointerValues;
    llvm::SmallVector<llvm::Value*, 8> RealArgPointerValues;
    llvm::SmallVector<llvm::Value*, 8> RealArgSizeValues;
    llvm::SmallVector<unsigned, 8> RealArgTypeValues;


    llvm::SmallVector<llvm::Value*, 8> HostArgs;

    if (!RD->field_empty()) {
      RecordDecl::field_iterator fb = RD->field_begin();
      unsigned idx = 0;

      // Get the map clause information
      ArrayRef<const Expr*>  MapClauseDecls;
      ArrayRef<llvm::Value*> MapClauseBasePointersArray;
      ArrayRef<llvm::Value*> MapClausePointersArray;
      ArrayRef<llvm::Value*> MapClauseSizesArray;
      ArrayRef<unsigned> MapClauseTypesArray;

      CGM.OpenMPSupport.getOffloadingMapArrays(MapClauseDecls,
          MapClauseBasePointersArray, MapClausePointersArray, MapClauseSizesArray,
          MapClauseTypesArray);

      // Scan the captured declarations
      for (CapturedStmt::capture_init_iterator ci = CS->capture_init_begin(),
          ce = CS->capture_init_end(); ci != ce; ++ci, ++fb, ++idx) {

        DeclRefExpr *DE = cast<DeclRefExpr>(*ci);

        QualType QTy = (*fb)->getType();
        LValue LV = MakeNaturalAlignAddrLValue(CreateMemTemp(QTy, ".tgt_arg"),
            QTy);
        EmitInitializerForField(*fb, LV, DE, ArrayRef<VarDecl *>());

        llvm::Value *Arg = Builder.CreateLoad(LV.getAddress());

        HostArgs.push_back(Arg);

        // Trace if this is already dealt with in the target region
        bool  dealtWithByMapClause = false;

        // Check if the captured declaration is already processed by the map
        // clause, and if so, leverage it
        for (unsigned i=0; i<MapClauseBasePointersArray.size() ; ++i){

          const DeclRefExpr *D =  cast<DeclRefExpr>(MapClauseDecls[i]);

          if (D->getDecl() != DE->getDecl())
            continue;

          RealArgBasePointerValues.push_back(MapClauseBasePointersArray[i]);
          RealArgPointerValues.push_back(MapClausePointersArray[i]);
          RealArgSizeValues.push_back(MapClauseSizesArray[i]);
          RealArgTypeValues.push_back(MapClauseTypesArray[i]);

          dealtWithByMapClause = true;
        }

        if (dealtWithByMapClause)
          continue;

        uint64_t VS = CGM.getDataLayout().getTypeSizeInBits(
            CGM.getTypes().ConvertTypeForMem(DE->getType())) / 8;

        unsigned DefaultType = OMP_TGT_MAPTYPE_TO | OMP_TGT_MAPTYPE_FROM;

        // if this declaration was processed in a map clause before, we should
        // only mark it alloc so that there are no default data_movements
        if (CGM.OpenMPSupport.locateInPreviousOffloadingMap(DE))
          DefaultType = OMP_TGT_MAPTYPE_ALLOC;

        RealArgBasePointerValues.push_back(Arg);
        RealArgPointerValues.push_back(Arg);
        RealArgSizeValues.push_back(Builder.getInt64(VS));
        RealArgTypeValues.push_back( DefaultType);

      }
    }

    // Because we are coalescing the codegen of the map clause with the target
    // outlining we need to inform the clause codegen to not do anything
    CGM.OpenMPSupport.setMapsBegin(true);
    CGM.OpenMPSupport.setMapsEnd(true);

    // Codegen target clauses after init, this currently include
    // - if clause
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_target,(*I)->getClauseKind()))
        EmitAfterInitOMPClause(*(*I), S);

    // If there was an if clause the insertion point should be set to the
    // if-true basic block

    // Codegen target clauses pre emission, this currently include
    // - map clause
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_target,(*I)->getClauseKind()))
        EmitPreOMPClause(*(*I), S);

    // Get or create value with the deviceID
    llvm::Value *DeviceID =
        (CGM.OpenMPSupport.getOffloadingDevice()) ?
            CGM.OpenMPSupport.getOffloadingDevice() :
            (llvm::Value*) Builder.getInt32(
                CGOpenMPRuntime::OMPRTL__target_device_id_undef);


    // Codegen tgt-target array arguments
    llvm::Value *FinalArgNumElems = Builder.getInt32(
        RealArgBasePointerValues.size());
    llvm::Value *FinalArgBasePointers = 0;
    llvm::Value *FinalArgPointers = 0;
    llvm::Value *FinalArgSizes = 0;
    llvm::Value *FinalArgTypes = 0;

    if (!RealArgBasePointerValues.empty()) {

      FinalArgBasePointers = Builder.CreateAlloca(CGM.VoidPtrTy,
                                                  FinalArgNumElems,
                                                  ".tgt_base_ptrs");
      FinalArgPointers = Builder.CreateAlloca(CGM.VoidPtrTy, FinalArgNumElems,
                                              ".tgt_ptrs");
      FinalArgSizes = Builder.CreateAlloca(CGM.Int64Ty, FinalArgNumElems,
                                              ".tgt_sizes");

      // Create the arrays to be passed to the runtime to map/launch the target
      // region

      for (unsigned i=0; i<RealArgBasePointerValues.size(); ++i){
        llvm::Value *BP =
            Builder.CreateConstInBoundsGEP1_32(FinalArgBasePointers,i);
        llvm::Value *P =
            Builder.CreateConstInBoundsGEP1_32(FinalArgPointers,i);
        llvm::Value *S =
            Builder.CreateConstInBoundsGEP1_32(FinalArgSizes,i);

        Builder.CreateStore(Builder.CreateBitCast(RealArgBasePointerValues[i],
                                                  CGM.VoidPtrTy),
                            BP);
        Builder.CreateStore(Builder.CreateBitCast(RealArgPointerValues[i],
                                                  CGM.VoidPtrTy),
                            P);
        Builder.CreateStore(Builder.CreateIntCast(RealArgSizeValues[i],
                                                  CGM.Int64Ty,true),
                            S);
      }

      // For the types, we don't need to allocate everything, we just create a
      // constant array
      llvm::Constant *FinalArgTypesInit = llvm::ConstantDataArray::get(
          Builder.getContext(), RealArgTypeValues);
      llvm::GlobalVariable *FinalArgTypesTmp = new llvm::GlobalVariable(
          CGM.getModule(), FinalArgTypesInit->getType(), true,
          llvm::GlobalValue::PrivateLinkage, FinalArgTypesInit, ".tgt_types");

      FinalArgTypes = Builder.CreateConstInBoundsGEP2_32(FinalArgTypesTmp, 0,0);

    } else {
      FinalArgBasePointers = llvm::Constant::getNullValue(CGM.VoidPtrPtrTy);
      FinalArgPointers = llvm::Constant::getNullValue(CGM.VoidPtrPtrTy);
      FinalArgSizes = llvm::Constant::getNullValue(CGM.Int64Ty->getPointerTo());
      FinalArgTypes = llvm::Constant::getNullValue(CGM.Int32Ty->getPointerTo());
    }

    // Obtain region arguments' references and fill the arguments ptr and size array
    //

    // Create call to tgt_target(_teams)
    llvm::SmallVector<llvm::Value*, 8> TgtArgs;

    TgtArgs.push_back(DeviceID);
    TgtArgs.push_back(llvm::ConstantExpr::getBitCast(Fn, CGM.VoidPtrTy));
    TgtArgs.push_back(FinalArgNumElems);
    TgtArgs.push_back(FinalArgBasePointers);
    TgtArgs.push_back(FinalArgPointers);
    TgtArgs.push_back(FinalArgSizes);
    TgtArgs.push_back(FinalArgTypes);

    Expr *NumTeamsExpr = CGM.OpenMPSupport.getNumTeams();
    Expr *ThreadLimitExpr = CGM.OpenMPSupport.getThreadLimit();
    llvm::Value *TgtTargetFn = 0;

    if (NumTeamsExpr && ThreadLimitExpr){
      TgtTargetFn = OPENMPRTL_FUNC(target_teams);
      TgtArgs.push_back(EmitScalarExpr(NumTeamsExpr,true));
      TgtArgs.push_back(EmitScalarExpr(ThreadLimitExpr,true));
    }
    else if (NumTeamsExpr) {
      TgtTargetFn = OPENMPRTL_FUNC(target_teams);
      TgtArgs.push_back(EmitScalarExpr(NumTeamsExpr,true));
      TgtArgs.push_back(Builder.getInt32(0));
    }
    else if (ThreadLimitExpr) {
      TgtTargetFn = OPENMPRTL_FUNC(target_teams);
      TgtArgs.push_back(Builder.getInt32(0));
      TgtArgs.push_back(EmitScalarExpr(ThreadLimitExpr,true));
    }
    else if (DKind == OMPD_target_teams) {
      // target teams without teams clauses
      TgtTargetFn = OPENMPRTL_FUNC(target_teams);
      TgtArgs.push_back(Builder.getInt32(0));
      TgtArgs.push_back(Builder.getInt32(0));
    }
    else {
      TgtTargetFn = OPENMPRTL_FUNC(target);
    }

    llvm::Value *Offload = Builder.CreateCall(TgtTargetFn, TgtArgs,
        "offloadret");

    // Create call to host if offloading failed
    llvm::Value *OffloadSuccess = Builder.CreateICmpEQ(Offload,
        Builder.getInt32(0));

    llvm::BasicBlock *OffloadFailedBB = this->createBasicBlock("offload_fail",
        this->CurFn);
    llvm::BasicBlock *AfterOffloadBB = this->createBasicBlock("after_offload",
        this->CurFn);

    Builder.CreateCondBr(OffloadSuccess, AfterOffloadBB, OffloadFailedBB);
    Builder.SetInsertPoint(OffloadFailedBB);

    // Here, we only need to pass the real base pointers
    llvm::CallInst *HostFunctionCall = Builder.CreateCall(Fn, HostArgs);

    Builder.CreateBr(AfterOffloadBB);
    Builder.SetInsertPoint(AfterOffloadBB);

    // We store the host function call to the OpenMP stack so that it can be
    // cloned to the if-false basic block generated by the if clause
    CGM.OpenMPSupport.setOffloadingHostFunctionCall(HostFunctionCall);

    // Codegen target clauses post emission, this currently include
    // - map clause
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_target,(*I)->getClauseKind()))
        EmitPostOMPClause(*(*I), S);

    // Codegen target clauses final, this currently include
    // - if clause
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(OMPD_target,(*I)->getClauseKind()))
        EmitFinalOMPClause(*(*I), S);

  }

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();
}

static void EmitUntiedPartIdInc(CodeGenFunction &CGF) {
  if (CGF.CGM.OpenMPSupport.getUntied()) {
    llvm::Value *PartIdAddr;
    llvm::Value *UntiedSwitch;
    llvm::BasicBlock *UntiedEnd;
    unsigned UntiedCounter;
    CGF.CGM.OpenMPSupport.getUntiedData(PartIdAddr, UntiedSwitch, UntiedEnd,
        UntiedCounter);
    ++UntiedCounter;
    CGF.Builder.CreateStore(CGF.Builder.getInt32(UntiedCounter), PartIdAddr);
    CGF.CGM.OpenMPSupport.setUntiedData(PartIdAddr, UntiedSwitch, UntiedEnd,
        UntiedCounter, &CGF);
  }
}

static void EmitUntiedBranchEnd(CodeGenFunction &CGF) {
  if (CGF.CGM.OpenMPSupport.getUntied()) {
    llvm::Value *PartIdAddr;
    llvm::Value *UntiedSwitch;
    llvm::BasicBlock *UntiedEnd;
    unsigned UntiedCounter;
    CGF.CGM.OpenMPSupport.getUntiedData(PartIdAddr, UntiedSwitch, UntiedEnd,
        UntiedCounter);
    CGF.EmitBranch(UntiedEnd);
  }
}

static void EmitUntiedTaskSwitch(CodeGenFunction &CGF, bool EmitBranch) {
  if (CGF.CGM.OpenMPSupport.getUntied()) {
    llvm::Value *PartIdAddr;
    llvm::Value *UntiedSwitch;
    llvm::BasicBlock *UntiedEnd;
    unsigned UntiedCounter;
    CGF.CGM.OpenMPSupport.getUntiedData(PartIdAddr, UntiedSwitch, UntiedEnd,
        UntiedCounter);
    llvm::BasicBlock *NextBlock = CGF.createBasicBlock("untied.sw.next");
    cast<llvm::SwitchInst>(UntiedSwitch)->addCase(
        CGF.Builder.getInt32(UntiedCounter), NextBlock);
    if (EmitBranch)
      CGF.EmitBranch(NextBlock);
    CGF.EmitBlock(NextBlock);
  }
}

static std::pair<llvm::Value *, unsigned> ProcessDependAddresses(
    CodeGenFunction &CGF, const OMPTaskDirective &S) {
  CodeGenModule &CGM = CGF.CGM;

  llvm::Value *DependenceAddresses = 0;
  unsigned ArraySize = 0;

  SmallVector<const OMPDependClause *, 16> DependClauses;
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I) {
    if (OMPDependClause *ODC = dyn_cast_or_null<OMPDependClause>(*I)) {
      ArraySize += ODC->varlist_size();
      DependClauses.push_back(ODC);
    }
  }
  if (ArraySize > 0) {
    llvm::Type *IntPtrTy = CGF.ConvertTypeForMem(
        CGF.getContext().getIntPtrType());
    llvm::Type *BoolTy = CGF.ConvertTypeForMem(CGF.getContext().BoolTy);
    llvm::Type *DepTy = OPENMPRTL_DINFOTY;
    llvm::ArrayType *DepListTy = llvm::ArrayType::get(DepTy, ArraySize);

    llvm::AllocaInst *Addresses = CGF.CreateTempAlloca(DepListTy, ".dep.list.");
    Addresses->setAlignment(CGM.OpenMPSupport.getKMPDependInfoTypeAlign());
    DependenceAddresses = CGF.Builder.CreateConstInBoundsGEP2_32(Addresses, 0,
        0);

    unsigned FieldCounter = 0;
    for (SmallVectorImpl<const OMPDependClause *>::iterator I =
        DependClauses.begin(), E = DependClauses.end(); I != E; ++I) {
      unsigned DepType = IN;
      switch ((*I)->getType()) {
      case OMPC_DEPEND_in:
        DepType = IN;
        break;
      case OMPC_DEPEND_out:
        DepType = OUT;
        break;
      case OMPC_DEPEND_inout:
        DepType = INOUT;
        break;
      case OMPC_DEPEND_unknown:
      case NUM_OPENMP_DEPENDENCE_TYPE:
        llvm_unreachable("Unknown kind of dependency");
        break;
      }
      for (unsigned i = 0, e = (*I)->varlist_size(); i < e;
          ++i, ++FieldCounter) {
        llvm::Value *DepElPtr = CGF.Builder.CreateConstInBoundsGEP2_32(
            Addresses, 0, FieldCounter);
        // [CounterVal].base_addr = &expr;
        llvm::Value *DepBaseAddr = CGF.Builder.CreateConstGEP2_32(DepElPtr, 0,
            0);
        llvm::Value *BaseAddr =
            CGF.EmitAnyExpr((*I)->getBegins(i)).getScalarVal();
        BaseAddr = CGF.Builder.CreatePointerCast(BaseAddr, IntPtrTy);
        CGF.Builder.CreateStore(BaseAddr, DepBaseAddr);
        // [CounterVal].len = size;
        llvm::Value *DepLen = CGF.Builder.CreateConstGEP2_32(DepElPtr, 0, 1);
        const Expr *Size = (*I)->getSizeInBytes(i);
        if (Size->getType()->isAnyPointerType()) {
          // Size is not a size, but the ending pointer
          // Calculate the real size
          llvm::Value *EndAddr = CGF.EmitScalarExpr(Size);
          llvm::Value *BaseVal = CGF.Builder.CreatePtrToInt(BaseAddr,
              CGF.SizeTy);
          llvm::Value *EndVal = CGF.Builder.CreatePtrToInt(EndAddr, CGF.SizeTy);
          llvm::Value *Cond = CGF.Builder.CreateICmpUGT(EndVal, BaseVal);
          llvm::Value *Res = CGF.Builder.CreateSelect(Cond,
              CGF.Builder.CreateSub(EndVal, BaseVal),
              llvm::Constant::getNullValue(CGF.SizeTy));
          CGF.Builder.CreateStore(Res, DepLen);
        } else {
          CGF.Builder.CreateStore(CGF.EmitScalarExpr(Size), DepLen);
        }
        // [CounterVal].flags = size;
        llvm::Value *DepFlags = CGF.Builder.CreateConstGEP2_32(DepElPtr, 0, 2);
        CGF.Builder.CreateStore(llvm::ConstantInt::get(BoolTy, DepType),
            DepFlags);
      }
    }
  } else {
    llvm::Type *DepTy = OPENMPRTL_DINFOTY;
    DependenceAddresses = llvm::Constant::getNullValue(DepTy->getPointerTo());
  }
  return std::make_pair(DependenceAddresses, ArraySize);
}

/// Generate an instructions for '#pragma omp task' directive.
void CodeGenFunction::EmitOMPTaskDirective(const OMPTaskDirective &S) {
  // Generate shared args for captured stmt.
  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());
  llvm::Value *Arg = GenerateCapturedStmtArgument(*CS);

  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(true);
  CGM.OpenMPSupport.setMergeable(false);
  CGM.OpenMPSupport.setOrdered(false);
  CGM.OpenMPSupport.setUntied(false);
  CGM.OpenMPSupport.setScheduleChunkSize(KMP_SCH_DEFAULT, 0);

  RecordDecl *RD;
  if (!getContext().getLangOpts().CPlusPlus)
    RD = RecordDecl::Create(getContext(), TTK_Struct,
        getContext().getTranslationUnitDecl(), SourceLocation(),
        SourceLocation(), &getContext().Idents.get(".omp.task.priv."));
  else
    RD = CXXRecordDecl::Create(getContext(), TTK_Struct,
        getContext().getTranslationUnitDecl(), SourceLocation(),
        SourceLocation(), &getContext().Idents.get(".omp.task.priv."));
  RD->startDefinition();
  SmallVector<FieldDecl *, 16> FieldsWithDestructors;
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I) {
    if (OMPPrivateClause *C = dyn_cast_or_null<OMPPrivateClause>(*I)) {
      for (OMPPrivateClause::varlist_const_iterator II = C->varlist_begin(),
          EE = C->varlist_end(); II != EE; ++II) {
        const ValueDecl *D = cast<DeclRefExpr>(*II)->getDecl();
        FieldDecl *FD = FieldDecl::Create(getContext(), RD, SourceLocation(),
            SourceLocation(), D->getIdentifier(), (*II)->getType(), 0, 0, false,
            ICIS_NoInit);
        FD->setAccess(AS_public);
        RD->addDecl(FD);
        CGM.OpenMPSupport.getTaskFields()[D] = FD;
        QualType ASTType = D->getType();
        if (CXXRecordDecl *RD =
            ASTType->getBaseElementTypeUnsafe()->getAsCXXRecordDecl()) {
          if (!RD->hasTrivialDestructor())
            FieldsWithDestructors.push_back(FD);
        }
      }
    } else if (OMPFirstPrivateClause *C =
        dyn_cast_or_null<OMPFirstPrivateClause>(*I)) {
      for (OMPFirstPrivateClause::varlist_const_iterator II =
          C->varlist_begin(), EE = C->varlist_end(); II != EE; ++II) {
        const ValueDecl *D = cast<DeclRefExpr>(*II)->getDecl();
        FieldDecl *FD = FieldDecl::Create(getContext(), RD, SourceLocation(),
            SourceLocation(), D->getIdentifier(), (*II)->getType(), 0, 0, false,
            ICIS_NoInit);
        FD->setAccess(AS_public);
        RD->addDecl(FD);
        CGM.OpenMPSupport.getTaskFields()[D] = FD;
        QualType ASTType = D->getType();
        if (CXXRecordDecl *RD =
            ASTType->getBaseElementTypeUnsafe()->getAsCXXRecordDecl()) {
          if (!RD->hasTrivialDestructor())
            FieldsWithDestructors.push_back(FD);
        }
      }
    }
  }
  RD->completeDefinition();
  QualType PrivateRecord = getContext().getRecordType(RD);
  llvm::Type *LPrivateTy = getTypes().ConvertTypeForMem(PrivateRecord);

  llvm::Function *Destructors = 0;
  if (!FieldsWithDestructors.empty()) {
    IdentifierInfo *Id = &getContext().Idents.get(".omp_ptask_destructors.");
    SmallVector<QualType, 2> FnArgTypes;
    FnArgTypes.push_back(getContext().getIntTypeForBitwidth(32, 1));
    FnArgTypes.push_back(getContext().VoidPtrTy);
    FunctionProtoType::ExtProtoInfo EPI;
    EPI.ExceptionSpecType = EST_BasicNoexcept;
    QualType FnTy = getContext().getFunctionType(
        getContext().getIntTypeForBitwidth(32, 1), FnArgTypes, EPI);
    TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(FnTy,
        SourceLocation());
    FunctionDecl *FD = FunctionDecl::Create(getContext(),
        getContext().getTranslationUnitDecl(), CS->getLocStart(),
        SourceLocation(), Id, FnTy, TI, SC_Static, false, false, false);
    TypeSourceInfo *IntTI = getContext().getTrivialTypeSourceInfo(
        getContext().getIntTypeForBitwidth(32, 1), SourceLocation());
    TypeSourceInfo *PtrVoidTI = getContext().getTrivialTypeSourceInfo(
        getContext().VoidPtrTy, SourceLocation());
    ParmVarDecl *Arg1 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
        SourceLocation(), 0, getContext().getIntTypeForBitwidth(32, 1), IntTI,
        SC_Auto, 0);
    ParmVarDecl *Arg2 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
        SourceLocation(), 0, getContext().VoidPtrTy, PtrVoidTI, SC_Auto, 0);
    CodeGenFunction CGF(CGM);
    const CGFunctionInfo &FI = getTypes().arrangeFunctionDeclaration(FD);
    Destructors = llvm::Function::Create(getTypes().GetFunctionType(FI),
        llvm::GlobalValue::PrivateLinkage, FD->getName(), &CGM.getModule());
    FunctionArgList FnArgs;
    FnArgs.push_back(Arg1);
    FnArgs.push_back(Arg2);
    CGF.StartFunction(FD, getContext().getIntTypeForBitwidth(32, 1),
        Destructors, FI, FnArgs, SourceLocation());
    llvm::Type *TaskTTy = llvm::TaskTBuilder::get(getLLVMContext());
    llvm::Value *TaskTPtr = CGF.Builder.CreatePointerCast(
        CGF.GetAddrOfLocalVar(Arg2), TaskTTy->getPointerTo()->getPointerTo());
    // Emit call to the helper function.
    llvm::Value *Locker = CGF.Builder.CreateConstGEP1_32(
        CGF.Builder.CreateLoad(TaskTPtr), 1);
    Locker = CGF.Builder.CreatePointerCast(Locker, LPrivateTy->getPointerTo());
    for (ArrayRef<FieldDecl *>::iterator I = FieldsWithDestructors.begin(), E =
        FieldsWithDestructors.end(); I != E; ++I) {
      QualType ASTType = (*I)->getType();
      if (CXXRecordDecl *RD =
          ASTType->getBaseElementTypeUnsafe()->getAsCXXRecordDecl()) {
        if (!RD->hasTrivialDestructor()) {
          llvm::Value *Private =
              CGF.EmitLValueForField(
                  CGF.MakeNaturalAlignAddrLValue(Locker, PrivateRecord), *I).getAddress();
          QualType::DestructionKind DtorKind = ASTType.isDestructedType();
          CGF.emitDestroy(Private, ASTType, CGF.getDestroyer(DtorKind),
              CGF.needsEHCleanup(DtorKind));
        }
      }
    }
    CGF.FinishFunction(SourceLocation());
  }

  //  llvm::Type *PTaskFnTy = llvm::TypeBuilder<kmp_routine_entry_t,
  // false>::get(getLLVMContext());
  //  llvm::AllocaInst *FnPtr = CreateTempAlloca(PTaskFnTy);
  //  FnPtr->setAlignment(llvm::ConstantExpr::getAlignOf(PTaskFnTy));

  // CodeGen for clauses (task init).
  llvm::AllocaInst *Flags = CreateMemTemp(
      getContext().getIntTypeForBitwidth(32, 1), ".flags.addr");
  CGM.OpenMPSupport.setTaskFlags(Flags);

  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I)
      EmitInitOMPClause(*(*I), S);

  uint64_t InitFlags =
      CGM.OpenMPSupport.getUntied() ? OMP_TASK_UNTIED : OMP_TASK_TIED;
  if (Destructors) {
    InitFlags |= OMP_TASK_DESTRUCTORS_THUNK;
  }
  InitTempAlloca(Flags, Builder.getInt32(InitFlags));

  // Generate microtask.
  // int32 .omp_ptask.(int32_t arg1, void */*kmp_task_t **/arg2) {
  //  captured_stmt(arg2->shareds);
  // }
  bool isTargetMode = CGM.getLangOpts().OpenMPTargetMode;
  std::string pTaskName;
  if (isTargetMode) 
    pTaskName = "omp_ptask";
  else 
    pTaskName = ".omp_ptask.";
  IdentifierInfo *Id = &getContext().Idents.get(pTaskName);
  SmallVector<QualType, 2> FnArgTypes;
  FnArgTypes.push_back(getContext().getIntTypeForBitwidth(32, 1));
  FnArgTypes.push_back(getContext().VoidPtrTy);
  FunctionProtoType::ExtProtoInfo EPI;
  EPI.ExceptionSpecType = EST_BasicNoexcept;
  QualType FnTy = getContext().getFunctionType(
      getContext().getIntTypeForBitwidth(32, 1), FnArgTypes, EPI);
  TypeSourceInfo *TI = getContext().getTrivialTypeSourceInfo(FnTy,
      SourceLocation());
  FunctionDecl *FD = FunctionDecl::Create(getContext(),
      getContext().getTranslationUnitDecl(), CS->getLocStart(),
      SourceLocation(), Id, FnTy, TI, SC_Static, false, false, false);
  TypeSourceInfo *IntTI = getContext().getTrivialTypeSourceInfo(
      getContext().getIntTypeForBitwidth(32, 1), SourceLocation());
  TypeSourceInfo *PtrVoidTI = getContext().getTrivialTypeSourceInfo(
      getContext().VoidPtrTy, SourceLocation());
  ParmVarDecl *Arg1 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, getContext().getIntTypeForBitwidth(32, 1), IntTI,
      SC_Auto, 0);
  ParmVarDecl *Arg2 = ParmVarDecl::Create(getContext(), FD, SourceLocation(),
      SourceLocation(), 0, getContext().VoidPtrTy, PtrVoidTI, SC_Auto, 0);
  CodeGenFunction CGF(CGM, true);
  const CGFunctionInfo &FI = getTypes().arrangeFunctionDeclaration(FD);
  llvm::Function *Fn = llvm::Function::Create(getTypes().GetFunctionType(FI),
      llvm::GlobalValue::PrivateLinkage, FD->getName(), &CGM.getModule());
  CGM.SetInternalFunctionAttributes(CurFuncDecl, Fn, FI);
  FunctionArgList FnArgs;
  FnArgs.push_back(Arg1);
  FnArgs.push_back(Arg2);
  CGF.OpenMPRoot = OpenMPRoot ? OpenMPRoot : this;
  CGF.StartFunction(FD, getContext().getIntTypeForBitwidth(32, 1), Fn, FI,
      FnArgs, SourceLocation());

  CGF.OMPCancelMap[OMPD_taskgroup] = CGF.ReturnBlock;

  llvm::AllocaInst *GTid = CGF.CreateMemTemp(
      getContext().getIntTypeForBitwidth(32, 1), ".__kmpc_global_thread_num.");
  CGF.EmitStoreOfScalar(CGF.Builder.CreateLoad(CGF.GetAddrOfLocalVar(Arg1)),
      MakeNaturalAlignAddrLValue(GTid,
          getContext().getIntTypeForBitwidth(32, 1)), false);
  llvm::Type *TaskTTy = llvm::TaskTBuilder::get(getLLVMContext());
  llvm::Value *TaskTPtr = CGF.Builder.CreatePointerCast(
      CGF.GetAddrOfLocalVar(Arg2), TaskTTy->getPointerTo()->getPointerTo());

  // Emit call to the helper function.
  llvm::Value *Addr = CGF.Builder.CreateConstInBoundsGEP2_32(
      CGF.Builder.CreateLoad(TaskTPtr, ".arg2.shareds"), 0,
      llvm::TaskTBuilder::shareds, ".arg2.shareds.addr");
  llvm::Value *Arg2Val = CGF.Builder.CreateLoad(Addr, ".arg2.shareds.");
  QualType QTy = getContext().getRecordType(CS->getCapturedRecordDecl());
  llvm::Type *ConvertedType =
      CGF.getTypes().ConvertTypeForMem(QTy)->getPointerTo();
  llvm::Value *RecArg = CGF.Builder.CreatePointerCast(Arg2Val, ConvertedType,
      "(anon)shared");

  llvm::Value *Locker = CGF.Builder.CreateConstGEP1_32(
      CGF.Builder.CreateLoad(TaskTPtr), 1);
  CGM.OpenMPSupport.setPTask(Fn, Arg2Val, LPrivateTy, PrivateRecord, Locker);

  // CodeGen for clauses (call start).
  {
    OpenMPRegionRAII OMPRegion(CGF, *CS, RecArg);
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I)
        CGF.EmitPreOMPClause(*(*I), S);

    llvm::BasicBlock *UntiedEnd = 0;
    if (CGM.OpenMPSupport.getUntied()) {
      llvm::Value *Addr = CGF.Builder.CreateConstInBoundsGEP2_32(
          CGF.Builder.CreateLoad(TaskTPtr, ".arg2.part_id."), 0,
          llvm::TaskTBuilder::part_id, ".part_id.addr");
      llvm::Value *PartId = CGF.Builder.CreateLoad(Addr, ".part_id.");
      UntiedEnd = CGF.createBasicBlock("untied.sw.end");
      llvm::SwitchInst *UntiedSwitch = CGF.Builder.CreateSwitch(PartId,
          UntiedEnd);
      llvm::BasicBlock *InitBlock = CGF.createBasicBlock("untied.sw.init");
      CGF.EmitBlock(InitBlock);
      UntiedSwitch->addCase(CGF.Builder.getInt32(0), InitBlock);
      CGM.OpenMPSupport.setUntiedData(Addr, UntiedSwitch, UntiedEnd, 0, &CGF);
    }
    CGF.EmitStmt(CS->getCapturedStmt());
    CGF.EnsureInsertPoint();
    if (UntiedEnd)
      CGF.EmitBlock(UntiedEnd);

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I)
        CGF.EmitPostOMPClause(*(*I), S);

    // CodeGen for clauses (closing steps).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I)
        CGF.EmitCloseOMPClause(*(*I), S);
  }

  CGF.FinishFunction();

  llvm::DenseMap<const ValueDecl *, FieldDecl *> SavedFields =
      CGM.OpenMPSupport.getTaskFields();
  CGM.OpenMPSupport.endOpenMPRegion();

  // CodeGen for 'depend' clause.
  llvm::Value *DependenceAddresses = 0;
  unsigned ArraySize = 0;
  if (!CGM.OpenMPSupport.getUntied()) {
    std::tie(DependenceAddresses, ArraySize) = ProcessDependAddresses(*this, S);
  }
  // CodeGen for "omp task {Associated statement}".
  CGM.OpenMPSupport.startOpenMPRegion(false);
  CGM.OpenMPSupport.getTaskFields() = SavedFields;
  {
    RunCleanupsScope MainBlock(*this);

    EmitUntiedPartIdInc(*this);

    llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
    llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
    llvm::Value *RealArgs[] = { Loc, GTid, Builder.CreateLoad(Flags, ".flags."),
        Builder.CreateAdd(
            Builder.CreateIntCast(llvm::ConstantExpr::getSizeOf(TaskTTy),
                SizeTy, false),
            llvm::ConstantInt::get(SizeTy,
                getContext().getTypeSizeInChars(PrivateRecord).getQuantity())),
        llvm::ConstantInt::get(SizeTy,
            getContext().getTypeSizeInChars(QTy).getQuantity()), Fn };
    // kmpc_task_t val = __kmpc_omp_task_alloc(&loc, gtid, flags,
    // sizeof(kmpc_task_t), sizeof(shareds), task_entry);
    llvm::Value *TaskTVal = EmitRuntimeCall(OPENMPRTL_FUNC(omp_task_alloc),
        makeArrayRef(RealArgs), ".task_t.val.addr");
    llvm::Value *SharedAddr = Builder.CreateConstInBoundsGEP2_32(TaskTVal, 0,
        llvm::TaskTBuilder::shareds, ".shared.addr");
    EmitAggregateAssign(Builder.CreateLoad(SharedAddr), Arg, QTy);
    if (Destructors) {
      llvm::Value *DestructorsAddr = Builder.CreateConstInBoundsGEP2_32(
          TaskTVal, 0, llvm::TaskTBuilder::destructors, ".destructors.addr");
      Builder.CreateStore(Destructors, DestructorsAddr);
    }
    llvm::Value *Locker = Builder.CreateConstGEP1_32(TaskTVal, 1);
    CGM.OpenMPSupport.setPTask(Fn, TaskTVal, LPrivateTy, PrivateRecord, Locker);
    {
      RunCleanupsScope ExecutedScope(*this);
      // Skip firstprivate sync for tasks.
      for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
          S.clauses().end(); I != E; ++I)
        if (*I && (isa<OMPPrivateClause>(*I) || isa<OMPFirstPrivateClause>(*I)))
          EmitPreOMPClause(*(*I), S);

      for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
          S.clauses().end(); I != E; ++I)
        if (*I)
          EmitAfterInitOMPClause(*(*I), S);

      if (CGM.OpenMPSupport.getUntied()) {
        llvm::Value *RealArgs1[] = { Loc, GTid, TaskTVal };
        llvm::Value *Res = EmitRuntimeCall(OPENMPRTL_FUNC(omp_task_parts),
            RealArgs1, ".task.res.");
        llvm::Value *Cond = Builder.CreateICmpEQ(Res,
            Builder.getInt32(OMP_TASK_CURRENT_QUEUED));
        llvm::BasicBlock *ThenBB = createBasicBlock("task.parts.then");
        llvm::BasicBlock *EndBB = createBasicBlock("task.parts.end");
        Builder.CreateCondBr(Cond, ThenBB, EndBB);
        EmitBlock(ThenBB);
        EmitUntiedBranchEnd(*this);
        EmitBlock(EndBB, true);
      } else {
        llvm::Type *PtrDepTy = OPENMPRTL_DINFOTY->getPointerTo();
        llvm::Value *RealArgs1[] = { Loc, GTid, TaskTVal,
            llvm::ConstantInt::get(Int32Ty, ArraySize), DependenceAddresses,
            llvm::ConstantInt::get(Int32Ty, 0), llvm::Constant::getNullValue(
                PtrDepTy) };
        EmitRuntimeCall(OPENMPRTL_FUNC(omp_task_with_deps), RealArgs1,
            ".task.res.");
        llvm::Value *WaitDepsArgs[] = { Loc, GTid, llvm::ConstantInt::get(
            Int32Ty, ArraySize), DependenceAddresses, llvm::ConstantInt::get(
            Int32Ty, 0), llvm::Constant::getNullValue(PtrDepTy) };
        CGM.OpenMPSupport.setWaitDepsArgs(WaitDepsArgs);
      }
      EmitUntiedTaskSwitch(*this, true);
    }
  }

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I)
      EmitFinalOMPClause(*(*I), S);

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();
}

/// Generate an instructions for '#pragma omp sections' directive.
void CodeGenFunction::EmitOMPSectionsDirective(OpenMPDirectiveKind,
    OpenMPDirectiveKind SKind, const OMPExecutableDirective &S) {
  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(false);
  CGM.OpenMPSupport.setNoWait(false);
  CGM.OpenMPSupport.setMergeable(true);
  CGM.OpenMPSupport.setOrdered(false);

  // Generate shared args for captured stmt.
  // CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());
  // llvm::Value *Arg = GenerateCapturedStmtArgument(*CS);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);

  // CodeGen for clauses (task init).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);

  int Schedule = KMP_SCH_DEFAULT;
  bool Ordered = CGM.OpenMPSupport.getOrdered();
  bool Merge = CGM.OpenMPSupport.getMergeable();
  int Offset = 0;
  if (Ordered && Merge)
    Offset = SCH_ORD;
  else if (!Ordered && !Merge)
    Offset = SCH_NM;
  else if (Ordered && !Merge)
    Offset = SCH_NM_ORD;
  Schedule += Offset;
  CGM.OpenMPSupport.setScheduleChunkSize(Schedule, 0);

  {
    RunCleanupsScope ExecutedScope(*this);
    // CodeGen for clauses (call start).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
        EmitPreOMPClause(*(*I), S);

    // CodeGen for "omp sections {Associated statement}".
    // Calculate number of sections.
    CompoundStmt *AStmt = cast<CompoundStmt>(
        cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
    unsigned NumberOfSections = AStmt->size() - 1;
    llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
    llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
    uint64_t TypeSize = getContext().getTypeSize(getContext().UnsignedIntTy);
    llvm::IntegerType *UnsignedTy = cast<llvm::IntegerType>(
        ConvertTypeForMem(getContext().UnsignedIntTy));
    llvm::AllocaInst *IterVar = CreateMemTemp(getContext().UnsignedIntTy,
        ".idx.addr");
    InitTempAlloca(IterVar, llvm::Constant::getNullValue(UnsignedTy));
    const Expr *ChunkSize;
    CGM.OpenMPSupport.getScheduleChunkSize(Schedule, ChunkSize);
    llvm::Value *Chunk;
    if (ChunkSize) {
      Chunk = EmitScalarExpr(ChunkSize);
      Chunk = Builder.CreateIntCast(Chunk,
          TypeSize == 32 ? Builder.getInt32Ty() : Builder.getInt64Ty(), true);
    } else {
      Chunk = (TypeSize == 32) ? Builder.getInt32(0) : Builder.getInt64(0);
    }
    llvm::Value *UBVal = llvm::ConstantInt::get(UnsignedTy, NumberOfSections);
    llvm::AllocaInst *PLast = CreateTempAlloca(Int32Ty, "last");
    PLast->setAlignment(CGM.getDataLayout().getPrefTypeAlignment(Int32Ty));
    InitTempAlloca(PLast, Builder.getInt32(0));
    llvm::AllocaInst *PLB = CreateMemTemp(getContext().UnsignedIntTy, "lb");
    InitTempAlloca(PLB, llvm::ConstantInt::get(UnsignedTy, 0));
    llvm::AllocaInst *PUB = CreateMemTemp(getContext().UnsignedIntTy, "ub");
    InitTempAlloca(PUB, UBVal);
    llvm::AllocaInst *PSt = CreateMemTemp(getContext().UnsignedIntTy, "st");
    InitTempAlloca(PSt, llvm::ConstantInt::get(UnsignedTy, 1));

    llvm::Value *RealArgs[] = { Loc, GTid, Builder.getInt32(Schedule), PLast,
        PLB, PUB, PSt,
            TypeSize == 32 ? Builder.getInt32(1) : Builder.getInt64(1), Chunk };
    if (TypeSize == 32)
      EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_4u), RealArgs);
    else
      EmitRuntimeCall(OPENMPRTL_FUNC(for_static_init_8u), RealArgs);

    llvm::BasicBlock *OMPSectionsBB = createBasicBlock("omp.sections.begin");
    EmitBranch(OMPSectionsBB);
    EmitBlock(OMPSectionsBB);
    llvm::Value *UB = Builder.CreateLoad(PUB);
    llvm::Value *Cond = Builder.CreateICmpULT(UB, UBVal);
    UB = Builder.CreateSelect(Cond, UB, UBVal);
    Builder.CreateStore(UB, PUB);

    llvm::BasicBlock *EndBB = createBasicBlock("omp.sections.end");
    llvm::Value *LB = Builder.CreateLoad(PLB);
    Builder.CreateStore(LB, IterVar);
    llvm::BasicBlock *UBLBCheckBB = createBasicBlock("omp.lb_ub.check_pass");
    llvm::Value *UBLBCheck = Builder.CreateICmpULE(LB, UB, "omp.lb.le.ub");
    Builder.CreateCondBr(UBLBCheck, UBLBCheckBB, EndBB);
    EmitBlock(UBLBCheckBB);

    llvm::Value *Idx = Builder.CreateLoad(IterVar, ".idx.");
    llvm::BasicBlock *SectionEndBB = createBasicBlock("omp.section.fini");
    llvm::SwitchInst *SectionSwitch = Builder.CreateSwitch(Idx, SectionEndBB,
        NumberOfSections + 1);
    if (SKind == OMPD_sections)
      OMPCancelMap[OMPD_sections] = getJumpDestInCurrentScope(EndBB);
    CompoundStmt::const_body_iterator I = AStmt->body_begin();
    for (unsigned i = 0; i <= NumberOfSections; ++i, ++I) {
      RunCleanupsScope ThenScope(*this);
      llvm::BasicBlock *SectionBB = createBasicBlock("omp.section");
      SectionSwitch->addCase(llvm::ConstantInt::get(UnsignedTy, i), SectionBB);
      EmitBlock(SectionBB);
      EmitStmt(*I);
      EnsureInsertPoint();
      EmitBranch(SectionEndBB);
    }
    EmitBlock(SectionEndBB, true);
    OMPCancelMap.erase(SKind);

    llvm::Value *NextIdx = Builder.CreateAdd(Idx,
        llvm::ConstantInt::get(UnsignedTy, 1), ".next.idx.");
    Builder.CreateStore(NextIdx, IterVar);
    UBLBCheck = Builder.CreateICmpULE(NextIdx, UB, "omp.idx.le.ub");
    if (ChunkSize != 0) {
      llvm::BasicBlock *OMPSectionsNB = createBasicBlock("omp.sections.next");
      Builder.CreateCondBr(UBLBCheck, UBLBCheckBB, OMPSectionsNB);
      EmitBlock(OMPSectionsNB);
      llvm::Value *St = Builder.CreateLoad(PSt);
      LB = Builder.CreateAdd(LB, St);
      Builder.CreateStore(LB, PLB);
      UB = Builder.CreateAdd(UB, St);
      Builder.CreateStore(UB, PUB);
      EmitBranch(OMPSectionsBB);
    } else {
      Builder.CreateCondBr(UBLBCheck, UBLBCheckBB, EndBB);
    }
    EmitBlock(EndBB);
    llvm::Value *RealArgsFini[] = { Loc, GTid };
    EmitRuntimeCall(OPENMPRTL_FUNC(for_static_fini), RealArgsFini);
    CGM.OpenMPSupport.setLastIterVar(PLast);

    if (CGM.OpenMPSupport.hasLastPrivate() || !CGM.OpenMPSupport.getNoWait())
      EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL_SECTIONS);

    // CodeGen for clauses (call end).
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I)
      if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
        EmitPostOMPClause(*(*I), S);
  }

  // CodeGen for clauses (closing steps).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitCloseOMPClause(*(*I), S);

  // CodeGen for clauses (task finalize).
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I && isAllowedClauseForDirective(SKind, (*I)->getClauseKind()))
      EmitFinalOMPClause(*(*I), S);

  EnsureInsertPoint();

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();
}

/// Generate an instructions for '#pragma omp sections' directive.
void CodeGenFunction::EmitOMPSectionsDirective(const OMPSectionsDirective &S) {
  EmitOMPSectionsDirective(OMPD_sections, OMPD_sections, S);
}

/// Generate an instructions for '#pragma omp section' directive.
void CodeGenFunction::EmitOMPSectionDirective(const OMPSectionDirective &S) {
  EmitStmt(cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
}

void CodeGenFunction::EmitInitOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_num_threads:
    EmitInitOMPNumThreadsClause(cast<OMPNumThreadsClause>(C), S);
    break;
  case OMPC_num_teams:
    EmitInitOMPNumTeamsClause(cast<OMPNumTeamsClause>(C), S);
    break;
  case OMPC_thread_limit:
    EmitInitOMPThreadLimitClause(cast<OMPThreadLimitClause>(C), S);
    break;
  case OMPC_proc_bind:
    EmitInitOMPProcBindClause(cast<OMPProcBindClause>(C), S);
    break;
  case OMPC_reduction:
    EmitInitOMPReductionClause(cast<OMPReductionClause>(C), S);
    break;
  case OMPC_nowait:
    EmitInitOMPNowaitClause(cast<OMPNowaitClause>(C), S);
    break;
  case OMPC_ordered:
    EmitInitOMPOrderedClause(cast<OMPOrderedClause>(C), S);
    break;
  case OMPC_untied:
    EmitInitOMPUntiedClause(cast<OMPUntiedClause>(C), S);
    break;
  case OMPC_final:
    EmitInitOMPFinalClause(cast<OMPFinalClause>(C), S);
    break;
  case OMPC_mergeable:
    EmitInitOMPMergeableClause(cast<OMPMergeableClause>(C), S);
    break;
  case OMPC_device:
    EmitInitOMPDeviceClause(cast<OMPDeviceClause>(C), S);
    break;
  case OMPC_map:
    EmitInitOMPMapClause(cast<OMPMapClause>(C), S);
    break;
  case OMPC_to:
    EmitInitOMPToClause(cast<OMPToClause>(C), S);
    break;
  case OMPC_from:
    EmitInitOMPFromClause(cast<OMPFromClause>(C), S);
    break;
  case OMPC_default:
  case OMPC_schedule:
  case OMPC_dist_schedule:
  case OMPC_copyin:
  case OMPC_copyprivate:
  case OMPC_shared:
  case OMPC_private:
  case OMPC_firstprivate:
  case OMPC_lastprivate:
  case OMPC_collapse:
  case OMPC_if:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
    break;
  }
}

void CodeGenFunction::EmitAfterInitOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_if:
    EmitAfterInitOMPIfClause(cast<OMPIfClause>(C), S);
    break;
  case OMPC_map:
  case OMPC_to:
  case OMPC_from:
  case OMPC_reduction:
  case OMPC_nowait:
  case OMPC_ordered:
  case OMPC_untied:
  case OMPC_final:
  case OMPC_mergeable:
  case OMPC_default:
  case OMPC_proc_bind:
  case OMPC_num_threads:
  case OMPC_num_teams:
  case OMPC_thread_limit:
  case OMPC_schedule:
  case OMPC_dist_schedule:
  case OMPC_device:
  case OMPC_copyin:
  case OMPC_shared:
  case OMPC_private:
  case OMPC_firstprivate:
  case OMPC_lastprivate:
  case OMPC_collapse:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
    break;
  }
}

void CodeGenFunction::EmitPreOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_num_threads:
  case OMPC_num_teams:
  case OMPC_thread_limit:
  case OMPC_device:
  case OMPC_if:
  case OMPC_default:
  case OMPC_proc_bind:
  case OMPC_shared:
  case OMPC_collapse:
  case OMPC_nowait:
  case OMPC_ordered:
  case OMPC_copyprivate:
  case OMPC_untied:
  case OMPC_final:
  case OMPC_mergeable:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
    break;
  case OMPC_map:
    EmitPreOMPMapClause(cast<OMPMapClause>(C), S);
    break;
  case OMPC_copyin:
    EmitPreOMPCopyinClause(cast<OMPCopyinClause>(C), S);
    break;
  case OMPC_private:
    EmitPreOMPPrivateClause(cast<OMPPrivateClause>(C), S);
    break;
  case OMPC_firstprivate:
    EmitPreOMPFirstPrivateClause(cast<OMPFirstPrivateClause>(C), S);
    break;
  case OMPC_lastprivate:
    EmitPreOMPLastPrivateClause(cast<OMPLastPrivateClause>(C), S);
    break;
  case OMPC_reduction:
    EmitPreOMPReductionClause(cast<OMPReductionClause>(C), S);
    break;
  case OMPC_schedule:
    EmitPreOMPScheduleClause(cast<OMPScheduleClause>(C), S);
    break;
  case OMPC_dist_schedule:
    EmitPreOMPDistScheduleClause(cast<OMPDistScheduleClause>(C), S);
    break;
  }
}

void CodeGenFunction::EmitPostOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_num_threads:
  case OMPC_num_teams:
  case OMPC_thread_limit:
  case OMPC_device:
  case OMPC_if:
  case OMPC_copyin:
  case OMPC_copyprivate:
  case OMPC_default:
  case OMPC_proc_bind:
  case OMPC_shared:
  case OMPC_collapse:
  case OMPC_nowait:
  case OMPC_ordered:
  case OMPC_schedule:
  case OMPC_dist_schedule:
  case OMPC_untied:
  case OMPC_final:
  case OMPC_mergeable:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
  case OMPC_private:
  case OMPC_firstprivate:
    break;
  case OMPC_map:
    EmitPostOMPMapClause(cast<OMPMapClause>(C), S);
    break;
  case OMPC_lastprivate:
    EmitPostOMPLastPrivateClause(cast<OMPLastPrivateClause>(C), S);
    break;
  case OMPC_reduction:
    EmitPostOMPReductionClause(cast<OMPReductionClause>(C), S);
    break;
  }
}

void CodeGenFunction::EmitCloseOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_num_threads:
  case OMPC_num_teams:
  case OMPC_thread_limit:
  case OMPC_device:
  case OMPC_if:
  case OMPC_copyin:
  case OMPC_copyprivate:
  case OMPC_default:
  case OMPC_proc_bind:
  case OMPC_shared:
  case OMPC_private:
  case OMPC_firstprivate:
  case OMPC_collapse:
  case OMPC_nowait:
  case OMPC_ordered:
  case OMPC_schedule:
  case OMPC_dist_schedule:
  case OMPC_untied:
  case OMPC_final:
  case OMPC_mergeable:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
  case OMPC_map:
    break;
  case OMPC_lastprivate:
    EmitCloseOMPLastPrivateClause(cast<OMPLastPrivateClause>(C), S);
    break;
  case OMPC_reduction:
    EmitCloseOMPReductionClause(cast<OMPReductionClause>(C), S);
    break;
  }
}

void CodeGenFunction::EmitFinalOMPClause(const OMPClause &C,
    const OMPExecutableDirective &S) {
  switch (C.getClauseKind()) {
  default:
    llvm_unreachable("Unknown clause kind!");
  case OMPC_num_threads:
  case OMPC_num_teams:
  case OMPC_thread_limit:
  case OMPC_device:
  case OMPC_copyin:
  case OMPC_copyprivate:
  case OMPC_default:
  case OMPC_proc_bind:
  case OMPC_shared:
  case OMPC_private:
  case OMPC_firstprivate:
  case OMPC_lastprivate:
  case OMPC_map:
  case OMPC_collapse:
  case OMPC_nowait:
  case OMPC_ordered:
  case OMPC_schedule:
  case OMPC_dist_schedule:
  case OMPC_untied:
  case OMPC_final:
  case OMPC_mergeable:
  case OMPC_read:
  case OMPC_write:
  case OMPC_capture:
  case OMPC_update:
  case OMPC_seq_cst:
  case OMPC_depend:
  case OMPC_linear:
  case OMPC_aligned:
  case OMPC_safelen:
    break;
  case OMPC_if:
    EmitFinalOMPIfClause(cast<OMPIfClause>(C), S);
    break;
  case OMPC_reduction:
    EmitFinalOMPReductionClause(cast<OMPReductionClause>(C), S);
    break;
  }
}

void CodeGenFunction::EmitInitOMPNowaitClause(const OMPNowaitClause &,
    const OMPExecutableDirective &) {
  CGM.OpenMPSupport.setNoWait(true);
}

void CodeGenFunction::EmitInitOMPOrderedClause(const OMPOrderedClause &,
    const OMPExecutableDirective &) {
  CGM.OpenMPSupport.setOrdered(true);
}

void CodeGenFunction::EmitInitOMPUntiedClause(const OMPUntiedClause &,
    const OMPExecutableDirective &) {
  CGM.OpenMPSupport.setUntied(true);
}

void CodeGenFunction::EmitInitOMPMergeableClause(const OMPMergeableClause &,
    const OMPExecutableDirective &) {
  CGM.OpenMPSupport.setMergeable(true);
}

void CodeGenFunction::EmitInitOMPFinalClause(const OMPFinalClause &C,
    const OMPExecutableDirective &) {
  llvm::Value *Flags = CGM.OpenMPSupport.getTaskFlags();
  llvm::BasicBlock *ThenBlock = createBasicBlock("task.final.then");
  llvm::BasicBlock *EndBlock = createBasicBlock("task.final.end");
  EmitBranchOnBoolExpr(C.getCondition(), ThenBlock, EndBlock, 0);
  EmitBlock(ThenBlock);
  llvm::Value *Val = Builder.CreateOr(Builder.CreateLoad(Flags, ".flags."),
      OMP_TASK_FINAL);
  Builder.CreateStore(Val, Flags);
  EmitBranch(EndBlock);
  EmitBlock(EndBlock, true);
}

void CodeGenFunction::EmitInitOMPNumThreadsClause(const OMPNumThreadsClause &C,
    const OMPExecutableDirective &) {
  // __kmpc_push_num_threads(&loc, global_tid, num_threads);
  // ident_t loc = {...};
  llvm::Value *Loc = OPENMPRTL_LOC(C.getLocStart(), *this);
  // global_tid = __kmpc_global_thread_num(...);
  llvm::Value *GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
  // num_threads = num_threads...;
  llvm::Value *NumThreads = EmitScalarExpr(C.getNumThreads(), true);
  llvm::Value *RealArgs[] = { Loc, GTid, NumThreads };
  EmitRuntimeCall(OPENMPRTL_FUNC(push_num_threads), RealArgs);
}

void CodeGenFunction::EmitInitOMPNumTeamsClause(const OMPNumTeamsClause &C,
    const OMPExecutableDirective &) {
  // num_teams = num_teams...;
  CGM.OpenMPSupport.setNumTeams(C.getNumTeams());
}

void CodeGenFunction::EmitInitOMPThreadLimitClause(
    const OMPThreadLimitClause &C, const OMPExecutableDirective &) {
  // thread_limit = thread_limit...;
  CGM.OpenMPSupport.setThreadLimit(C.getThreadLimit());
}

void CodeGenFunction::EmitInitOMPProcBindClause(const OMPProcBindClause &C,
    const OMPExecutableDirective &) {
  // __kmpc_push_proc_bind(&loc, global_tid, proc_bind);
  // ident_t loc = {...};
  llvm::Value *Loc = OPENMPRTL_LOC(C.getLocStart(), *this);
  // global_tid = __kmpc_global_thread_num(...);
  llvm::Value *GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
  // proc_bind = proc_bind...;
  llvm::Value *ProcBind = 0;
  switch (C.getThreadAffinity()) {
  case OMPC_PROC_BIND_master:
    ProcBind = llvm::ConstantInt::get(
        llvm::ProcBindTBuilder::get(CGM.getLLVMContext()),
        KMP_PROC_BIND_MASTER);
    break;
  case OMPC_PROC_BIND_close:
    ProcBind = llvm::ConstantInt::get(
        llvm::ProcBindTBuilder::get(CGM.getLLVMContext()), KMP_PROC_BIND_CLOSE);
    break;
  case OMPC_PROC_BIND_spread:
    ProcBind = llvm::ConstantInt::get(
        llvm::ProcBindTBuilder::get(CGM.getLLVMContext()),
        KMP_PROC_BIND_SPREAD);
    break;
  case OMPC_PROC_BIND_unknown:
  case NUM_OPENMP_PROC_BIND_KINDS:
    llvm_unreachable("Unknown thread affinity");
  }
  llvm::Value *RealArgs[] = { Loc, GTid, ProcBind };
  EmitRuntimeCall(OPENMPRTL_FUNC(push_proc_bind), RealArgs);
}

void CodeGenFunction::EmitInitOMPDeviceClause(const OMPDeviceClause &C,
    const OMPExecutableDirective &) {

  RValue Tmp = EmitAnyExprToTemp(C.getDevice());
  llvm::Value *DeviceID = Builder.CreateIntCast(Tmp.getScalarVal(), CGM.Int32Ty,
      false);

  CGM.OpenMPSupport.setOffloadingDevice(DeviceID);
}

void CodeGenFunction::AppendOpenMPStackWithMapInfo(const OMPClause &C){

  // Here we add information about the maps to the OpenMP stack so the runtime
  // calls can be easily generated.

  int MapType;
  ArrayRef<const Expr*> Vars;
  ArrayRef<const Expr*> BaseAddrs;
  ArrayRef<const Expr*> Addrs;
  ArrayRef<const Expr*> Sizes;

  switch (C.getClauseKind()){
  default:
    llvm_unreachable("Unsupported find to get data map informations");
    break;
  case OMPC_map:{
    const OMPMapClause &DC = cast<OMPMapClause>(C);
    Vars = DC.getVars();
    BaseAddrs = DC.getWholeStartAddresses();
    Addrs = DC.getCopyingStartAddresses();
    Sizes = DC.getCopyingSizesEndAddresses();

    switch (DC.getKind()) {
    default:
      llvm_unreachable("Unknown map clause type!");
      break;
    case OMPC_MAP_unknown:
    case OMPC_MAP_tofrom:
      MapType = OMP_TGT_MAPTYPE_TO | OMP_TGT_MAPTYPE_FROM;
      break;
    case OMPC_MAP_to:
      MapType = OMP_TGT_MAPTYPE_TO;
      break;
    case OMPC_MAP_from:
      MapType = OMP_TGT_MAPTYPE_FROM;
      break;
    case OMPC_MAP_alloc:
      MapType = OMP_TGT_MAPTYPE_ALLOC;
      break;
    }

  } break;
  case OMPC_to:{
    const OMPToClause &DC = cast<OMPToClause>(C);
    Vars = DC.getVars();
    BaseAddrs = DC.getWholeStartAddresses();
    Addrs = DC.getCopyingStartAddresses();
    Sizes = DC.getCopyingSizesEndAddresses();
    MapType = OMP_TGT_MAPTYPE_TO;
  } break;
  case OMPC_from:{
    const OMPFromClause &DC = cast<OMPFromClause>(C);
    Vars = DC.getVars();
    BaseAddrs = DC.getWholeStartAddresses();
    Addrs = DC.getCopyingStartAddresses();
    Sizes = DC.getCopyingSizesEndAddresses();
    MapType = OMP_TGT_MAPTYPE_FROM;
  } break;
  }

  assert(
      BaseAddrs.size() == Vars.size() && BaseAddrs.size() == Addrs.size()
          && BaseAddrs.size() == Sizes.size() && "Vars addresses mismatch!");

  for (unsigned i = 0; i < BaseAddrs.size(); ++i) {
    const Expr * Var = Vars[i];
    llvm::Value * VB = EmitAnyExprToTemp(BaseAddrs[i]).getScalarVal();
    llvm::Value * VP = EmitAnyExprToTemp(Addrs[i]).getScalarVal();
    llvm::Value * VS = EmitAnyExprToTemp(Sizes[i]).getScalarVal();

    // Subtract the two pointers to obtain the size or
    // use the value directly if it is a constant

    if (!isa<llvm::ConstantInt>(VS)) {
      llvm::Value *RBI = Builder.CreatePtrToInt(VP, CGM.Int64Ty);
      llvm::Value *REI = Builder.CreatePtrToInt(VS, CGM.Int64Ty);
      VS = Builder.CreateSub(REI, RBI);
    }


    //Save the declaration associated with a map so it can be related with the
    //parameters of a target region later on. As in the OpenMP spec, here we
    //expect all clause entries to be a declared variable or an array subscript
    //that defines a given range

    const DeclRefExpr *VarDecl = 0;

    if (const DeclRefExpr *T1 = dyn_cast<DeclRefExpr>(Var))
      VarDecl = T1;
    else {
      const ArraySubscriptExpr *T2 = dyn_cast<ArraySubscriptExpr>(Var);
      while (T2) {
        if (const ImplicitCastExpr *T3 = dyn_cast<ImplicitCastExpr>(T2->getBase())) {
          if (const DeclRefExpr *T4 = dyn_cast<DeclRefExpr>(T3->getSubExpr())) {
            VarDecl = T4;
            break;
          } else {
            T2 = dyn_cast<ArraySubscriptExpr>(T3->getSubExpr());
          }
        }
      }
    }

    assert(VarDecl && "Unexpected expression in the map clause");

    //     - If it is a pointer with a range and this pointer was not mapped in previous clauses:
    //       Add one entry for the pointer and one (extra) entry for the range
    //
    //     - If it is a pointer with a range that was mapped before:
    //       Add one entry for the range
    //
    //      - Else:
    //       Just add the data in the map as is

    bool hasRange = Var->getStmtClass() == Stmt::ArraySubscriptExprClass;

    // Is this a pointer with a range?
    if (VarDecl->getType()->isPointerType() && hasRange){
      llvm::Value *P = EmitDeclRefLValue(VarDecl).getAddress();

      CGM.OpenMPSupport.addOffloadingMap(VarDecl, P, P,
          Builder.getInt64(CGM.getDataLayout().getTypeSizeInBits(P->getType())/8),
          OMP_TGT_MAPTYPE_ALLOC | OMP_TGT_MAPTYPE_POINTER);

      CGM.OpenMPSupport.addOffloadingMap(VarDecl, VB, VP, VS,
          MapType | OMP_TGT_MAPTYPE_EXTRA);

      continue;
    }

    CGM.OpenMPSupport.addOffloadingMap(VarDecl, VB, VP, VS, MapType);
  }
}

void CodeGenFunction::EmitInitOMPMapClause(const OMPMapClause &C,
    const OMPExecutableDirective &) {
  AppendOpenMPStackWithMapInfo(C);
}

void CodeGenFunction::EmitPreOMPMapClause(const OMPMapClause &C,
    const OMPExecutableDirective &) {

  // Have we already created the data begin RTL call? If so, there is nothing
  // to be done here
  if (CGM.OpenMPSupport.getMapsBegin())
    return;

  CGM.OpenMPSupport.setMapsBegin(true);

  // Create data begin with the results of the map clause

  // The information is stored to the OpenMP stack during the init phase
  // of the map/to/form clause codegen
  // - Expressions in the map
  // - Base pointers (addresses)
  // - Pointers (addresses)
  // - Sizes
  // - Types (to, from, to/from)

  ArrayRef<const Expr*>  MapClauseDecls;
  ArrayRef<llvm::Value*> MapClauseBasePointersArray;
  ArrayRef<llvm::Value*> MapClausePointersArray;
  ArrayRef<llvm::Value*> MapClauseSizesArray;
  ArrayRef<unsigned> MapClauseTypesArray;

  CGM.OpenMPSupport.getOffloadingMapArrays(MapClauseDecls,
      MapClauseBasePointersArray, MapClausePointersArray, MapClauseSizesArray,
      MapClauseTypesArray);

  assert(
      MapClauseBasePointersArray.size() == MapClauseDecls.size()
          && MapClauseBasePointersArray.size() == MapClausePointersArray.size()
          && MapClauseBasePointersArray.size() == MapClauseSizesArray.size()
          && MapClauseBasePointersArray.size() == MapClauseTypesArray.size()
          && "Array size mismatch!");

  if (MapClauseBasePointersArray.empty())
    return;

  llvm::Value *MapClauseNumElems = Builder.getInt32(
      MapClauseBasePointersArray.size());

  // If we have pointers, lets create an array in the stack
  llvm::Value *MapClauseBasePointers = Builder.CreateAlloca(CGM.VoidPtrTy,
      MapClauseNumElems, ".mapped_base_ptrs");
  llvm::Value *MapClausePointers = Builder.CreateAlloca(CGM.VoidPtrTy,
      MapClauseNumElems, ".mapped_ptrs");
  llvm::Value *MapClauseSizes = Builder.CreateAlloca(CGM.Int64Ty,
      MapClauseNumElems, ".mapped_sizes");

  llvm::Constant *MapClauseTypesInit = llvm::ConstantDataArray::get(
      Builder.getContext(), MapClauseTypesArray);
  llvm::GlobalVariable *MapClauseTypesTmp = new llvm::GlobalVariable(
      CGM.getModule(), MapClauseTypesInit->getType(), true,
      llvm::GlobalValue::PrivateLinkage, MapClauseTypesInit, ".mapped_types");

  llvm::Value *MapClauseTypes = Builder.CreateConstInBoundsGEP2_32(
      MapClauseTypesTmp, 0, 0);

  for (unsigned i = 0; i < MapClauseBasePointersArray.size(); ++i) {

    llvm::Value *BP = Builder.CreateConstInBoundsGEP1_32(MapClauseBasePointers,
        i);
    llvm::Value *P = Builder.CreateConstInBoundsGEP1_32(MapClausePointers, i);
    llvm::Value *S = Builder.CreateConstInBoundsGEP1_32(MapClauseSizes, i);

    Builder.CreateStore(Builder.CreateBitCast(MapClauseBasePointersArray[i],
                                              CGM.VoidPtrTy),
                        BP);
    Builder.CreateStore(Builder.CreateBitCast(MapClausePointersArray[i],
                                              CGM.VoidPtrTy),
                        P);
    Builder.CreateStore(Builder.CreateIntCast(MapClauseSizesArray[i],
                                              CGM.Int64Ty,true),
                        S);
  }

  // Get or create value with the deviceID
  llvm::Value *DeviceID =
      (CGM.OpenMPSupport.getOffloadingDevice()) ?
          CGM.OpenMPSupport.getOffloadingDevice() :
          (llvm::Value*) Builder.getInt32(
              CGOpenMPRuntime::OMPRTL__target_device_id_undef);

  llvm::Value *Args[] = { DeviceID, MapClauseNumElems, MapClauseBasePointers,
      MapClausePointers, MapClauseSizes, MapClauseTypes };
  llvm::CallInst *FC = EmitRuntimeCall(OPENMPRTL_FUNC(target_data_begin), Args);

  // Save the runtime call so that it can be used by the map clause post
  // codegen to generate the corresponding target_data_end;
  CGM.OpenMPSupport.setOffloadingMapBeginFunctionCall(FC);

}

void CodeGenFunction::EmitPostOMPMapClause(const OMPMapClause &C,
    const OMPExecutableDirective &) {

  // If we have already emittewd the map end RTL call, we have nothing to do
  // here
  if (CGM.OpenMPSupport.getMapsEnd())
    return;

  CGM.OpenMPSupport.setMapsEnd(true);

  llvm::CallInst *B = CGM.OpenMPSupport.getOffloadingMapBeginFunctionCall();

  if (!B)
    return;

  llvm::CallInst *E = cast<llvm::CallInst>(B->clone());
  E->setCalledFunction(OPENMPRTL_FUNC(target_data_end));
  Builder.Insert(E);
}

void CodeGenFunction::EmitInitOMPToClause(const OMPToClause &C,
    const OMPExecutableDirective &) {
  AppendOpenMPStackWithMapInfo(C);
}

void CodeGenFunction::EmitInitOMPFromClause(const OMPFromClause &C,
    const OMPExecutableDirective &) {
  AppendOpenMPStackWithMapInfo(C);
}

void CodeGenFunction::EmitAfterInitOMPIfClause(const OMPIfClause &C,
    const OMPExecutableDirective &S) {

  switch (S.getStmtClass()) {
  default: {
    // if (Cond) {
    llvm::BasicBlock *ThenBlock = createBasicBlock("omp.if.then");
    llvm::BasicBlock *ElseBlock = createBasicBlock("omp.if.else");
    llvm::BasicBlock *ContBlock = createBasicBlock("omp.if.end");
    EmitBranchOnBoolExpr(C.getCondition(), ThenBlock, ElseBlock, 0);
    EmitBlock(ElseBlock);
    {
      // if we are in target mode, nvptx backend, and the construct is a #parallel
      // we need to go back into sequential mode (all threads are executing this)
      if (CGM.getLangOpts().OpenMPTargetMode
          && (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
              || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64)
          && isParallelDirective(&S))
        CGM.getOpenMPRuntime().GenerateNextLabel(*this, true, false);

      RunCleanupsScope ElseScope(*this);
      EmitStmt(cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
      EnsureInsertPoint();
    }
    EmitBranch(ContBlock);
    EmitBlock(ThenBlock);
    CGM.OpenMPSupport.setIfDest(ContBlock);
  }
    break;
  case Stmt::OMPTaskDirectiveClass:
  case Stmt::OMPTargetDirectiveClass:
  case Stmt::OMPTargetTeamsDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeParallelForDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeParallelForSimdDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeSimdDirectiveClass:
  case Stmt::OMPTargetUpdateDirectiveClass: {
    llvm::BasicBlock *ThenBlock = createBasicBlock("omp.if.then");
    llvm::BasicBlock *ElseBlock = createBasicBlock("omp.if.else");
    EmitBranchOnBoolExpr(C.getCondition(), ThenBlock, ElseBlock, 0);
    EmitBlock(ThenBlock);
    CGM.OpenMPSupport.setIfDest(ElseBlock);
  }
    break;
  }
}

void CodeGenFunction::EmitFinalOMPIfClause(const OMPIfClause &,
    const OMPExecutableDirective &S) {

  switch (S.getStmtClass()) {
  default: {
    llvm::BasicBlock *ContBlock = CGM.OpenMPSupport.takeIfDest();
    EmitBranch(ContBlock);
    EmitBlock(ContBlock, true);
  }
    break;
  case Stmt::OMPTaskDirectiveClass: {
    llvm::BasicBlock *ContBlock = createBasicBlock("omp.if.end");
    EmitBranch(ContBlock);
    EmitBlock(CGM.OpenMPSupport.takeIfDest());
    {
      if (CGM.OpenMPSupport.getWaitDepsArgs()) {
        EmitRuntimeCall(OPENMPRTL_FUNC(omp_wait_deps),
            makeArrayRef(CGM.OpenMPSupport.getWaitDepsArgs(), 6));
      }
      llvm::Value *PTask;
      llvm::Value *TaskTVal;
      llvm::Type *PrivateTy;
      QualType PrivateQTy;
      llvm::Value *Base;
      CGM.OpenMPSupport.getPTask(PTask, TaskTVal, PrivateTy, PrivateQTy, Base);
      llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
      llvm::Value *GTid =
      OPENMPRTL_THREADNUM(S.getLocStart(), *this);
      llvm::Value *RealArgs[] = { Loc, GTid, TaskTVal };
      EmitRuntimeCall(OPENMPRTL_FUNC(omp_task_begin_if0),
          makeArrayRef(RealArgs));
      llvm::Value *RealArgs1[] = { GTid, Builder.CreatePointerCast(TaskTVal,
          VoidPtrTy) };
      EmitCallOrInvoke(PTask, makeArrayRef(RealArgs1));
      EmitRuntimeCall(OPENMPRTL_FUNC(omp_task_complete_if0),
          makeArrayRef(RealArgs));
    }
    EmitBranch(ContBlock);
    EmitBlock(ContBlock, true);
  }
    break;
  case Stmt::OMPTargetDirectiveClass:
  case Stmt::OMPTargetTeamsDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeParallelForDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeParallelForSimdDirectiveClass:
  case Stmt::OMPTargetTeamsDistributeSimdDirectiveClass: {
    // The target directive and its combined variants store the host function
    // call to the stack, we only need to create the new basic block, make
    // the if-true and if-false basic block to jump there and clone the host
    // function into if-false
    llvm::BasicBlock *ContBlock = createBasicBlock("omp.if.end");
    llvm::BasicBlock *ElseBlock = CGM.OpenMPSupport.takeIfDest();
    EmitBranch(ContBlock);
    EmitBlock(ElseBlock);

    Builder.Insert(CGM.OpenMPSupport.getOffloadingHostFunctionCall()->clone());

    EmitBranch(ContBlock);
    EmitBlock(ContBlock, true);
  }
    break;
  case Stmt::OMPTargetUpdateDirectiveClass: {
    // These directives don't do anything in the if-false branch, therefore we
    // only need to continue the codegen in this basic block
    EmitBranch(CGM.OpenMPSupport.takeIfDest());
    EmitBlock(CGM.OpenMPSupport.takeIfDest());
  }
    break;
  }
}

void CodeGenFunction::EmitPreOMPScheduleClause(const OMPScheduleClause &C,
    const OMPExecutableDirective &) {
  int Schedule = KMP_SCH_DEFAULT;
  bool Ordered = CGM.OpenMPSupport.getOrdered();
  bool Merge = CGM.OpenMPSupport.getMergeable();
  int Offset = 0;
  if (Ordered && Merge)
    Offset = SCH_ORD;
  else if (!Ordered && !Merge)
    Offset = SCH_NM;
  else if (Ordered && !Merge)
    Offset = SCH_NM_ORD;
  const Expr *ChunkSize = C.getChunkSize();

  switch (C.getScheduleKind()) {
  case OMPC_SCHEDULE_static:
    Schedule = ChunkSize ? KMP_SCH_STATIC_CHUNKED : KMP_SCH_STATIC;
    break;
  case OMPC_SCHEDULE_dynamic:
    Schedule = KMP_SCH_DYNAMIC_CHUNKED;
    break;
  case OMPC_SCHEDULE_guided:
    Schedule = KMP_SCH_GUIDED_CHUNKED;
    break;
  case OMPC_SCHEDULE_auto:
    Schedule = KMP_SCH_AUTO;
    break;
  case OMPC_SCHEDULE_runtime:
    Schedule = KMP_SCH_RUNTIME;
    break;
  case OMPC_SCHEDULE_unknown:
  case NUM_OPENMP_SCHEDULE_KINDS:
    llvm_unreachable("Unknown schedule kind.");
  }
  Schedule += Offset;
  CGM.OpenMPSupport.setScheduleChunkSize(Schedule, ChunkSize);
}

void CodeGenFunction::EmitPreOMPDistScheduleClause(
    const OMPDistScheduleClause &C, const OMPExecutableDirective &) {
  int Schedule = KMP_SCH_DEFAULT;
  const Expr *ChunkSize = C.getDistChunkSize();

  switch (C.getDistScheduleKind()) {
  case OMPC_DIST_SCHEDULE_static:
    Schedule =
        ChunkSize ?
            KMP_SCH_DISTRIBUTE_STATIC_CHUNKED : KMP_SCH_DISTRIBUTE_STATIC;
    break;
  case OMPC_DIST_SCHEDULE_unknown:
  case NUM_OPENMP_DIST_SCHEDULE_KINDS:
    llvm_unreachable("Unknown dist_schedule kind.");
  }
  CGM.OpenMPSupport.setScheduleChunkSize(Schedule, ChunkSize);
}

void CodeGenFunction::EmitUniversalStore(LValue Dst, llvm::Value *Src,
    QualType ExprTy) {
  switch (getEvaluationKind(ExprTy)) {
  case TEK_Complex: {
    RValue Val = convertTempToRValue(Src, ExprTy, SourceLocation());
    EmitStoreOfComplex(Val.getComplexVal(), Dst, false);
    break;
  }
  case TEK_Aggregate:
    EmitAggregateAssign(Dst.getAddress(), Src, ExprTy);
    break;
  case TEK_Scalar:
    RValue Val = convertTempToRValue(Src, ExprTy, SourceLocation());
    EmitStoreThroughLValue(Val, Dst, false);
    break;
  }
}

void CodeGenFunction::EmitUniversalStore(llvm::Value *Dst, llvm::Value *Src,
    QualType ExprTy) {
  EmitUniversalStore(MakeNaturalAlignAddrLValue(Dst, ExprTy), Src, ExprTy);
}

// This helper is used for emitting copy-assignments for copyin clause and
// for copy_function generated for copyprivate clause.
void CodeGenFunction::EmitCopyAssignment(ArrayRef<const Expr *>::iterator I,
    ArrayRef<const Expr *>::iterator AssignIter,
    ArrayRef<const Expr *>::iterator VarIter1,
    ArrayRef<const Expr *>::iterator VarIter2, llvm::Value *Dst,
    llvm::Value *Src) {
  // This is called at each iteration of the loop through the clauses.
  {
    // Get element type.
    QualType QTy = (*I)->getType();
    const Type *MainTy = QTy.getTypePtr();


    if (!*AssignIter) {
      // For trivial assignment operator copy by memcpy.
      llvm::Value *VDAddr = Src;
      EmitUniversalStore(Builder.CreatePointerCast(Dst, VDAddr->getType()),
          VDAddr, QTy);
    } else {
      RunCleanupsScope InitBlock(*this);
      // Copy elements one by one.
      if (const ArrayType *ArrayTy = MainTy->getAsArrayTypeUnsafe()) {
        // Copy array.
        QualType ElementTy;
        llvm::Value *SharedVar = Dst;
        llvm::Value *NumElements = emitArrayLength(ArrayTy, ElementTy,
            SharedVar);
        llvm::Value *ArrayEnd = Builder.CreateGEP(SharedVar, NumElements);
        llvm::Value *MasterArray = Src;
        unsigned AddrSpace = MasterArray->getType()->getPointerAddressSpace();
        llvm::Type *BaseType = ConvertType(ElementTy)->getPointerTo(AddrSpace);
        llvm::Value *MasterArrayBegin = Builder.CreatePointerCast(MasterArray,
            BaseType, "master.array.begin");
        llvm::Value *MasterArrayEnd = Builder.CreateGEP(MasterArrayBegin,
            NumElements);
        // The basic structure here is a do-while loop, because we don't
        // need to check for the zero-element case.
        llvm::BasicBlock *BodyBB = createBasicBlock("omp.arraycpy.body");
        llvm::BasicBlock *DoneBB = createBasicBlock("omp.arraycpy.done");
        llvm::Value *IsEmpty = Builder.CreateICmpEQ(SharedVar, ArrayEnd,
            "omp.arraycpy.isempty");
        Builder.CreateCondBr(IsEmpty, DoneBB, BodyBB);

        // Enter the loop body, making that address the current address.
        llvm::BasicBlock *EntryBB = Builder.GetInsertBlock();
        EmitBlock(BodyBB);
        llvm::PHINode *ElementPast = Builder.CreatePHI(SharedVar->getType(), 2,
            "omp.arraycpy.elementPast");
        ElementPast->addIncoming(ArrayEnd, EntryBB);
        llvm::PHINode *MasterElementPast = Builder.CreatePHI(
            MasterArrayBegin->getType(), 2, "omp.arraycpy.masterElementPast");
        MasterElementPast->addIncoming(MasterArrayEnd, EntryBB);

        // Shift the address back by one element.
        llvm::Value *NegativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
        llvm::Value *Element = Builder.CreateGEP(ElementPast, NegativeOne,
            "omp.arraycpy.element");
        llvm::Value *MasterElement = Builder.CreateGEP(MasterElementPast,
            NegativeOne, "omp.arraycpy.master.element");

        const VarDecl *PseudoVar1 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter1)->getDecl());
        const VarDecl *PseudoVar2 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter2)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar1, Element);
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar2, MasterElement);
        EmitIgnoredExpr(*AssignIter);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar1);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar2);

        // Check whether we've reached the end.
        llvm::Value *Done = Builder.CreateICmpEQ(Element, SharedVar,
            "omp.arraycpy.done");
        Builder.CreateCondBr(Done, DoneBB, BodyBB);
        ElementPast->addIncoming(Element, Builder.GetInsertBlock());
        MasterElementPast->addIncoming(MasterElement, Builder.GetInsertBlock());

        // Done.
        EmitBlock(DoneBB, true);
      } else {
        // Copy single object.
        const VarDecl *PseudoVar1 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter1)->getDecl());
        const VarDecl *PseudoVar2 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter2)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar1, Dst);
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar2, Src);
        EmitIgnoredExpr(*AssignIter);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar1);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar2);
      }
    }
  }
}

void CodeGenFunction::EmitPreOMPCopyinClause(const OMPCopyinClause &C,
    const OMPExecutableDirective &) {
  // copy_data(var1);
  // copy_data(var2);
  // ...
  // __kmpc_barrier(&loc, global_tid);
  ArrayRef<const Expr *>::iterator AssignIter = C.getAssignments().begin();
  ArrayRef<const Expr *>::iterator VarIter1 = C.getPseudoVars1().begin();
  ArrayRef<const Expr *>::iterator VarIter2 = C.getPseudoVars2().begin();

  for (OMPCopyinClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++AssignIter, ++VarIter1, ++VarIter2) {
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    EmitCopyAssignment(I, AssignIter, VarIter1, VarIter2,
    OPENMPRTL_THREADPVTCACHED(VD, (*I)->getExprLoc(), *this, true),
        VD->isStaticLocal() ?
            CGM.getStaticLocalDeclAddress(VD) : CGM.GetAddrOfGlobal(VD));
  }
  SetFirstprivateInsertPt(*this);
}

/// \brief Determine whether the given initializer is trivial in the sense
/// that it requires no code to be generated.
static bool isTrivialInitializer(const Expr *Init) {
  if (!Init)
    return true;

  if (const CXXConstructExpr *Construct = dyn_cast<CXXConstructExpr>(Init))
    if (CXXConstructorDecl *Constructor = Construct->getConstructor())
      if (Constructor->isTrivial() && Constructor->isDefaultConstructor()
          && !Construct->requiresZeroInitialization())
        return true;

  return false;
}

void CodeGenFunction::EmitPreOMPPrivateClause(const OMPPrivateClause &C,
    const OMPExecutableDirective &) {
  // Type1 tmp1;
  // anon.field1 = &tmp1;
  // Type2 tmp2;
  // anon.field2 = &tmp2;
  // ...
  //
  ArrayRef<const Expr *>::iterator InitIter = C.getDefaultInits().begin();
  for (OMPPrivateClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++InitIter) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    if (CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD))
      continue;
    // if (VD->hasLocalStorage() &&
    //    (!CapturedStmtInfo ||
    //     !CapturedStmtInfo->lookup(VD))) {
    //  LocalDeclMap[VD] = CreateMemTemp(VD->getType(), CGM.getMangledName(VD));
    //}
    QualType QTy = (*I)->getType();
    const Type *MainTy = QTy.getTypePtr();
    // const Type *Ty = MainTy->getArrayElementTypeNoTypeQual();
    // const Type *PrevTy = MainTy;
    // while (Ty != 0) {
    //  PrevTy = Ty;
    //  Ty = Ty->getArrayElementTypeNoTypeQual();
    //}
    // Ty = PrevTy;
    llvm::Value *Private;
    llvm::Value *PTask;
    llvm::Value *TaskTVal;
    llvm::Type *PrivateTy;
    QualType PrivateQTy;
    llvm::Value *Base;
    CGM.OpenMPSupport.getPTask(PTask, TaskTVal, PrivateTy, PrivateQTy, Base);
    if (PTask) {
      Base = Builder.CreatePointerCast(Base, PrivateTy->getPointerTo());
      Private = EmitLValueForField(MakeNaturalAlignAddrLValue(Base, PrivateQTy),
          CGM.OpenMPSupport.getTaskFields()[VD]).getAddress();
    } else {
      LocalVarsDeclGuard Grd(*this, true);
      AutoVarEmission Emission = EmitAutoVarAlloca(*VD);
      Private = Emission.getAllocatedAddress();
      EmitAutoVarCleanups(Emission);
    }
    // CodeGen for classes with the default constructor.
    if (((!PTask || CurFn != PTask) && !isTrivialInitializer(*InitIter))
        || (MainTy->isVariablyModifiedType() && !MainTy->isPointerType())) {
      RunCleanupsScope InitBlock(*this);
      if (const ArrayType *ArrayTy = MainTy->getAsArrayTypeUnsafe()) {
        // Create array.
        QualType ElementTy;
        llvm::Value *ArrayBeg = Private;
        llvm::Value *NumElements = emitArrayLength(ArrayTy, ElementTy,
            ArrayBeg);
        llvm::Value *ArrayEnd = Builder.CreateGEP(ArrayBeg, NumElements,
            "omp.arrayctor.end");
        // The basic structure here is a do-while loop, because we don't
        // need to check for the zero-element case.
        llvm::BasicBlock *BodyBB = createBasicBlock("omp.arrayctor.body");
        llvm::BasicBlock *DoneBB = createBasicBlock("omp.arrayctor.done");
        llvm::Value *IsEmpty = Builder.CreateICmpEQ(ArrayBeg, ArrayEnd,
            "omp.arrayctor.isempty");
        Builder.CreateCondBr(IsEmpty, DoneBB, BodyBB);

        // Enter the loop body, making that address the current address.
        llvm::BasicBlock *EntryBB = Builder.GetInsertBlock();
        EmitBlock(BodyBB);
        llvm::PHINode *ElementPast = Builder.CreatePHI(ArrayBeg->getType(), 2,
            "omp.arrayctor.elementPast");
        ElementPast->addIncoming(ArrayEnd, EntryBB);

        // Shift the address back by one element.
        llvm::Value *NegativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
        llvm::Value *Element = Builder.CreateGEP(ElementPast, NegativeOne,
            "omp.arrayctor.element");
        EmitAnyExprToMem(*InitIter, Element,
            (*InitIter)->getType().getQualifiers(), false);
        //// Check whether we've reached the end.
        llvm::Value *Done = Builder.CreateICmpEQ(Element, ArrayBeg,
            "omp.arrayctor.done");
        Builder.CreateCondBr(Done, DoneBB, BodyBB);
        ElementPast->addIncoming(Element, Builder.GetInsertBlock());

        // Done.
        EmitBlock(DoneBB, true);
      } else {
        EmitAnyExprToMem(*InitIter, Private,
            (*InitIter)->getType().getQualifiers(), false);
      }
    }
    CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
  }
}

void CodeGenFunction::EmitPreOMPFirstPrivateClause(
    const OMPFirstPrivateClause &C, const OMPExecutableDirective &) {
  // Type1 tmp1(var1);
  // anon.field1 = &tmp1;
  // Type2 tmp2(var2);
  // anon.field2 = &tmp2;
  // ...
  //
  llvm::Value *PTask;
  llvm::Value *TaskTVal;
  llvm::Type *PrivateTy;
  QualType PrivateQTy;
  llvm::Value *Base;
  CGM.OpenMPSupport.getPTask(PTask, TaskTVal, PrivateTy, PrivateQTy, Base);

  ArrayRef<const Expr *>::iterator InitIter = C.getInits().begin();
  ArrayRef<const Expr *>::iterator VarIter = C.getPseudoVars().begin();
  for (OMPFirstPrivateClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++InitIter, ++VarIter) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    if (CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD))
      continue;
    // if (VD->hasLocalStorage() &&
    //    (!CapturedStmtInfo ||
    //     !CapturedStmtInfo->lookup(VD))) {
    //  LocalDeclMap[VD] = CreateMemTemp(VD->getType(), CGM.getMangledName(VD));
    //}
    QualType QTy = (*I)->getType();
    const Type *MainTy = QTy.getTypePtr();
    // const Type *Ty = MainTy->getArrayElementTypeNoTypeQual();
    // const Type *PrevTy = MainTy;
    // while (Ty != 0) {
    //  PrevTy = Ty;
    //  Ty = Ty->getArrayElementTypeNoTypeQual();
    //}
    llvm::Value *Private = 0;
    if (!CGM.OpenMPSupport.isNewTask() && !PTask) {
      if (llvm::AllocaInst *Val = dyn_cast_or_null<llvm::AllocaInst>(
          CGM.OpenMPSupport.getPrevOpenMPPrivateVar(VD))) {
        Private = Val;
        CGM.OpenMPSupport.delPrevOpenMPPrivateVar(VD);
        CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
        continue;
      }
    }
    if (PTask) {
      Base = Builder.CreatePointerCast(Base, PrivateTy->getPointerTo());
      Private = EmitLValueForField(MakeNaturalAlignAddrLValue(Base, PrivateQTy),
          CGM.OpenMPSupport.getTaskFields()[VD]).getAddress();
    } else {
      LocalVarsDeclGuard Grd(*this, true);
      AutoVarEmission Emission = EmitAutoVarAlloca(*VD);
      Private = Emission.getAllocatedAddress();
      EmitAutoVarCleanups(Emission);
    }
    // CodeGen for classes with the copy constructor.
    RunCleanupsScope InitBlock(*this);
    if (((!PTask || CurFn != PTask) && !isTrivialInitializer(*InitIter))
        || (MainTy->isVariablyModifiedType() && !MainTy->isPointerType())) {
      if (const ArrayType *ArrayTy = MainTy->getAsArrayTypeUnsafe()) {
        // Create array.
        QualType ElementTy;
        llvm::Value *ArrayBeg = Private;
        llvm::Value *NumElements = emitArrayLength(ArrayTy, ElementTy,
            ArrayBeg);
        llvm::Value *ArrayEnd = Builder.CreateGEP(ArrayBeg, NumElements);
        llvm::Value *MasterArray = EmitLValue(*I).getAddress();
        unsigned AddrSpace = MasterArray->getType()->getPointerAddressSpace();
        llvm::Type *BaseType = ConvertType(ElementTy)->getPointerTo(AddrSpace);
        llvm::Value *MasterArrayBegin = Builder.CreatePointerCast(MasterArray,
            BaseType, "master.array.begin");
        llvm::Value *MasterArrayEnd = Builder.CreateGEP(MasterArrayBegin,
            NumElements);
        // The basic structure here is a do-while loop, because we don't
        // need to check for the zero-element case.
        llvm::BasicBlock *BodyBB = createBasicBlock("omp.arraycpy.body");
        llvm::BasicBlock *DoneBB = createBasicBlock("omp.arraycpy.done");
        llvm::Value *IsEmpty = Builder.CreateICmpEQ(ArrayBeg, ArrayEnd,
            "omp.arraycpy.isempty");
        Builder.CreateCondBr(IsEmpty, DoneBB, BodyBB);

        // Enter the loop body, making that address the current address.
        llvm::BasicBlock *EntryBB = Builder.GetInsertBlock();
        EmitBlock(BodyBB);
        llvm::PHINode *MasterElementPast = Builder.CreatePHI(
            MasterArrayBegin->getType(), 2, "omp.arraycpy.masterElementPast");
        MasterElementPast->addIncoming(MasterArrayEnd, EntryBB);
        llvm::PHINode *ElementPast = Builder.CreatePHI(ArrayBeg->getType(), 2,
            "omp.arraycpy.elementPast");
        ElementPast->addIncoming(ArrayEnd, EntryBB);

        // Shift the address back by one element.
        llvm::Value *NegativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
        llvm::Value *Element = Builder.CreateGEP(ElementPast, NegativeOne,
            "omp.arraycpy.element");
        llvm::Value *MasterElement = Builder.CreateGEP(MasterElementPast,
            NegativeOne, "omp.arraycpy.master.element");

        const VarDecl *PseudoVar = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar, MasterElement);
        EmitAnyExprToMem(*InitIter, Element,
            (*InitIter)->getType().getQualifiers(), false);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar);

        // Check whether we've reached the end.
        llvm::Value *Done = Builder.CreateICmpEQ(Element, ArrayBeg,
            "omp.arraycpy.done");
        Builder.CreateCondBr(Done, DoneBB, BodyBB);
        ElementPast->addIncoming(Element, Builder.GetInsertBlock());
        MasterElementPast->addIncoming(MasterElement, Builder.GetInsertBlock());

        // Done.
        EmitBlock(DoneBB, true);
      } else {
        // Create single object.
        llvm::Value *RealAddr = EmitLValue(*I).getAddress();
        const VarDecl *PseudoVar = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar, RealAddr);
        EmitAnyExprToMem(*InitIter, Private,
            (*InitIter)->getType().getQualifiers(), false);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar);
      }
    } else if (!PTask || CurFn != PTask) {
      EmitAnyExprToMem(*I, Private, QTy.getQualifiers(), false);
    }
    CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
  }
  // Disable marking for tasks.
  if (!PTask || PTask == CurFn)
    SetFirstprivateInsertPt(*this);
}

void CodeGenFunction::EmitPreOMPLastPrivateClause(const OMPLastPrivateClause &C,
    const OMPExecutableDirective &S) {
  // Type1 tmp1;
  // Type2 tmp2;
  // ...
  //
  CGM.OpenMPSupport.setHasLastPrivate(true);
  ArrayRef<const Expr *>::iterator InitIter = C.getDefaultInits().begin();
  for (OMPLastPrivateClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++InitIter) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    bool FirstPrivateFound = false;
    for (ArrayRef<OMPClause *>::iterator FI = S.clauses().begin(), FE =
        S.clauses().end(); FI != FE; ++FI) {
      if (const OMPFirstPrivateClause *FC = dyn_cast<OMPFirstPrivateClause>(
          *FI)) {
        for (OMPFirstPrivateClause::varlist_const_iterator VI =
            FC->varlist_begin(), VE = FC->varlist_end(); VI != VE; ++VI) {
          if (VD == cast<DeclRefExpr>(*VI)->getDecl()) {
            FirstPrivateFound = true;
            break;
          }
        }
      }
      if (FirstPrivateFound)
        break;
    }
    // Lastprivate init is processed by firstprivate clause.
    if (FirstPrivateFound || CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD))
      continue;
    // if (VD->hasLocalStorage() &&
    //    (!CapturedStmtInfo ||
    //     !CapturedStmtInfo->lookup(VD))) {
    //  LocalDeclMap[VD] = CreateMemTemp(VD->getType(), CGM.getMangledName(VD));
    //}
    QualType QTy = (*I)->getType();
    const Type *MainTy = QTy.getTypePtr();
    // const Type *Ty = MainTy->getArrayElementTypeNoTypeQual();
    // const Type *PrevTy = MainTy;
    // while (Ty != 0) {
    //  PrevTy = Ty;
    //  Ty = Ty->getArrayElementTypeNoTypeQual();
    //}
    // Ty = PrevTy;
    llvm::Value *Private = 0;
    {
      LocalVarsDeclGuard Grd(*this, true);
      AutoVarEmission Emission = EmitAutoVarAlloca(*VD);
      Private = Emission.getAllocatedAddress();
      EmitAutoVarCleanups(Emission);
    }
    // CodeGen for classes with the default constructor.
    if (!isTrivialInitializer(*InitIter)
        || (MainTy->isVariablyModifiedType() && !MainTy->isPointerType())) {
      RunCleanupsScope InitBlock(*this);
      if (const ArrayType *ArrayTy = MainTy->getAsArrayTypeUnsafe()) {
        // Create array.
        QualType ElementTy;
        llvm::Value *ArrayBeg = Private;
        llvm::Value *NumElements = emitArrayLength(ArrayTy, ElementTy,
            ArrayBeg);
        llvm::Value *ArrayEnd = Builder.CreateGEP(ArrayBeg, NumElements,
            "omp.arrayctor.end");
        // The basic structure here is a do-while loop, because we don't
        // need to check for the zero-element case.
        llvm::BasicBlock *BodyBB = createBasicBlock("omp.arrayctor.body");
        llvm::BasicBlock *DoneBB = createBasicBlock("omp.arrayctor.done");
        llvm::Value *IsEmpty = Builder.CreateICmpEQ(ArrayBeg, ArrayEnd,
            "omp.arrayctor.isempty");
        Builder.CreateCondBr(IsEmpty, DoneBB, BodyBB);

        // Enter the loop body, making that address the current address.
        llvm::BasicBlock *EntryBB = Builder.GetInsertBlock();
        EmitBlock(BodyBB);
        llvm::PHINode *ElementPast = Builder.CreatePHI(ArrayBeg->getType(), 2,
            "omp.arrayctor.elementPast");
        ElementPast->addIncoming(ArrayEnd, EntryBB);

        // Shift the address back by one element.
        llvm::Value *NegativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
        llvm::Value *Element = Builder.CreateGEP(ElementPast, NegativeOne,
            "omp.arrayctor.element");
        EmitAnyExprToMem(*InitIter, Element,
            (*InitIter)->getType().getQualifiers(), false);
        //// Check whether we've reached the end.
        llvm::Value *Done = Builder.CreateICmpEQ(Element, ArrayBeg,
            "omp.arrayctor.done");
        Builder.CreateCondBr(Done, DoneBB, BodyBB);
        ElementPast->addIncoming(Element, Builder.GetInsertBlock());

        // Done.
        EmitBlock(DoneBB, true);
      } else {
        EmitAnyExprToMem(*InitIter, Private,
            (*InitIter)->getType().getQualifiers(), false);
      }
    }
    CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
  }
}

void CodeGenFunction::EmitPostOMPLastPrivateClause(
    const OMPLastPrivateClause &C, const OMPExecutableDirective &S) {
  // ~Type1(tmp1);
  // ~Type2(tmp2);
  // ...
  //

  llvm::BasicBlock *LPBB, *LPEndBB;
  llvm::Instruction *LPIP;
  CGM.OpenMPSupport.getLastprivateIP(LPBB, LPIP, LPEndBB);
  if (!LPBB && !LPIP && !LPEndBB) {
    LPBB = createBasicBlock("omp.if.liter.start", CurFn);
    LPEndBB = createBasicBlock("omp.if.liter.end", CurFn);
    llvm::Value *LiterVal = Builder.CreateLoad(
        CGM.OpenMPSupport.getLastIterVar(), "liter");
    Builder.CreateCondBr(Builder.CreateIsNull(LiterVal), LPEndBB, LPBB);
    LPIP = LPBB->end();
    if (isLoopDirective(&S)) {
      Builder.SetInsertPoint(LPBB);
      EmitStmt(getFinalFromLoopDirective(&S));
      EnsureInsertPoint();
      LPBB = Builder.GetInsertBlock();
      LPIP = Builder.GetInsertPoint();
    }
    Builder.SetInsertPoint(LPEndBB);
    if (!CGM.OpenMPSupport.getNoWait())
      EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL);
  }
  ArrayRef<const Expr *>::iterator AssignIter = C.getAssignments().begin();
  ArrayRef<const Expr *>::iterator VarIter1 = C.getPseudoVars1().begin();
  ArrayRef<const Expr *>::iterator VarIter2 = C.getPseudoVars2().begin();
  for (OMPLastPrivateClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++AssignIter, ++VarIter1, ++VarIter2) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    llvm::Value *Private = CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD);
    if (!Private)
      continue;
    QualType QTy = (*I)->getType();
    const Type *MainTy = QTy.getTypePtr();
    const Type *Ty = MainTy->getArrayElementTypeNoTypeQual();
    const Type *PrevTy = MainTy;
    while (Ty != 0) {
      PrevTy = Ty;
      Ty = Ty->getArrayElementTypeNoTypeQual();
    }
    Ty = PrevTy;
    CGM.OpenMPSupport.delOpenMPPrivateVar(VD);
    CGBuilderTy::InsertPoint SavedIP = Builder.saveIP();
    Builder.SetInsertPoint(LPBB, LPIP);
    // CodeGen for classes with the copy assignment operator.
    if (!*AssignIter) {
      // For trivial assignment operator copy by memcpy.
      // EmitAnyExprToMem(*I, Private, QTy.getQualifiers(), false);
      // EmitAggregateAssign(EmitLValue(*I).getAddress(), Private,
      // VD->getType());
      EmitUniversalStore(EmitLValue(*I), Private, QTy);
    } else {
      RunCleanupsScope InitBlock(*this);
      // Copy elements one by one.
      if (const ArrayType *ArrayTy = MainTy->getAsArrayTypeUnsafe()) {
        // Copy array.
        QualType ElementTy;
        llvm::Value *SharedVar = EmitLValue(*I).getAddress();
        llvm::Value *NumElements = emitArrayLength(ArrayTy, ElementTy,
            SharedVar);
        llvm::Value *ArrayEnd = Builder.CreateGEP(SharedVar, NumElements);
        llvm::Value *MasterArray = Private;
        unsigned AddrSpace = MasterArray->getType()->getPointerAddressSpace();
        llvm::Type *BaseType = ConvertType(ElementTy)->getPointerTo(AddrSpace);
        llvm::Value *MasterArrayBegin = Builder.CreatePointerCast(MasterArray,
            BaseType, "master.array.begin");
        llvm::Value *MasterArrayEnd = Builder.CreateGEP(MasterArrayBegin,
            NumElements);
        // The basic structure here is a do-while loop, because we don't
        // need to check for the zero-element case.
        llvm::BasicBlock *BodyBB = createBasicBlock("omp.arraycpy.body");
        llvm::BasicBlock *DoneBB = createBasicBlock("omp.arraycpy.done");
        llvm::Value *IsEmpty = Builder.CreateICmpEQ(SharedVar, ArrayEnd,
            "omp.arraycpy.isempty");
        Builder.CreateCondBr(IsEmpty, DoneBB, BodyBB);

        // Enter the loop body, making that address the current address.
        llvm::BasicBlock *EntryBB = Builder.GetInsertBlock();
        EmitBlock(BodyBB);
        llvm::PHINode *ElementPast = Builder.CreatePHI(SharedVar->getType(), 2,
            "omp.arraycpy.elementPast");
        ElementPast->addIncoming(ArrayEnd, EntryBB);
        llvm::PHINode *MasterElementPast = Builder.CreatePHI(
            MasterArrayBegin->getType(), 2, "omp.arraycpy.masterElementPast");
        MasterElementPast->addIncoming(MasterArrayEnd, EntryBB);

        // Shift the address back by one element.
        llvm::Value *NegativeOne = llvm::ConstantInt::get(SizeTy, -1, true);
        llvm::Value *Element = Builder.CreateGEP(ElementPast, NegativeOne,
            "omp.arraycpy.element");
        llvm::Value *MasterElement = Builder.CreateGEP(MasterElementPast,
            NegativeOne, "omp.arraycpy.master.element");

        const VarDecl *PseudoVar1 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter1)->getDecl());
        const VarDecl *PseudoVar2 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter2)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar1, MasterElement);
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar2, Element);
        EmitIgnoredExpr(*AssignIter);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar1);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar2);

        // Check whether we've reached the end.
        llvm::Value *Done = Builder.CreateICmpEQ(Element, SharedVar,
            "omp.arraycpy.done");
        Builder.CreateCondBr(Done, DoneBB, BodyBB);
        ElementPast->addIncoming(Element, Builder.GetInsertBlock());
        MasterElementPast->addIncoming(MasterElement, Builder.GetInsertBlock());

        // Done.
        EmitBlock(DoneBB, true);
      } else {
        // Copy single object.
        const VarDecl *PseudoVar1 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter1)->getDecl());
        const VarDecl *PseudoVar2 = cast<VarDecl>(
            cast<DeclRefExpr>(*VarIter2)->getDecl());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar1,
            EmitLValue(*I).getAddress());
        CGM.OpenMPSupport.addOpenMPPrivateVar(PseudoVar2, Private);
        EmitIgnoredExpr(*AssignIter);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar1);
        CGM.OpenMPSupport.delOpenMPPrivateVar(PseudoVar2);
      }
    }
    LPBB = Builder.GetInsertBlock();
    LPIP = Builder.GetInsertPoint();
    Builder.restoreIP(SavedIP);
  }
  CGM.OpenMPSupport.setLastprivateIP(LPBB, LPIP, LPEndBB);
}

void CodeGenFunction::EmitCloseOMPLastPrivateClause(
    const OMPLastPrivateClause &, const OMPExecutableDirective &) {
  // ~Type1(tmp1);
  // ~Type2(tmp2);
  // ...
  //

  llvm::BasicBlock *LPBB, *LPEndBB;
  llvm::Instruction *LPIP;
  CGM.OpenMPSupport.getLastprivateIP(LPBB, LPIP, LPEndBB);
  if (LPBB || LPIP || LPEndBB) {
    CGBuilderTy::InsertPoint SavedIP = Builder.saveIP();
    Builder.SetInsertPoint(LPBB, LPIP);
    EmitBranch(LPEndBB);
    Builder.restoreIP(SavedIP);
    CGM.OpenMPSupport.setLastprivateIP(0, 0, 0);
  }
}

void CodeGenFunction::EmitInitOMPReductionClause(const OMPReductionClause &C,
    const OMPExecutableDirective &S) {
  (void) S;
  assert(!isa<OMPSimdDirective>(S)); // Not yet supported
  // Type1 tmp1(var1);
  // anon.field1 = &tmp1;
  // Type2 tmp2(var2);
  // anon.field2 = &tmp2;
  // ...
  //
  // CodeGen for reduction clause.
  CodeGenFunction &CGF = CGM.OpenMPSupport.getCGFForReductionFunction();
  llvm::Function *ReductionFunc = CGF.CurFn;
  if (!ReductionFunc) {
    FunctionArgList Args;
    ImplicitParamDecl Arg1(getContext(), 0, SourceLocation(), 0,
        getContext().VoidPtrTy);
    ImplicitParamDecl Arg2(getContext(), 0, SourceLocation(), 0,
        getContext().VoidPtrTy);
    Args.push_back(&Arg1);
    Args.push_back(&Arg2);
    const CGFunctionInfo &FI = CGF.getTypes().arrangeFreeFunctionDeclaration(
        getContext().VoidTy, Args, FunctionType::ExtInfo(), false);
    llvm::FunctionType *FTy = CGF.getTypes().GetFunctionType(FI);

    std::string reductionOpName;
    if (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
       || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64)
 	    reductionOpName = "omp_reduction_op";
    else
 	    reductionOpName = ".omp_reduction_op.";

	llvm::Function *Fn = llvm::Function::Create(FTy,
		 llvm::GlobalValue::InternalLinkage, reductionOpName,
		 &CGM.getModule());

    CGM.SetInternalFunctionAttributes(CurFuncDecl, Fn, FI);
    CGF.StartFunction(GlobalDecl(), getContext().VoidTy, Fn, FI, Args,
        SourceLocation());
    ReductionFunc = CGF.CurFn;
  }

  for (OMPReductionClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    QualType QTy = (*I)->getType();
    // if (!QTy->isScalarType())
    //  llvm_unreachable("Reduction variables with aggregate"
    //                   "types are not supported yet!");
    llvm::Type *PtrType = ConvertType(getContext().getPointerType(QTy));
    CGM.OpenMPSupport.registerReductionVar(VD, PtrType);
  }
}

void CodeGenFunction::EmitPreOMPReductionClause(const OMPReductionClause &C,
    const OMPExecutableDirective &S) {
  (void) S;
  assert(!isa<OMPSimdDirective>(S)); // Not yet supported
  // Type1 tmp1(var1);
  // anon.field1 = &tmp1;
  // Type2 tmp2(var2);
  // anon.field2 = &tmp2;
  // ...
  //
  llvm::Value *ReductionRecVar = CGM.OpenMPSupport.getReductionRecVar(*this);
  ArrayRef<const Expr *>::iterator InitIter = C.getDefaultInits().begin();
  for (OMPReductionClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++InitIter) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    // if (VD->hasLocalStorage() &&
    //    (!CapturedStmtInfo ||
    //     !CapturedStmtInfo->lookup(VD))) {
    //  LocalDeclMap[VD] = CreateMemTemp(VD->getType(), CGM.getMangledName(VD));
    //}
    QualType QTy = (*I)->getType();
    llvm::AllocaInst *Private = 0;
    {
      LocalVarsDeclGuard Grd(*this, true);
      AutoVarEmission Emission = EmitAutoVarAlloca(*VD);
      Private = cast<llvm::AllocaInst>(Emission.getAllocatedAddress());
      EmitAutoVarCleanups(Emission);
    }
    //         CreateMemTemp(QTy, CGM.getMangledName(VD) + ".reduction.");

    // CodeGen for classes with the constructor.
    // const Type *Ty = QTy.getTypePtr();
    if (!isTrivialInitializer(*InitIter)) {
      RunCleanupsScope InitBlock(*this);
      const FunctionDecl *FD = 0;
      if (const DeclRefExpr *DRE = dyn_cast_or_null<DeclRefExpr>(*InitIter)) {
        if (const FunctionDecl *D = dyn_cast_or_null<FunctionDecl>(
            DRE->getDecl()))
          FD = D;
      }
      if (FD && isa<OMPDeclareReductionDecl>(FD->getDeclContext())) {
        llvm::Value *RegularAddr = EmitLValue(*I).getAddress();
        llvm::Value *Args[] = { Private, RegularAddr };
        EmitCallOrInvoke(CGM.GetAddrOfGlobal(FD), Args);
        SetFirstprivateInsertPt(*this);
      } else {
        EmitAnyExprToMem(*InitIter, Private,
            (*InitIter)->getType().getQualifiers(), false);
      }
    } else if (*InitIter) {
      switch (C.getOperator()) {
      case OMPC_REDUCTION_or:
      case OMPC_REDUCTION_bitxor:
      case OMPC_REDUCTION_bitor:
      case OMPC_REDUCTION_sub:
      case OMPC_REDUCTION_add: {
        llvm::Value *Zero = llvm::Constant::getNullValue(
            Private->getAllocatedType());
        InitTempAlloca(Private, Zero);
        break;
      }
      case OMPC_REDUCTION_and:
      case OMPC_REDUCTION_mult:
      case OMPC_REDUCTION_bitand: {
        llvm::Value *AllOnes = llvm::Constant::getAllOnesValue(
            Private->getAllocatedType());
        InitTempAlloca(Private, AllOnes);
        break;
      }
      case OMPC_REDUCTION_min:
      case OMPC_REDUCTION_max:
      case OMPC_REDUCTION_custom:
        llvm_unreachable("Operator kind not allowed.");
      case OMPC_REDUCTION_unknown:
      case NUM_OPENMP_REDUCTION_OPERATORS:
        llvm_unreachable("Unknown operator kind.");
      }
    } else {
      llvm::Type *Ty = ConvertTypeForMem(QTy);
      switch (C.getOperator()) {
      case OMPC_REDUCTION_or:
      case OMPC_REDUCTION_bitxor:
      case OMPC_REDUCTION_bitor:
      case OMPC_REDUCTION_sub:
      case OMPC_REDUCTION_add: {
        if (QTy->isIntegralOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getNullValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isRealFloatingType()) {
          const llvm::fltSemantics &FS = Ty->getFltSemantics();
          llvm::APFloat InitVal = llvm::APFloat::getZero(FS);
          llvm::Value *Init = llvm::ConstantFP::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isPointerType()) {
          InitTempAlloca(Private,
              llvm::ConstantPointerNull::get(cast<llvm::PointerType>(Ty)));
        } else if (QTy->isAnyComplexType()) {
          const ComplexType *CmplxTy = QTy->castAs<ComplexType>();
          QualType ElTy = CmplxTy->getElementType();
          Ty = ConvertTypeForMem(ElTy);
          llvm::Value *Init;
          if (ElTy->isIntegralOrEnumerationType()) {
            llvm::APInt InitVal = llvm::APInt::getNullValue(
                CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
            Init = llvm::ConstantInt::get(CGM.getLLVMContext(), InitVal);
          } else {
            const llvm::fltSemantics &FS = Ty->getFltSemantics();
            llvm::APFloat InitVal = llvm::APFloat::getZero(FS);
            Init = llvm::ConstantFP::get(CGM.getLLVMContext(), InitVal);
          }
          ComplexPairTy Value(Init, Init);
          LValue Dst = MakeNaturalAlignAddrLValue(Private, QTy);
          EmitStoreOfComplex(Value, Dst, true);
        }
        break;
      }
      case OMPC_REDUCTION_and:
      case OMPC_REDUCTION_mult: {
        if (QTy->isIntegralOrEnumerationType()) {
          llvm::APInt InitVal(CGM.getDataLayout().getTypeStoreSizeInBits(Ty),
              1);
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isRealFloatingType()) {
          const llvm::fltSemantics &FS = Ty->getFltSemantics();
          llvm::APFloat InitVal(FS, 1);
          llvm::Value *Init = llvm::ConstantFP::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isPointerType()) {
          llvm::APInt InitVal(CGM.getDataLayout().getTypeStoreSizeInBits(Ty),
              1);
          llvm::Constant *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          Init = llvm::ConstantExpr::getCast(llvm::Instruction::IntToPtr, Init,
              Ty);
          InitTempAlloca(Private, Init);
        } else if (QTy->isAnyComplexType()) {
          const ComplexType *CmplxTy = QTy->castAs<ComplexType>();
          QualType ElTy = CmplxTy->getElementType();
          Ty = ConvertTypeForMem(ElTy);
          llvm::Value *Init;
          if (ElTy->isIntegralOrEnumerationType()) {
            llvm::APInt InitVal(CGM.getDataLayout().getTypeStoreSizeInBits(Ty),
                1);
            Init = llvm::ConstantInt::get(CGM.getLLVMContext(), InitVal);
          } else {
            const llvm::fltSemantics &FS = Ty->getFltSemantics();
            llvm::APFloat InitVal(FS, 1);
            Init = llvm::ConstantFP::get(CGM.getLLVMContext(), InitVal);
          }
          ComplexPairTy Value(Init, Init);
          LValue Dst = MakeNaturalAlignAddrLValue(Private, QTy);
          EmitStoreOfComplex(Value, Dst, true);
        }
        break;
      }
      case OMPC_REDUCTION_bitand: {
        if (QTy->isIntegralOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getAllOnesValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isRealFloatingType()) {
          llvm::APFloat InitVal = llvm::APFloat::getAllOnesValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantFP::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isPointerType()) {
          llvm::Value *Init = llvm::Constant::getAllOnesValue(Ty);
          InitTempAlloca(Private, Init);
        } else if (QTy->isAnyComplexType()) {
          const ComplexType *CmplxTy = QTy->castAs<ComplexType>();
          QualType ElTy = CmplxTy->getElementType();
          Ty = ConvertTypeForMem(ElTy);
          llvm::Value *Init;
          if (ElTy->isIntegralOrEnumerationType()) {
            llvm::APInt InitVal = llvm::APInt::getAllOnesValue(
                CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
            Init = llvm::ConstantInt::get(CGM.getLLVMContext(), InitVal);
          } else {
            llvm::APFloat InitVal = llvm::APFloat::getAllOnesValue(
                CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
            Init = llvm::ConstantFP::get(CGM.getLLVMContext(), InitVal);
          }
          ComplexPairTy Value(Init, Init);
          LValue Dst = MakeNaturalAlignAddrLValue(Private, QTy);
          EmitStoreOfComplex(Value, Dst, true);
        }
        break;
      }
      case OMPC_REDUCTION_min: {
        if (QTy->isSignedIntegerOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getSignedMaxValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isUnsignedIntegerOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getMaxValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isRealFloatingType()) {
          const llvm::fltSemantics &FS = Ty->getFltSemantics();
          llvm::APFloat InitVal = llvm::APFloat::getLargest(FS);
          llvm::Value *Init = llvm::ConstantFP::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isPointerType()) {
          llvm::APInt InitVal = llvm::APInt::getMaxValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Constant *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          Init = llvm::ConstantExpr::getCast(llvm::Instruction::IntToPtr, Init,
              Ty);
          InitTempAlloca(Private, Init);
        }
        break;
      }
      case OMPC_REDUCTION_max: {
        if (QTy->isSignedIntegerOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getSignedMinValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isUnsignedIntegerOrEnumerationType()) {
          llvm::APInt InitVal = llvm::APInt::getMinValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Value *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isRealFloatingType()) {
          const llvm::fltSemantics &FS = Ty->getFltSemantics();
          llvm::APFloat InitVal = llvm::APFloat::getLargest(FS, true);
          llvm::Value *Init = llvm::ConstantFP::get(CGM.getLLVMContext(),
              InitVal);
          InitTempAlloca(Private, Init);
        } else if (QTy->isPointerType()) {
          llvm::APInt InitVal = llvm::APInt::getMinValue(
              CGM.getDataLayout().getTypeStoreSizeInBits(Ty));
          llvm::Constant *Init = llvm::ConstantInt::get(CGM.getLLVMContext(),
              InitVal);
          Init = llvm::ConstantExpr::getCast(llvm::Instruction::IntToPtr, Init,
              Ty);
          InitTempAlloca(Private, Init);
        }
        break;
      }
      case OMPC_REDUCTION_custom:
        llvm_unreachable("Custom initialization cannot be NULLed.");
      case OMPC_REDUCTION_unknown:
      case NUM_OPENMP_REDUCTION_OPERATORS:
        llvm_unreachable("Unkonwn operator kind.");
      }
    }
    llvm::Value *Addr = Builder.CreateConstGEP2_32(ReductionRecVar, 0,
        CGM.OpenMPSupport.getReductionVarIdx(VD),
        CGM.getMangledName(VD) + ".addr");
    Builder.CreateStore(Private, Addr);
    // llvm::Value *Var = Builder.CreateLoad(Addr, CGM.getMangledName(VD));
    CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
  }
}

void CodeGenFunction::EmitPostOMPReductionClause(const OMPReductionClause &C,
    const OMPExecutableDirective &S) {
  (void) S;
  assert(!isa<OMPSimdDirective>(S)); // Not yet supported
  CodeGenFunction &CGF = CGM.OpenMPSupport.getCGFForReductionFunction();
  llvm::Function *ReduceFunc = CGF.CurFn;
  llvm::SwitchInst *Switch = dyn_cast_or_null<llvm::SwitchInst>(
      CGM.OpenMPSupport.getReductionSwitch());
  llvm::BasicBlock *RedBB1;
  llvm::BasicBlock *RedBB2;
  llvm::Instruction *IP1;
  llvm::Instruction *IP2;
  if (!Switch) {
    // __kmpc_reduce[_nowait](ident_t *loc, int32_t global_tid, int32_t
    // num_vars,
    //                      size_t reduce_size, void *reduce_data,
    //                     kmp_reduce_func reduce_func, kmp_critical_name *lck);
    // ident_t loc = {...};
    llvm::Value *Loc = OPENMPRTL_LOCFLAGS(C.getLocStart(), *this,
        KMP_IDENT_ATOMIC_REDUCE);
    // global_tid = __kmpc_global_thread_num(...);
    llvm::Value *GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
    // int num_vars = c;
    unsigned NumVars = CGM.OpenMPSupport.getNumberOfReductionVars();
    llvm::Value *NumVarsVal = llvm::ConstantInt::get(Int32Ty, NumVars);
    // size_t reduce_size = sizeof(rec);
    uint64_t ReduceSize = CGM.getDataLayout().getTypeAllocSize(
        CGM.OpenMPSupport.getReductionRec());
    llvm::Value *ReduceSizeVal = llvm::ConstantInt::get(SizeTy, ReduceSize);
    // void *reduce_data = (void *)rec;
    llvm::Value *ReduceData = Builder.CreatePointerCast(
        CGM.OpenMPSupport.getReductionRecVar(*this), VoidPtrTy,
        "(void*)reductionrec");
    // kmpc_reduce_func reduce_func = reduce_func;
    // kmp_critical_name lck;
    llvm::Type *LckTy = llvm::TypeBuilder<kmp_critical_name, false>::get(
        CGM.getLLVMContext());

    llvm::GlobalVariable *Lck = CreateRuntimeVariable(CGM, ".lck.", LckTy);
    CGM.OpenMPSupport.setReductionLockVar(Lck);
    llvm::Value *RealArgs[] = { Loc, GTid, NumVarsVal, ReduceSizeVal,
        ReduceData, ReduceFunc, Lck };
    llvm::CallInst *Res = EmitRuntimeCall(
        CGM.OpenMPSupport.getNoWait() ?
            OPENMPRTL_FUNC(reduce_nowait) : OPENMPRTL_FUNC(reduce), RealArgs);
    RedBB1 = createBasicBlock("reduction.case1", CurFn);
    RedBB2 = createBasicBlock("reduction.case2", CurFn);
    llvm::BasicBlock *DefaultBlock = createBasicBlock("reduction.continue",
        CurFn);
    Switch = Builder.CreateSwitch(Res, DefaultBlock, 2);
    Switch->addCase(llvm::ConstantInt::get(Int32Ty, 1), RedBB1);
    Switch->addCase(llvm::ConstantInt::get(Int32Ty, 2), RedBB2);
    IP1 = RedBB1->end();
    IP2 = RedBB2->end();
    Builder.SetInsertPoint(DefaultBlock);
    CGM.OpenMPSupport.setReductionSwitch(Switch);
  } else {
    CGM.OpenMPSupport.getReductionIPs(RedBB1, IP1, RedBB2, IP2);
  }
  llvm::Value *ReductionRecVar = CGM.OpenMPSupport.getReductionRecVar(*this);
  ArrayRef<const Expr *>::iterator Par1I = C.getHelperParameters1st().begin();
  ArrayRef<const Expr *>::iterator Par2I = C.getHelperParameters2nd().begin();
  ArrayRef<const Expr *>::iterator OpI = C.getOpExprs().begin();
  for (OMPReductionClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++Par1I, ++Par2I, ++OpI) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    QualType QTy = (*I)->getType();
    llvm::Value *Private = CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD);
    if (!Private)
      continue;
    CGM.OpenMPSupport.delOpenMPPrivateVar(VD);

    CGBuilderTy::InsertPoint SavedIP = Builder.saveIP();
    Builder.SetInsertPoint(RedBB1, IP1);
    const VarDecl *Par1 = cast<VarDecl>(cast<DeclRefExpr>(*Par1I)->getDecl());
    const VarDecl *Par2 = cast<VarDecl>(cast<DeclRefExpr>(*Par2I)->getDecl());
    QualType PtrQTy = getContext().getPointerType(QTy);
    llvm::AllocaInst *AI = CreateMemTemp(PtrQTy,
        CGM.getMangledName(VD) + ".addr.lhs.");
    LValue LVal = MakeNaturalAlignAddrLValue(AI, PtrQTy);
    UnaryOperator UOp(const_cast<Expr *>(*I), UO_AddrOf, PtrQTy, VK_LValue,
        OK_Ordinary, SourceLocation());
    // EmitExprAsInit(&UOp, VD, LVal, false);
    EmitAnyExprToMem(&UOp, AI, UOp.getType().getQualifiers(), false);
    llvm::Value *Addr2 = Builder.CreateConstGEP2_32(ReductionRecVar, 0,
        CGM.OpenMPSupport.getReductionVarIdx(VD),
        CGM.getMangledName(VD) + ".addr.rhs");
    CGM.OpenMPSupport.addOpenMPPrivateVar(Par1, AI);
    CGM.OpenMPSupport.addOpenMPPrivateVar(Par2, Addr2);
    EmitIgnoredExpr(*OpI);
    CGM.OpenMPSupport.delOpenMPPrivateVar(Par1);
    CGM.OpenMPSupport.delOpenMPPrivateVar(Par2);
    IP1 = Builder.GetInsertPoint();
    RedBB1 = Builder.GetInsertBlock();
    Builder.SetInsertPoint(RedBB2, IP2);
    llvm::Value *AtomicFunc = OPENMPRTL_ATOMIC_FUNC(QTy, C.getOperator());
    if (isa<BinaryOperator>((*OpI)->IgnoreImpCasts()) && AtomicFunc) {
      // __kmpc_atomic_...(&loc, global_tid, &glob, &reduction);
      // ident_t loc = {...};
      llvm::Value *Loc = OPENMPRTL_LOC(C.getLocStart(), *this);
      // global_tid = __kmpc_global_thread_num(...);
      llvm::Value *GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
      Addr2 = Builder.CreateConstGEP2_32(ReductionRecVar, 0,
          CGM.OpenMPSupport.getReductionVarIdx(VD),
          CGM.getMangledName(VD) + ".addr.rhs");
      llvm::Type *ArgTy = ConvertTypeForMem(OPENMPRTL_ATOMICTYPE(*this, QTy));
      llvm::Type *PtrArgTy = ArgTy->getPointerTo();
      llvm::Value *RealArgs[] = { Loc, GTid, Builder.CreatePointerCast(
          EmitScalarExpr(&UOp), PtrArgTy), Builder.CreateLoad(
          Builder.CreatePointerCast(
              Builder.CreateLoad(Addr2, CGM.getMangledName(VD) + ".rhs"),
              PtrArgTy)) };
      EmitRuntimeCall(AtomicFunc, RealArgs);
    } else {
      // __kmpc_atomic_start();
      EmitRuntimeCall(OPENMPRTL_FUNC(atomic_start));
      AI = CreateMemTemp(PtrQTy, CGM.getMangledName(VD) + ".addr.lhs.");
      LVal = MakeNaturalAlignAddrLValue(AI, PtrQTy);
      EmitAnyExprToMem(&UOp, AI, UOp.getType().getQualifiers(), false);
      // EmitExprAsInit(&UOp, VD, LVal, false);
      Addr2 = Builder.CreateConstGEP2_32(ReductionRecVar, 0,
          CGM.OpenMPSupport.getReductionVarIdx(VD),
          CGM.getMangledName(VD) + "addr.rhs");
      CGM.OpenMPSupport.addOpenMPPrivateVar(Par1, AI);
      CGM.OpenMPSupport.addOpenMPPrivateVar(Par2, Addr2);
      EmitIgnoredExpr(*OpI);
      CGM.OpenMPSupport.delOpenMPPrivateVar(Par1);
      CGM.OpenMPSupport.delOpenMPPrivateVar(Par2);
      // __kmpc_atomic_end();
      EmitRuntimeCall(OPENMPRTL_FUNC(atomic_end));
    }
    IP2 = Builder.GetInsertPoint();
    RedBB2 = Builder.GetInsertBlock();
    Builder.restoreIP(SavedIP);
  }
  CGM.OpenMPSupport.setReductionIPs(RedBB1, IP1, RedBB2, IP2);
}

llvm::CallInst *CodeGenFunction::EmitOMPCallWithLocAndTidHelper(llvm::Value *F,
    SourceLocation L, unsigned Flags) {
  llvm::Value *Loc = OPENMPRTL_LOCFLAGS(L, *this, Flags);
  llvm::Value *GTid = OPENMPRTL_THREADNUM(L, *this);
  llvm::Value *RealArgs[] = { Loc, GTid };
  return EmitRuntimeCall(F, RealArgs);
}

void CodeGenFunction::EmitOMPCapturedBodyHelper(
    const OMPExecutableDirective &S) {
  // TODO: We may inline instead of calling it...
  RunCleanupsScope MyScope(*this);
  EmitStmt(cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
  EnsureInsertPoint();
}

void CodeGenFunction::EmitOMPConditionalIfHelper(
    const OMPExecutableDirective &S, llvm::Value *Func, SourceLocation Loc,
    llvm::Value *EndFunc, SourceLocation EndLoc, bool HasClauses,
    llvm::AllocaInst *DidIt, const std::string &NameStr) {

  // This is for master and single directives:
  // if (__kmpc_Call()) {
  //   <captured_body>
  //   __kmpc_EndCall();
  // }
  //
  RunCleanupsScope ExecutedScope(*this);
  if (HasClauses) {
    // Pre-process private and firstprivate clauses
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I) {
      if (*I)
        EmitPreOMPClause(*(*I), S);
    }
  }

  if (DidIt) {
    // Store 0 into .did_it. flag
    llvm::Value *Zero = llvm::Constant::getNullValue(
        ConvertTypeForMem(getContext().IntTy));
    EmitStoreOfScalar(Zero, DidIt, false,
        CGM.getDataLayout().getPrefTypeAlignment(
            ConvertTypeForMem(getContext().IntTy)), getContext().IntTy);
  }

  // Start with emission of __kmpc_Call()
  llvm::CallInst *Call = EmitOMPCallWithLocAndTidHelper(Func, Loc);
  // Convert Call's result to bool, to use in IF-stmt
  llvm::Value *CallBool = EmitScalarConversion(Call, getContext().IntTy,
      getContext().BoolTy);
  // Generate the basic blocks
  llvm::BasicBlock *ThenBlock = createBasicBlock((NameStr + ".then").c_str());
  llvm::BasicBlock *ContBlock = createBasicBlock((NameStr + ".end").c_str());
  // Generate the branch (If-stmt)
  Builder.CreateCondBr(CallBool, ThenBlock, ContBlock);
  EmitBlock(ThenBlock);
  // Here we are on Then-branch -- emit captured body and __kmpc_EndCall()
  EmitOMPCapturedBodyHelper(S);
  if (DidIt) {
    // Store 1 into .did_it. flag
    llvm::Value *One = llvm::ConstantInt::get(CGM.getLLVMContext(),
        llvm::APInt::getLowBitsSet(
            CGM.getDataLayout().getTypeStoreSizeInBits(
                ConvertTypeForMem(getContext().IntTy)), 1));
    EmitStoreOfScalar(One, DidIt, false,
        CGM.getDataLayout().getPrefTypeAlignment(
            DidIt->getType()->getSequentialElementType()), getContext().IntTy);
  }
  EmitOMPCallWithLocAndTidHelper(EndFunc, EndLoc);
  // Emit the rest of bblocks/branches
  EmitBranch(ContBlock);
  EmitBlock(ContBlock, true);

  if (HasClauses) {
    // Post-process private and firstprivate clauses
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I) {
      if (*I)
        EmitPostOMPClause(*(*I), S);
    }
  }
}

/// "One-call" OMP Directives (barrier, taskyield, taskwait, flush).
/// '#pragma omp barrier' directive.
void CodeGenFunction::EmitOMPBarrierDirective(const OMPBarrierDirective &S) {
  // EmitUntiedPartIdInc(*this);
    EmitOMPCancelBarrier(S.getLocStart(), KMP_IDENT_BARRIER_EXPL);
  // EmitUntiedBranchEnd(*this);
  // EmitUntiedTaskSwitch(*this, false);
}

/// '#pragma omp taskyield' directive.
void CodeGenFunction::EmitOMPTaskyieldDirective(
    const OMPTaskyieldDirective &S) {
  // EmitUntiedPartIdInc(*this);
  llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
  llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
  llvm::Value *RealArgs[] = { Loc, GTid, Builder.getInt32(0) };
  EmitRuntimeCall(OPENMPRTL_FUNC(omp_taskyield), RealArgs);
  // EmitUntiedBranchEnd(*this);
  // EmitUntiedTaskSwitch(*this, false);
}

/// '#pragma omp taskwait' directive.
void CodeGenFunction::EmitOMPTaskwaitDirective(const OMPTaskwaitDirective &S) {
  // If the task is untied, we may want to generate IF-stmt here:
  // if (__kmpc_omp_taskwait(loc_task_wait, gtid) == CURRENT_TASK_QUEUED) {
  //      T-return; // Exit t1 if it was suspended or queued
  // }
  // But currently RTL always returns TASK_CURRENT_NOT_QUEUED,
  // so probably that make no sence.
  //
  EmitUntiedPartIdInc(*this);
  llvm::Value *Res = EmitOMPCallWithLocAndTidHelper(
  OPENMPRTL_FUNC(omp_taskwait), S.getLocStart());
  if (CGM.OpenMPSupport.getUntied()) {
    llvm::BasicBlock *ThenBB = createBasicBlock("taskwait.then");
    llvm::BasicBlock *EndBB = createBasicBlock("taskwait.end");
    llvm::Value *Cond = Builder.CreateICmpEQ(Res,
        Builder.getInt32(OMP_TASK_CURRENT_QUEUED));
    Builder.CreateCondBr(Cond, ThenBB, EndBB);
    EmitBlock(ThenBB);
    EmitUntiedBranchEnd(*this);
    EmitBlock(EndBB);
    EmitUntiedTaskSwitch(*this, true);
  }
}

/// '#pragma omp flush' directive.
void CodeGenFunction::EmitOMPFlushDirective(const OMPFlushDirective &S) {
  SmallVector<llvm::Value *, 4> Args;
  Args.push_back(OPENMPRTL_LOC(S.getLocStart(), *this));
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I) {
    const OMPFlushClause *C = cast<OMPFlushClause>(*I);
    for (ArrayRef<const Expr *>::iterator J = C->varlist_begin(), F =
        C->varlist_end(); J != F; ++J) {
      QualType QTy = (*J)->getType();
      QualType PtrQTy = getContext().getPointerType(QTy);
      UnaryOperator UOp(const_cast<Expr *>(*J), UO_AddrOf, PtrQTy, VK_LValue,
          OK_Ordinary, S.getLocStart());
      llvm::Value *Val = EmitScalarExpr(&UOp);
      Args.push_back(Val);
    }
  }
  EmitRuntimeCall(OPENMPRTL_FUNC(flush), Args);
}

/// '#pragma omp cancel' directive.
void CodeGenFunction::EmitOMPCancelDirective(const OMPCancelDirective &S) {
  llvm::Value *Loc;
  llvm::Value *GTid;
  llvm::Value *Kind;
  EmitCancelArgs(*this, S.getConstructType(), S.getLocStart(), Loc, GTid, Kind);

  llvm::Value *RealArgs[] = { Loc, GTid, Kind };

  llvm::BasicBlock *ContBB = createBasicBlock("omp.cancel.continue");
  llvm::BasicBlock *ExitBB = createBasicBlock("omp.cancel.exit");
  if (!S.clauses().empty()) {
    assert(
        S.clauses().size() == 1 && isa<OMPIfClause>(S.clauses().front())
            && "Wrong number or type of clause in omp cancel directive");
    const OMPIfClause *Clause = cast<OMPIfClause>(S.clauses().front());
    llvm::BasicBlock *ThenBB = createBasicBlock("omp.cancel.then");
    llvm::BasicBlock *ElseBB = createBasicBlock("omp.cancel.else");
    EmitBranchOnBoolExpr(Clause->getCondition(), ThenBB, ElseBB, 0);
    EmitBlock(ElseBB);
    EmitCancellationPoint(*this, S.getLocStart(), RealArgs, ExitBB, ContBB);
    EmitBlock(ThenBB);
  }

  llvm::Value *CallRes = Builder.CreateIsNotNull(
      EmitRuntimeCall(OPENMPRTL_FUNC(cancel), RealArgs));
  Builder.CreateCondBr(CallRes, ExitBB, ContBB);
  EmitBlock(ExitBB);
  assert(
      OMPCancelMap.count(S.getConstructType()) && "No exit point for cancel");
  EmitOMPCancelBarrier(S.getLocStart(), KMP_IDENT_BARRIER_IMPL, true);
  EmitBranchThroughCleanup(OMPCancelMap[S.getConstructType()]);
  EmitBlock(ContBB);
}

/// '#pragma omp cancellation point' directive.
void CodeGenFunction::EmitOMPCancellationPointDirective(
    const OMPCancellationPointDirective &S) {
  llvm::Value *Loc;
  llvm::Value *GTid;
  llvm::Value *Kind;
  EmitCancelArgs(*this, S.getConstructType(), S.getLocStart(), Loc, GTid, Kind);

  llvm::Value *RealArgs[] = { Loc, GTid, Kind };

  llvm::BasicBlock *ExitBB = createBasicBlock("omp.cancellationpoint.exit");
  llvm::BasicBlock *ContBB = createBasicBlock("omp.cancellationpoint.continue");
  assert(
      OMPCancelMap.count(S.getConstructType())
          && "No exit point for cancellation point");
  EmitCancellationPoint(*this, S.getLocStart(), RealArgs, ExitBB, ContBB,
      OMPCancelMap[S.getConstructType()]);
}

/// Atomic OMP Directive -- pattern match and emit one RTL call.
/// In the future, we may want to generate some atomic llvm instruction
/// instead of RTL call here for some atomic directives.
void CodeGenFunction::EmitOMPAtomicDirective(const OMPAtomicDirective &S) {
  CGM.OpenMPSupport.startOpenMPRegion(false);
  bool IsSeqCst = false;
  bool AtLeastOneLoopTaken = false;
  OpenMPClauseKind Kind = OMPC_update;
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E || !AtLeastOneLoopTaken; ++I) {
    if (I != S.clauses().end()) {
      if ((*I)->getClauseKind() == OMPC_seq_cst) {
        IsSeqCst = true;
        continue;
      }
      Kind = (*I)->getClauseKind();
    }
    LValue X = EmitLValue(S.getX()->IgnoreParenLValueCasts());
    switch (Kind) {
    case OMPC_read: {
      QualType QTy = S.getX()->getType();
      QualType AQTy = OPENMPRTL_ATOMICTYPE(*this, QTy);
      llvm::Value *AtomicFunc = AQTy.isNull() ? 0 :
      OPENMPRTL_ATOMIC_FUNC_GENERAL(AQTy, AQTy,
          CGOpenMPRuntime::OMP_Atomic_rd, false, false);
      if (X.isSimple() && AtomicFunc) {
        llvm::Type *ATy = ConvertTypeForMem(AQTy);
        llvm::SmallVector<llvm::Value *, 5> Args;
        // __kmpc_atomic_..._rd(&loc, global_tid, &x);
        // ident_t loc = {...};
        llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
        // global_tid = __kmpc_global_thread_num(...);
        llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
        Args.push_back(Loc);
        Args.push_back(GTid);
        Args.push_back(
            Builder.CreatePointerCast(X.getAddress(), ATy->getPointerTo()));
        llvm::Value *Res = EmitRuntimeCall(AtomicFunc, Args);
        // v = x;
        Res = EmitScalarConversion(Res, AQTy, S.getV()->getType());
        EmitStoreOfScalar(Res, EmitLValue(S.getV()));
      } else {
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_start));
        RValue Val = EmitLoadOfLValue(X, S.getX()->getExprLoc());
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_end));
        EmitStoreThroughLValue(Val, EmitLValue(S.getV()));
      }
    }
      break;
    case OMPC_write: {
      QualType QTy = S.getX()->getType();
      QualType AQTy = OPENMPRTL_ATOMICTYPE(*this, QTy);
      QualType QTyIn = S.getExpr()->getType();
      llvm::Value *AtomicFunc = AQTy.isNull() ? 0 :
      OPENMPRTL_ATOMIC_FUNC_GENERAL(AQTy, AQTy,
          CGOpenMPRuntime::OMP_Atomic_wr, false, false);
      if (X.isSimple() && AtomicFunc && QTyIn->isScalarType()
          && !QTyIn->isAnyComplexType()) {
        llvm::Type *ATy = ConvertTypeForMem(AQTy);
        llvm::SmallVector<llvm::Value *, 5> Args;
        // __kmpc_atomic_..._wr(&loc, global_tid, &x, expr);
        // ident_t loc = {...};
        llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
        // global_tid = __kmpc_global_thread_num(...);
        llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
        Args.push_back(Loc);
        Args.push_back(GTid);
        Args.push_back(
            Builder.CreatePointerCast(X.getAddress(), ATy->getPointerTo()));
        Args.push_back(
            EmitScalarConversion(EmitAnyExpr(S.getExpr()).getScalarVal(),
                S.getExpr()->getType(), AQTy));
        EmitRuntimeCall(AtomicFunc, Args);
      } else {
        RValue Val = EmitAnyExpr(S.getExpr());
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_start));
        EmitStoreThroughLValue(Val, X);
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_end));
      }
    }
      break;
    case OMPC_update: {
      QualType QTyRes = S.getX()->getType();
      QualType AQTyRes = OPENMPRTL_ATOMICTYPE(*this, QTyRes);
      QualType QTyIn = S.getExpr()->getType();
      QualType AQTyIn = OPENMPRTL_ATOMICTYPE(*this, QTyIn);
      CGOpenMPRuntime::EAtomicOperation Aop;
      switch (S.getOperator()) {
      case BO_Add:
        Aop = CGOpenMPRuntime::OMP_Atomic_add;
        break;
      case BO_Sub:
        Aop = CGOpenMPRuntime::OMP_Atomic_sub;
        break;
      case BO_Mul:
        Aop = CGOpenMPRuntime::OMP_Atomic_mul;
        break;
      case BO_Div:
        Aop = CGOpenMPRuntime::OMP_Atomic_div;
        break;
      case BO_And:
        Aop = CGOpenMPRuntime::OMP_Atomic_andb;
        break;
      case BO_Or:
        Aop = CGOpenMPRuntime::OMP_Atomic_orb;
        break;
      case BO_Xor:
        Aop = CGOpenMPRuntime::OMP_Atomic_xor;
        break;
      case BO_Shl:
        Aop = CGOpenMPRuntime::OMP_Atomic_shl;
        break;
      case BO_Shr:
        Aop = CGOpenMPRuntime::OMP_Atomic_shr;
        break;
      default:
        Aop = CGOpenMPRuntime::OMP_Atomic_invalid;
        break;
      }
      llvm::Value *AtomicFunc = (AQTyRes.isNull() || AQTyIn.isNull()) ? 0 :
      OPENMPRTL_ATOMIC_FUNC_GENERAL(AQTyRes, AQTyIn, Aop, false,
          S.isReversed());
      if (X.isSimple() && AtomicFunc && QTyIn->isScalarType()
          && !QTyIn->isAnyComplexType()) {
        llvm::Type *ATyRes = ConvertTypeForMem(AQTyRes);
        llvm::SmallVector<llvm::Value *, 5> Args;
        // __kmpc_atomic_..._op(&loc, global_tid, &x, expr);
        // ident_t loc = {...};
        llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
        // global_tid = __kmpc_global_thread_num(...);
        llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
        Args.push_back(Loc);
        Args.push_back(GTid);
        Args.push_back(
            Builder.CreatePointerCast(X.getAddress(), ATyRes->getPointerTo()));
        Args.push_back(EmitAnyExpr(S.getExpr()).getScalarVal());
        EmitRuntimeCall(AtomicFunc, Args);
      } else {
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_start));
        EmitStmt(cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_end));
      }
    }
      break;
    case OMPC_capture: {
      QualType QTyRes = S.getX()->getType();
      QualType AQTyRes = OPENMPRTL_ATOMICTYPE(*this, QTyRes);
      QualType QTyIn = S.getExpr()->getType();
      QualType AQTyIn = OPENMPRTL_ATOMICTYPE(*this, QTyIn);
      CGOpenMPRuntime::EAtomicOperation Aop;
      switch (S.getOperator()) {
      case BO_Add:
        Aop = CGOpenMPRuntime::OMP_Atomic_add;
        break;
      case BO_Sub:
        Aop = CGOpenMPRuntime::OMP_Atomic_sub;
        break;
      case BO_Mul:
        Aop = CGOpenMPRuntime::OMP_Atomic_mul;
        break;
      case BO_Div:
        Aop = CGOpenMPRuntime::OMP_Atomic_div;
        break;
      case BO_And:
        Aop = CGOpenMPRuntime::OMP_Atomic_andb;
        break;
      case BO_Or:
        Aop = CGOpenMPRuntime::OMP_Atomic_orb;
        break;
      case BO_Xor:
        Aop = CGOpenMPRuntime::OMP_Atomic_xor;
        break;
      case BO_Shl:
        Aop = CGOpenMPRuntime::OMP_Atomic_shl;
        break;
      case BO_Shr:
        Aop = CGOpenMPRuntime::OMP_Atomic_shr;
        break;
      case BO_Assign:
        Aop = CGOpenMPRuntime::OMP_Atomic_assign;
        break;
      default:
        Aop = CGOpenMPRuntime::OMP_Atomic_invalid;
        break;
      }
      llvm::Value *AtomicFunc = (AQTyRes.isNull() || AQTyIn.isNull()) ? 0 :
      OPENMPRTL_ATOMIC_FUNC_GENERAL(AQTyRes, AQTyIn, Aop, true,
          S.isReversed());
      if (X.isSimple() && AtomicFunc && QTyIn->isScalarType()
          && !QTyIn->isAnyComplexType()) {
        llvm::Type *ATy = ConvertTypeForMem(AQTyRes);
        llvm::SmallVector<llvm::Value *, 5> Args;
        // __kmpc_atomic_..._op(&loc, global_tid, &x, expr);
        // ident_t loc = {...};
        llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
        // global_tid = __kmpc_global_thread_num(...);
        llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
        Args.push_back(Loc);
        Args.push_back(GTid);
        Args.push_back(
            Builder.CreatePointerCast(X.getAddress(), ATy->getPointerTo()));
        Args.push_back(EmitAnyExpr(S.getExpr()).getScalarVal());
        Args.push_back(Builder.getInt32(S.isCaptureAfter() ? 1 : 0));
        llvm::Value *Res = EmitRuntimeCall(AtomicFunc, Args);
        // v = x;
        Res = EmitScalarConversion(Res, AQTyRes, S.getV()->getType());
        EmitStoreOfScalar(Res, EmitLValue(S.getV()));
      } else {
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_start));
        EmitStmt(cast<CapturedStmt>(S.getAssociatedStmt())->getCapturedStmt());
        EmitRuntimeCall(OPENMPRTL_FUNC(atomic_end));
      }
    }
      break;
    case OMPC_seq_cst:
      llvm_unreachable("SEQ_CST should be processed already.");
      break;
    default:
      llvm_unreachable("Not allowed operation in atomic directive.");
    }
    if (I == E && !AtLeastOneLoopTaken)
      break;
    AtLeastOneLoopTaken = true;
  }
  if (IsSeqCst) {
    SmallVector<llvm::Value *, 1> Args;
    Args.push_back(OPENMPRTL_LOC(S.getLocStart(), *this));
    EmitRuntimeCall(OPENMPRTL_FUNC(flush), Args);
  }
  CGM.OpenMPSupport.endOpenMPRegion();
}

/// "Two-calls" OMP Directives (master, single, critical, ordered).
/// '#pragma omp master' directive.
void CodeGenFunction::EmitOMPMasterDirective(const OMPMasterDirective &S) {
  // if (__kmpc_master()) {
  //   <captured_body>
  //   __kmpc_end_master();
  // }
  EmitOMPConditionalIfHelper(S, OPENMPRTL_FUNC(master), S.getLocStart(),
  OPENMPRTL_FUNC(end_master), S.getLocStart(), false, // pragma has no clauses
      0,     // has no need for "didit"
      "omp.master");
}

/// '#pragma omp single' directive.
void CodeGenFunction::EmitOMPSingleDirective(const OMPSingleDirective &S) {

  // Init list of private globals in the stack.
  CGM.OpenMPSupport.startOpenMPRegion(false);
  CGM.OpenMPSupport.setNoWait(false);
  bool HasClauses = S.getNumClauses();
  if (HasClauses) {
    // Set NoWait flag if the clause nowait is there
    for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
        S.clauses().end(); I != E; ++I) {
      if (*I)
        EmitInitOMPClause(*(*I), S);
    }
  }

  // did_it = 0;
  // if (__kmpc_single()) {
  //   <captured_body>
  //   did_it = 1;
  //   __kmpc_end_single();
  // }
  // ... if there is copyprivate clause, call to __kmpc_copyprivate()
  // ... if there is no nowait, call to __kmpc_barrier()
  //

  // Create a "did_it" temp for passing into copyprivate routine.
  llvm::AllocaInst *DidIt = CreateMemTemp(getContext().IntTy, ".did_it.");
  InitTempAlloca(DidIt,
      llvm::Constant::getNullValue(ConvertTypeForMem(getContext().IntTy)));

  EmitOMPConditionalIfHelper(S, OPENMPRTL_FUNC(single), S.getLocStart(),
  OPENMPRTL_FUNC(end_single), S.getLocStart(), HasClauses, // pragma has clauses (private and
                                                           // firstprivate will be processed)
      DidIt,                       // address to store 1 for "the single" thread
      "omp.single");

  // Copyprivate clause.
  // Restrictions to copyprivate (from standard):
  // The items that appear in copyprivate must be either threadprivate or
  // private in the enclosing context.
  // A list item that appears in copyprivate clause may not appear in a private
  // or firstprivate clause on the single construct.
  //
  bool HasCopyPrivate = false;
  for (ArrayRef<OMPClause *>::iterator ICL = S.clauses().begin(), ECL =
      S.clauses().end(); ICL != ECL; ++ICL) {
    if (*ICL) {
      if (const OMPCopyPrivateClause *C = dyn_cast<OMPCopyPrivateClause>(
          *ICL)) {
        // Begin copyprivate clause processing
        HasCopyPrivate = true;
        // Start a copy-function.
        CodeGenFunction CGF(CGM, true);
        CGF.CurFn = 0;
        FunctionArgList Args;
        ImplicitParamDecl Arg1(getContext(), 0, SourceLocation(), 0,
            getContext().VoidPtrTy);
        ImplicitParamDecl Arg2(getContext(), 0, SourceLocation(), 0,
            getContext().VoidPtrTy);
        Args.push_back(&Arg1);
        Args.push_back(&Arg2);
        const CGFunctionInfo &FI =
            CGF.getTypes().arrangeFreeFunctionDeclaration(getContext().VoidTy,
                Args, FunctionType::ExtInfo(), false);
        llvm::FunctionType *FTy = CGF.getTypes().GetFunctionType(FI);
        llvm::Function *Fn = llvm::Function::Create(FTy,
            llvm::GlobalValue::InternalLinkage, StringRef(".omp_copy_func."),
            &CGM.getModule());
        CGM.SetInternalFunctionAttributes(CurFuncDecl, Fn, FI);
        CGF.StartFunction(GlobalDecl(), getContext().VoidTy, Fn, FI, Args,
            SourceLocation());

        // Generate the record of pointers - cpy.var
        llvm::SmallVector<llvm::Type *, 16> CpyFieldTypes;
        for (OMPCopyPrivateClause::varlist_const_iterator I =
            C->varlist_begin(), E = C->varlist_end(); I != E; ++I) {
          // const VarDecl *VD =
          // cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
          QualType QTy = (*I)->getType();
          llvm::Type *PtrType = ConvertType(getContext().getPointerType(QTy));
          CpyFieldTypes.push_back(PtrType);
        }
        llvm::StructType *CpyType = llvm::StructType::get(CGM.getLLVMContext(),
            CpyFieldTypes);
        llvm::AllocaInst *CpyVar = CreateTempAlloca(CpyType, "cpy.var");
        CpyVar->setAlignment(CGM.PointerAlignInBytes);

        // Generate initializaion of our local record with addresses.
        int32_t FieldNum = 0;
        for (OMPCopyPrivateClause::varlist_const_iterator I =
            C->varlist_begin(), E = C->varlist_end(); I != E; ++I, ++FieldNum) {
          // Store the address into our record.
          Builder.CreateStore(EmitLValue(*I).getAddress(),
              Builder.CreateConstGEP2_32(CpyVar, 0, FieldNum));
        }

        // Generate field copying in the copy-function.
        {
          llvm::Function::arg_iterator ArgIt = CGF.CurFn->arg_begin();
          llvm::Value *DstPtr = ArgIt;
          llvm::Value *SrcPtr = ++ArgIt;
          llvm::Value *DstBase = CGF.Builder.CreatePointerCast(DstPtr,
              CpyType->getPointerTo(), "cpy.dst");
          llvm::Value *SrcBase = CGF.Builder.CreatePointerCast(SrcPtr,
              CpyType->getPointerTo(), "cpy.src");

          ArrayRef<const Expr *>::iterator AssignIter =
              C->getAssignments().begin();
          ArrayRef<const Expr *>::iterator VarIter1 =
              C->getPseudoVars1().begin();
          ArrayRef<const Expr *>::iterator VarIter2 =
              C->getPseudoVars2().begin();
          FieldNum = 0;
          for (OMPCopyPrivateClause::varlist_const_iterator I =
              C->varlist_begin(), E = C->varlist_end(); I != E;
              ++I, ++AssignIter, ++VarIter1, ++VarIter2, ++FieldNum) {
            // const VarDecl *VD =
            // cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
            QualType QTy = (*I)->getType();
            llvm::Value *Dst = CGF.Builder.CreateConstGEP2_32(DstBase, 0,
                FieldNum);
            llvm::Value *Src = CGF.Builder.CreateConstGEP2_32(SrcBase, 0,
                FieldNum);
            llvm::Type *PtrType = ConvertType(getContext().getPointerType(QTy));
            llvm::Value *LoadDst = CGF.EmitLoadOfScalar(Dst, false,
                CGM.getDataLayout().getPrefTypeAlignment(PtrType),
                getContext().getPointerType(QTy), SourceLocation());
            llvm::Value *LoadSrc = CGF.EmitLoadOfScalar(Src, false,
                CGM.getDataLayout().getPrefTypeAlignment(PtrType),
                getContext().getPointerType(QTy), SourceLocation());
            CGF.EmitCopyAssignment(I, AssignIter, VarIter1, VarIter2, LoadDst,
                LoadSrc);
          }
        }

        // Generate a call to __kmpc_copyprivate.
        {
          // __kmpc_copyprivate(ident_t *loc, int32_t global_tid,
          //                    size_t cpy_size, void *cpy_data,
          //                    kmp_copy_func cpy_func, int32_t didit);
          llvm::Value *Loc = OPENMPRTL_LOC(C->getLocStart(), *this);
          llvm::Value *GTid = OPENMPRTL_THREADNUM(C->getLocStart(), *this);
          int32_t CpySizeInt = CGM.getDataLayout().getTypeAllocSize(CpyType);
          llvm::Value *CpySize = llvm::ConstantInt::get(SizeTy, CpySizeInt);
          llvm::Value *LoadDidIt = EmitLoadOfScalar(DidIt, false,
              CGM.getDataLayout().getPrefTypeAlignment(
                  DidIt->getType()->getSequentialElementType()),
              getContext().IntTy, SourceLocation());
          llvm::Value *RealArgs[] = { Loc, GTid, CpySize, Builder.CreateBitCast(
              CpyVar, VoidPtrTy, "(void*)cpyrec"), CGF.CurFn, LoadDidIt };
          EmitRuntimeCall(OPENMPRTL_FUNC(copyprivate), RealArgs);
        }

        // Stop the copy-function.
        CGF.FinishFunction();
        // End copyprivate clause processing
      }
    }
  }

  if (!HasCopyPrivate && !CGM.OpenMPSupport.getNoWait()) {
    // Note: __kmpc_copyprivate already has a couple of barriers internally.
    EmitOMPCancelBarrier(S.getLocEnd(), KMP_IDENT_BARRIER_IMPL_SINGLE);
  }

  // Remove list of private globals from the stack.
  CGM.OpenMPSupport.endOpenMPRegion();
}

/// '#pragma omp critical' directive.
void CodeGenFunction::EmitOMPCriticalDirective(const OMPCriticalDirective &S) {
  // __kmpc_critical();
  // <captured_body>
  // __kmpc_end_critical();
  //

  // Prepare kmp_critical_name -- the name of our critical section.
  std::string directive_name = S.getDirectiveName().getAsString();
  std::string name;
  if (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
     || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64)
	    name = "_gomp_critical_user_" + directive_name + "_var";
  else
	    name = ".gomp_critical_user_" + directive_name + ".var";
  llvm::Type *LckTy = llvm::TypeBuilder<kmp_critical_name, false>::get(
      CGM.getLLVMContext());
  llvm::GlobalVariable *Lck = cast<llvm::GlobalVariable>(
      CGM.CreateRuntimeVariable(LckTy, name.c_str()));
  Lck->setLinkage(llvm::GlobalValue::CommonLinkage);
  Lck->setInitializer(llvm::Constant::getNullValue(LckTy));

  // Prepare other arguments and build a call to __kmpc_critical
  llvm::Value *Loc = OPENMPRTL_LOC(S.getLocStart(), *this);
  llvm::Value *GTid = OPENMPRTL_THREADNUM(S.getLocStart(), *this);
  llvm::Value *RealArgs[] = { Loc, GTid, Lck };

  // FIXME: this needs a better design
  if (CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx
     || CGM.getTarget().getTriple().getArch() == llvm::Triple::nvptx64) {
    CGM.getOpenMPRuntime().SupportCritical(S, *this, CurFn, Lck);
  }
  else {
	  EmitRuntimeCall(OPENMPRTL_FUNC(critical), RealArgs);
	  EmitOMPCapturedBodyHelper(S);
	  EmitRuntimeCall(OPENMPRTL_FUNC(end_critical), RealArgs);
  }
}

/// '#pragma omp ordered' directive.
void CodeGenFunction::EmitOMPOrderedDirective(const OMPOrderedDirective &S) {
  // __kmpc_ordered();
  //   <captured_body>
  // __kmpc_enc_ordered();
  //
  EmitOMPCallWithLocAndTidHelper(OPENMPRTL_FUNC(ordered), S.getLocStart());
  EmitOMPCapturedBodyHelper(S);
  EmitOMPCallWithLocAndTidHelper(OPENMPRTL_FUNC(end_ordered), S.getLocStart());
}

/// '#pragma omp taskgroup' directive.
void CodeGenFunction::EmitOMPTaskgroupDirective(
    const OMPTaskgroupDirective &S) {
  // __kmpc_taskgroup();
  //   <captured_body>
  // __kmpc_enc_taskgroup();
  //
  EmitOMPCallWithLocAndTidHelper(OPENMPRTL_FUNC(taskgroup), S.getLocStart());
  EmitOMPCapturedBodyHelper(S);
  EmitOMPCallWithLocAndTidHelper(OPENMPRTL_FUNC(end_taskgroup), S.getLocEnd());

  // EmitUntiedPartIdInc(*this);
  // EmitUntiedBranchEnd(*this);
  // EmitUntiedTaskSwitch(*this, false);
}

void CodeGenFunction::EmitCloseOMPReductionClause(const OMPReductionClause &C,
    const OMPExecutableDirective &S) {
  (void) S;
  assert(!isa<OMPSimdDirective>(S)); // Not yet supported
  llvm::BasicBlock *RedBB1;
  llvm::BasicBlock *RedBB2;
  llvm::Instruction *IP1;
  llvm::Instruction *IP2;
  CGM.OpenMPSupport.getReductionIPs(RedBB1, IP1, RedBB2, IP2);
  llvm::SwitchInst *Switch = dyn_cast_or_null<llvm::SwitchInst>(
      CGM.OpenMPSupport.getReductionSwitch());
  if (Switch && (IP1 || IP2 || RedBB1 || RedBB2)) {
    CGBuilderTy::InsertPoint SavedIP = Builder.saveIP();
    Builder.SetInsertPoint(RedBB1, IP1);
    // __kmpc_end_reduce[_nowait](ident_t *loc, int32_t global_tid, *lck);
    // ident_t loc = {...};
    llvm::Value *Loc = OPENMPRTL_LOC(C.getLocStart(), *this);
    // global_tid = __kmpc_global_thread_num(...);
    llvm::Value *GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
    // kmp_critical_name lck;
    llvm::Value *RealArgs[] = { Loc, GTid,
        CGM.OpenMPSupport.getReductionLockVar() };
    EmitRuntimeCall(
        CGM.OpenMPSupport.getNoWait() ?
            OPENMPRTL_FUNC(end_reduce_nowait) : OPENMPRTL_FUNC(end_reduce),
        RealArgs);
    Builder.CreateBr(Switch->getDefaultDest());
    // Switch->addCase(llvm::ConstantInt::get(Int32Ty, 1), RedBB1);
    Builder.SetInsertPoint(RedBB2, IP2);
    // __kmpc_end_reduce[_nowait](ident_t *loc, int32_t global_tid, *lck);
    // ident_t loc = {...};
    Loc = OPENMPRTL_LOC(C.getLocStart(), *this);
    // global_tid = __kmpc_global_thread_num(...);
    GTid = OPENMPRTL_THREADNUM(C.getLocStart(), *this);
    // kmp_critical_name lck;
    RealArgs[0] = Loc;
    RealArgs[1] = GTid;
    RealArgs[2] = CGM.OpenMPSupport.getReductionLockVar();
    EmitRuntimeCall(CGM.OpenMPSupport.getNoWait()
                        ? OPENMPRTL_FUNC(end_reduce_nowait)
                        : OPENMPRTL_FUNC(end_reduce),
                    RealArgs);
    Builder.CreateBr(Switch->getDefaultDest());
    // Switch->addCase(llvm::ConstantInt::get(Int32Ty, 2), RedBB2);
    Builder.restoreIP(SavedIP);
    CGM.OpenMPSupport.setReductionIPs(0, 0, 0, 0);
  }

  CodeGenFunction &CGF = CGM.OpenMPSupport.getCGFForReductionFunction();
  llvm::Value *Arg1;
  llvm::Value *Arg2;
  CGM.OpenMPSupport.getReductionFunctionArgs(Arg1, Arg2);
  ArrayRef<const Expr *>::iterator Par1I = C.getHelperParameters1st().begin();
  ArrayRef<const Expr *>::iterator Par2I = C.getHelperParameters2nd().begin();
  ArrayRef<const Expr *>::iterator OpI = C.getOpExprs().begin();
  for (OMPReductionClause::varlist_const_iterator I = C.varlist_begin(), E =
      C.varlist_end(); I != E; ++I, ++Par1I, ++Par2I, ++OpI) {
    // Get element type.
    const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*I)->getDecl());
    if (VD->hasLocalStorage()
        && (!CapturedStmtInfo || !CapturedStmtInfo->lookup(VD)))
      continue;
    const VarDecl *Par1 = cast<VarDecl>(cast<DeclRefExpr>(*Par1I)->getDecl());
    const VarDecl *Par2 = cast<VarDecl>(cast<DeclRefExpr>(*Par2I)->getDecl());
    llvm::Value *Addr1 = CGF.Builder.CreateConstGEP2_32(Arg1, 0,
        CGM.OpenMPSupport.getReductionVarIdx(VD),
        CGM.getMangledName(VD) + ".addr.lhs");
    llvm::Value *Addr2 = CGF.Builder.CreateConstGEP2_32(Arg2, 0,
        CGM.OpenMPSupport.getReductionVarIdx(VD),
        CGM.getMangledName(VD) + ".addr.rhs");
    CGM.OpenMPSupport.addOpenMPPrivateVar(Par1, Addr1);
    CGM.OpenMPSupport.addOpenMPPrivateVar(Par2, Addr2);
    CGF.EmitIgnoredExpr(*OpI);
    CGM.OpenMPSupport.delOpenMPPrivateVar(Par1);
    CGM.OpenMPSupport.delOpenMPPrivateVar(Par2);
  }
}

void CodeGenFunction::EmitFinalOMPReductionClause(const OMPReductionClause &,
    const OMPExecutableDirective &S) {
  (void) S;
  assert(!isa<OMPSimdDirective>(S)); // Not yet supported
  CodeGenFunction &CGF = CGM.OpenMPSupport.getCGFForReductionFunction();
  if (CGF.CurFn) {
    CGF.FinishFunction();
    CGF.CurFn = 0;
  }
}

// Implementation of '#pragma omp simd'.
//

SourceLocation CodeGenFunction::CGPragmaOmpSimd::getForLoc() const {
  const CapturedStmt *Cap = cast<CapturedStmt>(SimdOmp->getAssociatedStmt());
  const ForStmt *For = dyn_cast<ForStmt>(Cap->getCapturedStmt());
  if (For) {
    return For->getSourceRange().getBegin();
  }
  return SimdOmp->getSourceRange().getBegin();
}

SourceRange CodeGenFunction::CGPragmaOmpSimd::getSourceRange() const {
  return SimdOmp->getSourceRange();
}

const Stmt *CodeGenFunction::CGPragmaOmpSimd::getInit() const {
  return getInitFromLoopDirective(SimdOmp);
}

const Expr *CodeGenFunction::CGPragmaOmpSimd::getCond() const {
  const CapturedStmt *Cap = dyn_cast_or_null<CapturedStmt>(getAssociatedStmt());
  if (!Cap)
    return 0;
  const ForStmt *For = dyn_cast_or_null<ForStmt>(Cap->getCapturedStmt());
  if (!For)
    return 0;
  return For->getCond();
}

const CapturedStmt *
CodeGenFunction::CGPragmaOmpSimd::getAssociatedStmt() const {
  return dyn_cast_or_null<CapturedStmt>(SimdOmp->getAssociatedStmt());
}

const Expr *CodeGenFunction::CGPragmaOmpSimd::getLoopCount() const {
  const Expr *Op = getNewIterEndFromLoopDirective(SimdOmp);
  if (const BinaryOperator *Bop = dyn_cast<BinaryOperator>(Op)) {
    // Expected "N-1" here, so why not eat "-1" to get "N".
    if (Bop->getOpcode() == BO_Sub) {
      const Expr *Op = Bop->getRHS();
      if (const ImplicitCastExpr *Cast = dyn_cast<ImplicitCastExpr>(Op)) {
        Op = Cast->getSubExpr();
      }
      if (const IntegerLiteral *One = dyn_cast<IntegerLiteral>(Op)) {
        if (One->getValue() == 1) {
          return Bop->getLHS();
        }
      }
    }
  }
  assert(0 && "Unexpected loop count expression");
  return Op;
}

Stmt *CodeGenFunction::CGPragmaOmpSimd::extractLoopBody(Stmt *S) const {
  // '#pragma omp simd' stores the full loop nest, and now we are
  // going to extract the loop body.
  unsigned CollapseNum = getCollapsedNumberFromLoopDirective(SimdOmp);
  if (CollapseNum == 0) {
    CollapseNum = 1;
  }
  Stmt *Body = S;
  while (CollapseNum > 0) {
    if (ForStmt *For = dyn_cast<ForStmt>(Body)) {
      Body = For->getBody();
      --CollapseNum;
    } else if (AttributedStmt *AS = dyn_cast<AttributedStmt>(Body)) {
      Body = AS->getSubStmt();
    } else if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Body)) {
      if (CS->size() == 1) {
        Body = CS->body_back();
      } else {
        assert(0 && "Unexpected compound stmt in the loop nest");
      }
    } else {
      assert(0 && "Unexpected stmt in the loop nest");
    }
  }
  assert(Body && "Failed to extract the loop body for 'omp simd'");
  return Body;
}

// Simd wrappers implementation for '#pragma omp simd'.
bool CodeGenFunction::CGPragmaOmpSimd::emitSafelen(CodeGenFunction *CGF) const {
  bool SeparateLastIter = false;
  CGF->LoopStack.SetParallel();
  CGF->LoopStack.SetVectorizerEnable(true);
  for (ArrayRef<OMPClause *>::iterator I = SimdOmp->clauses().begin(), E =
      SimdOmp->clauses().end(); I != E; ++I) {
    OMPClause *C = dyn_cast<OMPClause>(*I);
    switch (C->getClauseKind()) {
    case OMPC_safelen: {
      RValue Len = CGF->EmitAnyExpr(cast<OMPSafelenClause>(C)->getSafelen(),
          AggValueSlot::ignored(), true);
      llvm::ConstantInt *Val = dyn_cast<llvm::ConstantInt>(Len.getScalarVal());
      assert(Val);
      CGF->LoopStack.SetVectorizerWidth(Val->getZExtValue());
      // In presence of finite 'safelen', it may be unsafe to mark all
      // the memory instructions parallel, because loop-carried
      // dependences of 'safelen' iterations are possible.
      CGF->LoopStack.SetParallel(false);
      break;
    }
    case OMPC_lastprivate: {
      SeparateLastIter = true;
      break;
    }
    default:
      // Not handled yet
      ;
    }
  }
  return SeparateLastIter;
}

llvm::ConstantInt *
CodeGenFunction::CGPragmaOmpSimd::emitClauseTail(CodeGenFunction *CGF,
    Expr *E) const {
  // Emit a constant integer for clause's tail expression.
  // E can be an integer or NULL.
  llvm::ConstantInt *Val = 0;
  if (E != 0) {
    RValue RVal = CGF->EmitAnyExpr(E, AggValueSlot::ignored(), true);
    Val = dyn_cast<llvm::ConstantInt>(RVal.getScalarVal());
  } else {
    Val = cast<llvm::ConstantInt>(
        llvm::ConstantInt::getNullValue(CGF->CGM.IntTy));
  }
  assert(Val);
  return Val;
}

// Walker for '#pragma omp simd'
bool CodeGenFunction::CGPragmaOmpSimd::walkLocalVariablesToEmit(
    CodeGenFunction *CGF, CGSIMDForStmtInfo *) const {

  // Init the OpenMP local vars stack.
  CGF->CGM.OpenMPSupport.startOpenMPRegion(true);
  CGF->CGM.OpenMPSupport.setMergeable(false);
  CGF->CGM.OpenMPSupport.setOrdered(false);

  // Make sure we have local vars for all the loop counters.
  ArrayRef<Expr *> Counters = getCountersFromLoopDirective(SimdOmp);
  for (unsigned I = 0; I < getCollapsedNumberFromLoopDirective(SimdOmp); ++I) {
    const VarDecl *VD = cast<VarDecl>(
        cast<DeclRefExpr>(Counters[I])->getDecl());
    if (CGF->CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD))
      continue;
    QualType QTy = Counters[I]->getType();
    llvm::AllocaInst *Private = CGF->CreateMemTemp(QTy,
        CGF->CGM.getMangledName(VD) + ".counter.");
    CGF->CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
  }

  // Here we push index parameter into openmp map.
  // It is useful for loop counters calculation.
  const CapturedDecl *CD =
      cast<CapturedStmt>(getAssociatedStmt())->getCapturedDecl();
  llvm::Value *LoopIndex = CGF->LocalDeclMap.lookup(CD->getParam(1));
  const VarDecl *IndexVD = cast<VarDecl>(
      cast<DeclRefExpr>(getNewIterVarFromLoopDirective(SimdOmp))->getDecl());
  CGF->CGM.OpenMPSupport.addOpenMPPrivateVar(IndexVD, LoopIndex);

  for (ArrayRef<OMPClause *>::iterator I = SimdOmp->clauses().begin(), E =
      SimdOmp->clauses().end(); I != E; ++I) {
    OMPClause *C = dyn_cast<OMPClause>(*I);
    switch (C->getClauseKind()) {
    case OMPC_private: {
      CGF->EmitPreOMPClause(*(*I), *SimdOmp);
      break;
    }
    case OMPC_lastprivate: {
      CGF->EmitPreOMPClause(*(*I), *SimdOmp);
      break;
    }
    case OMPC_linear: {
      // Linear vars are calculated from index, similar to loop indices.
      OMPLinearClause *L = cast<OMPLinearClause>(C);
      for (OMPLinearClause::varlist_const_iterator J = L->varlist_begin(), F =
          L->varlist_end(); J != F; ++J) {
        const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*J)->getDecl());
        if (CGF->CGM.OpenMPSupport.getTopOpenMPPrivateVar(VD)) {
          continue;
        }
        QualType QTy = (*J)->getType();
        llvm::Value *Private = CGF->CreateMemTemp(QTy,
            CGF->CGM.getMangledName(VD) + ".linear.");

        // Generate "Private = Index * Step + Start"
        llvm::Value *Start = CGF->EmitAnyExprToTemp(*J).getScalarVal();
        llvm::Value *Index = CGF->Builder.CreateLoad(LoopIndex);
        llvm::Value *Result = 0;
        if (const Expr *StepExpr = L->getStep()) {
          Result = CGF->EmitAnyExpr(StepExpr).getScalarVal();
          QualType IndexTy = CD->getParam(1)->getType();
          Result = CGF->Builder.CreateIntCast(Result, Index->getType(),
              IndexTy->hasSignedIntegerRepresentation());
        } else
          Result = llvm::ConstantInt::get(Index->getType(), 1);
        Result = CGF->Builder.CreateMul(Index, Result);
        if (Start->getType()->isPointerTy()) {
          Result = CGF->Builder.CreateGEP(Start, Result);
        } else {
          Result = CGF->Builder.CreateIntCast(Result, Start->getType(), false);
          Result = CGF->Builder.CreateAdd(Start, Result, "add", false,
              QTy->isSignedIntegerOrEnumerationType());
        }
        CGF->Builder.CreateStore(Result, Private);

        CGF->CGM.OpenMPSupport.addOpenMPPrivateVar(VD, Private);
      }
      break;
    }
    default:
      break;
    }
  }

  // Mark 'aligned' variables -- do this after all private variables are
  // made 'omp-private' in CGM.OpenMPSupport.
  for (ArrayRef<OMPClause *>::iterator I = SimdOmp->clauses().begin(), E =
      SimdOmp->clauses().end(); I != E; ++I) {
    OMPClause *C = dyn_cast<OMPClause>(*I);
    switch (C->getClauseKind()) {
    case OMPC_aligned: {
      OMPAlignedClause *A = cast<OMPAlignedClause>(C);
      // Prepare alignment expression for using it below.
      llvm::ConstantInt *AVal = emitClauseTail(CGF, A->getAlignment());
      // Walk the list and push each var's alignment into metadata.
      for (OMPAlignedClause::varlist_iterator J = A->varlist_begin(), F =
          A->varlist_end(); J != F; ++J) {
        LValue LVal = CGF->EmitLValue(*J);
        CGF->LoopStack.AddAligned(LVal.getAddress(),
            (int) (AVal->getZExtValue()));
      }
      break;
    }
    default:
      break;
    }
  }

  // Emit initializations of loop indices.
  CGF->EmitStmt(getInitFromLoopDirective(SimdOmp));
  return false;
}

void CodeGenFunction::CGPragmaOmpSimd::emitInit(CodeGenFunction &CGF,
    llvm::Value *&LoopIndex, llvm::Value *&LoopCount) {
  // Emit loop index
  const Expr *IterVar = getNewIterVarFromLoopDirective(SimdOmp);
  LoopIndex = CGF.CreateMemTemp(IterVar->getType(), ".idx.");
  const VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(IterVar)->getDecl());
  CGF.CGM.OpenMPSupport.addOpenMPPrivateVar(VD, LoopIndex);

  // Emit loop count.
  LoopCount = CGF.EmitAnyExpr(getLoopCount()).getScalarVal();
}

// Emit the final values of the loop counters and linear vars.
void CodeGenFunction::CGPragmaOmpSimd::emitLinearFinal(
    CodeGenFunction &CGF) const {

  // Check if we need to update the loop counters.
  bool NeedUpdateLC = true;
  ArrayRef<Expr *> Counters = getCountersFromLoopDirective(SimdOmp);
  for (unsigned I = 0; I < getCollapsedNumberFromLoopDirective(SimdOmp); ++I) {
    const DeclRefExpr *DRE = cast<DeclRefExpr>(Counters[I]);
    if (!CGF.LocalDeclMap.lookup(DRE->getDecl())) {
      NeedUpdateLC = false;
    }
  }

  // Emit final values of the loop-counters.
  if (NeedUpdateLC)
    CGF.EmitStmt(getFinalFromLoopDirective(SimdOmp));

  // Emit final values of the linear vars.
  for (ArrayRef<OMPClause *>::iterator I = SimdOmp->clauses().begin(), E =
      SimdOmp->clauses().end(); I != E; ++I) {
    OMPClause *C = dyn_cast<OMPClause>(*I);
    switch (C->getClauseKind()) {
    case OMPC_linear: {
      OMPLinearClause *L = cast<OMPLinearClause>(C);
      for (OMPLinearClause::varlist_const_iterator J = L->varlist_begin(), F =
          L->varlist_end(); J != F; ++J) {

        // Generate "L = LoopCount * Step + L"
        const Expr *CountExpr = getLoopCount();
        llvm::Value *Index = CGF.EmitAnyExpr(CountExpr).getScalarVal();
        llvm::Value *Result = 0;
        if (const Expr *StepExpr = L->getStep()) {
          Result = CGF.EmitAnyExpr(StepExpr).getScalarVal();
          QualType IndexTy = CountExpr->getType();
          Result = CGF.Builder.CreateIntCast(Result, Index->getType(),
              IndexTy->hasSignedIntegerRepresentation());
        } else
          Result = llvm::ConstantInt::get(Index->getType(), 1);
        Result = CGF.Builder.CreateMul(Index, Result);

        // Prepare destination lvalue to store result into.
        LValue LV = CGF.EmitLValue(*J);
        llvm::Value *Start =
            CGF.EmitLoadOfLValue(LV, (*J)->getExprLoc()).getScalarVal();

        if (Start->getType()->isPointerTy()) {
          Result = CGF.Builder.CreateGEP(Start, Result);
        } else {
          Result = CGF.Builder.CreateIntCast(Result, Start->getType(), false);
          Result = CGF.Builder.CreateAdd(Start, Result, "add", false,
              (*J)->getType()->isSignedIntegerOrEnumerationType());
        }
        CGF.EmitStoreOfScalar(Result, LV, false);
      }
      break;
    }
    default:
      break;
    }
  }
}

/// Generate an instructions for '#pragma omp teams' directive.
void CodeGenFunction::EmitOMPTeamsDirective(const OMPTeamsDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTeams(OMPD_teams, OMPD_unknown, S);
}

// Generate the instructions for '#pragma omp simd' directive.
void CodeGenFunction::EmitOMPSimdDirective(const OMPSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  CGPragmaOmpSimd Wrapper(&S);
  EmitPragmaSimd(Wrapper);
}

// Generate the instructions for '#pragma omp for simd' directive.
void CodeGenFunction::EmitOMPForSimdDirective(const OMPForSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithLoop(OMPD_for_simd, OMPD_for_simd, S);
}

// Generate the instructions for '#pragma omp distribute simd' directive.
void CodeGenFunction::EmitOMPDistributeSimdDirective(
    const OMPDistributeSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithLoop(OMPD_distribute_simd, OMPD_distribute_simd, S);
}

// Generate the instructions for '#pragma omp distribute parallel for'
// directive.
void CodeGenFunction::EmitOMPDistributeParallelForDirective(
    const OMPDistributeParallelForDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  assert(S.getLowerBound() && "No lower bound");
  assert(S.getUpperBound() && "No upper bound");
  EmitAutoVarDecl(
      *cast<VarDecl>(cast<DeclRefExpr>(S.getLowerBound())->getDecl()));
  EmitAutoVarDecl(
      *cast<VarDecl>(cast<DeclRefExpr>(S.getUpperBound())->getDecl()));
  EmitOMPDirectiveWithLoop(OMPD_distribute_parallel_for, OMPD_distribute, S);
}

// Generate the instructions for '#pragma omp distribute parallel for simd'
// directive.
void CodeGenFunction::EmitOMPDistributeParallelForSimdDirective(
    const OMPDistributeParallelForSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  assert(S.getLowerBound() && "No lower bound");
  assert(S.getUpperBound() && "No upper bound");
  EmitAutoVarDecl(
      *cast<VarDecl>(cast<DeclRefExpr>(S.getLowerBound())->getDecl()));
  EmitAutoVarDecl(
      *cast<VarDecl>(cast<DeclRefExpr>(S.getUpperBound())->getDecl()));
  EmitOMPDirectiveWithLoop(OMPD_distribute_parallel_for_simd, OMPD_distribute,
      S);
}

// Generate the instructions for '#pragma omp teams distribute parallel for'
// directive.
void CodeGenFunction::EmitOMPTeamsDistributeParallelForDirective(
    const OMPTeamsDistributeParallelForDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTeams(OMPD_teams_distribute_parallel_for,
      OMPD_distribute_parallel_for, S);
}

// Generate the instructions for '#pragma omp teams distribute parallel for simd'
// directive.
void CodeGenFunction::EmitOMPTeamsDistributeParallelForSimdDirective(
    const OMPTeamsDistributeParallelForSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTeams(OMPD_teams_distribute_parallel_for_simd,
      OMPD_distribute_parallel_for_simd, S);
}

// Generate the instructions for '#pragma omp target teams distribute parallel
// for' directive.
void CodeGenFunction::EmitOMPTargetTeamsDistributeParallelForDirective(
    const OMPTargetTeamsDistributeParallelForDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target_teams_distribute_parallel_for,
      OMPD_teams_distribute_parallel_for, S);
}

// Generate the instructions for '#pragma omp target teams distribute parallel
// for simd' directive.
void CodeGenFunction::EmitOMPTargetTeamsDistributeParallelForSimdDirective(
    const OMPTargetTeamsDistributeParallelForSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target_teams_distribute_parallel_for_simd,
      OMPD_teams_distribute_parallel_for_simd, S);
}

// Generate the instructions for '#pragma omp target' directive.
void CodeGenFunction::EmitOMPTargetDirective(const OMPTargetDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target, OMPD_unknown, S);
}

// Generate the instructions for '#pragma omp target data' directive.
void CodeGenFunction::EmitOMPTargetDataDirective(
    const OMPTargetDataDirective &S) {

  CGM.OpenMPSupport.startOpenMPRegion(false);

  // Codegen target clauses init, this currently include
  // - device clause
  // - map clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);

  // Codegen target clauses after init, this currently include
  // - if clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);

  // Codegen target clauses pre, this currently include
  // - map clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitPreOMPClause(*(*I), S);

  // FIXME: Currently we are replicating all the code captured by the directive
  // in case we have an if-clause. It would be enough to guard the map runtime
  // calls with the if-clause conditional.

  CapturedStmt *CS = cast<CapturedStmt>(S.getAssociatedStmt());
  EmitStmt(CS->getCapturedStmt());

  // Codegen target clauses post, this currently include
  // - map clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitPostOMPClause(*(*I), S);

  // Codegen target clauses final, this currently include
  // - if clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitFinalOMPClause(*(*I), S);

  CGM.OpenMPSupport.endOpenMPRegion();
}

// Generate the instructions for '#pragma omp target update' directive.
void CodeGenFunction::EmitOMPTargetUpdateDirective(
    const OMPTargetUpdateDirective &S) {

  // We create a new region for this so we can reuse whatever is in the stack for
  // the map clause.
  CGM.OpenMPSupport.startOpenMPRegion(false);

  // Codegen target clauses init, this currently include
  // - device clause
  // - to/from clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitInitOMPClause(*(*I), S);

  // Codegen target clauses after init, this currently include
  // - if clause
  for (ArrayRef<OMPClause *>::iterator I = S.clauses().begin(), E =
      S.clauses().end(); I != E; ++I)
    if (*I
        && isAllowedClauseForDirective(S.getDirectiveKind(),
            (*I)->getClauseKind()))
      EmitAfterInitOMPClause(*(*I), S);


  // The information is stored to the OpenMP stack during the init phase
  // of the map/to/form clause codegen
  // - Expressions in the map
  // - Base pointers (addresses)
  // - Pointers (addresses)
  // - Sizes
  // - Types (to, from, to/from)

  ArrayRef<const Expr*>  MapClauseDecls;
  ArrayRef<llvm::Value*> MapClauseBasePointersArray;
  ArrayRef<llvm::Value*> MapClausePointersArray;
  ArrayRef<llvm::Value*> MapClauseSizesArray;
  ArrayRef<unsigned> MapClauseTypesArray;

  CGM.OpenMPSupport.getOffloadingMapArrays(MapClauseDecls,
      MapClauseBasePointersArray, MapClausePointersArray, MapClauseSizesArray,
      MapClauseTypesArray);

  // remove pointers and extra flag of items associated to them
  // (no pointer to target update call)


  assert(
      MapClauseBasePointersArray.size() == MapClauseDecls.size()
          && MapClauseBasePointersArray.size() == MapClausePointersArray.size()
          && MapClauseBasePointersArray.size() == MapClauseSizesArray.size()
          && MapClauseBasePointersArray.size() == MapClauseTypesArray.size()
          && "Array size mismatch!");

  if (MapClauseBasePointersArray.empty())
    return;

  // exclude pointers from number of elements in map
  // (no pointer to target update call)
  int NumMapClausesAfterPeeling = MapClauseBasePointersArray.size();
  for (unsigned i = 0; i < MapClauseBasePointersArray.size(); ++i)
    if(MapClauseTypesArray[i] & OMP_TGT_MAPTYPE_POINTER)
      NumMapClausesAfterPeeling--;

  llvm::Value *MapClauseNumElems = Builder.getInt32(
      NumMapClausesAfterPeeling);

  // If we have pointers, lets create an array in the stack
  llvm::Value *MapClauseBasePointers = Builder.CreateAlloca(CGM.VoidPtrTy,
      MapClauseNumElems, ".update_base_ptrs");
  llvm::Value *MapClausePointers = Builder.CreateAlloca(CGM.VoidPtrTy,
      MapClauseNumElems, ".update_ptrs");
  llvm::Value *MapClauseSizes = Builder.CreateAlloca(CGM.Int64Ty,
      MapClauseNumElems, ".update_sizes");

  // target update does not accept pointers. Exclude map types pointers
  // and zero the flag bits for extra which refers to them
  int StoreIndex = 0;
  bool PrevIsPtr = false;
  llvm::SmallVector<unsigned,16> MapClauseTypesArrayNoPtrsNoExtraVec;
  for (unsigned i = 0; i < MapClauseBasePointersArray.size(); ++i) {
    // exclude pointer types
    if(MapClauseTypesArray[i] & OMP_TGT_MAPTYPE_POINTER) {
      PrevIsPtr = true;
      continue;
    }

    if(PrevIsPtr) {
      MapClauseTypesArrayNoPtrsNoExtraVec.push_back(MapClauseTypesArray[i] &
          ~OMP_TGT_MAPTYPE_EXTRA);
      PrevIsPtr = false;
    } else
    	MapClauseTypesArrayNoPtrsNoExtraVec.push_back(MapClauseTypesArray[i]);


    llvm::Value *BP = Builder.CreateConstInBoundsGEP1_32(MapClauseBasePointers,
        StoreIndex);
    llvm::Value *P = Builder.CreateConstInBoundsGEP1_32(MapClausePointers,
        StoreIndex);
    llvm::Value *S = Builder.CreateConstInBoundsGEP1_32(MapClauseSizes,
        StoreIndex);

    Builder.CreateStore(Builder.CreateBitCast(MapClauseBasePointersArray[i],
                                              CGM.VoidPtrTy),
                        BP);
    Builder.CreateStore(Builder.CreateBitCast(MapClausePointersArray[i],
                                              CGM.VoidPtrTy),
                        P);
    Builder.CreateStore(Builder.CreateIntCast(MapClauseSizesArray[i],
                                              CGM.Int64Ty,true),
                        S);

    StoreIndex++;
  }

  llvm::Constant *MapClauseTypesInit = llvm::ConstantDataArray::get(
      Builder.getContext(), MapClauseTypesArrayNoPtrsNoExtraVec);
  llvm::GlobalVariable *MapClauseTypesTmp = new llvm::GlobalVariable(
      CGM.getModule(), MapClauseTypesInit->getType(), true,
      llvm::GlobalValue::PrivateLinkage, MapClauseTypesInit, ".update_types");

  llvm::Value *MapClauseTypes = Builder.CreateConstInBoundsGEP2_32(
      MapClauseTypesTmp, 0, 0);


  // Get or create value with the deviceID
  llvm::Value *DeviceID =
      (CGM.OpenMPSupport.getOffloadingDevice()) ?
          CGM.OpenMPSupport.getOffloadingDevice() :
          (llvm::Value*) Builder.getInt32(
              CGOpenMPRuntime::OMPRTL__target_device_id_undef);

  llvm::Value *Args[] = { DeviceID, MapClauseNumElems, MapClauseBasePointers,
      MapClausePointers, MapClauseSizes, MapClauseTypes };
  EmitRuntimeCall(OPENMPRTL_FUNC(target_data_update), Args);

  CGM.OpenMPSupport.endOpenMPRegion();
}

// Generate the instructions for '#pragma omp target teams' directive.
void CodeGenFunction::EmitOMPTargetTeamsDirective(
    const OMPTargetTeamsDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target_teams, OMPD_teams, S);
}

/// Generate an instructions for '#pragma omp teams distribute' directive.
void CodeGenFunction::EmitOMPTeamsDistributeDirective(
    const OMPTeamsDistributeDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTeams(OMPD_teams_distribute, OMPD_distribute, S);
}

/// Generate an instructions for '#pragma omp teams distribute simd' directive.
void CodeGenFunction::EmitOMPTeamsDistributeSimdDirective(
    const OMPTeamsDistributeSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTeams(OMPD_teams_distribute_simd, OMPD_distribute_simd,
      S);
}

/// Generate an instructions for '#pragma omp target teams distribute'
/// directive.
void CodeGenFunction::EmitOMPTargetTeamsDistributeDirective(
    const OMPTargetTeamsDistributeDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target_teams_distribute,
      OMPD_teams_distribute, S);
}

/// Generate an instructions for '#pragma omp target teams distribute simd'
/// directive.
void CodeGenFunction::EmitOMPTargetTeamsDistributeSimdDirective(
    const OMPTargetTeamsDistributeSimdDirective &S) {
  RunCleanupsScope ExecutedScope(*this);
  EmitOMPDirectiveWithTarget(OMPD_target_teams_distribute_simd,
      OMPD_teams_distribute_simd, S);
}

