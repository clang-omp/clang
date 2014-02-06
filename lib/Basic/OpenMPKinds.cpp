//===--- OpenMPKinds.cpp - Token Kinds Support ----------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file implements the OpenMP enum and support functions.
///
//===----------------------------------------------------------------------===//

#include "clang/Basic/OpenMPKinds.h"
#include "clang/Basic/IdentifierTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include <cassert>

using namespace clang;

OpenMPDirectiveKind clang::getOpenMPDirectiveKind(StringRef Str) {
  return llvm::StringSwitch<OpenMPDirectiveKind>(Str)
#define OPENMP_DIRECTIVE(Name) \
           .Case(#Name, OMPD_##Name)
#define OPENMP_DIRECTIVE_EXT(Name, Str) \
           .Case(Str, OMPD_##Name)
#include "clang/Basic/OpenMPKinds.def"
           .Default(OMPD_unknown);
}

const char *clang::getOpenMPDirectiveName(OpenMPDirectiveKind Kind) {
  assert(Kind < NUM_OPENMP_DIRECTIVES);
  switch (Kind) {
  case OMPD_unknown:
    return "unknown";
#define OPENMP_DIRECTIVE(Name) \
  case OMPD_##Name : return #Name;
#define OPENMP_DIRECTIVE_EXT(Name, Str) \
  case OMPD_##Name : return Str;
#include "clang/Basic/OpenMPKinds.def"
  case NUM_OPENMP_DIRECTIVES:
    break;
  }
  llvm_unreachable("Invalid OpenMP directive kind");
}

OpenMPClauseKind clang::getOpenMPClauseKind(StringRef Str) {
  return llvm::StringSwitch<OpenMPClauseKind>(Str)
#define OPENMP_CLAUSE(Name, Class) \
           .Case(#Name, OMPC_##Name)
#include "clang/Basic/OpenMPKinds.def"
           .Default(OMPC_unknown);
}

const char *clang::getOpenMPClauseName(OpenMPClauseKind Kind) {
  assert(Kind < NUM_OPENMP_CLAUSES);
  switch (Kind) {
  case OMPC_unknown:
    return "unknown";
#define OPENMP_CLAUSE(Name, Class) \
  case OMPC_##Name : return #Name;
#include "clang/Basic/OpenMPKinds.def"
  case OMPC_threadprivate:
    return "threadprivate or thread local";
  case NUM_OPENMP_CLAUSES:
    break;
  }
  llvm_unreachable("Invalid OpenMP clause kind");
}

unsigned clang::getOpenMPSimpleClauseType(OpenMPClauseKind Kind,
                                          StringRef Str) {
  switch (Kind) {
  case OMPC_default:
    return llvm::StringSwitch<OpenMPDefaultClauseKind>(Str)
#define OPENMP_DEFAULT_KIND(Name) \
             .Case(#Name, OMPC_DEFAULT_##Name)
#include "clang/Basic/OpenMPKinds.def"
             .Default(OMPC_DEFAULT_unknown);
  case OMPC_proc_bind:
    return llvm::StringSwitch<OpenMPProcBindClauseKind>(Str)
#define OPENMP_PROC_BIND_KIND(Name) \
             .Case(#Name, OMPC_PROC_BIND_##Name)
#include "clang/Basic/OpenMPKinds.def"
             .Default(OMPC_PROC_BIND_unknown);
  case OMPC_reduction:
    return llvm::StringSwitch<OpenMPReductionClauseOperator>(Str)
#define OPENMP_REDUCTION_OPERATOR(Name, Symbol) \
             .Case(Symbol, OMPC_REDUCTION_##Name)
#include "clang/Basic/OpenMPKinds.def"
             .Default(OMPC_REDUCTION_unknown);
  case OMPC_schedule:
    return llvm::StringSwitch<OpenMPScheduleClauseKind>(Str)
#define OPENMP_SCHEDULE_KIND(Name) \
             .Case(#Name, OMPC_SCHEDULE_##Name)
#include "clang/Basic/OpenMPKinds.def"
             .Default(OMPC_SCHEDULE_unknown);
  default:
    break;
  }
  llvm_unreachable("Invalid OpenMP simple clause kind");
}

const char *clang::getOpenMPSimpleClauseTypeName(OpenMPClauseKind Kind,
                                                 unsigned Type) {
  switch (Kind) {
  case OMPC_default:
    switch (Type) {
    case OMPC_DEFAULT_unknown:
      return "unknown";
#define OPENMP_DEFAULT_KIND(Name) \
    case OMPC_DEFAULT_##Name : return #Name;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    llvm_unreachable("Invalid OpenMP 'default' clause type");
  case OMPC_proc_bind:
    switch (Type) {
    case OMPC_PROC_BIND_unknown:
      return "unknown";
#define OPENMP_PROC_BIND_KIND(Name) \
    case OMPC_PROC_BIND_##Name : return #Name;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    llvm_unreachable("Invalid OpenMP 'proc_bind' clause type");
  case OMPC_reduction:
    switch (Type) {
    case OMPC_REDUCTION_unknown:
      return "unknown";
#define OPENMP_REDUCTION_OPERATOR(Name, Symbol) \
    case OMPC_REDUCTION_##Name : return Symbol;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    llvm_unreachable("Invalid OpenMP 'reduction' clause operator");
  case OMPC_schedule:
  case OMPC_dist_schedule:
    switch (Type) {
    case OMPC_SCHEDULE_unknown:
      return "unknown";
#define OPENMP_SCHEDULE_KIND(Name) \
    case OMPC_SCHEDULE_##Name : return #Name;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    llvm_unreachable("Invalid OpenMP 'schedule' clause operator");
  default:
    break;
  }
  llvm_unreachable("Invalid OpenMP simple clause kind");
}

bool clang::isAllowedClauseForDirective(OpenMPDirectiveKind DKind,
                                        OpenMPClauseKind CKind) {
  assert(DKind < NUM_OPENMP_DIRECTIVES);
  assert(CKind < NUM_OPENMP_CLAUSES);
  switch (DKind) {
  case OMPD_parallel:
    switch (CKind) {
#define OPENMP_PARALLEL_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_for:
    switch (CKind) {
#define OPENMP_FOR_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_simd:
    switch (CKind) {
#define OPENMP_SIMD_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_for_simd:
    switch (CKind) {
#define OPENMP_FOR_SIMD_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;

  case OMPD_sections:
    switch (CKind) {
#define OPENMP_SECTIONS_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_single:
    switch (CKind) {
#define OPENMP_SINGLE_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_task:
    switch (CKind) {
#define OPENMP_TASK_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_atomic:
    switch (CKind) {
#define OPENMP_ATOMIC_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_flush:
    switch (CKind) {
#define OPENMP_FLUSH_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_parallel_for:
    switch (CKind) {
#define OPENMP_PARALLEL_FOR_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  case OMPD_parallel_sections:
    switch (CKind) {
#define OPENMP_PARALLEL_SECTIONS_CLAUSE(Name) \
    case OMPC_##Name: return true;
#include "clang/Basic/OpenMPKinds.def"
    default:
      break;
    }
    break;
  default:
    break;
  }
  return false;
}
