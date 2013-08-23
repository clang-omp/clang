//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file implements semantic analysis for OpenMP directives and
/// clauses.
///
//===----------------------------------------------------------------------===//

#include "clang/Basic/OpenMPKinds.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Initialization.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/ScopeInfo.h"
using namespace clang;

//===----------------------------------------------------------------------===//
// Stack of data-sharing attributes for variables
//===----------------------------------------------------------------------===//

namespace {
/// \brief Default data sharing attributes, which can be applied to directive.
enum DefaultDataSharingAttributes {
  DSA_unspecified = 0,   /// \brief Data sharing attribute not specified.
  DSA_none = 1 << 0,     /// \brief Default data sharing attribute 'none'.
  DSA_shared = 1 << 1    /// \brief Default data sharing attribute 'shared'.
};

/// \brief Stack for tracking declarations used in OpenMP directives and
/// clauses and their data-sharing attributes.
class DSAStackTy {
  struct DSAInfo {
    OpenMPClauseKind Attributes;
    DeclRefExpr *RefExpr;
  };
  typedef llvm::SmallDenseMap<VarDecl *, DSAInfo, 64> DeclSAMapTy;

  struct SharingMapTy {
    DeclSAMapTy SharingMap;
    DefaultDataSharingAttributes DefaultAttr;
    OpenMPDirectiveKind Directive;
    DeclarationNameInfo DirectiveName;
    bool IsOrdered;
    Scope *CurScope;
    SharingMapTy(OpenMPDirectiveKind DKind,
                 const DeclarationNameInfo &Name,
                 Scope *CurScope)
      : SharingMap(), DefaultAttr(DSA_unspecified), Directive(DKind),
        DirectiveName(Name), IsOrdered(false), CurScope(CurScope) { }
    SharingMapTy()
      : SharingMap(), DefaultAttr(DSA_unspecified),
        Directive(OMPD_unknown), DirectiveName(), IsOrdered(false),
        CurScope(0) { }
  };

  typedef SmallVector<SharingMapTy, 64> StackTy;

  /// \brief Stack of used declaration and their data-sharing attributes.
  StackTy Stack;
  Sema &Actions;

  typedef SmallVector<SharingMapTy, 8>::reverse_iterator reverse_iterator;

OpenMPClauseKind getDSA(StackTy::reverse_iterator Iter,
                        VarDecl *D,
                        OpenMPDirectiveKind &Kind,
                        DeclRefExpr *&E);
public:
  DSAStackTy(Sema &S) : Stack(1), Actions(S) { }

  void push(OpenMPDirectiveKind DKind, const DeclarationNameInfo &DirName, Scope *CurScope) {
    Stack.push_back(SharingMapTy(DKind, DirName, CurScope));
  }

  void pop() {
    assert(Stack.size() > 1 && "Stack is empty!");
    Stack.pop_back();
  }

  /// \brief Adds explicit data sharing attribute to the specified declaration.
  void addDSA(VarDecl *D, DeclRefExpr *E, OpenMPClauseKind A);

  /// \brief Adds explicit data sharing attribute to the specified declaration
  /// to parent scope.
  void addParentDSA(VarDecl *D, DeclRefExpr *E, OpenMPClauseKind A);

  /// \brief Checks if the variable is a local for OpenMP region.
  bool isOpenMPLocal(VarDecl *D);

  /// \brief Returns data sharing attributes from top of the stack for the
  /// specified declaration.
  OpenMPClauseKind getTopDSA(VarDecl *D, DeclRefExpr *&E);
  /// \brief Returns data-sharing attributes for the specified declaration.
  OpenMPClauseKind getImplicitDSA(VarDecl *D, OpenMPDirectiveKind &Kind,
                                  DeclRefExpr *&E);

  /// \brief Checks if the specified variables has \a CKind data-sharing
  /// attribute in \a DKind directive.
  bool hasDSA(VarDecl *D, OpenMPClauseKind CKind,
              OpenMPDirectiveKind DKind, DeclRefExpr *&E);

  /// \brief Checks if the specified variables has \a CKind data-sharing
  /// attribute in an innermost \a DKind directive.
  bool hasInnermostDSA(VarDecl *D, OpenMPClauseKind CKind,
                       OpenMPDirectiveKind DKind, DeclRefExpr *&E);

  /// \brief Returns currently analized directive.
  OpenMPDirectiveKind getCurrentDirective() const {
    return Stack.back().Directive;
  }

  /// \brief Returns parent directive.
  OpenMPDirectiveKind getParentDirective() const {
    if (Stack.size() > 2)
      return Stack[Stack.size() - 2].Directive;
    return OMPD_unknown;
  }

  /// \brief Returns true if parent region is an ordered parallel or
  /// worksharing region.
  bool isParentOrdered() const {
    if (Stack.size() > 2)
      return Stack[Stack.size() - 2].IsOrdered;
    return false;
  }

  /// \brief Marks current regions as ordered.
  void setOrdered() {
    Stack.back().IsOrdered = true;
  }

  /// \brief Marks current regions as ordered.
  void clearParentOrdered() {
    Stack[Stack.size() - 2].IsOrdered = false;
  }

  /// \brief Checks if the specified kind of directive with the given name
  /// already exists.
  bool hasDirectiveWithName(OpenMPDirectiveKind Kind,
                            DeclarationNameInfo DirName);

  /// \brief Set default data sharing attribute to none.
  void setDefaultDSANone() { Stack.back().DefaultAttr = DSA_none; }
  /// \brief Set default data sharing attribute to shared.
  void setDefaultDSAShared() { Stack.back().DefaultAttr = DSA_shared; }
  DefaultDataSharingAttributes getDefaultDSA() { return Stack.back().DefaultAttr; }

  Scope *getCurScope() { return Stack.back().CurScope; }
};
} // end anonymous namespace.

OpenMPClauseKind DSAStackTy::getDSA(StackTy::reverse_iterator Iter,
                                    VarDecl *D,
                                    OpenMPDirectiveKind &Kind,
                                    DeclRefExpr *&E) {
  E = 0;
  if (Iter == Stack.rend() - 1) {
    Kind = OMPD_unknown;
    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a region but not in construct]
    //  File-scope or namespace-scope variables referenced in called routines
    //  in the region are shared unless they appear in a threadprivate
    //  directive.
    if (!D->isFunctionOrMethodVarDecl())
      return OMPC_shared;

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a region but not in construct]
    //  Other variables declared in called routines in the region are private.
    if (!D->isFunctionOrMethodVarDecl())
      return OMPC_private;

    return OMPC_unknown;
  }
  Kind = Iter->Directive;
  // Explicitly specified attributes and local variables with predetermined
  // attributes.
  if (Iter->SharingMap.count(D)) {
    E = Iter->SharingMap[D].RefExpr;
    return Iter->SharingMap[D].Attributes;
  }

  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, implicitly determined, p.1]
  //  In a parallel or task construct, the data-sharing attributes of these
  //  variables are determined by the default clause, if present.
  switch (Iter->DefaultAttr) {
  case DSA_shared:
    return OMPC_shared;
  case DSA_none:
    return OMPC_unknown;
  case DSA_unspecified:
    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct, implicitly determined, p.2]
    //  In a parallel construct, if no default clause is present, these
    //  variables are shared.
    if (Kind == OMPD_parallel)
      return OMPC_shared;

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct, implicitly determined, p.4]
    //  In a task construct, if no default clause is present, a variable that in
    //  the enclosing context is determined to be shared by all implicit tasks
    //  bound to the current team is shared.
    if (Kind == OMPD_task) {
      OpenMPClauseKind CKind = OMPC_unknown;
      for (StackTy::reverse_iterator I = Iter + 1,
                                     EE = Stack.rend() - 1;
           I != EE; ++I) {
        // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
        // in a Construct, implicitly determined, p.6]
        //  In a task construct, if no default clause is present, a variable
        //  whose data-sharing attribute is not determined by the rules above is
        //  firstprivate.
        CKind = getDSA(I, D, Kind, E);
        if (CKind != OMPC_shared) {
          E = 0;
          Kind = OMPD_task;
          return OMPC_firstprivate;
        }
        if (I->Directive == OMPD_parallel) break;
      }
      Kind = OMPD_task;
      return (CKind == OMPC_unknown) ? OMPC_firstprivate : OMPC_shared;
    }
  }
  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, implicitly determined, p.3]
  //  For constructs other than task, if no default clause is present, these
  //  variables inherit their data-sharing attributes from the enclosing
  //  context.
  return getDSA(Iter + 1, D, Kind, E);
}

void DSAStackTy::addDSA(VarDecl *D, DeclRefExpr *E, OpenMPClauseKind A) {
  if (A == OMPC_threadprivate) {
    Stack[0].SharingMap[D].Attributes = A;
    Stack[0].SharingMap[D].RefExpr = E;
  } else {
    assert(Stack.size() > 1 && "Data sharing attributes stack is empty");
    Stack.back().SharingMap[D].Attributes = A;
    Stack.back().SharingMap[D].RefExpr = E;
  }
}

void DSAStackTy::addParentDSA(VarDecl *D, DeclRefExpr *E, OpenMPClauseKind A) {
  assert(Stack.size() > 2 && "Data sharing attributes stack does not have parent");
  Stack[Stack.size() - 2].SharingMap[D].Attributes = A;
  Stack[Stack.size() - 2].SharingMap[D].RefExpr = E;
}

bool DSAStackTy::isOpenMPLocal(VarDecl *D) {
  Scope *CurScope = getCurScope();
  while (CurScope && !CurScope->isDeclScope(D)) {
    CurScope = CurScope->getParent();
  }
  while (CurScope && !CurScope->isOpenMPDirectiveScope()) {
    CurScope = CurScope->getParent();
  }
  bool isOpenMPLocal = !!CurScope;
  if (!isOpenMPLocal) {
    Scope *CurScope = getCurScope();
    while (CurScope && !CurScope->isOpenMPDirectiveScope()) {
      CurScope = CurScope->getParent();
    }
    isOpenMPLocal = isa<CapturedDecl>(D->getDeclContext()) &&
                    CurScope &&
                    static_cast<DeclContext *>(CurScope->getFnParent()->getEntity())->Encloses(D->getDeclContext());
  }
  return isOpenMPLocal;
}

OpenMPClauseKind DSAStackTy::getTopDSA(VarDecl *D, DeclRefExpr *&E) {
  E = 0;

  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, predetermined, p.1]
  //  Variables appearing in threadprivate directives are threadprivate.
  if (D->getTLSKind() != VarDecl::TLS_None)
    return OMPC_threadprivate;
  if (Stack[0].SharingMap.count(D)) {
    E = Stack[0].SharingMap[D].RefExpr;
    return OMPC_threadprivate;
  }

  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, predetermined, p.1]
  // Variables with automatic storage duration that are declared in a scope
  // inside the construct are private.
  if (isOpenMPLocal(D) && D->isLocalVarDecl() &&
      (D->getStorageClass() == SC_Auto ||
       D->getStorageClass() == SC_None))
    return OMPC_private;

  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, predetermined, p.4]
  //  Static data memebers are shared.
  if (D->isStaticDataMember()) {
    DeclRefExpr *E;
    // Variables with const-qualified type having no mutable member may be listed
    // in a firstprivate clause, even if they are static data members.
    if (hasDSA(D, OMPC_firstprivate, OMPD_unknown, E) && E)
      return OMPC_unknown;
    return OMPC_shared;
  }

  QualType Type = D->getType().getNonReferenceType().getCanonicalType();
  bool IsConstant = Type.isConstant(Actions.getASTContext());
  while (Type->isArrayType()) {
    QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
    Type = ElemType.getNonReferenceType().getCanonicalType();
  }
  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, predetermined, p.6]
  //  Variables with const qualified type having no mutable member are
  //  shared.
  CXXRecordDecl *RD = Actions.getLangOpts().CPlusPlus ?
                                Type->getAsCXXRecordDecl() : 0;
  if (IsConstant &&
      !(Actions.getLangOpts().CPlusPlus && RD && RD->hasMutableFields())) {
    DeclRefExpr *E;
    // Variables with const-qualified type having no mutable member may be listed
    // in a firstprivate clause, even if they are static data members.
    if (hasDSA(D, OMPC_firstprivate, OMPD_unknown, E) && E)
      return OMPC_unknown;
    return OMPC_shared;
  }

  // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++, predetermined, p.7]
  //  Variables with static storage duration that are declared in a scope
  //  inside the construct are shared.
  if (isOpenMPLocal(D) && D->isStaticLocal())
    return OMPC_shared;

  // Explicitly specified attributes and local variables with predetermined
  // attributes.
  if (Stack.back().SharingMap.count(D)) {
    E = Stack.back().SharingMap[D].RefExpr;
    return Stack.back().SharingMap[D].Attributes;
  }

  return OMPC_unknown;
}

OpenMPClauseKind DSAStackTy::getImplicitDSA(VarDecl *D,
                                            OpenMPDirectiveKind &Kind,
                                            DeclRefExpr *&E) {
  return getDSA(Stack.rbegin() + 1, D, Kind, E);
}

bool DSAStackTy::hasDSA(VarDecl *D, OpenMPClauseKind CKind,
                        OpenMPDirectiveKind DKind, DeclRefExpr *&E) {
  for (StackTy::reverse_iterator I = Stack.rbegin() + 1,
                                 EE = Stack.rend() - 1;
       I != EE; ++I) {
    if (DKind != OMPD_unknown && DKind != I->Directive) continue;
    OpenMPDirectiveKind K;
    if (getDSA(I, D, K, E) == CKind)
      return true;
  }
  E = 0;
  return false;
}

bool DSAStackTy::hasInnermostDSA(VarDecl *D, OpenMPClauseKind CKind,
                                 OpenMPDirectiveKind DKind, DeclRefExpr *&E) {
  assert(DKind != OMPD_unknown && "Directive must be specified explicitly");
  for (StackTy::reverse_iterator I = Stack.rbegin(),
                                 EE = Stack.rend() - 1;
       I != EE; ++I) {
    if (DKind != I->Directive) continue;
    if (getDSA(I, D, DKind, E) == CKind)
      return true;
    return false;
  }
  return false;
}

bool DSAStackTy::hasDirectiveWithName(OpenMPDirectiveKind Kind,
                                      DeclarationNameInfo DirName) {
  for (reverse_iterator I = Stack.rbegin() + 1,
                        E = Stack.rend() - 1;
       I != E; ++I) {
    if (I->Directive == Kind &&
       !DeclarationName::compare(I->DirectiveName.getName(), DirName.getName()))
      return true;
  }
  return false;
}

void Sema::InitDataSharingAttributesStack() {
  VarDataSharingAttributesStack = new DSAStackTy(*this);
}

#define DSAStack static_cast<DSAStackTy *>(VarDataSharingAttributesStack)

void Sema::DestroyDataSharingAttributesStack() {
  delete DSAStack;
}

void Sema::StartOpenMPDSABlock(OpenMPDirectiveKind DKind,
                               const DeclarationNameInfo &DirName,
                               Scope *CurScope) {
  DSAStack->push(DKind, DirName, CurScope);

  if (DKind == OMPD_parallel_for && DSAStack->isParentOrdered()) {
    DSAStack->setOrdered();
    DSAStack->clearParentOrdered();
  }
  PushExpressionEvaluationContext(PotentiallyEvaluated);
}

void Sema::EndOpenMPDSABlock(Stmt *CurDirective) {
//  if (!getCurScope()->isOpenMPDirectiveScope()) return;
  // OpenMP [2.9.3.5, Restrictions, C/C++, p.1]
  //  A variable of class type (or array thereof) that appears in a lastprivate
  //  clause requires an accessible, unambiguous default constructor for the
  //  class type, unless the list item is also specified in a firstprivate
  //  clause.

  if (OMPExecutableDirective *D = dyn_cast_or_null<OMPExecutableDirective>(CurDirective)) {
    for (ArrayRef<OMPClause *>::iterator I = D->clauses().begin(),
                                         E = D->clauses().end();
         I != E; ++I) {
      if (OMPLastPrivateClause *Clause = dyn_cast<OMPLastPrivateClause>(*I)) {
        SmallVector<Expr *, 8> DefaultInits;
        ArrayRef<Expr *>::iterator PVIter = Clause->getPseudoVars1().begin();
        for (OMPLastPrivateClause::varlist_iterator VI = Clause->varlist_begin(),
                                                    VE = Clause->varlist_end();
             VI != VE; ++VI, ++PVIter) {
          DeclRefExpr *DE;
          VarDecl *VD = cast<VarDecl>(cast<DeclRefExpr>(*VI)->getDecl());
          QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
          OpenMPDirectiveKind DKind = DSAStack->getCurrentDirective();
          if ((DSAStack->getTopDSA(VD, DE) == OMPC_lastprivate ||
               (DKind == OMPD_parallel_for &&
                DSAStack->hasInnermostDSA(VD, OMPC_lastprivate,
                                          OMPD_parallel, DE))) &&
              !Type->isDependentType()) {
            SourceLocation ELoc = (*VI)->getExprLoc();
            while (Type->isArrayType()) {
              QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
              Type = ElemType.getNonReferenceType().getCanonicalType();
            }
            CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                                  Type->getAsCXXRecordDecl() : 0;
            if (RD) {
              CXXConstructorDecl *CD = LookupDefaultConstructor(RD);
              PartialDiagnostic PD =
                PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
              if (!CD ||
                  CheckConstructorAccess(ELoc, CD,
                                         InitializedEntity::InitializeTemporary(Type),
                                         CD->getAccess(), PD) == AR_inaccessible ||
                  CD->isDeleted()) {
                Diag(ELoc, diag::err_omp_required_method)
                     << getOpenMPClauseName(OMPC_lastprivate) << 0;
                bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                              VarDecl::DeclarationOnly;
                Diag(VD->getLocation(),
                     IsDecl ? diag::note_previous_decl :
                              diag::note_defined_here) << VD;
                Diag(RD->getLocation(), diag::note_previous_decl) << RD;
                continue;
              }
              MarkFunctionReferenced(ELoc, CD);
              DiagnoseUseOfDecl(CD, ELoc);
            }
            VD = cast<VarDecl>(cast<DeclRefExpr>(*PVIter)->getDecl());
            InitializedEntity Entity = InitializedEntity::InitializeVariable(VD);
            InitializationKind InitKind = InitializationKind::CreateDefault(ELoc);
            InitializationSequence InitSeq(*this, Entity, InitKind, MultiExprArg());
            ExprResult Res = InitSeq.Perform(*this, Entity, InitKind, MultiExprArg());
            if (Res.isInvalid()) continue;
            DefaultInits.push_back(Res.take());
          } else {
            DefaultInits.push_back(0);
          }
        }
        if (DefaultInits.size() == Clause->numberOfVariables())
          Clause->setDefaultInits(DefaultInits);
      }
    }
  }

  DSAStack->pop();
  DiscardCleanupsInEvaluationContext();
  PopExpressionEvaluationContext();
}

namespace {
class VarDeclFilterCCC : public CorrectionCandidateCallback {
private:
  Sema &Actions;
public:
  VarDeclFilterCCC(Sema &S) : Actions(S) { }
  virtual bool ValidateCandidate(const TypoCorrection &Candidate) {
    NamedDecl *ND = Candidate.getCorrectionDecl();
    if (VarDecl *VD = dyn_cast_or_null<VarDecl>(ND)) {
      return VD->hasGlobalStorage() &&
             ((Actions.getCurLexicalContext()->isFileContext() &&
               VD->getDeclContext()->isFileContext()) ||
              Actions.isDeclInScope(ND, Actions.getCurLexicalContext(),
                                    Actions.getCurScope()));
    }
    return false;
  }
};
}

ExprResult Sema::ActOnOpenMPIdExpression(Scope *CurScope,
                                         CXXScopeSpec &ScopeSpec,
                                         const DeclarationNameInfo &Id) {
  LookupResult Lookup(*this, Id, LookupOrdinaryName);
  LookupParsedName(Lookup, CurScope, &ScopeSpec, true);

  if (Lookup.isAmbiguous())
    return ExprError();

  VarDecl *VD;
  if (!Lookup.isSingleResult()) {
    VarDeclFilterCCC Validator(*this);
    TypoCorrection Corrected = CorrectTypo(Id, LookupOrdinaryName, CurScope,
                                           0, Validator);
    std::string CorrectedStr = Corrected.getAsString(getLangOpts());
    std::string CorrectedQuotedStr = Corrected.getQuoted(getLangOpts());
    if (Lookup.empty()) {
      if (Corrected.isResolved()) {
        Diag(Id.getLoc(), diag::err_undeclared_var_use_suggest)
          << Id.getName() << CorrectedQuotedStr
          << FixItHint::CreateReplacement(Id.getLoc(), CorrectedStr);
      } else {
        Diag(Id.getLoc(), diag::err_undeclared_var_use)
          << Id.getName();
      }
    } else {
      Diag(Id.getLoc(), diag::err_omp_expected_var_arg_suggest)
        << Id.getName() << Corrected.isResolved() << CorrectedQuotedStr
        << FixItHint::CreateReplacement(Id.getLoc(), CorrectedStr);
    }
    if (!Corrected.isResolved()) return ExprError();
    VD = Corrected.getCorrectionDeclAs<VarDecl>();
  } else {
    if (!(VD = Lookup.getAsSingle<VarDecl>())) {
      Diag(Id.getLoc(), diag::err_omp_expected_var_arg_suggest)
        << Id.getName() << 0;
      Diag(Lookup.getFoundDecl()->getLocation(), diag::note_declared_at);
      return ExprError();
    }
  }
  Lookup.suppressDiagnostics();

  // OpenMP [2.9.2, Syntax, C/C++]
  //   Variables must be file-scope, namespace-scope, or static block-scope.
  if (!VD->hasGlobalStorage()) {
    Diag(Id.getLoc(), diag::err_omp_global_var_arg)
      << getOpenMPDirectiveName(OMPD_threadprivate)
      << !VD->isStaticLocal();
    bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                  VarDecl::DeclarationOnly;
    Diag(VD->getLocation(),
         IsDecl ? diag::note_previous_decl : diag::note_defined_here) << VD;
    return ExprError();
  }

  // OpenMP [2.9.2, Restrictions, C/C++, p.2]
  //   A threadprivate directive for file-scope variables must appear outside
  //   any definition or declaration.
  // OpenMP [2.9.2, Restrictions, C/C++, p.3]
  //   A threadprivate directive for static class member variables must appear
  //   in the class definition, in the same scope in which the member
  //   variables are declared.
  // OpenMP [2.9.2, Restrictions, C/C++, p.4]
  //   A threadprivate directive for namespace-scope variables must appear
  //   outside any definition or declaration other than the namespace
  //   definition itself.
  // OpenMP [2.9.2, Restrictions, C/C++, p.6]
  //   A threadprivate directive for static block-scope variables must appear
  //   in the scope of the variable and not in a nested scope.
  NamedDecl *ND = cast<NamedDecl>(VD);
  if ((!getCurLexicalContext()->isFileContext() ||
       !VD->getDeclContext()->isFileContext()) &&
       !isDeclInScope(ND, getCurLexicalContext(), getCurScope())) {
    Diag(Id.getLoc(), diag::err_omp_var_scope)
      << getOpenMPDirectiveName(OMPD_threadprivate) << VD;
    bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                  VarDecl::DeclarationOnly;
    Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                     diag::note_defined_here) << VD;
    return ExprError();
  }

  // OpenMP [2.9.2, Restrictions, C/C++, p.2-6]
  //   A threadprivate directive must lexically precede all references to any
  //   of the variables in its list.
  if (VD->isUsed()) {
    Diag(Id.getLoc(), diag::err_omp_var_used)
      << getOpenMPDirectiveName(OMPD_threadprivate) << VD;
    return ExprError();
  }

  QualType ExprType = VD->getType().getNonReferenceType();
  ExprResult DE = BuildDeclRefExpr(VD, ExprType, VK_RValue, Id.getLoc());
  DSAStack->addDSA(VD, cast<DeclRefExpr>(DE.get()), OMPC_threadprivate);
  return DE;
}

Sema::DeclGroupPtrTy Sema::ActOnOpenMPThreadprivateDirective(
                                SourceLocation Loc,
                                ArrayRef<Expr *> VarList) {
  if (OMPThreadPrivateDecl *D = CheckOMPThreadPrivateDecl(Loc, VarList)) {
    CurContext->addDecl(D);
    return DeclGroupPtrTy::make(DeclGroupRef(D));
  }
  return DeclGroupPtrTy();
}

OMPThreadPrivateDecl *Sema::CheckOMPThreadPrivateDecl(
                                 SourceLocation Loc,
                                 ArrayRef<Expr *> VarList) {
  SmallVector<Expr *, 8> Vars;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(),
                                         E = VarList.end();
       I != E; ++I) {
    DeclRefExpr *DE = cast<DeclRefExpr>(*I);
    VarDecl *VD = cast<VarDecl>(DE->getDecl());
    SourceLocation ILoc = DE->getExprLoc();

    // OpenMP [2.9.2, Restrictions, C/C++, p.10]
    //   A threadprivate variable must not have an incomplete type.
    if (RequireCompleteType(ILoc, VD->getType(),
                            diag::err_omp_threadprivate_incomplete_type)) {
      continue;
    }

    // OpenMP [2.9.2, Restrictions, C/C++, p.10]
    //   A threadprivate variable must not have a reference type.
    if (VD->getType()->isReferenceType()) {
      Diag(ILoc, diag::err_omp_ref_type_arg)
        << getOpenMPDirectiveName(OMPD_threadprivate);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // Check if this is a TLS variable.
    if (VD->getTLSKind()) {
      Diag(ILoc, diag::err_omp_var_thread_local) << VD;
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      SourceLocation ELoc = (*I)->getExprLoc();
      CXXDestructorDecl *DD = RD->getDestructor();
      PartialDiagnostic PD =
        PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
      if (DD &&
          (CheckDestructorAccess(ELoc, DD, PD) == AR_inaccessible ||
           DD->isDeleted())) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_threadprivate) << 4;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(),
             IsDecl ? diag::note_previous_decl :
                      diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      } else if (DD) {
        MarkFunctionReferenced(ELoc, DD);
        DiagnoseUseOfDecl(DD, ELoc);
      }
    }

    Vars.push_back(*I);
  }
  return Vars.empty() ?
              0 : OMPThreadPrivateDecl::Create(Context,
                                               getCurLexicalContext(),
                                               Loc, Vars);
}

namespace {
class DeclRefExprMark : public StmtVisitor<DeclRefExprMark, void> {
  Sema &Actions;
public:
  void VisitDeclRefExpr(DeclRefExpr *E) {
    if (!E->getDecl()->isHidden())
      Actions.MarkDeclRefReferenced(E);
  }
  void VisitStmt(Stmt *S) {
    for (Stmt::child_iterator I = S->child_begin(), E = S->child_end();
         I != E; ++I) {
      if (Stmt *Child = *I)
        Visit(Child);
    }
  }

  DeclRefExprMark(Sema &Actions) : Actions(Actions) { }
};
}

void Sema::MarkOpenMPClauses(ArrayRef<OMPClause *> Clauses) {
  DeclRefExprMark Visitor(*this);
  for (ArrayRef<OMPClause *>::iterator I = Clauses.begin(), E = Clauses.end();
       I != E; ++I)
    for (Stmt::child_range S = (*I)->children(); S; ++S)
      if (*S) Visitor.Visit(*S);
}

namespace {
class DSAAttrChecker : public StmtVisitor<DSAAttrChecker, void> {
  DSAStackTy *Stack;
  Sema &Actions;
  llvm::SmallVector<Expr *, 8> ImplicitFirstprivate;
  bool ErrorFound;
  CapturedStmt *CS;
public:
  void VisitDeclRefExpr(DeclRefExpr *E) {
    if(VarDecl *VD = dyn_cast<VarDecl>(E->getDecl())) {
      if (VD->isImplicit() && VD->hasAttr<UnusedAttr>()) return;
      // Skip internally declared variables.
      if (VD->isLocalVarDecl() && !CS->capturesVariable(VD)) return;
      //NamedDecl *ND = VD;
      //if (
      //    Actions.isDeclInScope(ND, Actions.CurContext,
      //                          Stack->getCurScope())) return;
      SourceLocation ELoc = E->getExprLoc();
      DeclRefExpr *PrevRef;

      OpenMPDirectiveKind DKind = Stack->getCurrentDirective();
      OpenMPClauseKind Kind = Stack->getTopDSA(VD, PrevRef);
      if (Kind != OMPC_unknown) {
        if (DKind == OMPD_task && Kind != OMPC_shared && Kind != OMPC_threadprivate &&
            !PrevRef && Stack->isOpenMPLocal(VD))
          ImplicitFirstprivate.push_back(E);
        return;
      }
      // The default(none) clause requires that each variable that is referenced
      // in the construct, and does not have a predetermined data-sharing attribute,
      // must have its data-sharing attribute explicitly determined by being listed
      // in a data-sharing attribute clause.
      if (Kind == OMPC_unknown && Stack->getDefaultDSA() == DSA_none &&
          (DKind == OMPD_parallel || DKind == OMPD_task)) {
        ErrorFound = true;
        Actions.Diag(ELoc, diag::err_omp_no_dsa_for_variable) << VD;
        return;
      }

      // OpenMP [2.9.3.6, Restrictions, p.2]
      //  A list item that appears in a reduction clause of the innermost
      //  enclosing worksharing or parallel construct may not be accessed in an
      //  explicit task.
      if (DKind == OMPD_task &&
          (Stack->hasInnermostDSA(VD, OMPC_reduction, OMPD_for, PrevRef) ||
           Stack->hasInnermostDSA(VD, OMPC_reduction, OMPD_sections, PrevRef) ||
           Stack->hasInnermostDSA(VD, OMPC_reduction, OMPD_parallel, PrevRef) ||
           Stack->hasInnermostDSA(VD, OMPC_reduction, OMPD_parallel_for, PrevRef) ||
           Stack->hasInnermostDSA(VD, OMPC_reduction, OMPD_parallel_sections, PrevRef))) {
        ErrorFound = true;
        Actions.Diag(ELoc, diag::err_omp_reduction_in_task);
        if (PrevRef) {
          Actions.Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
              << getOpenMPClauseName(OMPC_reduction);
        }
        return;
      }
      // Define implicit data-sharing attributes for task.
      if (DKind == OMPD_task &&
          Stack->getImplicitDSA(VD, DKind, PrevRef) != OMPC_shared) {
        ImplicitFirstprivate.push_back(E);
      }
    }
  }
  void VisitOMPExecutableDirective(OMPExecutableDirective *S) {
    for (ArrayRef<OMPClause *>::iterator I = S->clauses().begin(),
                                         E = S->clauses().end();
         I != E; ++I) {
      if (OMPClause *C = *I)
        for (StmtRange R = C->children(); R; ++R) {
          if (Stmt *Child = *R)
            Visit(Child);
        }
    }
  }
  void VisitStmt(Stmt *S) {
    for (Stmt::child_iterator I = S->child_begin(), E = S->child_end();
         I != E; ++I) {
      if (Stmt *Child = *I) {
        if (!isa<OMPExecutableDirective>(Child))
          Visit(Child);
      }
    }
  }

  ArrayRef<Expr *> getImplicitFirstprivate() { return ImplicitFirstprivate; }
  bool isErrorFound() { return ErrorFound; }

  DSAAttrChecker(DSAStackTy *S, Sema &Actions, CapturedStmt *CS) : Stack(S), Actions(Actions),
    ImplicitFirstprivate(), ErrorFound(false), CS(CS) { }
};
}

StmtResult Sema::ActOnOpenMPExecutableDirective(OpenMPDirectiveKind Kind,
                                                const DeclarationNameInfo &DirName,
                                                ArrayRef<OMPClause *> Clauses,
                                                Stmt *AStmt,
                                                SourceLocation StartLoc,
                                                SourceLocation EndLoc) {
  // OpenMP [2.13, Nesting of Regions]
  llvm::SmallVector<OMPClause *, 8> ClausesWithImplicit;
  bool ErrorFound = false;
  if (DSAStack->getCurScope()) {
    OpenMPDirectiveKind ParentKind = DSAStack->getParentDirective();
    bool NestingProhibited = false;
    bool CloseNesting = true;
    bool HasNamedDirective = false;
    StringRef Region;
    switch (ParentKind) {
    case OMPD_for:
    case OMPD_sections:
    case OMPD_parallel_for:
    case OMPD_parallel_sections:
    case OMPD_single:
      // Worksharing region
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 3]
      //  A master region may not be closely nested inside a worksharing, atomic,
      //  or explicit task region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || Kind == OMPD_master ||
                          Kind == OMPD_barrier;
      Region = "a worksharing";
      break;
    case OMPD_task:
      // Task region
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 3]
      //  A master region may not be closely nested inside a worksharing, atomic,
      //  or explicit task region.
      // OpenMP [2.13, Nesting of Regions, p. 4]
      //  An ordered region may not be closely nested inside a critical, atomic,
      //  or explicit task region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || Kind == OMPD_master ||
                          Kind == OMPD_barrier || Kind == OMPD_ordered;
      Region = "explicit task";
      break;
    case OMPD_master:
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || Kind == OMPD_barrier;
      Region = "a master";
      break;
    case OMPD_critical:
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 4]
      //  An ordered region may not be closely nested inside a critical, atomic,
      //  or explicit task region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || HasNamedDirective ||
                          Kind == OMPD_barrier || Kind == OMPD_ordered;
      Region = "a critical";
      break;
    case OMPD_atomic:
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 3]
      //  A master region may not be closely nested inside a worksharing, atomic,
      //  or explicit task region.
      // OpenMP [2.13, Nesting of Regions, p. 4]
      //  An ordered region may not be closely nested inside a critical, atomic,
      //  or explicit task region.
      // OpenMP [2.13, Nesting of Regions, p. 7]
      //  parallel, flush, critical, atomic, taskyield, and explicit task regions
      //  may not be closely nested inside an atomic region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || Kind == OMPD_master ||
                          Kind == OMPD_barrier || Kind == OMPD_parallel ||
                          Kind == OMPD_critical || Kind == OMPD_atomic ||
                          Kind == OMPD_taskyield || Kind == OMPD_task ||
                          Kind == OMPD_flush || Kind == OMPD_ordered;
      Region = "an atomic";
      break;
    case OMPD_ordered:
      // OpenMP [2.13, Nesting of Regions, p. 1]
      //  A worksharing region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 2]
      //  A barrier region may not be closely nested inside a worksharing,
      //  explicit task, critical, ordered, atomic, or master region.
      // OpenMP [2.13, Nesting of Regions, p. 3]
      //  A master region may not be closely nested inside a worksharing, atomic,
      //  or explicit task region.
      NestingProhibited = Kind == OMPD_for || Kind == OMPD_sections ||
                          Kind == OMPD_parallel_for || Kind == OMPD_parallel_sections ||
                          Kind == OMPD_single || Kind == OMPD_master ||
                          Kind == OMPD_barrier;
      Region = "an ordered";
      break;
    default:
      break;
    }
    // OpenMP [2.13, Nesting of Regions, p. 6]
    //  A critical region may not be nested (closely or otherwise) inside a
    //  critical region with the same name. Note that this restriction is not
    //  sufficient to prevent deadlock.
    if (DirName.getName() && Kind == OMPD_critical) {
      HasNamedDirective = DSAStack->hasDirectiveWithName(Kind, DirName);
      CloseNesting = false;
      NestingProhibited = HasNamedDirective;
      Region = "a critical";
    }
    if (NestingProhibited) {
      Diag(StartLoc, diag::err_omp_prohibited_region)
        << CloseNesting << Region << HasNamedDirective << DirName.getName();
      return StmtError();
    }
    // OpenMP [2.13, Nesting of Regions, p. 5]
    //  An ordered region must be closely nested inside a loop region (or
    //  parallel loop region) with an ordered clause.
    if (Kind == OMPD_ordered &&
        (ParentKind != OMPD_unknown && !DSAStack->isParentOrdered())) {
      Diag(StartLoc, diag::err_omp_prohibited_ordered_region);
      return StmtError();
    }

    switch (Kind) {
    case OMPD_taskyield:
    case OMPD_barrier:
    case OMPD_taskwait:
    case OMPD_flush:
      break;
    default: {
      assert(AStmt && isa<CapturedStmt>(AStmt) && "Captured statement expected");
      // Check default data sharing attributes for captured variables.
      DSAAttrChecker DSAChecker(DSAStack, *this, cast<CapturedStmt>(AStmt));
      DSAChecker.Visit(cast<CapturedStmt>(AStmt)->getCapturedStmt());
      if (DSAChecker.isErrorFound()) return StmtError();
      if (DSAChecker.getImplicitFirstprivate().size() > 0) {
        if (OMPClause *Implicit =
             ActOnOpenMPFirstPrivateClause(DSAChecker.getImplicitFirstprivate(),
                                           SourceLocation(),
                                           SourceLocation())) {
          ClausesWithImplicit.push_back(Implicit);
          if (Implicit &&
              cast<OMPFirstPrivateClause>(Implicit)->varlist_size() !=
                                  DSAChecker.getImplicitFirstprivate().size())
            ErrorFound = true;
        } else
          ErrorFound = true;
      }
      }
      break;
    }
  }
  ClausesWithImplicit.append(Clauses.begin(), Clauses.end());

  StmtResult Res = StmtError();
  switch (Kind) {
  case OMPD_parallel:
    Res = ActOnOpenMPParallelDirective(ClausesWithImplicit, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_parallel_for:
  case OMPD_for:
    Res = ActOnOpenMPForDirective(Kind, ClausesWithImplicit, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_parallel_sections:
  case OMPD_sections:
    Res = ActOnOpenMPSectionsDirective(Kind, ClausesWithImplicit, AStmt, StartLoc, EndLoc);
     break;
  case OMPD_section:
    assert (Clauses.empty() && "Clauses are not allowed for section");
    Res = ActOnOpenMPSectionDirective(AStmt, StartLoc, EndLoc);
    break;
  case OMPD_single:
    Res = ActOnOpenMPSingleDirective(ClausesWithImplicit, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_task:
    Res = ActOnOpenMPTaskDirective(ClausesWithImplicit, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_taskyield:
    assert (Clauses.empty() && !AStmt &&
            "Clauses and statement are not allowed for taskyield");
    Res = ActOnOpenMPTaskyieldDirective(StartLoc, EndLoc);
    break;
  case OMPD_master:
    assert (Clauses.empty() && "Clauses are not allowed for master");
    Res = ActOnOpenMPMasterDirective(AStmt, StartLoc, EndLoc);
    break;
  case OMPD_critical:
    assert (Clauses.empty() && "Clauses are not allowed for critical");
    Res = ActOnOpenMPCriticalDirective(DirName, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_barrier:
    assert (Clauses.empty() && !AStmt &&
            "Clauses and statement are not allowed for barrier");
    Res = ActOnOpenMPBarrierDirective(StartLoc, EndLoc);
    break;
  case OMPD_taskwait:
    assert (Clauses.empty() && !AStmt &&
            "Clauses and statement are not allowed for taskwait");
    Res = ActOnOpenMPTaskwaitDirective(StartLoc, EndLoc);
    break;
  case OMPD_taskgroup:
    assert (Clauses.empty() && "Clauses are not allowed for taskgroup");
    Res = ActOnOpenMPTaskgroupDirective(AStmt, StartLoc, EndLoc);
    break;
  case OMPD_atomic:
    Res = ActOnOpenMPAtomicDirective(ClausesWithImplicit, AStmt, StartLoc, EndLoc);
    break;
  case OMPD_flush:
    assert (!AStmt &&
            "Statement is not allowed for flush");
    Res = ActOnOpenMPFlushDirective(ClausesWithImplicit, StartLoc, EndLoc);
    break;
  case OMPD_ordered:
    assert (Clauses.empty() && "Clauses are not allowed for ordered");
    Res = ActOnOpenMPOrderedDirective(AStmt, StartLoc, EndLoc);
    break;
  default:
    break;
  }
  if (ErrorFound) return StmtError();

  return Res;
}

StmtResult Sema::ActOnOpenMPParallelDirective(ArrayRef<OMPClause *> Clauses,
                                              Stmt *AStmt,
                                              SourceLocation StartLoc,
                                              SourceLocation EndLoc) {
  // OpenMP [2.13, Nesting of Regions, p. 7]
  //  parallel, flush, critical
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPParallelDirective::Create(Context, StartLoc, EndLoc,
                                            Clauses, AStmt));
}

namespace {
class ForBreakStmtChecker : public StmtVisitor<ForBreakStmtChecker, bool> {
  Stmt *Break;
public:
  bool VisitBreakStmt(BreakStmt *S) {Break = S; return true;}
  bool VisitSwitchStmt(SwitchStmt *S) {return false;}
  bool VisitWhileStmt(WhileStmt *S) {return false;}
  bool VisitDoStmt(DoStmt *S) {return false;}
  bool VisitForStmt(ForStmt *S) {return false;}
  bool VisitCXXForRangeStmt(CXXForRangeStmt *S) {return false;}
  bool VisitStmt(Stmt *S) {
    for (Stmt::child_iterator I = S->child_begin(), E = S->child_end();
         I != E; ++I) {
      if (*I && Visit(*I)) return true;
    }
    return false;
  }
  ForBreakStmtChecker() { }
  Stmt *getBreak() { return Break; }
};
}

StmtResult Sema::ActOnOpenMPForDirective(OpenMPDirectiveKind Kind,
                                         ArrayRef<OMPClause *> Clauses,
                                         Stmt *AStmt,
                                         SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Ends;
  SmallVector<Expr *, 8> Incrs;
  SmallVector<Expr *, 8> Inits;
  SmallVector<Expr *, 8> VarCnts;
  SmallVector<BinaryOperatorKind, 8> OpKinds;
  unsigned StmtCount = 1;
  for (ArrayRef<OMPClause *>::iterator I = Clauses.begin(), E = Clauses.end();
       I != E; ++I) {
    if (OMPCollapseClause *Clause = dyn_cast_or_null<OMPCollapseClause>(*I)) {
      IntegerLiteral *IL = cast<IntegerLiteral>(Clause->getNumForLoops());
      StmtCount = IL->getValue().getLimitedValue();
      break;
    }
  }
  Stmt *CStmt = AStmt;
  while (CapturedStmt *CS = dyn_cast_or_null<CapturedStmt>(CStmt))
    CStmt = CS->getCapturedStmt();
  while (AttributedStmt *AS = dyn_cast_or_null<AttributedStmt>(CStmt))
    CStmt = AS->getSubStmt();
  bool SkipExprCount = false;
  for (unsigned Cnt = 0; Cnt < StmtCount; ++Cnt) {
    Expr *NewEnd;
    Expr *NewIncr;
    Expr *Init;
    Expr *VarCnt;
    BinaryOperatorKind OpKind;
    if (isNotOpenMPCanonicalLoopForm(CStmt, Kind, NewEnd, NewIncr, Init, VarCnt,
                                     OpKind))
      return StmtError();
    if (NewEnd->getType()->isDependentType() || NewIncr->getType()->isDependentType() ||
        Init->getType()->isDependentType() || VarCnt->getType()->isDependentType())
      SkipExprCount = true;
    Ends.push_back(NewEnd);
    Incrs.push_back(NewIncr);
    Inits.push_back(Init);
    VarCnts.push_back(VarCnt);
    OpKinds.push_back(OpKind);
    CStmt = cast<ForStmt>(CStmt)->getBody();
    bool SkippedContainers = false;
    while (!SkippedContainers) {
      if (AttributedStmt *AS = dyn_cast_or_null<AttributedStmt>(CStmt))
        CStmt = AS->getSubStmt();
      else if (CompoundStmt *CS = dyn_cast_or_null<CompoundStmt>(CStmt)) {
        if (CS->size() != 1) {
          SkippedContainers = true;
        } else {
          CStmt = CS->body_back();
        }
      } else
        SkippedContainers = true;
    }
  }
  ForBreakStmtChecker Check;
  if (CStmt && Check.Visit(CStmt)) {
    Diag(Check.getBreak()->getLocStart(), diag::err_omp_for_cannot_break);
    return StmtError();
  }
  // Build ending for Idx var;
  Expr *NewEnd = 0;
  Expr *NewVar = 0;
  Expr *NewVarCntExpr = 0;
  if (!SkipExprCount) {
    NewEnd = Ends[0];
    for (unsigned I = 1; I < StmtCount; ++I) {
      ExprResult Res = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, Ends[I],
                                  NewEnd);
      if (!Res.isUsable()) return StmtError();
      NewEnd = Res.take();
    }
    QualType IdxTy = NewEnd->getType();
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(IdxTy, StartLoc);
    VarDecl *Idx = VarDecl::Create(
                              Context, Context.getTranslationUnitDecl(), StartLoc,
                              StartLoc, 0, IdxTy, TI, SC_Static);
    Idx->setImplicit();
    Idx->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(Idx);
    ExprResult IdxExprRes = BuildDeclRefExpr(Idx, IdxTy,
                                             VK_LValue, StartLoc);
    NewVar = IdxExprRes.take();

    //Build new values for actual indeces.
    Expr *NewDiv = Ends[0];
    Expr *IdxRVal = ImpCastExprToType(NewVar, IdxTy, CK_LValueToRValue).take();
    Expr* IdxRem = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Div, IdxRVal,
                              NewEnd).take();
    if (!IdxRem) return StmtError();
    ExprResult Res = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Sub,
                                NewEnd,
                                ActOnIntegerConstant(SourceLocation(), 1).take());
    if (!Res.isUsable()) return StmtError();
    NewEnd = Res.take();

    Expr *NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Rem, IdxRVal,
                               Ends[0]).take();
    if (!NewIncr) return StmtError();
    NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, NewIncr,
                         Incrs[0]).take();
    if (!NewIncr) return StmtError();
    Expr *Offs = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, IdxRem,
                            Ends[0]).take();
    if (!Offs) return StmtError();
    Offs = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, Offs,
                      Incrs[0]).take();
    if (!Offs) return StmtError();
    NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Add,
                         NewIncr, Offs).take();
    if (!NewIncr) return StmtError();
    //Expr *NewStep = BuildBinOp(DSAStack->getCurScope(), StartLoc, OpKinds[0],
    //                           Inits[0], NewIncr).take();
    //if (!NewStep) return StmtError();
    //NewVarCntExpr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Assign,
    //                           VarCnts[0], NewStep).take();
    NewVarCntExpr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Assign,
                               VarCnts[0], Inits[0]).take();
    if (!NewVarCntExpr) return StmtError();
    NewVarCntExpr = ImpCastExprToType(NewVarCntExpr,
                                      Context.VoidTy, CK_ToVoid).take();
    if (!NewVarCntExpr) return StmtError();
    Expr *NewVarCntExpr1 = BuildBinOp(DSAStack->getCurScope(), StartLoc,
                                      (OpKinds[0] == BO_Add) ? BO_AddAssign : BO_SubAssign,
                                      VarCnts[0], NewIncr).take();
    if (!NewVarCntExpr1) return StmtError();
    NewVarCntExpr1 = ImpCastExprToType(NewVarCntExpr1,
                                      Context.VoidTy, CK_ToVoid).take();
    if (!NewVarCntExpr1) return StmtError();
    NewVarCntExpr = CreateBuiltinBinOp(StartLoc, BO_Comma,
                                       NewVarCntExpr, NewVarCntExpr1).take();
    if (!NewVarCntExpr) return StmtError();
    for (unsigned I = 1; I < StmtCount; ++I) {
      NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Div, IdxRVal,
                           NewDiv).take();
      if (!NewIncr) return StmtError();
      NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Rem, NewIncr,
                           Ends[I]).take();
      if (!NewIncr) return StmtError();
      NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, NewIncr,
                           Incrs[I]).take();
      if (!NewIncr) return StmtError();
      Offs = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, IdxRem,
                        Ends[I]).take();
      if (!Offs) return StmtError();
      Offs = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, Offs,
                        Incrs[I]).take();
      if (!Offs) return StmtError();
      NewIncr = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Add,
                           NewIncr, Offs).take();
      if (!NewIncr) return StmtError();
//      NewStep = BuildBinOp(DSAStack->getCurScope(), StartLoc, OpKinds[I],
//                           Inits[I], NewIncr).take();
//      if (!NewStep) return StmtError();
//      Expr *NewVarCntExpr1 = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Assign,
//                                        VarCnts[I], NewStep).take();
      NewVarCntExpr1 = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Assign,
                                  VarCnts[I], Inits[I]).take();
      if (!NewVarCntExpr1) return StmtError();
      NewVarCntExpr1 = ImpCastExprToType(NewVarCntExpr1,
                                         Context.VoidTy, CK_ToVoid).take();
      if (!NewVarCntExpr1) return StmtError();
      NewVarCntExpr = CreateBuiltinBinOp(StartLoc, BO_Comma,
                                         NewVarCntExpr, NewVarCntExpr1).take();
      if (!NewVarCntExpr) return StmtError();
      NewVarCntExpr1 = BuildBinOp(DSAStack->getCurScope(), StartLoc,
                                  (OpKinds[I] == BO_Add) ? BO_AddAssign : BO_SubAssign,
                                  VarCnts[I], NewIncr).take();
      if (!NewVarCntExpr1) return StmtError();
      NewVarCntExpr1 = ImpCastExprToType(NewVarCntExpr1,
                                         Context.VoidTy, CK_ToVoid).take();
      if (!NewVarCntExpr1) return StmtError();
      NewVarCntExpr = CreateBuiltinBinOp(StartLoc, BO_Comma,
                                         NewVarCntExpr, NewVarCntExpr1).take();
      if (!NewVarCntExpr) return StmtError();
      NewDiv = BuildBinOp(DSAStack->getCurScope(), StartLoc, BO_Mul, NewDiv,
                          Ends[I]).take();
      if (!NewDiv) return StmtError();
    }
    NewVarCntExpr = ImpCastExprToType(NewVarCntExpr,
                                      Context.VoidTy, CK_ToVoid).take();
  }

  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPForDirective::Create(Context, StartLoc, EndLoc,
                                       Clauses, AStmt, NewVar, NewEnd,
                                       NewVarCntExpr, VarCnts));
}

StmtResult Sema::ActOnOpenMPSectionsDirective(OpenMPDirectiveKind Kind,
                                              ArrayRef<OMPClause *> Clauses,
                                              Stmt *AStmt,
                                              SourceLocation StartLoc,
                                              SourceLocation EndLoc) {
  Stmt *BaseStmt = AStmt;
  while (CapturedStmt *CS = dyn_cast_or_null<CapturedStmt>(BaseStmt))
    BaseStmt = CS->getCapturedStmt();
  CompoundStmt *C = dyn_cast_or_null<CompoundStmt>(BaseStmt);
  if (!C) {
    Diag(AStmt->getLocStart(), diag::err_omp_sections_not_compound_stmt)
      << getOpenMPDirectiveName(Kind);
    return StmtError();
  }
  // All associated statements must be '#pragma omp section' except for
  // the first one.
  Stmt::child_range S = C->children();
  for (++S; S; ++S) {
    Stmt *SectionStmt = *S;
    if (!SectionStmt || !isa<OMPSectionDirective>(SectionStmt)) {
      if (SectionStmt)
        Diag(SectionStmt->getLocStart(), diag::err_omp_sections_not_section)
          << getOpenMPDirectiveName(Kind);
      return StmtError();
    }
  }

  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPSectionsDirective::Create(Context, StartLoc, EndLoc,
                                            Clauses, AStmt));
}

StmtResult Sema::ActOnOpenMPSectionDirective(Stmt *AStmt,
                                             SourceLocation StartLoc,
                                             SourceLocation EndLoc) {
  // OpenMP [2.6.2, Sections Construct, Restrictions, p.1]
  //  Orphaned section directives are prohibited. That is, the section
  //  directives must appear within the sections construct and must not
  //  be encountered elsewhere in the sections region.
  // OpenMP scope for current directive.
  if (DSAStack->getCurScope()) {
    Scope *ParentScope = DSAStack->getCurScope()->getParent();
    // CompoundStmt scope for sections scope.
    ParentScope = ParentScope ? getCurScope()->getParent() : 0;
    // Sections scope.
    ParentScope = ParentScope ? ParentScope->getParent() : 0;
    if (!ParentScope || !ParentScope->isOpenMPDirectiveScope() ||
        (DSAStack->getParentDirective() != OMPD_sections &&
         DSAStack->getParentDirective() != OMPD_parallel_sections)) {
      Diag(StartLoc, diag::err_omp_section_orphaned);
      return StmtError();
    }
  }

  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPSectionDirective::Create(Context, StartLoc, EndLoc, AStmt));
}

StmtResult Sema::ActOnOpenMPSingleDirective(ArrayRef<OMPClause *> Clauses,
                                            Stmt *AStmt,
                                            SourceLocation StartLoc,
                                            SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPSingleDirective::Create(Context, StartLoc, EndLoc,
                                          Clauses, AStmt));
}

StmtResult Sema::ActOnOpenMPTaskDirective(ArrayRef<OMPClause *> Clauses,
                                          Stmt *AStmt,
                                          SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPTaskDirective::Create(Context, StartLoc, EndLoc,
                                        Clauses, AStmt));
}

StmtResult Sema::ActOnOpenMPTaskyieldDirective(SourceLocation StartLoc,
                                               SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPTaskyieldDirective::Create(Context, StartLoc, EndLoc));
}

StmtResult Sema::ActOnOpenMPMasterDirective(Stmt *AStmt,
                                            SourceLocation StartLoc,
                                            SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPMasterDirective::Create(Context, StartLoc, EndLoc, AStmt));
}

StmtResult Sema::ActOnOpenMPCriticalDirective(
                                          const DeclarationNameInfo &DirName,
                                          Stmt *AStmt,
                                          SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPCriticalDirective::Create(Context, DirName, StartLoc,
                                            EndLoc, AStmt));
}

StmtResult Sema::ActOnOpenMPBarrierDirective(SourceLocation StartLoc,
                                             SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPBarrierDirective::Create(Context, StartLoc, EndLoc));
}

StmtResult Sema::ActOnOpenMPTaskwaitDirective(SourceLocation StartLoc,
                                              SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPTaskwaitDirective::Create(Context, StartLoc, EndLoc));
}

StmtResult Sema::ActOnOpenMPTaskgroupDirective(Stmt *AStmt,
                                               SourceLocation StartLoc,
                                               SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPTaskgroupDirective::Create(Context, StartLoc, EndLoc, AStmt));
}

namespace {
class ExprUseChecker : public StmtVisitor<ExprUseChecker, bool> {
  const llvm::FoldingSetNodeID &ExprID;
  const ASTContext &Context;
public:
  bool VisitStmt(Stmt *S) {
    if (!S) return false;
    for (Stmt::child_range R = S->children(); R; ++R) {
      if (Visit(*R))
        return true;
    }
    llvm::FoldingSetNodeID ID;
    S->Profile(ID, Context, true);
    return ID == ExprID;
  }
  ExprUseChecker(const llvm::FoldingSetNodeID &ExprID,
                 const ASTContext& Context)
    : ExprID(ExprID), Context(Context) { }
};
}

StmtResult Sema::ActOnOpenMPAtomicDirective(ArrayRef<OMPClause *> Clauses,
                                            Stmt *AStmt,
                                            SourceLocation StartLoc,
                                            SourceLocation EndLoc) {
  // OpenMP [2.10.6, atomic Construct, Syntax]
  //  There should not be no more than 1 clause 'read', 'write', 'update'
  //  or 'capture'.
  OpenMPClauseKind Kind = OMPC_update;
  if (!Clauses.empty()) {
    bool FoundClauses = false;
    for (ArrayRef<OMPClause *>::iterator I = Clauses.begin(), E = Clauses.end();
         I != E; ++I) {
      if ((*I)->getClauseKind() != OMPC_seq_cst) {
        Kind = (*I)->getClauseKind();
        bool CurFoundClauses = Kind == OMPC_read || Kind == OMPC_write ||
                               Kind == OMPC_update || Kind == OMPC_capture;
        if (FoundClauses && CurFoundClauses) {
          Diag(StartLoc, diag::err_omp_atomic_more_one_clause);
          Kind = OMPC_unknown;
          return StmtError();
        }
        FoundClauses = FoundClauses || CurFoundClauses;
      }
    }
  }

  // OpenMP [2.10.6, atomic Construct, Syntax]
  //  For 'read', 'write', 'update' clauses only expression statements are
  //  allowed.
  Stmt *BaseStmt = AStmt;
  while (CapturedStmt *CS = dyn_cast_or_null<CapturedStmt>(BaseStmt))
    BaseStmt = CS->getCapturedStmt();
  while (AttributedStmt *AS = dyn_cast_or_null<AttributedStmt>(BaseStmt))
    BaseStmt = AS->getSubStmt();
  bool ExprStmt = isa<Expr>(BaseStmt);
  if (Kind != OMPC_capture && !ExprStmt) {
    Diag(BaseStmt->getLocStart(), diag::err_omp_atomic_not_expression)
      << getOpenMPClauseName(Kind);
    return StmtError();
  }
  bool WrongStmt = false;
  Expr *V = 0;
  Expr *X = 0;
  Expr *OpExpr = 0;
  BinaryOperatorKind Op = BO_Assign;
  bool CaptureAfter = false;
  bool Reversed = false;
  switch (Kind) {
  case OMPC_read: {
    // expr : v = x, where x and v are both l-value with scalar type.
    BinaryOperator *BinOp = dyn_cast_or_null<BinaryOperator>(BaseStmt);
    ImplicitCastExpr *ImpCast;
    WrongStmt =
           !BinOp || BinOp->getOpcode() != BO_Assign ||
           !BinOp->getLHS() || !BinOp->getRHS() ||
           (!BinOp->getLHS()->getType().getCanonicalType()->isScalarType() &&
            !BinOp->getLHS()->getType().getCanonicalType()->isDependentType()) ||
           (!BinOp->getRHS()->getType().getCanonicalType()->isScalarType() &&
            !BinOp->getRHS()->getType().getCanonicalType()->isDependentType()) ||
           !(ImpCast = dyn_cast_or_null<ImplicitCastExpr>(BinOp->getRHS())) ||
           ImpCast->getCastKind() != CK_LValueToRValue;
    if (!WrongStmt) {
      llvm::FoldingSetNodeID ID;
      BinOp->getLHS()->IgnoreParenCasts()->Profile(ID, Context, true);
      ExprUseChecker UseCheck(ID, Context);
      WrongStmt = UseCheck.Visit(BinOp->getRHS()->IgnoreParenCasts());
      if (!WrongStmt) {
        V = BinOp->getLHS();
        X = BinOp->getRHS();
      }
    }
    }
    break;
  case OMPC_write: {
    // expr : x = expr, where x is an l-value with scalar type and expr has
    // scalar type.
    BinaryOperator *BinOp = dyn_cast_or_null<BinaryOperator>(BaseStmt);
    WrongStmt =
        !BinOp || BinOp->getOpcode() != BO_Assign ||
        !BinOp->getLHS() || !BinOp->getRHS() ||
        (!BinOp->getLHS()->getType().getCanonicalType()->isScalarType() &&
         !BinOp->getLHS()->getType().getCanonicalType()->isDependentType()) ||
        (!BinOp->getRHS()->getType().getCanonicalType()->isScalarType() &&
         !BinOp->getRHS()->getType().getCanonicalType()->isDependentType());
    if (!WrongStmt) {
      llvm::FoldingSetNodeID ID;
      BinOp->getLHS()->IgnoreParenCasts()->Profile(ID, Context, true);
      ExprUseChecker UseCheck(ID, Context);
      WrongStmt = UseCheck.Visit(BinOp->getRHS()->IgnoreParenCasts());
      if (!WrongStmt) {
        X = BinOp->getLHS();
        OpExpr = BinOp->getRHS();
      }
    }
    }
    break;
  case OMPC_update: {
    // expr : x++, where x is an l-value with scalar type.
    // expr : x--, where x is an l-value with scalar type.
    // expr : ++x, where x is an l-value with scalar type.
    // expr : --x, where x is an l-value with scalar type.
    // expr : x binop= expr, where x is an l-value with scalar type and expr is
    // scalar.
    // expr : x = x binop expr, where x is an l-value with scalar type and expr
    // is scalar.
    // expr : x = expr binop x, where x is an l-value with scalar type and expr
    // is scalar.
    // binop : +, *, -, /, &, ^, |, << or >>.
    UnaryOperator *UnOp = dyn_cast_or_null<UnaryOperator>(BaseStmt);
    BinaryOperator *BinOp = dyn_cast_or_null<BinaryOperator>(BaseStmt);
    BinaryOperator *RHSBinOp = BinOp ?
                               dyn_cast_or_null<BinaryOperator>(BinOp->getRHS()->IgnoreParenCasts())
                               : 0;
    WrongStmt =
       (!UnOp && !BinOp) ||
       (UnOp && ((!UnOp->getType().getCanonicalType()->isScalarType() &&
                  !UnOp->getType().getCanonicalType()->isDependentType()) ||
                 !UnOp->isIncrementDecrementOp())) ||
       (BinOp &&
        ((!BinOp->getLHS()->getType().getCanonicalType()->isScalarType() &&
          !BinOp->getLHS()->getType().getCanonicalType()->isDependentType()) ||
         (!BinOp->getRHS()->getType().getCanonicalType()->isScalarType() &&
          !BinOp->getRHS()->getType().getCanonicalType()->isDependentType()))) ||
       (BinOp && (!BinOp->isCompoundAssignmentOp() && !BinOp->isShiftAssignOp()) && RHSBinOp &&
        (BinOp->getOpcode() != BO_Assign ||
         (!RHSBinOp->isAdditiveOp() &&
          RHSBinOp->getOpcode() != BO_Mul &&
          RHSBinOp->getOpcode() != BO_Div &&
          !RHSBinOp->isBitwiseOp() &&
          !RHSBinOp->isShiftOp()))) ||
       (BinOp && !RHSBinOp &&
        ((!BinOp->isCompoundAssignmentOp() && !BinOp->isShiftAssignOp()) ||
         BinOp->getOpcode() == BO_RemAssign));
    if (!WrongStmt && UnOp) {
      X = UnOp->getSubExpr();
      OpExpr = ActOnIntegerConstant(BaseStmt->getLocStart(), 1).take();
      if (UnOp->isIncrementOp())
        Op = BO_Add;
      else
        Op = BO_Sub;
    } else if (!WrongStmt && BinOp &&
               (BinOp->isCompoundAssignmentOp() || BinOp->isShiftAssignOp())) {
      llvm::FoldingSetNodeID ID;
      BinOp->getLHS()->IgnoreParenCasts()->Profile(ID, Context, true);
      ExprUseChecker UseCheck(ID, Context);
      WrongStmt = UseCheck.Visit(BinOp->getRHS()->IgnoreParenCasts());
      if (!WrongStmt) {
        X = BinOp->getLHS();
        OpExpr = BinOp->getRHS();
        switch (BinOp->getOpcode()) {
        case BO_AddAssign:
          Op = BO_Add;
          break;
        case BO_MulAssign:
          Op = BO_Mul;
          break;
        case BO_SubAssign:
          Op = BO_Sub;
          break;
        case BO_DivAssign:
          Op = BO_Div;
          break;
        case BO_AndAssign:
          Op = BO_And;
          break;
        case BO_XorAssign:
          Op = BO_Xor;
          break;
        case BO_OrAssign:
          Op = BO_Or;
          break;
        case BO_ShlAssign:
          Op = BO_Shl;
          break;
        case BO_ShrAssign:
          Op = BO_Shr;
          break;
        default:
          WrongStmt = true;
          break;
        }
      }
    } else if (!WrongStmt && RHSBinOp) {
      llvm::FoldingSetNodeID ID1, ID2;
      BinOp->getLHS()->IgnoreParenCasts()->Profile(ID1, Context, true);
      RHSBinOp->getLHS()->IgnoreParenCasts()->Profile(ID2, Context, true);
      if (ID1 == ID2) {
        ExprUseChecker UseCheck(ID1, Context);
        WrongStmt = UseCheck.Visit(RHSBinOp->getRHS()->IgnoreParenCasts());
        if (!WrongStmt) {
          X = BinOp->getLHS();
          OpExpr = RHSBinOp->getRHS();
          Op = RHSBinOp->getOpcode();
        }
      } else {
        ID2.clear();
        RHSBinOp->getRHS()->IgnoreParenCasts()->Profile(ID2, Context, true);
        if (ID1 == ID2) {
          ExprUseChecker UseCheck(ID2, Context);
          WrongStmt = UseCheck.Visit(RHSBinOp->getLHS()->IgnoreParenCasts());
          if (!WrongStmt) {
            X = BinOp->getLHS();
            OpExpr = RHSBinOp->getLHS();
            Op = RHSBinOp->getOpcode();
            Reversed = true;
          }
        } else
          WrongStmt = true;
      }
    }
    }
    break;
  case OMPC_capture: {
    // expr : v = x++, where v and x are l-values with scalar types.
    // expr : v = x--, where v and x are l-values with scalar types.
    // expr : v = ++x, where v and x are l-values with scalar types.
    // expr : v = --x, where v and x are l-values with scalar types.
    // expr : v = x binop= expr, where v and x are l-values with scalar types
    // and expr is scalar.
    // expr : v = x = x binop expr, where v and x are l-values with scalar type
    // and expr is scalar.
    // expr : v = x = expr binop x, where v and x are l-values with scalar type
    // and expr is scalar.
    // stmt : {v = x; x binop= expr;}
    // stmt : {x binop= expr; v = x;}
    // stmt : {v = x; x = x binop expr;}
    // stmt : {v = x; x = expr binop x;}
    // stmt : {x = x binop expr; v = x;}
    // stmt : {x = expr binop x; v = x;}
    // stmt : {v = x; x = expr;}
    // stmt : {v = x; x++;}
    // stmt : {v = x; ++x;}
    // stmt : {x++; v = x;}
    // stmt : {++x; v = x;}
    // stmt : {v = x; x--;}
    // stmt : {v = x; --x;}
    // stmt : {x--; v = x;}
    // stmt : {--x; v = x;}
    // binop : +, *, -, /, &, ^, |, << or >>.

    //Expr *V = 0;
    //Expr *X = 0;
    llvm::FoldingSetNodeID VID, XID;
    BinaryOperator *BinOp = dyn_cast_or_null<BinaryOperator>(BaseStmt);
    if (ExprStmt && (!BinOp || BinOp->getOpcode() != BO_Assign)) {
      WrongStmt = true;
      break;
    }
    if (ExprStmt) {
      V = BinOp->getLHS();
      V->IgnoreParenCasts()->Profile(VID, Context, true);
      ExprUseChecker UseCheck(VID, Context);
      WrongStmt =
              (!V->getType().getCanonicalType()->isScalarType() &&
               !V->getType().getCanonicalType()->isDependentType()) ||
              (!BinOp->getRHS()->getType().getCanonicalType()->isScalarType() &&
               !BinOp->getRHS()->getType().getCanonicalType()->isDependentType());
      Expr *RHS = BinOp->getRHS()->IgnoreParenLValueCasts();
      if (UnaryOperator *XOp = dyn_cast_or_null<UnaryOperator>(RHS)) {
        X = XOp->getSubExpr();
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        OpExpr = ActOnIntegerConstant(X->getLocStart(), 1).take();
        if (XOp->isIncrementOp())
          Op = BO_Add;
        else
          Op = BO_Sub;
        CaptureAfter = XOp->isPrefix();
      } else if (BinaryOperator *XOp = dyn_cast_or_null<BinaryOperator>(RHS)) {
        X = XOp->getLHS();
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        CaptureAfter = true;
      } else
        WrongStmt = true;
      if (WrongStmt) break;
      BaseStmt = RHS;
    } else if (CompoundStmt *CStmt = dyn_cast_or_null<CompoundStmt>(BaseStmt)) {
      WrongStmt = CStmt->size() != 2;
      if (WrongStmt) break;
      Stmt *S1 = *(CStmt->body_begin());
      Stmt *S2 = CStmt->body_back();
      BinaryOperator *VXOp1 = dyn_cast_or_null<BinaryOperator>(S1);
      BinaryOperator *VXOp2 = dyn_cast_or_null<BinaryOperator>(S2);
      UnaryOperator *XOp1 = dyn_cast_or_null<UnaryOperator>(S1);
      UnaryOperator *XOp2 = dyn_cast_or_null<UnaryOperator>(S2);
      if (VXOp1 && VXOp2 &&
          VXOp1->getOpcode() == BO_Assign &&
          VXOp2->getOpcode() == BO_Assign) {
        V = VXOp1->getLHS();
        X = VXOp1->getRHS()->IgnoreParenLValueCasts();
        V->IgnoreParenCasts()->Profile(VID, Context, true);
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        llvm::FoldingSetNodeID X2ID;
        VXOp2->getLHS()->IgnoreParenCasts()->Profile(X2ID, Context, true);
        if (!(XID == X2ID)) {
          llvm::FoldingSetNodeID ExprID;
          VXOp2->getRHS()->IgnoreParenCasts()->Profile(ExprID, Context, true);
          if (ExprID == VID) {
            X = VXOp1->getLHS();
            XID = VID;
            V = VXOp2->getLHS();
            VID = X2ID;
            BaseStmt = S1;
            CaptureAfter = true;
          } else {
            WrongStmt = true;
            break;
          }
        }
        else {
          BaseStmt = S2;
        }
      } else if (VXOp1 && VXOp2 &&
                 VXOp1->getOpcode() == BO_Assign &&
                 VXOp2->isCompoundAssignmentOp()) {
        V = VXOp1->getLHS();
        X = VXOp1->getRHS()->IgnoreParenLValueCasts();
        V->IgnoreParenCasts()->Profile(VID, Context, true);
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        llvm::FoldingSetNodeID X2ID;
        VXOp2->getLHS()->IgnoreParenCasts()->Profile(X2ID, Context, true);
        if (!(XID == X2ID)) {
          WrongStmt = true;
          break;
        }
        BaseStmt = S2;
      } else if (VXOp1 && VXOp2 &&
                 VXOp2->getOpcode() == BO_Assign &&
                 VXOp1->isCompoundAssignmentOp()) {
        V = VXOp2->getLHS();
        X = VXOp2->getRHS()->IgnoreParenLValueCasts();
        V->IgnoreParenCasts()->Profile(VID, Context, true);
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        llvm::FoldingSetNodeID X2ID;
        VXOp1->getLHS()->IgnoreParenCasts()->Profile(X2ID, Context, true);
        if (!(XID == X2ID)) {
          WrongStmt = true;
          break;
        }
        BaseStmt = S1;
        CaptureAfter = true;
      } else if (VXOp1 && XOp2 &&
                 VXOp1->getOpcode() == BO_Assign) {
        V = VXOp1->getLHS();
        X = VXOp1->getRHS()->IgnoreParenLValueCasts();
        V->IgnoreParenCasts()->Profile(VID, Context, true);
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        llvm::FoldingSetNodeID X2ID;
        XOp2->getSubExpr()->IgnoreParenCasts()->Profile(X2ID, Context, true);
        if (!(XID == X2ID)) {
          WrongStmt = true;
          break;
        }
        BaseStmt = S2;
      } else if (VXOp2 && XOp1 &&
                 VXOp2->getOpcode() == BO_Assign) {
        V = VXOp2->getLHS();
        X = VXOp2->getRHS()->IgnoreParenLValueCasts();
        V->IgnoreParenCasts()->Profile(VID, Context, true);
        X->IgnoreParenCasts()->Profile(XID, Context, true);
        llvm::FoldingSetNodeID X2ID;
        XOp1->getSubExpr()->IgnoreParenCasts()->Profile(X2ID, Context, true);
        if (!(XID == X2ID)) {
          WrongStmt = true;
          break;
        }
        BaseStmt = S1;
        CaptureAfter = true;
      } else {
        WrongStmt = true;
        break;
      }
      if ((!V->getType().getCanonicalType()->isScalarType() &&
           !V->getType().getCanonicalType()->isDependentType()) ||
          (!X->getType().getCanonicalType()->isScalarType() &&
           !X->getType().getCanonicalType()->isDependentType())) {
        WrongStmt = true;
        break;
      }
    } else {
      WrongStmt = true;
      break;
    }
    ExprUseChecker UseCheckV(VID, Context);
    ExprUseChecker UseCheckX(XID, Context);
    WrongStmt = UseCheckV.Visit(X->IgnoreParenCasts()) ||
                UseCheckX.Visit(V->IgnoreParenCasts());
    if (WrongStmt) break;
    UnaryOperator *UnOp = dyn_cast_or_null<UnaryOperator>(BaseStmt);
    BinOp = dyn_cast_or_null<BinaryOperator>(BaseStmt);
    BinaryOperator *RHSBinOp = BinOp ?
                               dyn_cast_or_null<BinaryOperator>(BinOp->getRHS()->IgnoreParenCasts())
                               : 0;
    WrongStmt =
       (!UnOp && !BinOp) ||
       (UnOp && ((!UnOp->getType().getCanonicalType()->isScalarType() &&
                  !UnOp->getType().getCanonicalType()->isDependentType()) ||
                 !UnOp->isIncrementDecrementOp())) ||
       (BinOp &&
        ((!BinOp->getLHS()->getType().getCanonicalType()->isScalarType() &&
          !BinOp->getLHS()->getType().getCanonicalType()->isDependentType()) ||
         (!BinOp->getRHS()->getType().getCanonicalType()->isScalarType() &&
          !BinOp->getRHS()->getType().getCanonicalType()->isDependentType()))) ||
       (BinOp && (!BinOp->isCompoundAssignmentOp() && !BinOp->isShiftAssignOp()) && RHSBinOp &&
        (BinOp->getOpcode() != BO_Assign ||
         (!RHSBinOp->isAdditiveOp() &&
          RHSBinOp->getOpcode() != BO_Mul &&
          RHSBinOp->getOpcode() != BO_Div &&
          !RHSBinOp->isBitwiseOp() &&
          !RHSBinOp->isShiftOp()))) ||
       (BinOp && !RHSBinOp &&
        ((!BinOp->isCompoundAssignmentOp() &&
          !BinOp->isShiftAssignOp() &&
          BinOp->getOpcode() != BO_Assign) ||
         BinOp->getOpcode() == BO_RemAssign));
    if (!WrongStmt && UnOp) {
      OpExpr = ActOnIntegerConstant(BaseStmt->getLocStart(), 1).take();
      if (UnOp->isIncrementOp())
        Op = BO_Add;
      else
        Op = BO_Sub;
    } else if (!WrongStmt && BinOp && !RHSBinOp && BinOp->getOpcode() == BO_Assign) {
        Op = BO_Assign;
        OpExpr = BinOp->getRHS();
    } else if (!WrongStmt && BinOp &&
               (BinOp->isCompoundAssignmentOp() || BinOp->isShiftAssignOp())) {
      ExprUseChecker UseCheckX(XID, Context);
      ExprUseChecker UseCheckV(VID, Context);
      WrongStmt = UseCheckX.Visit(BinOp->getRHS()->IgnoreParenCasts()) ||
                  UseCheckV.Visit(BinOp->getRHS()->IgnoreParenCasts());
      if (!WrongStmt) {
        OpExpr = BinOp->getRHS();
        switch (BinOp->getOpcode()) {
        case BO_AddAssign:
          Op = BO_Add;
          break;
        case BO_MulAssign:
          Op = BO_Mul;
          break;
        case BO_SubAssign:
          Op = BO_Sub;
          break;
        case BO_DivAssign:
          Op = BO_Div;
          break;
        case BO_AndAssign:
          Op = BO_And;
          break;
        case BO_XorAssign:
          Op = BO_Xor;
          break;
        case BO_OrAssign:
          Op = BO_Or;
          break;
        case BO_ShlAssign:
          Op = BO_Shl;
          break;
        case BO_ShrAssign:
          Op = BO_Shr;
          break;
        default:
          WrongStmt = true;
          break;
        }
      }
    } else if (!WrongStmt && RHSBinOp) {
      llvm::FoldingSetNodeID ID;
      RHSBinOp->getLHS()->IgnoreParenCasts()->Profile(ID, Context, true);
      if (XID == ID) {
        ExprUseChecker UseCheckX(XID, Context);
        ExprUseChecker UseCheckV(VID, Context);
        WrongStmt = UseCheckX.Visit(RHSBinOp->getRHS()->IgnoreParenCasts()) ||
                    UseCheckV.Visit(RHSBinOp->getRHS()->IgnoreParenCasts());
        if (!WrongStmt) {
          OpExpr = RHSBinOp->getRHS();
          Op = RHSBinOp->getOpcode();
        }
      } else {
        ID.clear();
        RHSBinOp->getRHS()->IgnoreParenCasts()->Profile(ID, Context, true);
        if (XID == ID) {
          ExprUseChecker UseCheckX(XID, Context);
          ExprUseChecker UseCheckV(VID, Context);
          WrongStmt = UseCheckX.Visit(RHSBinOp->getLHS()->IgnoreParenCasts()) ||
                      UseCheckV.Visit(RHSBinOp->getLHS()->IgnoreParenCasts());
          if (!WrongStmt) {
            OpExpr = RHSBinOp->getLHS();
            Op = RHSBinOp->getOpcode();
            Reversed = true;
          }
        } else
          WrongStmt = true;
      }
    }
    }
    break;
  default:
    break;
  }
  if (WrongStmt) {
    Diag(BaseStmt->getLocStart(), diag::err_omp_atomic_wrong_statement)
      << getOpenMPClauseName(Kind);
    return StmtError();
  }
//  if (OpExpr && !X->getType()->isDependentType() &&
//      !OpExpr->getType()->isDependentType()) {
//    ExprResult Res = Owned(OpExpr);
//    CastKind CK = PrepareScalarCast(Res, X->getType());
//    if (CK != CK_NoOp)
//      OpExpr = ImpCastExprToType(Res.take(), X->getType(), CK).take();
//  }
//  if (V && !V->getType()->isDependentType()) {
//    ExprResult Res = Owned(X);
//    CastKind CK = PrepareScalarCast(Res, V->getType());
//    if (CK != CK_NoOp)
//      X = ImpCastExprToType(Res.take(), V->getType(), CK).take();
//  }

  getCurFunction()->setHasBranchProtectedScope();


  return Owned(OMPAtomicDirective::Create(Context, StartLoc, EndLoc,
                                          Clauses, AStmt, V, X, OpExpr, Op,
                                          CaptureAfter, Reversed));
}

StmtResult Sema::ActOnOpenMPFlushDirective(ArrayRef<OMPClause *> Clauses,
                                           SourceLocation StartLoc,
                                           SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPFlushDirective::Create(Context, StartLoc, EndLoc,
                                         Clauses));
}

StmtResult Sema::ActOnOpenMPOrderedDirective(Stmt *AStmt,
                                             SourceLocation StartLoc,
                                             SourceLocation EndLoc) {
  getCurFunction()->setHasBranchProtectedScope();

  return Owned(OMPOrderedDirective::Create(Context, StartLoc, EndLoc, AStmt));
}

OMPClause *Sema::ActOnOpenMPSingleExprClause(OpenMPClauseKind Kind,
                                             Expr *Expr,
                                             SourceLocation StartLoc,
                                             SourceLocation EndLoc) {
  OMPClause *Res = 0;
  switch (Kind) {
  case OMPC_if:
    Res = ActOnOpenMPIfClause(Expr, StartLoc, EndLoc);
    break;
  case OMPC_num_threads:
    Res = ActOnOpenMPNumThreadsClause(Expr, StartLoc, EndLoc);
    break;
  case OMPC_collapse:
    Res = ActOnOpenMPCollapseClause(Expr, StartLoc, EndLoc);
    break;
  case OMPC_final:
    Res = ActOnOpenMPFinalClause(Expr, StartLoc, EndLoc);
    break;
  default:
    break;
  }
  return Res;
}

OMPClause *Sema::ActOnOpenMPIfClause(Expr *Condition,
                                     SourceLocation StartLoc,
                                     SourceLocation EndLoc) {
  ExprResult Val = ActOnBooleanCondition(DSAStack->getCurScope(), Condition->getExprLoc(),
                                         Condition);
  if (Val.isInvalid())
    return 0;

  Expr *ValExpr = Val.take();
  if (!ValExpr->isEvaluatable(Context)) {
    SourceLocation ELoc = ValExpr->getExprLoc();
    QualType QTy = ValExpr->getType().getUnqualifiedType().getCanonicalType();
    IdentifierInfo *Id = &Context.Idents.get(".omp.if.var.");
    DeclarationName DN(Id);
    VarDecl *PseudoVar;
    DeclContext *DC = Context.getTranslationUnitDecl();
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(QTy,
                                                          ELoc);
    PseudoVar =
            VarDecl::Create(Context, DC, SourceLocation(),
                            SourceLocation(), Id, QTy, TI,
                            SC_Static);
    PseudoVar->setImplicit();
    PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    ExprResult Init = ActOnIntegerConstant(SourceLocation(), 0);
    CastKind CK = PrepareScalarCast(Init, QTy);
    if (CK != CK_NoOp) {
      Init = ImpCastExprToType(Init.take(), QTy, CK);
    }
    PseudoVar->setInit(Init.take());
    Expr *DRE = BuildDeclRefExpr(PseudoVar, QTy, VK_LValue, ELoc).take();
    Expr *Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, DRE, ValExpr).take();
    ValExpr = ImpCastExprToType(DRE, QTy, CK_LValueToRValue).take();
    AdditionalOpenMPStmt.push_back(ImpCastExprToType(Res, Context.VoidTy, CK_ToVoid).take());
    Consumer.HandleTopLevelDecl(DeclGroupRef(PseudoVar));
  }

  return new (Context) OMPIfClause(ValExpr, StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPFinalClause(Expr *Condition,
                                        SourceLocation StartLoc,
                                        SourceLocation EndLoc) {
  ExprResult Val = ActOnBooleanCondition(DSAStack->getCurScope(), Condition->getExprLoc(),
                                         Condition);
  if (Val.isInvalid())
    return 0;

  Expr *ValExpr = Val.take();
  if (!ValExpr->isEvaluatable(Context)) {
    SourceLocation ELoc = ValExpr->getExprLoc();
    QualType QTy = ValExpr->getType().getUnqualifiedType().getCanonicalType();
    IdentifierInfo *Id = &Context.Idents.get(".omp.final.var.");
    DeclarationName DN(Id);
    VarDecl *PseudoVar;
    DeclContext *DC = Context.getTranslationUnitDecl();
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(QTy,
                                                          ELoc);
    PseudoVar =
            VarDecl::Create(Context, DC, SourceLocation(),
                            SourceLocation(), Id, QTy, TI,
                            SC_Static);
    PseudoVar->setImplicit();
    PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    ExprResult Init = ActOnIntegerConstant(SourceLocation(), 0);
    CastKind CK = PrepareScalarCast(Init, QTy);
    if (CK != CK_NoOp) {
      Init = ImpCastExprToType(Init.take(), QTy, CK);
    }
    PseudoVar->setInit(Init.take());
    Expr *DRE = BuildDeclRefExpr(PseudoVar, QTy, VK_LValue, ELoc).take();
    Expr *Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, DRE, ValExpr).take();
    ValExpr = ImpCastExprToType(DRE, QTy, CK_LValueToRValue).take();
    AdditionalOpenMPStmt.push_back(ImpCastExprToType(Res, Context.VoidTy, CK_ToVoid).take());
    Consumer.HandleTopLevelDecl(DeclGroupRef(PseudoVar));
  }

  return new (Context) OMPFinalClause(ValExpr, StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPNumThreadsClause(Expr *NumThreads,
                                             SourceLocation StartLoc,
                                             SourceLocation EndLoc) {
  class CConvertDiagnoser : public ICEConvertDiagnoser {
  public:
    CConvertDiagnoser() : ICEConvertDiagnoser(false, true) { }
    virtual DiagnosticBuilder diagnoseNotInt(Sema &S, SourceLocation Loc,
                                             QualType T) {
      return S.Diag(Loc, diag::err_typecheck_statement_requires_integer) << T;
    }
    virtual DiagnosticBuilder diagnoseIncomplete(Sema &S,
                                                 SourceLocation Loc,
                                                 QualType T) {
      return S.Diag(Loc, diag::err_incomplete_class_type) << T;
    }
    virtual DiagnosticBuilder diagnoseExplicitConv(Sema &S,
                                                   SourceLocation Loc,
                                                   QualType T,
                                                   QualType ConvTy) {
      return S.Diag(Loc, diag::err_explicit_conversion) << T << ConvTy;
    }

    virtual DiagnosticBuilder noteExplicitConv(Sema &S,
                                               CXXConversionDecl *Conv,
                                               QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_conversion) <<
        ConvTy->isEnumeralType() << ConvTy;
    }
    virtual DiagnosticBuilder diagnoseAmbiguous(Sema &S, SourceLocation Loc,
                                                QualType T) {
      return S.Diag(Loc, diag::err_multiple_conversions) << T;
    }

    virtual DiagnosticBuilder noteAmbiguous(Sema &S,
                                            CXXConversionDecl *Conv,
                                            QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_conversion) <<
        ConvTy->isEnumeralType() << ConvTy;
    }

    virtual DiagnosticBuilder diagnoseConversion(Sema &S,
                                                 SourceLocation Loc,
                                                 QualType T,
                                                 QualType ConvTy) {
      return DiagnosticBuilder::getEmpty();
    }
  } ConvertDiagnoser;

  if (!NumThreads)
    return 0;

  SourceLocation Loc = NumThreads->getExprLoc();
  ExprResult Value = ConvertToIntegralOrEnumerationType(Loc,
                                                        NumThreads,
                                                        ConvertDiagnoser,
                                                        true);
  if (Value.isInvalid())
    return 0;

  llvm::APSInt Result;
  if (Value.get()->isIntegerConstantExpr(Result, Context) &&
      !Result.isStrictlyPositive()) {
    Diag(Loc, diag::err_negative_expression_in_clause)
      << NumThreads->getSourceRange();
    return 0;
  }
  return new (Context) OMPNumThreadsClause(Value.take(), StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPCollapseClause(Expr *NumLoops,
                                           SourceLocation StartLoc,
                                           SourceLocation EndLoc) {
  if (!NumLoops)
    return 0;

  llvm::APSInt Result;
  ExprResult ICE = VerifyIntegerConstantExpression(NumLoops, &Result);
  if (ICE.isInvalid())
    return 0;

  if (!Result.isStrictlyPositive()) {
    Diag(NumLoops->getExprLoc(), diag::err_negative_expression_in_clause)
      << NumLoops->getSourceRange();
    return 0;
  }
  Expr *Val = IntegerLiteral::Create(Context, Result,
                                     ICE.get()->getType().getNonReferenceType(),
                                     NumLoops->getExprLoc());
  return new (Context) OMPCollapseClause(Val, StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPSimpleClause(OpenMPClauseKind Kind,
                                         unsigned Argument,
                                         SourceLocation ArgumentLoc,
                                         SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  OMPClause *Res = 0;
  switch (Kind) {
  case OMPC_default:
    Res = ActOnOpenMPDefaultClause(
                             static_cast<OpenMPDefaultClauseKind>(Argument),
                             ArgumentLoc, StartLoc, EndLoc);
    break;
  default:
    break;
  }
  return Res;
}

OMPClause *Sema::ActOnOpenMPDefaultClause(OpenMPDefaultClauseKind Kind,
                                          SourceLocation KindLoc,
                                          SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  if (Kind == OMPC_DEFAULT_unknown) {
    std::string Values;
    std::string Sep(NUM_OPENMP_DEFAULT_KINDS > 1 ? ", " : "");
    for (unsigned i = OMPC_DEFAULT_unknown + 1;
         i < NUM_OPENMP_DEFAULT_KINDS; ++i) {
      Values += "'";
      Values += getOpenMPSimpleClauseTypeName(OMPC_default, i);
      Values += "'";
      switch (i) {
      case NUM_OPENMP_DEFAULT_KINDS - 2:
        Values += " or ";
        break;
      case NUM_OPENMP_DEFAULT_KINDS - 1:
        break;
      default:
        Values += Sep;
        break;
      }
    }
    Diag(KindLoc, diag::err_omp_unexpected_clause_value)
      << Values << getOpenMPClauseName(OMPC_default);
    return 0;
  }
  switch (Kind) {
  case OMPC_DEFAULT_none:
    DSAStack->setDefaultDSANone();
    break;
  case OMPC_DEFAULT_shared:
    DSAStack->setDefaultDSAShared();
    break;
  default:
    break;
  }
  return new (Context) OMPDefaultClause(Kind, KindLoc, StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPClause(OpenMPClauseKind Kind,
                                   SourceLocation StartLoc,
                                   SourceLocation EndLoc) {
  OMPClause *Res = 0;
  switch (Kind) {
  case OMPC_ordered:
    Res = ActOnOpenMPOrderedClause(StartLoc, EndLoc);
    break;
  case OMPC_nowait:
    Res = ActOnOpenMPNowaitClause(StartLoc, EndLoc);
    break;
  case OMPC_untied:
    Res = ActOnOpenMPUntiedClause(StartLoc, EndLoc);
    break;
  case OMPC_mergeable:
    Res = ActOnOpenMPMergeableClause(StartLoc, EndLoc);
    break;
  case OMPC_read:
    Res = ActOnOpenMPReadClause(StartLoc, EndLoc);
    break;
  case OMPC_write:
    Res = ActOnOpenMPWriteClause(StartLoc, EndLoc);
    break;
  case OMPC_update:
    Res = ActOnOpenMPUpdateClause(StartLoc, EndLoc);
    break;
  case OMPC_capture:
    Res = ActOnOpenMPCaptureClause(StartLoc, EndLoc);
    break;
  case OMPC_seq_cst:
    Res = ActOnOpenMPSeqCstClause(StartLoc, EndLoc);
    break;
  default:
    break;
  }
  return Res;
}

OMPClause *Sema::ActOnOpenMPOrderedClause(SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  DSAStack->setOrdered();
  return new (Context) OMPOrderedClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPNowaitClause(SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  return new (Context) OMPNowaitClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPUntiedClause(SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  return new (Context) OMPUntiedClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPMergeableClause(SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  return new (Context) OMPMergeableClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPSingleExprWithTypeClause(OpenMPClauseKind Kind,
                                                     unsigned Argument,
                                                     SourceLocation ArgumentLoc,
                                                     Expr *Expr,
                                                     SourceLocation StartLoc,
                                                     SourceLocation EndLoc) {
  OMPClause *Res = 0;
  switch (Kind) {
  case OMPC_schedule:
    Res = ActOnOpenMPScheduleClause(static_cast<OpenMPScheduleClauseKind>(Argument),
                                    ArgumentLoc, Expr, StartLoc, EndLoc);
    break;
  default:
    break;
  }
  return Res;
}

OMPClause *Sema::ActOnOpenMPScheduleClause(OpenMPScheduleClauseKind Kind,
                                           SourceLocation KindLoc,
                                           Expr *ChunkSize,
                                           SourceLocation StartLoc,
                                           SourceLocation EndLoc) {
  class CConvertDiagnoser : public ICEConvertDiagnoser {
  public:
    CConvertDiagnoser() : ICEConvertDiagnoser(false, true) { }
    virtual DiagnosticBuilder diagnoseNotInt(Sema &S, SourceLocation Loc,
                                             QualType T) {
      return S.Diag(Loc, diag::err_typecheck_statement_requires_integer) << T;
    }
    virtual DiagnosticBuilder diagnoseIncomplete(Sema &S,
                                                 SourceLocation Loc,
                                                 QualType T) {
      return S.Diag(Loc, diag::err_incomplete_class_type) << T;
    }
    virtual DiagnosticBuilder diagnoseExplicitConv(Sema &S,
                                                   SourceLocation Loc,
                                                   QualType T,
                                                   QualType ConvTy) {
      return S.Diag(Loc, diag::err_explicit_conversion) << T << ConvTy;
    }

    virtual DiagnosticBuilder noteExplicitConv(Sema &S,
                                               CXXConversionDecl *Conv,
                                               QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_conversion) <<
        ConvTy->isEnumeralType() << ConvTy;
    }
    virtual DiagnosticBuilder diagnoseAmbiguous(Sema &S, SourceLocation Loc,
                                                QualType T) {
      return S.Diag(Loc, diag::err_multiple_conversions) << T;
    }

    virtual DiagnosticBuilder noteAmbiguous(Sema &S,
                                            CXXConversionDecl *Conv,
                                            QualType ConvTy) {
      return S.Diag(Conv->getLocation(), diag::note_conversion) <<
        ConvTy->isEnumeralType() << ConvTy;
    }

    virtual DiagnosticBuilder diagnoseConversion(Sema &S,
                                                 SourceLocation Loc,
                                                 QualType T,
                                                 QualType ConvTy) {
      return DiagnosticBuilder::getEmpty();
    }
  } ConvertDiagnoser;

  if (Kind == OMPC_SCHEDULE_unknown) {
    std::string Values;
    std::string Sep(NUM_OPENMP_SCHEDULE_KINDS > 1 ? ", " : "");
    for (int i = OMPC_SCHEDULE_unknown + 1;
         i < NUM_OPENMP_SCHEDULE_KINDS; ++i) {
      Values += "'";
      Values += getOpenMPSimpleClauseTypeName(OMPC_schedule, i);
      Values += "'";
      switch (i) {
      case NUM_OPENMP_SCHEDULE_KINDS - 2:
        Values += " or ";
        break;
      case NUM_OPENMP_SCHEDULE_KINDS - 1:
        break;
      default:
        Values += Sep;
        break;
      }
    }
    Diag(KindLoc, diag::err_omp_unexpected_clause_value)
      << Values << getOpenMPClauseName(OMPC_schedule);
    return 0;
  }
  ExprResult Value;
  if (ChunkSize) {
    SourceLocation Loc = ChunkSize->getExprLoc();
    Value = ConvertToIntegralOrEnumerationType(Loc, ChunkSize,
                                               ConvertDiagnoser, true);
    if (Value.isInvalid())
      return 0;

    llvm::APSInt Result;
    if (Value.get()->isIntegerConstantExpr(Result, Context) &&
        !Result.isStrictlyPositive()) {
      Diag(Loc, diag::err_negative_expression_in_clause)
        << ChunkSize->getSourceRange();
      return 0;
    }
  } else {
    // OpenMP [2.5.1, Loop Construct, Description, Table 2-1]
    //  dynamic       When no chunk_size is specified, it defaults to 1.
    //  guided        When no chunk_size is specified, it defaults to 1.
    switch (Kind) {
      case OMPC_SCHEDULE_dynamic:
      case OMPC_SCHEDULE_guided:
        Value = ActOnIntegerConstant(StartLoc, 1);
        break;
      default:
        break;
    }
  }
  Expr *ValExpr = Value.take();
  if (ValExpr && !ValExpr->isEvaluatable(Context)) {
    SourceLocation ELoc = ValExpr->getExprLoc();
    QualType QTy = ValExpr->getType().getUnqualifiedType().getCanonicalType();
    IdentifierInfo *Id = &Context.Idents.get(".omp.schedule.var.");
    DeclarationName DN(Id);
    VarDecl *PseudoVar;
    DeclContext *DC = Context.getTranslationUnitDecl();
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(QTy,
                                                          ELoc);
    PseudoVar =
            VarDecl::Create(Context, DC, SourceLocation(),
                            SourceLocation(), Id, QTy, TI,
                            SC_Static);
    PseudoVar->setImplicit();
    PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    ExprResult Init = ActOnIntegerConstant(SourceLocation(), 0);
    CastKind CK = PrepareScalarCast(Init, QTy);
    if (CK != CK_NoOp) {
      Init = ImpCastExprToType(Init.take(), QTy, CK);
    }
    PseudoVar->setInit(Init.take());
    Expr *DRE = BuildDeclRefExpr(PseudoVar, QTy, VK_LValue, ELoc).take();
    Expr *Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, DRE, ValExpr).take();
    ValExpr = ImpCastExprToType(DRE, QTy, CK_LValueToRValue).take();
    AdditionalOpenMPStmt.push_back(ImpCastExprToType(Res, Context.VoidTy, CK_ToVoid).take());
    Consumer.HandleTopLevelDecl(DeclGroupRef(PseudoVar));
  }

  return new (Context) OMPScheduleClause(Kind, KindLoc, ValExpr,
                                         StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPVarListClause(OpenMPClauseKind Kind,
                                          ArrayRef<Expr *> VarList,
                                          SourceLocation StartLoc,
                                          SourceLocation EndLoc,
                                          unsigned Op,
                                          SourceLocation OpLoc) {
  OMPClause *Res = 0;
  switch (Kind) {
  case OMPC_private:
    Res = ActOnOpenMPPrivateClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_lastprivate:
    Res = ActOnOpenMPLastPrivateClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_firstprivate:
    Res = ActOnOpenMPFirstPrivateClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_shared:
    Res = ActOnOpenMPSharedClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_copyin:
    Res = ActOnOpenMPCopyinClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_copyprivate:
    Res = ActOnOpenMPCopyPrivateClause(VarList, StartLoc, EndLoc);
    break;
  case OMPC_reduction:
    Res = ActOnOpenMPReductionClause(
                            VarList, StartLoc, EndLoc,
                            static_cast<OpenMPReductionClauseOperator>(Op),
                            OpLoc);
    break;
  case OMPC_flush:
    Res = ActOnOpenMPFlushClause(VarList, StartLoc, EndLoc);
    break;
  default:
    break;
  }
  return Res;
}

OMPClause *Sema::ActOnOpenMPPrivateClause(ArrayRef<Expr *> VarList,
                                          SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  SmallVector<Expr *, 8> DefaultInits;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      DefaultInits.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    // OpenMP  [2.9.3.3, Restrictions, p.1]
    //  A variable that is part of another variable (as an array or
    //  structure element) cannot appear in a private clause.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.9.3.3, Restrictions, C/C++, p.3]
    //  A variable that appears in a private clause must not have an incomplete
    //  type or a reference type.
    if (RequireCompleteType(ELoc, VD->getType(),
                            diag::err_omp_private_incomplete_type)) {
      continue;
    }
    if (VD->getType()->isReferenceType()) {
      Diag(ELoc, diag::err_omp_clause_ref_type_arg)
        << getOpenMPClauseName(OMPC_private);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct]
    //  Variables with the predetermined data-sharing attributes may not be
    //  listed in data-sharing attributes clauses, except for the cases
    //  listed below. For these exceptions only, listing a predetermined
    //  variable in a data-sharing attribute clause is allowed and overrides
    //  the variable's predetermined data-sharing attributes.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    if (Kind != OMPC_unknown && Kind != OMPC_private) {
      Diag(ELoc, diag::err_omp_wrong_dsa)
         << getOpenMPClauseName(Kind)
         << getOpenMPClauseName(OMPC_private);
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      } else {
        Diag(VD->getLocation(), diag::note_omp_predetermined_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }

    // OpenMP [2.9.3.3, Restrictions, C/C++, p.1]
    //  A variable of class type (or array thereof) that appears in a private
    //  clause requires an accesible, unambiguous default constructor for the
    //  class type.
    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      CXXConstructorDecl *CD = LookupDefaultConstructor(RD);
      PartialDiagnostic PD =
        PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
      if (!CD ||
          CheckConstructorAccess(ELoc, CD,
                                 InitializedEntity::InitializeTemporary(Type),
                                 CD->getAccess(), PD) == AR_inaccessible ||
          CD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_private) << 0;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, CD);
      DiagnoseUseOfDecl(CD, ELoc);

      CXXDestructorDecl *DD = RD->getDestructor();
      if (DD &&
          (CheckDestructorAccess(ELoc, DD, PD) == AR_inaccessible ||
           DD->isDeleted())) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_private) << 4;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(),
             IsDecl ? diag::note_previous_decl :
                      diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      } else if (DD) {
        MarkFunctionReferenced(ELoc, DD);
        DiagnoseUseOfDecl(DD, ELoc);
      }
    }
    Type = Type.getUnqualifiedType();
    IdentifierInfo *Id = &Context.Idents.get(".private.");
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(Type,
                                                          ELoc);
    VarDecl *PseudoVar =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id, Type, TI,
                            SC_Static);
    PseudoVar->setImplicit();
    PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    InitializedEntity Entity = InitializedEntity::InitializeVariable(PseudoVar);
    InitializationKind InitKind = InitializationKind::CreateDefault(ELoc);
    InitializationSequence InitSeq(*this, Entity, InitKind, MultiExprArg());
    ExprResult Res = InitSeq.Perform(*this, Entity, InitKind, MultiExprArg());
    if (Res.isInvalid()) continue;
    DefaultInits.push_back(Res.take());
    DSAStack->addDSA(VD, DE, OMPC_private);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPPrivateClause::Create(Context, StartLoc, EndLoc, Vars, DefaultInits);
}

OMPClause *Sema::ActOnOpenMPFirstPrivateClause(ArrayRef<Expr *> VarList,
                                               SourceLocation StartLoc,
                                               SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  SmallVector<DeclRefExpr *, 8> PseudoVars;
  SmallVector<Expr *, 8> Inits;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      PseudoVars.push_back(0);
      Inits.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    // OpenMP  [2.9.3.4, Restrictions, p.1]
    //  A variable that is part of another variable (as an array or
    //  structure element) cannot appear in a private clause.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.9.3.4, Restrictions, C/C++, p.2]
    //  A variable that appears in a firstprivate clause must not have an
    //  incomplete type or a reference type.
    if (RequireCompleteType(ELoc, VD->getType(),
                            diag::err_omp_firstprivate_incomplete_type)) {
      continue;
    }
    if (VD->getType()->isReferenceType()) {
      Diag(ELoc, diag::err_omp_clause_ref_type_arg)
        << getOpenMPClauseName(OMPC_firstprivate);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct]
    //  Variables with the predetermined data-sharing attributes may not be
    //  listed in data-sharing attributes clauses, except for the cases
    //  listed below. For these exceptions only, listing a predetermined
    //  variable in a data-sharing attribute clause is allowed and overrides
    //  the variable's predetermined data-sharing attributes.
    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct, C/C++, p.2]
    //  Variables with const-qualified type having no mutable member may be
    //  listed in a firstprivate clause, even if they are static data members.
    // OpenMP [2.9.3.4, Description]
    //  If a list item appears in both firstprivate and lastprivate clauses,
    //  the update requires for lastprivate occurs after all the initializations
    //  for firstprivate.
    DeclRefExpr *PrevRef;
    OpenMPDirectiveKind CurrDir = DSAStack->getCurrentDirective();
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    bool IsConstant = Type.isConstant(Context);
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    if (Kind != OMPC_unknown && Kind != OMPC_firstprivate &&
        Kind != OMPC_lastprivate &&
        !(Kind == OMPC_shared && !PrevRef &&
          (IsConstant || VD->isStaticDataMember()))) {
      if ((CurrDir != OMPD_task || PrevRef) &&
          StartLoc.isValid() && EndLoc.isValid()) {
        Diag(ELoc, diag::err_omp_wrong_dsa)
           << getOpenMPClauseName(Kind)
           << getOpenMPClauseName(OMPC_firstprivate);
        if (PrevRef) {
          Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
               << getOpenMPClauseName(Kind);
        } else {
          Diag(VD->getLocation(), diag::note_omp_predetermined_dsa)
               << getOpenMPClauseName(Kind);
        }
        continue;
      }
    }

    // OpenMP [2.9.3.4, Restrictions, p.2]
    //  A list item that is private within a parallel region must not appear in
    //  a firstprivate clause on a worksharing construct if any of the
    //  worksharing regions arising from the worksharing construct ever bind to
    //  any of the parallel regions arising from the parallel construct.
    // OpenMP [2.9.3.4, Restrictions, p.3]
    //  A list item that appears in a reduction clause of a parallel construct
    //  must not appear in a firstprivate clause on a worksharing or task
    //  construct if any of the worksharing or task regions arising from the
    //  worksharing or task construct ever bind to any of the parallel regions
    //  arising from the parallel construct.
    // OpenMP [2.9.3.4, Restrictions, p.4]
    //  A list item that appears in a reduction clause in worksharing construct
    //  must not appear in a firstprivate clause in a task construct encountered
    //  during execution of any of the worksharing regions arising from the
    //  worksharing construct.
    OpenMPDirectiveKind DKind;
    Kind = DSAStack->getImplicitDSA(VD, DKind, PrevRef);
    if ((Kind != OMPC_shared && (CurrDir == OMPD_for ||
                                 CurrDir == OMPD_sections ||
                                 CurrDir == OMPD_parallel_for ||
                                 CurrDir == OMPD_parallel_sections ||
                                 CurrDir == OMPD_single)) ||
        (CurrDir == OMPD_task &&
         DSAStack->hasDSA(VD, OMPC_reduction, OMPD_parallel, PrevRef))) {
      if (Kind == OMPC_unknown) {
        Diag(ELoc, diag::err_omp_required_access)
          << getOpenMPClauseName(OMPC_firstprivate)
          << getOpenMPClauseName(OMPC_shared);
        if (PrevRef) {
          Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
               << getOpenMPClauseName(Kind);
        }
        continue;
      }
      else if (DKind == OMPD_unknown) {
        Diag(ELoc, diag::err_omp_wrong_dsa)
           << getOpenMPClauseName(Kind)
           << getOpenMPClauseName(OMPC_firstprivate);
        if (PrevRef) {
          Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
               << getOpenMPClauseName(Kind);
        }
        continue;
      } else {
        // Skip template instations for parallel for and parallel sections.
        if (Kind != OMPC_firstprivate || DKind != OMPD_parallel ||
            (CurrDir != OMPD_for && CurrDir != OMPD_sections) || !PrevRef ||
            PrevRef->getExprLoc() != ELoc) {
          Diag(ELoc, diag::err_omp_dsa_with_directives)
             << getOpenMPClauseName(Kind)
             << getOpenMPDirectiveName(DKind)
             << getOpenMPClauseName(OMPC_firstprivate)
             << getOpenMPDirectiveName(CurrDir);
          if (PrevRef) {
            Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
                 << getOpenMPClauseName(Kind);
          }
          continue;
        }
      }
    }

    // OpenMP [2.9.3.4, Restrictions, C/C++, p.1]
    //  A variable of class type (or array thereof) that appears in a
    //  firstprivate clause requires an accesible, unambiguous copy constructor
    //  for the class type.
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      CXXConstructorDecl *CD = LookupCopyingConstructor(RD, 0);
      PartialDiagnostic PD =
        PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
      if (!CD ||
          CheckConstructorAccess(ELoc, CD,
                                 InitializedEntity::InitializeTemporary(Type),
                                 CD->getAccess(), PD) == AR_inaccessible ||
          CD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_firstprivate) << 1;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, CD);
      DiagnoseUseOfDecl(CD, ELoc);

      CXXDestructorDecl *DD = RD->getDestructor();
      if (DD &&
          (CheckDestructorAccess(ELoc, DD, PD) == AR_inaccessible ||
           DD->isDeleted())) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_firstprivate) << 4;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(),
             IsDecl ? diag::note_previous_decl :
                      diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      } else if (DD) {
        MarkFunctionReferenced(ELoc, DD);
        DiagnoseUseOfDecl(DD, ELoc);
      }
    }

    Type = Type.getUnqualifiedType();
    if (RD && !RD->isTriviallyCopyable()) {
      DeclRefExpr *PseudoDE = DE;
      IdentifierInfo *Id = &Context.Idents.get(".firstprivate.");
      TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(Type,
                                                            ELoc);
      VarDecl *PseudoVar =
              VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                              SourceLocation(), Id, Type, TI,
                              SC_Static);
      PseudoVar->setImplicit();
      PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
      Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar);
      PseudoDE = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar,
                                                    Type,
                                                    VK_LValue,
                                                    ELoc).take());
      InitializedEntity Entity = InitializedEntity::InitializeVariable(PseudoVar);
      InitializationKind InitKind = InitializationKind::CreateCopy(ELoc, ELoc);
      Expr *Arg = ImpCastExprToType(PseudoDE, PseudoDE->getType(), CK_LValueToRValue).take();
      InitializationSequence InitSeq(*this, Entity, InitKind, MultiExprArg(&Arg, 1));
      ExprResult Res = InitSeq.Perform(*this, Entity, InitKind, MultiExprArg(&Arg, 1));
      if (Res.isInvalid()) continue;
      PseudoVars.push_back(PseudoDE);
      Inits.push_back(Res.take());
    } else {
      PseudoVars.push_back(0);
      Inits.push_back(0);
    }
    DSAStack->addDSA(VD, DE, OMPC_firstprivate);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPFirstPrivateClause::Create(Context, StartLoc, EndLoc, Vars, PseudoVars, Inits);
}

OMPClause *Sema::ActOnOpenMPLastPrivateClause(ArrayRef<Expr *> VarList,
                                              SourceLocation StartLoc,
                                              SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  SmallVector<DeclRefExpr *, 8> PseudoVars1;
  SmallVector<DeclRefExpr *, 8> PseudoVars2;
  SmallVector<Expr *, 8> Assignments;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      PseudoVars1.push_back(0);
      PseudoVars2.push_back(0);
      Assignments.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    // OpenMP  [2.11.3.5, Restrictions, p.1]
    //  A variable that is part of another variable (as an array or
    //  structure element) cannot appear in a private clause.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.9.3.11, Restrictions, C/C++, p.4]
    //  A variable that appears in a firstprivate clause must not have an
    //  incomplete type or a reference type.
    if (RequireCompleteType(ELoc, VD->getType(),
                            diag::err_omp_lastprivate_incomplete_type)) {
      continue;
    }
    if (VD->getType()->isReferenceType()) {
      Diag(ELoc, diag::err_omp_clause_ref_type_arg)
        << getOpenMPClauseName(OMPC_lastprivate);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct]
    //  Variables with the predetermined data-sharing attributes may not be
    //  listed in data-sharing attributes clauses, except for the cases
    //  listed below. For these exceptions only, listing a predetermined
    //  variable in a data-sharing attribute clause is allowed and overrides
    //  the variable's predetermined data-sharing attributes.
    // OpenMP [2.9.3.4, Description]
    //  If a list item appears in both firstprivate and lastprivate clauses,
    //  the update requires for lastprivate occurs after all the initializations
    //  for firstprivate.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    if (Kind != OMPC_unknown && Kind != OMPC_firstprivate &&
        Kind != OMPC_lastprivate) {
      Diag(ELoc, diag::err_omp_wrong_dsa)
         << getOpenMPClauseName(Kind)
         << getOpenMPClauseName(OMPC_lastprivate);
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      } else {
        Diag(VD->getLocation(), diag::note_omp_predetermined_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }
    bool IsNotFirstprivate = Kind != OMPC_firstprivate;

    // OpenMP [2.9.3.5, Restrictions, p.2]
    //  A list item that is private within a parallel region, or that appears
    //  in the reduction clause of a parallel construct,  must not appear in
    //  a lastprivate clause on a worksharing construct if any of the
    //  worksharing regions ever bind to any of the correspponding parallel
    //  regions.
    OpenMPDirectiveKind DKind;
    OpenMPDirectiveKind CurrDir = DSAStack->getCurrentDirective();
    Kind = DSAStack->getImplicitDSA(VD, DKind, PrevRef);
    if (Kind != OMPC_shared && (CurrDir == OMPD_for ||
                                CurrDir == OMPD_sections ||
                                CurrDir == OMPD_parallel_for ||
                                CurrDir == OMPD_parallel_sections)) {
      if (Kind == OMPC_unknown) {
        Diag(ELoc, diag::err_omp_required_access)
          << getOpenMPClauseName(OMPC_lastprivate)
          << getOpenMPClauseName(OMPC_shared);
      }
      else if (DKind == OMPD_unknown) {
        Diag(ELoc, diag::err_omp_wrong_dsa)
           << getOpenMPClauseName(Kind)
           << getOpenMPClauseName(OMPC_lastprivate);
      } else {
        Diag(ELoc, diag::err_omp_dsa_with_directives)
           << getOpenMPClauseName(Kind)
           << getOpenMPDirectiveName(DKind)
           << getOpenMPClauseName(OMPC_lastprivate)
           << getOpenMPDirectiveName(CurrDir);
      }
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }

    // OpenMP [2.9.3.5, Restrictions, C/C++, p.2]
    //  A variable of class type (or array thereof) that appears in a
    //  lastprivate clause requires an accesible, unambiguous copy assignment
    //  operator for the class type.
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      CXXMethodDecl *MD = LookupCopyingAssignment(RD, 0, false, 0);
      if (!MD || CheckMemberAccess(ELoc, RD, MD) == AR_inaccessible ||
          MD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_lastprivate) << 2;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, MD);
      DiagnoseUseOfDecl(MD, ELoc);
      PartialDiagnostic PD =
        PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
      CXXDestructorDecl *DD = RD->getDestructor();
      if (DD &&
          (CheckDestructorAccess(ELoc, DD, PD) == AR_inaccessible ||
           DD->isDeleted())) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_lastprivate) << 4;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(),
             IsDecl ? diag::note_previous_decl :
                      diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      } else if (DD) {
        MarkFunctionReferenced(ELoc, DD);
        DiagnoseUseOfDecl(DD, ELoc);
      }
    }

    Type = Type.getUnqualifiedType();
    IdentifierInfo *Id = &Context.Idents.get(".lastprivate.");
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(Type,
                                                          ELoc);
    VarDecl *PseudoVar1 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id, Type, TI,
                            SC_Static);
    PseudoVar1->setImplicit();
    PseudoVar1->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar1);
    DeclRefExpr *PseudoDE1 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar1,
                                                                Type,
                                                                VK_LValue,
                                                                ELoc).take());
    if (RD && !RD->isTriviallyCopyable()) {
      VarDecl *PseudoVar2 =
              VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                              SourceLocation(), Id, Type, TI,
                              SC_Static);
      PseudoVar2->setImplicit();
      PseudoVar2->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
      Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar2);
      DeclRefExpr *PseudoDE2 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar2,
                                                                  Type,
                                                                  VK_LValue,
                                                                  ELoc).take());
      Expr *PseudoDE2RVal = ImpCastExprToType(PseudoDE2, Type, CK_LValueToRValue).take();
      ExprResult Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, PseudoDE1, PseudoDE2RVal).take();
      if (Res.isInvalid()) continue;
      PseudoVars2.push_back(PseudoDE2);
      Assignments.push_back(ImpCastExprToType(Res.take(), Context.VoidTy, CK_ToVoid).take());
    } else {
      PseudoVars2.push_back(0);
      Assignments.push_back(0);
    }
    PseudoVars1.push_back(PseudoDE1);
    if (IsNotFirstprivate)
      DSAStack->addDSA(VD, DE, OMPC_lastprivate);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPLastPrivateClause::Create(Context, StartLoc, EndLoc, Vars, PseudoVars1, PseudoVars2, Assignments);
}

OMPClause *Sema::ActOnOpenMPSharedClause(ArrayRef<Expr *> VarList,
                                         SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    // OpenMP  [2.9.3.4, Restrictions, p.1]
    //  A variable that is part of another variable (as an array or
    //  structure element) cannot appear in a private clause.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct]
    //  Variables with the predetermined data-sharing attributes may not be
    //  listed in data-sharing attributes clauses, except for the cases
    //  listed below. For these exceptions only, listing a predetermined
    //  variable in a data-sharing attribute clause is allowed and overrides
    //  the variable's predetermined data-sharing attributes.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    if (Kind != OMPC_unknown && Kind != OMPC_shared && PrevRef) {
      Diag(ELoc, diag::err_omp_wrong_dsa)
         << getOpenMPClauseName(Kind)
         << getOpenMPClauseName(OMPC_shared);
      Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
           << getOpenMPClauseName(Kind);
      continue;
    }

    DSAStack->addDSA(VD, DE, OMPC_shared);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPSharedClause::Create(Context, StartLoc, EndLoc, Vars);
}

OMPClause *Sema::ActOnOpenMPCopyinClause(ArrayRef<Expr *> VarList,
                                         SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  SmallVector<DeclRefExpr *, 8> PseudoVars1;
  SmallVector<DeclRefExpr *, 8> PseudoVars2;
  SmallVector<Expr *, 8> Assignments;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      PseudoVars1.push_back(0);
      PseudoVars2.push_back(0);
      Assignments.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.9.2, Restrictions, p.1]
    //  A threadprivate variable must not appear in any clause except the
    //  copyin, copyprivate, schedule, num_threads, and if clauses.
    // OpenMP [2.9.4.1, Restrictions, C/C++, p.1]
    //  A list item that appears in a copyin clause must be threadprivate.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    if (Kind != OMPC_threadprivate && Kind != OMPC_copyin) {
      Diag(ELoc, diag::err_omp_required_access)
           << getOpenMPClauseName(OMPC_copyin)
           << getOpenMPDirectiveName(OMPD_threadprivate);
      continue;
    }

    // OpenMP [2.9.3.4, Restrictions, C/C++, p.1]
    //  A variable of class type (or array thereof) that appears in a
    //  firstprivate clause requires an accesible, unambiguous copy assignment
    //  operator for the class type.
    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      CXXMethodDecl *MD = LookupCopyingAssignment(RD, 0, false, 0);
      if (!MD || CheckMemberAccess(ELoc, RD, MD) == AR_inaccessible ||
          MD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_copyin) << 2;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, MD);
      DiagnoseUseOfDecl(MD, ELoc);
    }

    Type = Type.getUnqualifiedType();
    IdentifierInfo *Id = &Context.Idents.get(".copyin.");
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(Type,
                                                          ELoc);
    VarDecl *PseudoVar1 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id, Type, TI,
                            SC_Static);
    PseudoVar1->setImplicit();
    PseudoVar1->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar1);
    DeclRefExpr *PseudoDE1 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar1,
                                                                Type,
                                                                VK_LValue,
                                                                ELoc).take());
    if (RD && !RD->isTriviallyCopyable()) {
      VarDecl *PseudoVar2 =
              VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                              SourceLocation(), Id, Type, TI,
                              SC_Static);
      PseudoVar2->setImplicit();
      PseudoVar2->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
      Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar2);
      DeclRefExpr *PseudoDE2 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar2,
                                                                  Type,
                                                                  VK_LValue,
                                                                  ELoc).take());
      Expr *PseudoDE2RVal = ImpCastExprToType(PseudoDE2, Type, CK_LValueToRValue).take();
      ExprResult Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, PseudoDE1, PseudoDE2RVal).take();
      if (Res.isInvalid()) continue;
      PseudoVars2.push_back(PseudoDE2);
      Assignments.push_back(ImpCastExprToType(Res.take(), Context.VoidTy, CK_ToVoid).take());
    } else {
      PseudoVars2.push_back(0);
      Assignments.push_back(0);
    }
    PseudoVars1.push_back(PseudoDE1);
    DSAStack->addDSA(VD, DE, OMPC_copyin);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPCopyinClause::Create(Context, StartLoc, EndLoc, Vars, PseudoVars1, PseudoVars2, Assignments);
}

OMPClause *Sema::ActOnOpenMPCopyPrivateClause(ArrayRef<Expr *> VarList,
                                              SourceLocation StartLoc,
                                              SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  SmallVector<DeclRefExpr *, 8> PseudoVars1;
  SmallVector<DeclRefExpr *, 8> PseudoVars2;
  SmallVector<Expr *, 8> Assignments;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      PseudoVars1.push_back(0);
      PseudoVars2.push_back(0);
      Assignments.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);

    // OpenMP [2.11.4.2, Restrictions, p.2]
    //  A list item that appears in a copyprivate clause may not appear in
    //  a private or firstprivate clause on the single construct.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    if (Kind != OMPC_threadprivate && Kind != OMPC_copyprivate &&
        Kind != OMPC_unknown && !(Kind == OMPC_private && !PrevRef)) {
      Diag(ELoc, diag::err_omp_wrong_dsa)
           << getOpenMPClauseName(Kind)
           << getOpenMPClauseName(OMPC_copyprivate);
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      } else {
        Diag(VD->getLocation(), diag::note_omp_predetermined_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }

    // OpenMP [2.11.4.2, Restrictions, p.1]
    //  All list items that appear in a copyprivate clause must be either
    //  threadprivate or private in the enclosing context.
    if (Kind == OMPC_unknown) {
      OpenMPDirectiveKind DKind;
      Kind = DSAStack->getImplicitDSA(VD, DKind, PrevRef);
      if (Kind == OMPC_shared) {
        Diag(ELoc, diag::err_omp_required_access)
             << getOpenMPClauseName(OMPC_copyprivate)
             << "threadprivate or private in the enclosing context";
        if (PrevRef) {
          Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
               << getOpenMPClauseName(Kind);
        }
        continue;
      }
    }

    // OpenMP [2.11.4.2, Restrictions, C/C++, p.1]
    //  A variable of class type (or array thereof) that appears in a
    //  copytprivate clause requires an accesible, unambiguous copy assignment
    //  operator for the class type.
    QualType Type = VD->getType().getNonReferenceType().getCanonicalType();
    while (Type->isArrayType()) {
      QualType ElemType = cast<ArrayType>(Type.getTypePtr())->getElementType();
      Type = ElemType.getNonReferenceType().getCanonicalType();
    }
    CXXRecordDecl *RD = getLangOpts().CPlusPlus ?
                          Type->getAsCXXRecordDecl() : 0;
    if (RD) {
      CXXMethodDecl *MD = LookupCopyingAssignment(RD, 0, false, 0);
      if (!MD || CheckMemberAccess(ELoc, RD, MD) == AR_inaccessible ||
          MD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_copyprivate) << 2;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, MD);
      DiagnoseUseOfDecl(MD, ELoc);
    }

    Type = Type.getUnqualifiedType();
    IdentifierInfo *Id = &Context.Idents.get(".copyin.");
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(Type,
                                                          ELoc);
    VarDecl *PseudoVar1 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id, Type, TI,
                            SC_Static);
    PseudoVar1->setImplicit();
    PseudoVar1->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar1);
    DeclRefExpr *PseudoDE1 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar1,
                                                                Type,
                                                                VK_LValue,
                                                                ELoc).take());
    VarDecl *PseudoVar2 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id, Type, TI,
                            SC_Static);
    PseudoVar2->setImplicit();
    PseudoVar2->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(PseudoVar2);
    DeclRefExpr *PseudoDE2 = cast<DeclRefExpr>(BuildDeclRefExpr(PseudoVar2,
                                                                Type,
                                                                VK_LValue,
                                                                ELoc).take());
    Expr *PseudoDE2RVal = ImpCastExprToType(PseudoDE2, Type, CK_LValueToRValue).take();
    ExprResult Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, PseudoDE1, PseudoDE2RVal).take();
    if (Res.isInvalid()) continue;
    PseudoVars1.push_back(PseudoDE1);
    PseudoVars2.push_back(PseudoDE2);
    Assignments.push_back(ImpCastExprToType(Res.take(), Context.VoidTy, CK_ToVoid).take());
    DSAStack->addDSA(VD, DE, OMPC_copyprivate);
    Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPCopyPrivateClause::Create(Context, StartLoc, EndLoc, Vars, PseudoVars1, PseudoVars2, Assignments);
}

namespace {
class DSARefChecker : public StmtVisitor<DSARefChecker, bool> {
  DSAStackTy *Stack;
public:
  bool VisitDeclRefExpr(DeclRefExpr *E) {
    if(VarDecl *VD = dyn_cast<VarDecl>(E->getDecl())) {
      DeclRefExpr *PrevRef;
      OpenMPClauseKind Kind = Stack->getTopDSA(VD, PrevRef);
      if (Kind == OMPC_shared) return false;
      if (Kind != OMPC_unknown) return true;
      OpenMPDirectiveKind DKind;
      Kind = Stack->getImplicitDSA(VD, DKind, PrevRef);
      return Kind != OMPC_shared && Kind != OMPC_unknown;
    }
    return false;
  }
  bool VisitStmt(Stmt *S) {
    for (Stmt::child_iterator I = S->child_begin(), E = S->child_end();
         I != E; ++I) {
      if (Stmt *Child = *I)
        if (Visit(Child)) return true;
    }
    return false;
  }
  DSARefChecker(DSAStackTy *S) : Stack(S) { }
};
}

OMPClause *Sema::ActOnOpenMPReductionClause(ArrayRef<Expr *> VarList,
                                            SourceLocation StartLoc,
                                            SourceLocation EndLoc,
                                            OpenMPReductionClauseOperator Op,
                                            SourceLocation OpLoc) {
  BinaryOperatorKind NewOp = BO_Assign;
  switch (Op) {
  case OMPC_REDUCTION_add:
    NewOp = BO_AddAssign;
    break;
  case OMPC_REDUCTION_mult:
    NewOp = BO_MulAssign;
    break;
  case OMPC_REDUCTION_sub:
    NewOp = BO_SubAssign;
    break;
  case OMPC_REDUCTION_bitand:
    NewOp = BO_AndAssign;
    break;
  case OMPC_REDUCTION_bitor:
    NewOp = BO_OrAssign;
    break;
  case OMPC_REDUCTION_bitxor:
    NewOp = BO_XorAssign;
    break;
  case OMPC_REDUCTION_and:
    NewOp = BO_LAnd;
    break;
  case OMPC_REDUCTION_or:
    NewOp = BO_LOr;
    break;
  case OMPC_REDUCTION_min:
    NewOp = BO_LT;
    break;
  case OMPC_REDUCTION_max:
    NewOp = BO_GT;
    break;
  default:
    break;
  }
  SmallVector<Expr *, 8> Vars;
  SmallVector<Expr *, 8> DefaultInits;
  SmallVector<Expr *, 8> OpExprs;
  SmallVector<Expr *, 8> HelperParams1;
  SmallVector<Expr *, 8> HelperParams2;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      DefaultInits.push_back(0);
      OpExprs.push_back(0);
      HelperParams1.push_back(0);
      HelperParams2.push_back(0);
      continue;
    }

    SourceLocation ELoc = (*I)->getExprLoc();
    // OpenMP [2.1, C/C++]
    //  A list item is a variable name.
    // OpenMP  [2.9.3.3, Restrictions, p.1]
    //  A variable that is part of another variable (as an array or
    //  structure element) cannot appear in a private clause.
    DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I);
    if (!DE || !isa<VarDecl>(DE->getDecl())) {
      Diag(ELoc, diag::err_omp_expected_var_name)
        << (*I)->getSourceRange();
      continue;
    }
    Decl *D = DE->getDecl();
    VarDecl *VD = cast<VarDecl>(D);
    QualType Type = VD->getType();

    // OpenMP [2.9.3.6, Restrictions, C/C++, p.4]
    //  If a list-item is a reference type then it must bind to the same object
    //  for all threads of the team.
    if (Type.getCanonicalType()->isReferenceType() && VD->hasInit()) {
      DSARefChecker Check(DSAStack);
      if (Check.Visit(VD->getInit())) {
        Diag(ELoc, diag::err_omp_reduction_ref_type_arg)
             << getOpenMPClauseName(OMPC_reduction);
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        continue;
      }
    }
    // OpenMP [2.9.3.6, Restrictions, C/C++, p.2]
    //  Aggregate types (including arrays), pointer types and reference types
    //  may not appear in a reduction clause.
    if (RequireCompleteType(ELoc, Type,
                            diag::err_omp_reduction_incomplete_type))
      continue;
    Type = Type.getNonReferenceType().getCanonicalType();
    if (Type->isArrayType()) {
      Diag(ELoc, diag::err_omp_clause_array_type_arg)
        << getOpenMPClauseName(OMPC_reduction);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // OpenMP [2.9.3.6, Restrictions, C/C++, p.3]
    //  A list item that appears in a reduction clause must not be
    //  const-qualified.
    if (Type.isConstant(Context)) {
      Diag(ELoc, diag::err_omp_const_variable)
           << getOpenMPClauseName(OMPC_reduction);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }

    // OpenMP [2.9.3.6, Restrictions, C/C++, p.1]
    //  The type of a list item that appears in a reduction clause must be valid
    //  for the reduction operator. For max or min reduction in C/C++ must be an
    //  arithmetic type.
    if (((Op == OMPC_REDUCTION_min || Op == OMPC_REDUCTION_max) &&
         !Type->isArithmeticType() && !Type->isDependentType()) ||
        (!getLangOpts().CPlusPlus && !Type->isScalarType() &&
         !Type->isDependentType())) {
      Diag(ELoc, diag::err_omp_clause_not_arithmetic_type_arg)
        << getOpenMPClauseName(OMPC_reduction) << getLangOpts().CPlusPlus;
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }
    if ((Op == OMPC_REDUCTION_bitor || Op == OMPC_REDUCTION_bitand ||
         Op == OMPC_REDUCTION_bitxor) && Type->isFloatingType()) {
      Diag(ELoc, diag::err_omp_clause_floating_type_arg);
      bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                    VarDecl::DeclarationOnly;
      Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                       diag::note_defined_here) << VD;
      continue;
    }
    QualType PtrQTy = Context.getPointerType(DE->getType());
    TypeSourceInfo *TI = Context.getTrivialTypeSourceInfo(PtrQTy,
                                                          SourceLocation());
    IdentifierInfo *Id1 = &Context.Idents.get(".ptr1.");
    VarDecl *Parameter1 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id1, PtrQTy, TI,
                            SC_Static);
    Parameter1->setImplicit();
    Parameter1->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    IdentifierInfo *Id2 = &Context.Idents.get(".ptr2.");
    VarDecl *Parameter2 =
            VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                            SourceLocation(), Id2, PtrQTy, TI,
                            SC_Static);
    Parameter2->setImplicit();
    Parameter2->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
    Context.getTranslationUnitDecl()->addHiddenDecl(Parameter1);
    Context.getTranslationUnitDecl()->addHiddenDecl(Parameter2);
    ExprResult PtrDE1 = BuildDeclRefExpr(Parameter1, PtrQTy,
                                         VK_LValue, SourceLocation());
    ExprResult PtrDE2 = BuildDeclRefExpr(Parameter2, PtrQTy,
                                         VK_RValue, SourceLocation());
    Expr *PtrDE1Expr = PtrDE1.take();
    Expr *PtrDE2Expr = PtrDE2.take();
    ExprResult DE1 = CreateBuiltinUnaryOp(ELoc, UO_Deref, PtrDE1Expr);
    ExprResult DE2 = CreateBuiltinUnaryOp(ELoc, UO_Deref, PtrDE2Expr);
    if (NewOp == BO_SubAssign) {
      NewOp = BO_AddAssign;
    }
    ExprResult Res = BuildBinOp(DSAStack->getCurScope(), ELoc, NewOp, DE1.take(), DE2.take());
    if (Res.isInvalid()) continue;
    CXXRecordDecl *RD = Type->getAsCXXRecordDecl();
    if (RD) {
      CXXConstructorDecl *CD = LookupDefaultConstructor(RD);
      PartialDiagnostic PD =
        PartialDiagnostic(PartialDiagnostic::NullDiagnostic());
      if (!CD ||
          CheckConstructorAccess(ELoc, CD,
                                 InitializedEntity::InitializeTemporary(Type),
                                 CD->getAccess(), PD) == AR_inaccessible ||
          CD->isDeleted()) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_reduction) << 0;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(), IsDecl ? diag::note_previous_decl :
                                         diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      }
      MarkFunctionReferenced(ELoc, CD);
      DiagnoseUseOfDecl(CD, ELoc);
      CXXDestructorDecl *DD = RD->getDestructor();
      if (DD &&
          (CheckDestructorAccess(ELoc, DD, PD) == AR_inaccessible ||
           DD->isDeleted())) {
        Diag(ELoc, diag::err_omp_required_method)
             << getOpenMPClauseName(OMPC_reduction) << 4;
        bool IsDecl = VD->isThisDeclarationADefinition(Context) ==
                      VarDecl::DeclarationOnly;
        Diag(VD->getLocation(),
             IsDecl ? diag::note_previous_decl :
                      diag::note_defined_here) << VD;
        Diag(RD->getLocation(), diag::note_previous_decl) << RD;
        continue;
      } else if (DD) {
        MarkFunctionReferenced(ELoc, DD);
        DiagnoseUseOfDecl(DD, ELoc);
      }
    }
    if (NewOp == BO_LAnd || NewOp == BO_LOr) {
      Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, DE1.take(), Res.take());
    } else if (NewOp == BO_LT || NewOp == BO_GT) {
      Res = ActOnConditionalOp(ELoc, ELoc, Res.take(), DE1.take(), DE2.take());
      if (Res.isInvalid()) continue;
      Res = BuildBinOp(DSAStack->getCurScope(), ELoc, BO_Assign, DE1.take(), Res.take());
    }
    if (Res.isInvalid()) continue;
    Res = ImpCastExprToType(Res.take(), Context.VoidTy, CK_ToVoid);

    // OpenMP [2.9.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct]
    //  Variables with the predetermined data-sharing attributes may not be
    //  listed in data-sharing attributes clauses, except for the cases
    //  listed below. For these exceptions only, listing a predetermined
    //  variable in a data-sharing attribute clause is allowed and overrides
    //  the variable's predetermined data-sharing attributes.
    // OpenMP [2.9.3.6, Restrictions, p.3]
    //  Any number of reduction clauses can be specified on the directive,
    //  but a list item can appear only once in the reduction clauses for that
    //  directive.
    DeclRefExpr *PrevRef;
    OpenMPClauseKind Kind = DSAStack->getTopDSA(VD, PrevRef);
    if (Kind == OMPC_reduction) {
      Diag(ELoc, diag::err_omp_once_referenced)
         << getOpenMPClauseName(OMPC_reduction);
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_referenced);
      }
    } else if (Kind != OMPC_unknown) {
      Diag(ELoc, diag::err_omp_wrong_dsa)
         << getOpenMPClauseName(Kind)
         << getOpenMPClauseName(OMPC_reduction);
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      } else {
        Diag(VD->getLocation(), diag::note_omp_predetermined_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }

    // OpenMP [2.9.3.6, Restrictions, p.1]
    //  A list item that appears in a reduction clause of a worksharing
    //  construct must be shared in the parallel regions to which any of the
    //  worksharing regions arising from the worksharing construct bind.
    OpenMPDirectiveKind DKind;
    OpenMPDirectiveKind CurrDir = DSAStack->getCurrentDirective();
    Kind = DSAStack->getImplicitDSA(VD, DKind, PrevRef);
    if (Kind != OMPC_shared && (CurrDir == OMPD_for ||
                                CurrDir == OMPD_sections ||
                                CurrDir == OMPD_parallel_for ||
                                CurrDir == OMPD_parallel_sections)) {
      if (Kind == OMPC_unknown) {
        Diag(ELoc, diag::err_omp_required_access)
          << getOpenMPClauseName(OMPC_reduction)
          << getOpenMPClauseName(OMPC_shared);
      }
      else if (DKind == OMPD_unknown) {
        Diag(ELoc, diag::err_omp_wrong_dsa)
           << getOpenMPClauseName(Kind)
           << getOpenMPClauseName(OMPC_reduction);
      } else {
        Diag(ELoc, diag::err_omp_dsa_with_directives)
           << getOpenMPClauseName(Kind)
           << getOpenMPDirectiveName(DKind)
           << getOpenMPClauseName(OMPC_reduction)
           << getOpenMPDirectiveName(CurrDir);
      }
      if (PrevRef) {
        Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
             << getOpenMPClauseName(Kind);
      }
      continue;
    }

    Type = Type.getUnqualifiedType();
    if (RD) {
      IdentifierInfo *Id = &Context.Idents.get(".firstprivate.");
      TypeSourceInfo *TI1 = Context.getTrivialTypeSourceInfo(Type,
                                                            ELoc);
      VarDecl *PseudoVar =
              VarDecl::Create(Context, Context.getTranslationUnitDecl(), SourceLocation(),
                              SourceLocation(), Id, Type, TI1,
                              SC_Static);
      PseudoVar->setImplicit();
      PseudoVar->addAttr(new (Context) UnusedAttr(SourceLocation(), Context));
      InitializedEntity Entity = InitializedEntity::InitializeVariable(PseudoVar);
      InitializationKind InitKind = InitializationKind::CreateDefault(ELoc);
      InitializationSequence InitSeq(*this, Entity, InitKind, MultiExprArg());
      ExprResult CPRes = InitSeq.Perform(*this, Entity, InitKind, MultiExprArg());
      if (CPRes.isInvalid()) continue;
      DefaultInits.push_back(CPRes.take());
    } else {
      DefaultInits.push_back(0);
    }
    DSAStack->addDSA(VD, DE, OMPC_reduction);
    Vars.push_back(DE);
    OpExprs.push_back(Res.take());
    HelperParams1.push_back(PtrDE1Expr);
    HelperParams2.push_back(PtrDE2Expr);
  }

  if (Vars.empty()) return 0;

  return OMPReductionClause::Create(Context, StartLoc, EndLoc, Vars,
                                    OpExprs, HelperParams1, HelperParams2, DefaultInits,
                                    Op, OpLoc);
}

OMPClause *Sema::ActOnOpenMPReadClause(SourceLocation StartLoc,
                                       SourceLocation EndLoc) {
  return new (Context) OMPReadClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPWriteClause(SourceLocation StartLoc,
                                        SourceLocation EndLoc) {
  return new (Context) OMPWriteClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPUpdateClause(SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  return new (Context) OMPUpdateClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPCaptureClause(SourceLocation StartLoc,
                                          SourceLocation EndLoc) {
  return new (Context) OMPCaptureClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPSeqCstClause(SourceLocation StartLoc,
                                         SourceLocation EndLoc) {
  return new (Context) OMPSeqCstClause(StartLoc, EndLoc);
}

OMPClause *Sema::ActOnOpenMPFlushClause(ArrayRef<Expr *> VarList,
                                        SourceLocation StartLoc,
                                        SourceLocation EndLoc) {
  SmallVector<Expr *, 8> Vars;
  for (ArrayRef<Expr *>::iterator I = VarList.begin(), E = VarList.end();
       I != E; ++I) {
    if (*I && isa<DependentScopeDeclRefExpr>(*I)) {
      // It will be analyzed later.
      Vars.push_back(*I);
      continue;
    }

    if (DeclRefExpr *DE = dyn_cast_or_null<DeclRefExpr>(*I))
      Vars.push_back(DE);
  }

  if (Vars.empty()) return 0;

  return OMPFlushClause::Create(Context, StartLoc, EndLoc, Vars);
}

namespace {
class ForInitChecker : public StmtVisitor<ForInitChecker, Decl *> {
  class ForInitVarChecker : public StmtVisitor<ForInitVarChecker, Decl *> {
  public:
    VarDecl *VisitDeclRefExpr(DeclRefExpr *E) {
      return dyn_cast_or_null<VarDecl>(E->getDecl());
    }
    Decl *VisitStmt(Stmt *S) {
      return 0;
    }
    ForInitVarChecker() { }
  } VarChecker;
  Expr *InitValue;
public:
  Decl *VisitBinaryOperator(BinaryOperator *BO) {
    if (BO->getOpcode() != BO_Assign)
      return 0;

    InitValue = BO->getRHS();
    return VarChecker.Visit(BO->getLHS());
  }
  Decl *VisitDeclStmt(DeclStmt *S) {
    if (S->isSingleDecl()) {
      VarDecl *Var = dyn_cast_or_null<VarDecl>(S->getSingleDecl());
      if (Var && Var->hasInit()) {
        if (CXXConstructExpr *Init
              = dyn_cast<CXXConstructExpr>(Var->getInit())) {
          if (Init->getNumArgs() != 1)
            return 0;
          InitValue = Init->getArg(0);
        } else {
          InitValue = Var->getInit();
        }
        return Var;
      }
    }
    return 0;
  }
  Decl *VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E) {
    switch (E->getOperator()) {
    case OO_Equal:
      InitValue = E->getArg(1);
      return VarChecker.Visit(E->getArg(0));
    default:
      break;
    }
    return 0;
  }
  Decl *VisitStmt(Stmt *S) {
    return 0;
  }
  ForInitChecker() : VarChecker(), InitValue(0) { }
  Expr *getInitValue() { return InitValue; }
};

class ForVarChecker : public StmtVisitor<ForVarChecker, bool> {
  Decl *InitVar;
public:
  bool VisitDeclRefExpr(DeclRefExpr *E) {
    return E->getDecl() == InitVar;
  }
  bool VisitImplicitCastExpr(ImplicitCastExpr *E) {
    return Visit(E->getSubExpr());
  }
  bool VisitStmt(Stmt *S) {
    return false;
  }
  ForVarChecker(Decl *D) : InitVar(D) { }
};

class ForTestChecker : public StmtVisitor<ForTestChecker, bool> {
  ForVarChecker VarChecker;
  Expr *CheckValue;
  bool IsLessOp;
  bool IsStrictOp;
public:
  bool VisitBinaryOperator(BinaryOperator *BO) {
    if (!BO->isRelationalOp())
      return false;
    if (VarChecker.Visit(BO->getLHS())) {
      CheckValue = BO->getRHS();
      IsLessOp = BO->getOpcode() == BO_LT || BO->getOpcode() == BO_LE;
      IsStrictOp = BO->getOpcode() == BO_LT || BO->getOpcode() == BO_GT;
    }
    else if (VarChecker.Visit(BO->getRHS())) {
      CheckValue = BO->getLHS();
      IsLessOp = BO->getOpcode() == BO_GT || BO->getOpcode() == BO_GE;
      IsStrictOp = BO->getOpcode() == BO_LT || BO->getOpcode() == BO_GT;
    }
    return CheckValue != 0;
  }
  bool VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E) {
    switch (E->getOperator()) {
    case OO_Greater:
    case OO_GreaterEqual:
    case OO_Less:
    case OO_LessEqual:
      break;
    default:
      return false;
    }
    if (E->getNumArgs() != 2) return false;

    if (VarChecker.Visit(E->getArg(0))) {
      CheckValue = E->getArg(1);
      IsLessOp = E->getOperator() == OO_Less ||
                 E->getOperator() == OO_LessEqual;
      IsStrictOp = E->getOperator() == OO_Less;
    }
    else if (VarChecker.Visit(E->getArg(1))) {
      CheckValue = E->getArg(0);
      IsLessOp = E->getOperator() == OO_Greater ||
                 E->getOperator() == OO_GreaterEqual;
      IsStrictOp = E->getOperator() == OO_Greater;
    }

    return CheckValue != 0;
  }
  bool VisitStmt(Stmt *S) {
    return false;
  }
  ForTestChecker(Decl *D) : VarChecker(D), CheckValue(0), IsLessOp(false),
                            IsStrictOp(false) { }
  Expr *getCheckValue() { return CheckValue; }
  bool isLessOp() const { return IsLessOp; }
  bool isStrictOp() const { return IsStrictOp; }
};

class ForIncrChecker : public StmtVisitor<ForIncrChecker, bool> {
  ForVarChecker VarChecker;
  class ForIncrExprChecker : public StmtVisitor<ForIncrExprChecker, bool> {
    ForVarChecker VarChecker;
    Expr *StepValue;
    bool IsIncrement;
  public:
    bool VisitBinaryOperator(BinaryOperator *BO) {
      if (!BO->isAdditiveOp())
        return false;
      if (BO->getOpcode() == BO_Add) {
        IsIncrement = true;
        if (VarChecker.Visit(BO->getLHS()))
          StepValue = BO->getRHS();
        else if (VarChecker.Visit(BO->getRHS()))
          StepValue = BO->getLHS();
        return StepValue != 0;
      }
      // BO_Sub
      if (VarChecker.Visit(BO->getLHS()))
        StepValue = BO->getRHS();
      return StepValue != 0;
    }
    bool VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E) {
      switch (E->getOperator()) {
      case OO_Plus:
        IsIncrement = true;
        if (VarChecker.Visit(E->getArg(0)))
          StepValue = E->getArg(1);
        else if (VarChecker.Visit(E->getArg(1)))
          StepValue = E->getArg(0);
        return StepValue != 0;
      case OO_Minus:
        if (VarChecker.Visit(E->getArg(0)))
          StepValue = E->getArg(1);
        return StepValue != 0;
      default:
        return false;
      }
    }
    bool VisitStmt(Stmt *S) {
      return false;
    }
    ForIncrExprChecker(ForVarChecker &C) : VarChecker(C), StepValue(0), IsIncrement(false) { }
    Expr *getStepValue() { return StepValue; }
    bool isIncrement() const { return IsIncrement; }
  } ExprChecker;
  Expr *StepValue;
  Sema &Actions;
  bool IsLessOp, IsCompatibleWithTest;
public:
  bool VisitUnaryOperator(UnaryOperator *UO) {
    if (!UO->isIncrementDecrementOp())
      return false;
    if (VarChecker.Visit(UO->getSubExpr())) {
      IsCompatibleWithTest = (IsLessOp && UO->isIncrementOp()) ||
                             (!IsLessOp && UO->isDecrementOp());
      if (!IsCompatibleWithTest && IsLessOp)
        StepValue = Actions.ActOnIntegerConstant(SourceLocation(), -1).take();
      else
        StepValue = Actions.ActOnIntegerConstant(SourceLocation(), 1).take();
    }
    return StepValue != 0;
  }
  bool VisitBinaryOperator(BinaryOperator *BO) {
    IsCompatibleWithTest = (IsLessOp && BO->getOpcode() == BO_AddAssign) ||
                           (!IsLessOp && BO->getOpcode() == BO_SubAssign);
    switch (BO->getOpcode()) {
    case BO_AddAssign:
    case BO_SubAssign:
      if (VarChecker.Visit(BO->getLHS())) {
        StepValue = BO->getRHS();
        IsCompatibleWithTest = (IsLessOp && BO->getOpcode() == BO_AddAssign) ||
                               (!IsLessOp && BO->getOpcode() == BO_SubAssign);
      }
      return StepValue != 0;
    case BO_Assign:
      if (VarChecker.Visit(BO->getLHS()) && ExprChecker.Visit(BO->getRHS())) {
        StepValue = ExprChecker.getStepValue();
        IsCompatibleWithTest = IsLessOp == ExprChecker.isIncrement();
      }
      return StepValue != 0;
    default:
      break;
    }
    return false;
  }
  bool VisitCXXOperatorCallExpr(CXXOperatorCallExpr *E) {
    switch (E->getOperator()) {
    case OO_PlusPlus:
    case OO_MinusMinus:
      if (VarChecker.Visit(E->getArg(0))) {
        IsCompatibleWithTest = (IsLessOp && E->getOperator() == OO_PlusPlus) ||
                               (!IsLessOp && E->getOperator() == OO_MinusMinus);
        if (!IsCompatibleWithTest && IsLessOp)
          StepValue = Actions.ActOnIntegerConstant(SourceLocation(), -1).take();
        else
          StepValue = Actions.ActOnIntegerConstant(SourceLocation(), 1).take();
        }
      return StepValue != 0;
    case OO_PlusEqual:
    case OO_MinusEqual:
      if (VarChecker.Visit(E->getArg(0))) {
        StepValue = E->getArg(1);
        IsCompatibleWithTest = (IsLessOp && E->getOperator() == OO_PlusEqual) ||
                               (!IsLessOp && E->getOperator() == OO_MinusEqual);
      }
      return StepValue != 0;
    case OO_Equal:
      if (VarChecker.Visit(E->getArg(0)) && ExprChecker.Visit(E->getArg(1))) {
        StepValue = ExprChecker.getStepValue();
        IsCompatibleWithTest = IsLessOp == ExprChecker.isIncrement();
      }
      return StepValue != 0;
    default:
      break;
    }
    return false;
  }
  bool VisitStmt(Stmt *S) {
    return false;
  }
  ForIncrChecker(Decl *D, Sema &S, bool LessOp)
    : VarChecker(D), ExprChecker(VarChecker), StepValue(0), Actions(S),
      IsLessOp(LessOp), IsCompatibleWithTest(false) { }
  Expr *getStepValue() { return StepValue; }
  bool isCompatibleWithTest() const { return IsCompatibleWithTest; }
};
}

bool Sema::isNotOpenMPCanonicalLoopForm(Stmt *S, OpenMPDirectiveKind Kind,
                                        Expr *&NewEnd, Expr *&NewIncr, Expr *&InitVal,
                                        Expr *&VarCnt, BinaryOperatorKind &OpKind) {
  //assert(S && "non-null statement must be specified");
  // OpenMP [2.9.5, Canonical Loop Form]
  //  for (init-expr; test-expr; incr-expr) structured-block
  OpKind = BO_Assign;
  ForStmt *For = dyn_cast_or_null<ForStmt>(S);
  if (!For) {
    Diag(S->getLocStart(), diag::err_omp_not_for)
           << getOpenMPDirectiveName(Kind);
    return true;
  }
  Stmt *Body = For->getBody();
  if (!Body) {
    Diag(S->getLocStart(), diag::err_omp_directive_nonblock)
      << getOpenMPDirectiveName(Kind) << Body;
    return true;
  }

  // OpenMP [2.9.5, Canonical Loop Form]
  //  init-expr One of the following:
  //  var = lb
  //  integer-type var = lb
  //  random-access-iterator-type var = lb
  //  pointer-type var = lb
  ForInitChecker InitChecker;
  Stmt *Init = For->getInit();
  VarDecl *Var;
  if (!Init || !(Var = dyn_cast_or_null<VarDecl>(InitChecker.Visit(Init)))) {
    Diag(Init ? Init->getLocStart() : For->getForLoc(),
         diag::err_omp_not_canonical_for) << 0;
    return true;
  }
  SourceLocation InitLoc = Init->getLocStart();

  // OpenMP [2.11.1.1, Data-sharing Attribute Rules for Variables Referenced
  // in a Construct, C/C++]
  // The loop iteration variable(s) in the associated for-loop(s) of a for or
  // parallel for construct may be listed in a private or lastprivate clause.
  bool HasErrors = false;
  DeclRefExpr *PrevRef;
  OpenMPClauseKind CKind = DSAStack->getTopDSA(Var, PrevRef);
  if (CKind == OMPC_threadprivate) {
//    Diag(InitLoc, diag::err_omp_for_loop_var_dsa)
//         << getOpenMPClauseName(CKind);
//    if (PrevRef)
//      Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
//           << getOpenMPClauseName(CKind);
//    HasErrors = true;
  } else if (CKind != OMPC_unknown && CKind != OMPC_private &&
             CKind != OMPC_lastprivate) {
    Diag(InitLoc, diag::err_omp_for_loop_var_dsa)
         << getOpenMPClauseName(CKind);
    if (PrevRef) {
      Diag(PrevRef->getExprLoc(), diag::note_omp_explicit_dsa)
           << getOpenMPClauseName(CKind);
    } else {
      Diag(Var->getLocation(), diag::note_omp_predetermined_dsa)
           << getOpenMPClauseName(CKind);
    }
    HasErrors = true;
  } else {
    // OpenMP [2.11.1.1, Data-sharing Attribute Rules for Variables Referenced
    // in a Construct, C/C++]
    // The loop iteration variable(s) in the associated for-loop(s)of a for or
    // parallel for construct is (are) private.
    DSAStack->addDSA(Var, 0, OMPC_private);
    if (DSAStack->getCurrentDirective() == OMPD_parallel_for)
      DSAStack->addParentDSA(Var, 0, OMPC_private);
  }

  // OpenMP [2.9.5, Canonical Loop Form]
  // Var One of the following
  // A variable of signed or unsigned integer type
  // For C++, a variable of a random access iterator type.
  // For C, a variable of a pointer type.
  QualType Type =
   Var->getType().getNonReferenceType().getCanonicalType().getUnqualifiedType();
  if (!Type->isIntegerType() && !Type->isPointerType() &&
      (!getLangOpts().CPlusPlus || !Type->isOverloadableType())) {
    Diag(Init->getLocStart(), diag::err_omp_for_variable)
      << getLangOpts().CPlusPlus;
    HasErrors = true;
  }

  // OpenMP [2.9.5, Canonical Loop Form]
  //  test-expr One of the following:
  //  var relational-op b
  //  b relational-op var
  ForTestChecker TestChecker(Var);
  Stmt *Cond = For->getCond();
  bool TestCheckCorrect = false;
  if (!Cond || !(TestCheckCorrect = TestChecker.Visit(Cond))) {
    Diag(Cond ? Cond->getLocStart() : For->getForLoc(),
         diag::err_omp_not_canonical_for) << 1;
    HasErrors = true;
  }

  // OpenMP [2.9.5, Canonical Loop Form]
  //  incr-expr One of the following:
  //  ++var
  //  var++
  //  --var
  //  var--
  //  var += incr
  //  var -= incr
  //  var = var + incr
  //  var = incr + var
  //  var = var - incr
  ForIncrChecker IncrChecker(Var, *this, TestChecker.isLessOp());
  Stmt *Incr = For->getInc();
  bool IncrCheckCorrect = false;
  if (!Incr || !(IncrCheckCorrect = IncrChecker.Visit(Incr))) {
    Diag(Incr ? Incr->getLocStart() : For->getForLoc(),
         diag::err_omp_not_canonical_for) << 2;
    HasErrors = true;
  }

  // OpenMP [2.9.5, Canonical Loop Form]
  //  lb and b Loop invariant expressions of a type compatible with the type
  //  of var.
  Expr *InitValue = InitChecker.getInitValue();
//  QualType InitTy =
//    InitValue ? InitValue->getType().getNonReferenceType().
//                                  getCanonicalType().getUnqualifiedType() :
//                QualType();
//  if (InitValue &&
//      Context.mergeTypes(Type, InitTy, false, true).isNull()) {
//    Diag(InitValue->getExprLoc(), diag::err_omp_for_type_not_compatible)
//      << InitValue->getType()
//      << Var << Var->getType();
//    HasErrors = true;
//  }
  Expr *CheckValue = TestChecker.getCheckValue();
//  QualType CheckTy =
//    CheckValue ? CheckValue->getType().getNonReferenceType().
//                                  getCanonicalType().getUnqualifiedType() :
//                 QualType();
//  if (CheckValue &&
//      Context.mergeTypes(Type, CheckTy, false, true).isNull()) {
//    Diag(CheckValue->getExprLoc(), diag::err_omp_for_type_not_compatible)
//      << CheckValue->getType()
//      << Var << Var->getType();
//    HasErrors = true;
//  }

  // OpenMP [2.9.5, Canonical Loop Form]
  //  incr A loop invariant integer expression.
  Expr *Step = IncrChecker.getStepValue();
  if (Step && !Step->getType()->isIntegralOrEnumerationType()) {
    Diag(Step->getExprLoc(), diag::err_omp_for_incr_not_integer);
    HasErrors = true;
  }
  //llvm::APSInt Result;
  //if (Step && Step->isIntegerConstantExpr(Result, Context) &&
  //    !Result.isStrictlyPositive()) {
  //  Diag(Step->getExprLoc(), diag::err_negative_expression_in_clause);
  //  HasErrors = true;
  //}

  // OpenMP [2.9.5, Canonical Loop Form, Restrictions]
  //  If test-expr is of form var relational-op b and relational-op is < or
  //  <= then incr-expr must cause var to increase on each iteration of the
  //  loop. If test-expr is of form var relational-op b and relational-op is
  //  > or >= then incr-expr must cause var to decrease on each iteration of the
  //  loop.
  //  If test-expr is of form b relational-op var and relational-op is < or
  //  <= then incr-expr must cause var to decrease on each iteration of the
  //  loop. If test-expr is of form b relational-op var and relational-op is
  //  > or >= then incr-expr must cause var to increase on each iteration of the
  //  loop.
  if (Incr && TestCheckCorrect && IncrCheckCorrect &&
      !IncrChecker.isCompatibleWithTest()) {
    // Additional type checking.
    llvm::APSInt Result;
    bool IsConst = Step->isIntegerConstantExpr(Result, getASTContext());
    bool IsConstNeg = IsConst && Result.isSigned() && Result.isNegative();
    bool IsSigned = Step->getType()->hasSignedIntegerRepresentation();
    if ((TestChecker.isLessOp() && IsConst && IsConstNeg) ||
        (!TestChecker.isLessOp() &&
         ((IsConst && !IsConstNeg) || (!IsConst && !IsSigned)))) {
      Diag(Incr->getLocStart(), diag::err_omp_for_incr_not_compatible)
        << Var << TestChecker.isLessOp();
      HasErrors = true;
    } else {
      Step = CreateBuiltinUnaryOp(Step->getExprLoc(), UO_Minus, Step).take();
    }
  }
  if (HasErrors) return true;

  // Build expression for number of iterations.
  //if (getLangOpts().CPlusPlus && !StdNamespace && !Type->isIntegerType()) {
  //  Diag(Var->getLocation(), diag::err_omp_type_not_rai);
  //  return true;
  //}

  ExprResult Diff;
  Step = Step->IgnoreParenImpCasts();
  CheckValue = CheckValue->IgnoreParenImpCasts();
  InitValue = InitValue->IgnoreParenImpCasts();
  if (Step->getType()->isDependentType() || CheckValue->getType()->isDependentType() ||
      InitValue->getType()->isDependentType()) {
    NewEnd = CheckValue;
    NewIncr = Step;
    InitVal = InitValue;
    VarCnt = CheckValue;
    return false;
  }
  if (getLangOpts().CPlusPlus && !Type->isIntegerType() && !Type->isPointerType()) {
    // Check that var type is a random access iterator, i.e.
    // we can apply 'std::distance' to the init and test arguments
    // of the for-loop.
    CXXScopeSpec SS;
    SS.Extend(Context, getStdNamespace(), SourceLocation(), SourceLocation());
    IdentifierInfo *IIT = &Context.Idents.get("iterator_traits");
    DeclarationNameInfo DNIIT(IIT, SourceLocation());
    LookupResult RIT(*this, DNIIT, LookupNestedNameSpecifierName);
    TemplateDecl *D;
    if (!LookupParsedName(RIT, DSAStack->getCurScope(), &SS) ||
        !RIT.isSingleResult() || !(D = RIT.getAsSingle<TemplateDecl>())) {
      Diag(Var->getLocation(), diag::err_omp_type_not_rai);
      return true;
    }

    TemplateArgumentListInfo Args;
    TemplateArgument Arg(Type);
    TemplateArgumentLoc ArgLoc(Arg, Context.CreateTypeSourceInfo(Type));
    Args.addArgument(ArgLoc);
    QualType T = CheckTemplateIdType(TemplateName(D), SourceLocation(), Args);
    CXXRecordDecl *TRDType;
    if (T.isNull() || RequireCompleteType(Var->getLocation(), T, 0) ||
        !(TRDType = T->getAsCXXRecordDecl())) {
      Diag(Var->getLocation(), diag::err_omp_type_not_rai);
      return true;
    }

    IdentifierInfo *IIRAI = &Context.Idents.get("random_access_iterator_tag");
    DeclarationNameInfo DNIRAI(IIRAI, SourceLocation());
    LookupResult RRAI(*this, DNIRAI, LookupTagName);
    TypeDecl *TDRAI;
    CXXRecordDecl *RDType = Type->getAsCXXRecordDecl();
    if (!LookupParsedName(RRAI, DSAStack->getCurScope(), &SS) ||
         !RRAI.isSingleResult() ||
         !(TDRAI = RRAI.getAsSingle<TypeDecl>()) || !RDType) {
      Diag(Var->getLocation(), diag::err_omp_type_not_rai);
      return true;
    }

    IdentifierInfo *IIC = &Context.Idents.get("iterator_category");
    DeclarationNameInfo DNIIC(IIC, SourceLocation());
    LookupResult RIC(*this, DNIIC, LookupTagName);
    TypeDecl *TDIC;
    if (!LookupQualifiedName(RIC, TRDType) || !RIC.isSingleResult() ||
        !(TDIC = RIC.getAsSingle<TypeDecl>()) ||
        !Context.hasSameType(Context.getTypeDeclType(TDRAI),
                             Context.getTypeDeclType(TDIC))) {
      Diag(Var->getLocation(), diag::err_omp_type_not_rai);
      return true;
    }

    IdentifierInfo *IID = &Context.Idents.get("distance");
    DeclarationNameInfo DNID(IID, SourceLocation());
    ExprResult ER = BuildQualifiedTemplateIdExpr(SS, InitLoc,
                                                 DNID, &Args);
    Expr *CallArgs[2] = {TestChecker.isLessOp() ? InitValue : CheckValue,
                         TestChecker.isLessOp() ? CheckValue : InitValue};
    Diff = ActOnCallExpr(DSAStack->getCurScope(), ER.take(), InitLoc,
                         CallArgs, InitLoc);
    if (Diff.isInvalid()) {
      Diag(Var->getLocation(), diag::err_omp_type_not_rai);
      return true;
    }
  } else {
    Diff = BuildBinOp(DSAStack->getCurScope(), InitLoc, BO_Sub,
                      TestChecker.isLessOp() ? CheckValue : InitValue,
                      TestChecker.isLessOp() ? InitValue : CheckValue);
  }

  if (Diff.isUsable() && TestChecker.isStrictOp()) {
    Diff = BuildBinOp(DSAStack->getCurScope(), InitLoc, BO_Sub,
                      Diff.take(), ActOnIntegerConstant(SourceLocation(), 1).take());
  }
  if (Diff.isUsable()) {
    Diff = BuildBinOp(DSAStack->getCurScope(), InitLoc, BO_Add,
                      Diff.take(), Step);
  }
  if (Diff.isUsable()) {
    Diff = BuildBinOp(DSAStack->getCurScope(), InitLoc, BO_Div,
                      Diff.take(), Step);
  }
  if (Diff.isInvalid() ||
      !Diff.get()->getType()->isIntegerType()) {
    Diag(S->getLocStart(), diag::err_omp_for_wrong_count);
    return true;
  }
  NewEnd = Diff.take();
  NewIncr = Step;
  InitVal = InitValue;
  //NamedDecl *ND = Var;
  VarCnt = DeclRefExpr::Create(Context, NestedNameSpecifierLoc(),
                               SourceLocation(), Var, false,
                               SourceLocation(), Type,
                               VK_LValue);
  //if (!isDeclInScope(ND, CurContext, DSAStack->getCurScope())) {
  //  DeclContext *SavedCurContext = CurContext;
  //  CurContext = Var->getDeclContext();
  //  VarCnt = BuildDeclRefExpr(Var, Type, VK_LValue, InitLoc).take();
  //  CurContext = SavedCurContext;
  //}
  OpKind = TestChecker.isLessOp() ? BO_Add : BO_Sub;
  return false;
}

