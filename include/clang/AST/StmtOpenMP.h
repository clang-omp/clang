//===- StmtOpenMP.h - Classes for OpenMP directives and clauses --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file defines OpenMP AST classes for executable directives and
/// clauses.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_STMTOPENMP_H
#define LLVM_CLANG_AST_STMTOPENMP_H

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/OpenMPKinds.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/Expr.h"

namespace clang {

//===----------------------------------------------------------------------===//
// AST classes for clauses.
//===----------------------------------------------------------------------===//

/// \brief This is a basic class for representing single OpenMP clause.
///
class OMPClause {
  /// \brief Starting location of the clause.
  SourceLocation StartLoc;
  /// \brief Ending location of the clause.
  SourceLocation EndLoc;
  /// \brief Kind of the clause.
  OpenMPClauseKind Kind;
protected:
  OMPClause(OpenMPClauseKind K,
            SourceLocation StartLoc, SourceLocation EndLoc)
    : StartLoc(StartLoc), EndLoc(EndLoc), Kind(K) {}

public:

  /// \brief Fetches the starting location of the clause.
  SourceLocation getLocStart() const { return StartLoc; }
  /// \brief Fetches the ending location of the clause.
  SourceLocation getLocEnd() const { return EndLoc; }

  /// \brief Sets the starting location of the clause.
  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }
  /// \brief Sets the ending location of the clause.
  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  /// \brief Fetches kind of OpenMP clause (private, shared, reduction, etc.).
  OpenMPClauseKind getClauseKind() const { return Kind; }

  static bool classof(const OMPClause *T) {
    return true;
  }

  bool isImplicit() { return StartLoc.isInvalid();}

  StmtRange children();
  ConstStmtRange children() const {
    return const_cast<OMPClause *>(this)->children();
  }
};

/// \brief This represents clauses with the list of variables like 'private',
/// 'firstprivate', 'copyin', 'shared', 'reduction' or 'flush' clauses in the
/// '#pragma omp ...' directives.
template <class T>
class OMPVarList {
  friend class OMPClauseReader;
  /// \brief Number of variables in the list.
  unsigned NumVars;
protected:
  /// \brief Fetches the list of variables associated with this clause.
  llvm::MutableArrayRef<Expr *> getVars() {
    return llvm::MutableArrayRef<Expr *>(
                         reinterpret_cast<Expr **>(static_cast<T *>(this) + 1),
                         NumVars);
  }

  /// \brief Sets the list of variables for this clause.
  void setVars(ArrayRef<Expr *> VL) {
    assert(VL.size() == NumVars &&
           "Number of variables is not the same as the preallocated buffer");
    std::copy(VL.begin(), VL.end(),
              reinterpret_cast<Expr **>(static_cast<T *>(this) + 1));
  }

  /// \brief Build clause with number of variables \a N.
  ///
  /// \param N Number of the variables in the clause.
  ///
  OMPVarList(unsigned N) : NumVars(N) { }
public:
  typedef llvm::MutableArrayRef<Expr *>::iterator varlist_iterator;
  typedef ArrayRef<const Expr *>::iterator varlist_const_iterator;

  unsigned varlist_size() const { return NumVars; }
  bool varlist_empty() const { return NumVars == 0; }
  varlist_iterator varlist_begin() { return getVars().begin(); }
  varlist_iterator varlist_end() { return getVars().end(); }
  varlist_const_iterator varlist_begin() const { return getVars().begin(); }
  varlist_const_iterator varlist_end() const { return getVars().end(); }
  unsigned numberOfVariables() const { return NumVars; }

  /// \brief Return the list of all variables in the clause.
  ArrayRef<const Expr *> getVars() const {
    return ArrayRef<const Expr *>(
              reinterpret_cast<const Expr *const *>(static_cast<const T *>(this) + 1),
              NumVars);
  }
};

/// \brief This represents 'if' clause in the '#pragma omp ...' directive.
///
/// \code
/// #pragma omp parallel if(a)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'if' with
/// single expression 'a'.
///
class OMPIfClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Clause condition.
  Stmt *Condition;
  /// \brief Set the condition.
  ///
  /// \param E New condition.
  ///
  void setCondition(Expr *E) { Condition = cast<Stmt>(E); }
public:
  /// \brief Build 'if' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPIfClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_if, StartLoc, EndLoc), Condition(cast<Stmt>(E)) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPIfClause()
    : OMPClause(OMPC_if, SourceLocation(), SourceLocation()), Condition(0) { }

  /// \brief Return condition.
  Expr *getCondition() { return dyn_cast_or_null<Expr>(Condition); }

  /// \brief Return condition.
  Expr *getCondition() const { return dyn_cast_or_null<Expr>(Condition); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_if;
  }

  StmtRange children() {
    return StmtRange(&Condition, &Condition + 1);
  }
};

/// \brief This represents 'final' clause in the '#pragma omp ...' directive.
///
/// \code
/// #pragma omp task final(a)
/// \endcode
/// In this example directive '#pragma omp task' has clause 'final' with
/// single expression 'a'.
///
class OMPFinalClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Clause condition.
  Stmt *Condition;
  /// \brief Set the condition.
  ///
  /// \param E New condition.
  ///
  void setCondition(Expr *E) { Condition = cast<Stmt>(E); }
public:
  /// \brief Build 'if' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPFinalClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_final, StartLoc, EndLoc), Condition(cast<Stmt>(E)) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPFinalClause()
    : OMPClause(OMPC_final, SourceLocation(), SourceLocation()),
      Condition(0) { }

  /// \brief Return condition.
  Expr *getCondition() { return dyn_cast_or_null<Expr>(Condition); }
  /// \brief Return condition.
  Expr *getCondition() const { return dyn_cast_or_null<Expr>(Condition); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_final;
  }

  StmtRange children() {
    return StmtRange(&Condition, &Condition + 1);
  }
};

/// \brief This represents 'num_threads' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp parallel num_threads(a)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'num_threads'
/// with single expression 'a'.
///
class OMPNumThreadsClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Number of threads.
  Stmt *NumThreads;
  /// \brief Set the number of threads.
  ///
  /// \param E Number of threads.
  ///
  void setNumThreads(Expr *E) { NumThreads = cast<Stmt>(E); }
public:
  /// \brief Build 'num_threads' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPNumThreadsClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_num_threads, StartLoc, EndLoc), NumThreads(cast<Stmt>(E)) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPNumThreadsClause()
    : OMPClause(OMPC_num_threads, SourceLocation(), SourceLocation()),
      NumThreads(0) { }

  /// \brief Return number of threads.
  Expr *getNumThreads() { return dyn_cast_or_null<Expr>(NumThreads); }

  /// \brief Return number of threads.
  Expr *getNumThreads() const { return dyn_cast_or_null<Expr>(NumThreads); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_num_threads;
  }

  StmtRange children() {
    return StmtRange(&NumThreads, &NumThreads + 1);
  }
};

/// \brief This represents 'collapse' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp for collapse(3)
/// \endcode
/// In this example directive '#pragma omp for' has clause 'collapse'
/// with single expression '3'.
///
class OMPCollapseClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Number of for-loops.
  Stmt *NumForLoops;
  /// \brief Set the number of associated for-loops.
  ///
  /// \param E Number of for-loops.
  ///
  void setNumForLoops(Expr *E) { NumForLoops = cast<Stmt>(E); }
public:
  /// \brief Build 'collapse' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPCollapseClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_collapse, StartLoc, EndLoc), NumForLoops(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPCollapseClause()
    : OMPClause(OMPC_collapse, SourceLocation(), SourceLocation()),
      NumForLoops(0) { }

  /// \brief Return number of associated for-loops.
  ///
  Expr *getNumForLoops() { return dyn_cast_or_null<Expr>(NumForLoops); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_collapse;
  }

  StmtRange children() {
    return StmtRange(&NumForLoops, &NumForLoops + 1);
  }
};

/// \brief This represents 'default' clause in the '#pragma omp ...' directive.
///
/// \code
/// #pragma omp parallel default(shared)
/// \endcode
/// In this example directive '#pragma omp parallel' has simple 'default'
/// clause with kind 'shared'.
///
class OMPDefaultClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief A kind of the 'default' clause.
  OpenMPDefaultClauseKind Kind;
  /// \brief Start location of the kind in cource code.
  SourceLocation KindLoc;

  /// \brief Set kind of the clauses.
  ///
  /// \param K Argument of clause.
  ///
  void setDefaultKind(OpenMPDefaultClauseKind K) { Kind = K; }

  /// \brief Set argument location.
  ///
  /// \param KLoc Argument location.
  ///
  void setDefaultKindLoc(SourceLocation KLoc) { KindLoc = KLoc; }
public:
  /// \brief Build 'default' clause with argument \a A ('none' or 'shared').
  ///
  /// \brief A Argument of the clause ('none' or 'shared').
  /// \brief ALoc Starting location of the argument.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  ///
  OMPDefaultClause(OpenMPDefaultClauseKind A, SourceLocation ALoc,
                   SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_default, StartLoc, EndLoc), Kind(A), KindLoc(ALoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPDefaultClause()
    : OMPClause(OMPC_default, SourceLocation(), SourceLocation()),
      Kind(OMPC_DEFAULT_unknown), KindLoc(SourceLocation()) { }

  /// \brief Fetches kind of the clause.
  ///
  OpenMPDefaultClauseKind getDefaultKind() const { return Kind; }

  /// \brief Fetches location of clause kind.
  ///
  SourceLocation getDefaultKindLoc() const { return KindLoc; }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_default;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents clause 'private' in the '#pragma omp ...' directives.
///
/// \code
/// #pragma omp parallel private(a,b)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'private'
/// with the variables 'a' and 'b'.
///
class OMPPrivateClause : public OMPClause, public OMPVarList<OMPPrivateClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPPrivateClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N)
    : OMPClause(OMPC_private, StartLoc, EndLoc),
      OMPVarList<OMPPrivateClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPPrivateClause(unsigned N)
    : OMPClause(OMPC_private, SourceLocation(), SourceLocation()),
      OMPVarList<OMPPrivateClause>(N) { }

  /// \brief Sets the list of generated default inits.
  void setDefaultInits(ArrayRef<Expr *> DefaultInits);

  /// \brief Return the list of all generated expressions.
  llvm::MutableArrayRef<Expr *> getDefaultInits() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }
public:

  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPPrivateClause *Create(ASTContext &C,
                                  SourceLocation StartLoc,
                                  SourceLocation EndLoc,
                                  ArrayRef<Expr *> VL,
                                  ArrayRef<Expr *> DefaultInits);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPPrivateClause *CreateEmpty(ASTContext &C,
                                       unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_private;
  }

  /// \brief Return the list of all default initializations.
  ArrayRef<const Expr *> getDefaultInits() const {
    return llvm::makeArrayRef(varlist_end(), numberOfVariables());
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(getDefaultInits().end()));
  }
};

/// \brief This represents clause 'firstprivate' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp parallel firstprivate(a,b)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'firstprivate'
/// with the variables 'a' and 'b'.
///
class OMPFirstPrivateClause : public OMPClause,
                              public OMPVarList<OMPFirstPrivateClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPFirstPrivateClause(SourceLocation StartLoc, SourceLocation EndLoc,
                        unsigned N)
    : OMPClause(OMPC_firstprivate, StartLoc, EndLoc),
      OMPVarList<OMPFirstPrivateClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPFirstPrivateClause(unsigned N)
    : OMPClause(OMPC_firstprivate, SourceLocation(), SourceLocation()),
      OMPVarList<OMPFirstPrivateClause>(N) { }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }

  /// \brief Sets the list of generated inits.
  void setInits(ArrayRef<Expr *> Inits);

  /// \brief Return the list of all inits.
  llvm::MutableArrayRef<Expr *> getInits() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars().end(), numberOfVariables());
  }

public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPFirstPrivateClause *Create(ASTContext &C,
                                       SourceLocation StartLoc,
                                       SourceLocation EndLoc,
                                       ArrayRef<Expr *> VL,
                                       ArrayRef<DeclRefExpr *> PseudoVars,
                                       ArrayRef<Expr *> Inits);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPFirstPrivateClause *CreateEmpty(ASTContext &C,
                                            unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_firstprivate;
  }

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars() const {
    return llvm::makeArrayRef(varlist_end(), numberOfVariables());
  }

  /// \brief Return the list of all initializations.
  ArrayRef<const Expr *> getInits() const {
    return llvm::makeArrayRef(getPseudoVars().end(), numberOfVariables());
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(getInits().end()));
  }
};

/// \brief This represents clause 'lastprivate' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp for lastprivate(a,b)
/// \endcode
/// In this example directive '#pragma omp for' has clause 'lastprivate'
/// with the variables 'a' and 'b'.
///
class OMPLastPrivateClause : public OMPClause,
                             public OMPVarList<OMPLastPrivateClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  friend class Sema;
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  explicit OMPLastPrivateClause(SourceLocation StartLoc, SourceLocation EndLoc,
                       unsigned N)
    : OMPClause(OMPC_lastprivate, StartLoc, EndLoc),
      OMPVarList<OMPLastPrivateClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPLastPrivateClause(unsigned N)
    : OMPClause(OMPC_lastprivate, SourceLocation(), SourceLocation()),
      OMPVarList<OMPLastPrivateClause>(N) { }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars1(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars1() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars2(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars2() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Sets the list of generated default inits.
  void setDefaultInits(ArrayRef<Expr *> DefaultInits);

  /// \brief Return the list of all generated expressions.
  llvm::MutableArrayRef<Expr *> getDefaultInits() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars2().end(), numberOfVariables());
  }
  /// \brief Sets the list of generated inits.
  void setAssignments(ArrayRef<Expr *> Assignments);

  /// \brief Return the list of all inits.
  llvm::MutableArrayRef<Expr *> getAssignments() {
    return llvm::MutableArrayRef<Expr *>(getDefaultInits().end(), numberOfVariables());
  }

public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPLastPrivateClause *Create(ASTContext &C,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc,
                                      ArrayRef<Expr *> VL,
                                      ArrayRef<DeclRefExpr *> PseudoVars1,
                                      ArrayRef<DeclRefExpr *> PseudoVars2,
                                      ArrayRef<Expr *> Assignments);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPLastPrivateClause *CreateEmpty(ASTContext &C, unsigned N);

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars1() const {
    return llvm::makeArrayRef(varlist_end(), numberOfVariables());
  }

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars2() const {
    return llvm::makeArrayRef(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Return the list of all default initializations.
  ArrayRef<const Expr *> getDefaultInits() const {
    return llvm::makeArrayRef(getPseudoVars2().end(), numberOfVariables());
  }

  /// \brief Return the list of all initializations.
  ArrayRef<const Expr *> getAssignments() const {
    return llvm::makeArrayRef(getDefaultInits().end(), numberOfVariables());
  }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_lastprivate;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt**>(varlist_begin()),
                     reinterpret_cast<Stmt**>(getAssignments().end()));
  }
};

/// \brief This represents clause 'shared' in the '#pragma omp ...' directives.
///
/// \code
/// #pragma omp parallel shared(a,b)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'shared'
/// with the variables 'a' and 'b'.
///
class OMPSharedClause : public OMPClause,
                        public OMPVarList<OMPSharedClause> {
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPSharedClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N)
    : OMPClause(OMPC_shared, StartLoc, EndLoc),
      OMPVarList<OMPSharedClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPSharedClause(unsigned N)
    : OMPClause(OMPC_shared, SourceLocation(), SourceLocation()),
      OMPVarList<OMPSharedClause>(N) { }
public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPSharedClause *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<Expr *> VL);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPSharedClause *CreateEmpty(ASTContext &C,
                                      unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_shared;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(varlist_end()));
  }
};

/// \brief This represents clause 'copyin' in the '#pragma omp ...' directives.
///
/// \code
/// #pragma omp parallel copyin(a,b)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'copyin'
/// with the variables 'a' and 'b'.
///
class OMPCopyinClause : public OMPClause,
                        public OMPVarList<OMPCopyinClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPCopyinClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N)
    : OMPClause(OMPC_copyin, StartLoc, EndLoc),
      OMPVarList<OMPCopyinClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPCopyinClause(unsigned N)
    : OMPClause(OMPC_copyin, SourceLocation(), SourceLocation()),
      OMPVarList<OMPCopyinClause>(N) { }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars1(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars1() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars2(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars2() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Sets the list of generated inits.
  void setAssignments(ArrayRef<Expr *> Assignments);

  /// \brief Return the list of all inits.
  llvm::MutableArrayRef<Expr *> getAssignments() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars2().end(), numberOfVariables());
  }

public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPCopyinClause *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<Expr *> VL,
                                 ArrayRef<DeclRefExpr *> PseudoVars1,
                                 ArrayRef<DeclRefExpr *> PseudoVars2,
                                 ArrayRef<Expr *> Assignments);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPCopyinClause *CreateEmpty(ASTContext &C,
                                      unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_copyin;
  }

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars1() const {
    return llvm::makeArrayRef(varlist_end(), numberOfVariables());
  }

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars2() const {
    return llvm::makeArrayRef(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Return the list of all initializations.
  ArrayRef<const Expr *> getAssignments() const {
    return llvm::makeArrayRef(getPseudoVars2().end(), numberOfVariables());
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(getAssignments().end()));
  }
};

/// \brief This represents clause 'copyprivate' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp single copyprivate(a,b)
/// \endcode
/// In this example directive '#pragma omp single' has clause 'copyprivate'
/// with the variables 'a' and 'b'.
///
class OMPCopyPrivateClause : public OMPClause,
                             public OMPVarList<OMPCopyPrivateClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPCopyPrivateClause(SourceLocation StartLoc, SourceLocation EndLoc,
                       unsigned N)
    : OMPClause(OMPC_copyprivate, StartLoc, EndLoc),
      OMPVarList<OMPCopyPrivateClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPCopyPrivateClause(unsigned N)
    : OMPClause(OMPC_copyprivate, SourceLocation(), SourceLocation()),
      OMPVarList<OMPCopyPrivateClause>(N) { }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars1(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars1() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }

  /// \brief Sets the list of pseudo vars.
  void setPseudoVars2(ArrayRef<DeclRefExpr *> PseudoVars);

  /// \brief Return the list of pseudo vars.
  llvm::MutableArrayRef<Expr *> getPseudoVars2() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Sets the list of generated inits.
  void setAssignments(ArrayRef<Expr *> Assignments);

  /// \brief Return the list of all inits.
  llvm::MutableArrayRef<Expr *> getAssignments() {
    return llvm::MutableArrayRef<Expr *>(getPseudoVars2().end(), numberOfVariables());
  }

public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPCopyPrivateClause *Create(ASTContext &C,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc,
                                      ArrayRef<Expr *> VL,
                                      ArrayRef<DeclRefExpr *> PseudoVars1,
                                      ArrayRef<DeclRefExpr *> PseudoVars2,
                                      ArrayRef<Expr *> Assignments);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPCopyPrivateClause *CreateEmpty(ASTContext &C,
                                           unsigned N);

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars1() const {
    return llvm::makeArrayRef(varlist_end(), numberOfVariables());
  }

  /// \brief Return the list of pseudo vars.
  ArrayRef<const Expr *> getPseudoVars2() const {
    return llvm::makeArrayRef(getPseudoVars1().end(), numberOfVariables());
  }

  /// \brief Return the list of all initializations.
  ArrayRef<const Expr *> getAssignments() const {
    return llvm::makeArrayRef(getPseudoVars2().end(), numberOfVariables());
  }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_copyprivate;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(getAssignments().end()));
  }
};

/// \brief This represents clause 'reduction' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp parallel reduction(+ : a,b)
/// \endcode
/// In this example directive '#pragma omp parallel' has clause 'reduction'
/// with operator '+' and variables 'a' and 'b'.
///
class OMPReductionClause : public OMPClause,
                           public OMPVarList<OMPReductionClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;
  /// \brief An operator for the 'reduction' clause.
  OpenMPReductionClauseOperator Operator;
  /// \brief Start location of the kind in cource code.
  SourceLocation OperatorLoc;

  /// \brief Set operator for the clause.
  ///
  /// \param Op Operator for the clause.
  ///
  void setOperator(OpenMPReductionClauseOperator Op) { Operator = Op; }

  /// \brief Set operator location.
  ///
  /// \param OpLoc Operator location.
  ///
  void setOperatorLoc(SourceLocation OpLoc) { OperatorLoc = OpLoc; }

  /// \brief Build clause with number of variables \a N and an operator \a Op.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  /// \param Op reduction operator.
  /// \param OpLoc Location of the operator.
  ///
  OMPReductionClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N,
                     OpenMPReductionClauseOperator Op, SourceLocation OpLoc)
    : OMPClause(OMPC_reduction, StartLoc, EndLoc),
      OMPVarList<OMPReductionClause>(N), Operator(Op), OperatorLoc(OpLoc) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPReductionClause(unsigned N)
    : OMPClause(OMPC_reduction, SourceLocation(), SourceLocation()),
      OMPVarList<OMPReductionClause>(N),
      Operator(OMPC_REDUCTION_unknown), OperatorLoc(SourceLocation()) { }

  /// \brief Sets the list of generated expresssions.
  void setOpExprs(ArrayRef<Expr *> OpExprs);
  /// \brief Sets the list of 1st helper parameters.
  void setHelperParameters1st(ArrayRef<Expr *> HelperParams);
  /// \brief Sets the list of 1st helper parameters.
  void setHelperParameters2nd(ArrayRef<Expr *> HelperParams);

  /// \brief Return the list of all generated expressions.
  llvm::MutableArrayRef<Expr *> getOpExprs() {
    return llvm::MutableArrayRef<Expr *>(varlist_end(), numberOfVariables());
  }

  /// \brief Return the list of 1st helper parameters.
  llvm::MutableArrayRef<Expr *> getHelperParameters1st() {
    return llvm::MutableArrayRef<Expr *>(getOpExprs().end(),
                                         numberOfVariables());
  }

  /// \brief Return the list of 2nd helper parameters.
  llvm::MutableArrayRef<Expr *> getHelperParameters2nd() {
    return llvm::MutableArrayRef<Expr *>(getHelperParameters1st().end(),
                                         numberOfVariables());
  }

  /// \brief Sets the list of generated default inits.
  void setDefaultInits(ArrayRef<Expr *> DefaultInits);

  /// \brief Return the list of all generated expressions.
  llvm::MutableArrayRef<Expr *> getDefaultInits() {
    return llvm::MutableArrayRef<Expr *>(getHelperParameters2nd().end(), numberOfVariables());
  }
public:
  /// \brief Creates clause with a list of variables \a VL and an operator
  /// \a Op.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  /// \param Op reduction operator.
  /// \param OpLoc Location of the operator.
  ///
  static OMPReductionClause *Create(ASTContext &C,
                                    SourceLocation StartLoc,
                                    SourceLocation EndLoc,
                                    ArrayRef<Expr *> VL,
                                    ArrayRef<Expr *> OpExprs,
                                    ArrayRef<Expr *> HelperParams1,
                                    ArrayRef<Expr *> HelperParams2,
                                    ArrayRef<Expr *> DefaultInits,
                                    OpenMPReductionClauseOperator Op,
                                    SourceLocation OpLoc);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPReductionClause *CreateEmpty(ASTContext &C, unsigned N);

  /// \brief Fetches operator for the clause.
  OpenMPReductionClauseOperator getOperator() const { return Operator; }

  /// \brief Fetches location of clause operator.
  SourceLocation getOperatorLoc() const { return OperatorLoc; }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_reduction;
  }

  /// \brief Return the list of all generated expressions.
  ArrayRef<const Expr *> getOpExprs() const {
    return llvm::makeArrayRef(getVars().end(), numberOfVariables());
  }
  /// \brief Return the list of 1st helper parameters.
  ArrayRef<const Expr *> getHelperParameters1st() const {
    return llvm::makeArrayRef(getOpExprs().end(), numberOfVariables());
  }
  /// \brief Return the list of 2nd helper parameters.
  ArrayRef<const Expr *> getHelperParameters2nd() const {
    return llvm::makeArrayRef(getHelperParameters1st().end(), numberOfVariables());
  }

  /// \brief Return the list of all default initializations.
  ArrayRef<const Expr *> getDefaultInits() const {
    return llvm::makeArrayRef(getHelperParameters2nd().end(), numberOfVariables());
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(getDefaultInits().end()));
  }
};

/// \brief This represents 'schedule' clause in the '#pragma omp ...' directive.
///
/// \code
/// #pragma omp for schedule(static, 3)
/// \endcode
/// In this example directive '#pragma omp for' has 'schedule'
/// clause with arguments 'static' and '3'.
///
class OMPScheduleClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief A kind of the 'schedule' clause.
  OpenMPScheduleClauseKind Kind;
  /// \brief Start location of the kind in cource code.
  SourceLocation KindLoc;
  /// \brief Chunk size.
  Stmt *ChunkSize;

  /// \brief Set kind of the clauses.
  ///
  /// \param K Argument of clause.
  ///
  void setScheduleKind(OpenMPScheduleClauseKind K) { Kind = K; }
  /// \brief Set kind location.
  ///
  /// \param KLoc Kind location.
  ///
  void setScheduleKindLoc(SourceLocation KLoc) { KindLoc = KLoc; }
  /// \brief Set chunk size.
  ///
  /// \param E Chunk size.
  ///
  void setChunkSize(Expr *E) { ChunkSize = cast<Stmt>(E); }
public:
  /// \brief Build 'schedule' clause with argument \a Kind and
  /// an expression \a E.
  ///
  /// \brief K Argument of the clause.
  /// \brief KLoc Starting location of the argument.
  /// \brief E Chunk size.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  ///
  OMPScheduleClause(OpenMPScheduleClauseKind K, SourceLocation KLoc, Expr *E,
                    SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_schedule, StartLoc, EndLoc), Kind(K), KindLoc(KLoc),
      ChunkSize(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPScheduleClause()
    : OMPClause(OMPC_schedule, SourceLocation(), SourceLocation()),
      Kind(OMPC_SCHEDULE_unknown), KindLoc(SourceLocation()), ChunkSize(0) { }

  /// \brief Get kind of the clause.
  ///
  OpenMPScheduleClauseKind getScheduleKind() const { return Kind; }
  /// \brief Get kind location.
  ///
  SourceLocation getScheduleKindLoc() { return KindLoc; }
  /// \brief Get chunk size.
  ///
  Expr *getChunkSize() { return dyn_cast_or_null<Expr>(ChunkSize); }
  /// \brief Get chunk size.
  ///
  Expr *getChunkSize() const { return dyn_cast_or_null<Expr>(ChunkSize); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_schedule;
  }

  StmtRange children() {
    return StmtRange(&ChunkSize, &ChunkSize + 1);
  }
};

/// \brief This represents 'ordered' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp for ordered
/// \endcode
/// In this example directive '#pragma omp for' has clause 'ordered'.
///
class OMPOrderedClause : public OMPClause {
public:
  /// \brief Build 'ordered' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPOrderedClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_ordered, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPOrderedClause()
    : OMPClause(OMPC_ordered, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_ordered;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'nowait' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp for nowait
/// \endcode
/// In this example directive '#pragma omp for' has clause 'nowait'.
///
class OMPNowaitClause : public OMPClause {
public:
  /// \brief Build 'nowait' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPNowaitClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_nowait, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPNowaitClause()
    : OMPClause(OMPC_nowait, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_nowait;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'untied' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp task untied
/// \endcode
/// In this example directive '#pragma omp task' has clause 'untied'.
///
class OMPUntiedClause : public OMPClause {
public:
  /// \brief Build 'untied' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPUntiedClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_untied, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPUntiedClause()
    : OMPClause(OMPC_untied, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_untied;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'mergeable' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp task mergeable
/// \endcode
/// In this example directive '#pragma omp task' has clause 'mergeable'.
///
class OMPMergeableClause : public OMPClause {
public:
  /// \brief Build 'mergeable' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPMergeableClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_mergeable, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPMergeableClause()
    : OMPClause(OMPC_mergeable, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_mergeable;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'read' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp atomic read
/// \endcode
/// In this example directive '#pragma omp atomic' has clause 'read'.
///
class OMPReadClause : public OMPClause {
public:
  /// \brief Build 'read' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPReadClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_read, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPReadClause()
    : OMPClause(OMPC_read, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_read;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'write' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp atomic write
/// \endcode
/// In this example directive '#pragma omp atomic' has clause 'write'.
///
class OMPWriteClause : public OMPClause {
public:
  /// \brief Build 'write' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPWriteClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_write, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPWriteClause()
    : OMPClause(OMPC_write, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_write;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'update' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp atomic update
/// \endcode
/// In this example directive '#pragma omp atomic' has clause 'update'.
///
class OMPUpdateClause : public OMPClause {
public:
  /// \brief Build 'update' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPUpdateClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_update, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPUpdateClause()
    : OMPClause(OMPC_update, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_update;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'capture' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp atomic capture
/// \endcode
/// In this example directive '#pragma omp atomic' has clause 'capture'.
///
class OMPCaptureClause : public OMPClause {
public:
  /// \brief Build 'write' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPCaptureClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_capture, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPCaptureClause()
    : OMPClause(OMPC_capture, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_capture;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'seq_cst' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp atomic capture seq_cst
/// \endcode
/// In this example directive '#pragma omp atomic' has clauses 'capture' and
/// 'seq_cst'.
///
class OMPSeqCstClause : public OMPClause {
public:
  /// \brief Build 'seq_cst' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPSeqCstClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_seq_cst, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPSeqCstClause()
    : OMPClause(OMPC_seq_cst, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_seq_cst;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents clause 'flush' in the '#pragma omp ...' directives.
///
/// \code
/// #pragma omp flush(a,b)
/// \endcode
/// In this example directive '#pragma omp flush' has pseudo clause 'flush'
/// with the variables 'a' and 'b'.
///
class OMPFlushClause : public OMPClause,
                       public OMPVarList<OMPFlushClause> {
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPFlushClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N)
    : OMPClause(OMPC_flush, StartLoc, EndLoc), OMPVarList<OMPFlushClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPFlushClause(unsigned N)
    : OMPClause(OMPC_flush, SourceLocation(), SourceLocation()),
      OMPVarList<OMPFlushClause>(N) { }
public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPFlushClause *Create(ASTContext &C,
                                SourceLocation StartLoc,
                                SourceLocation EndLoc,
                                ArrayRef<Expr *> VL);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPFlushClause *CreateEmpty(ASTContext &C, unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_flush;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(varlist_end()));
  }
};

template <typename T> struct make_ptr_clause       { typedef       T *type; };
template <typename T> struct make_const_ptr_clause { typedef const T *type; };
/// \brief This class implements a simple visitor for OMPClause
/// subclasses.
template<class ImplClass, template <typename> class Ptr, typename RetTy>
class OMPClauseVisitorBase {
public:
#define PTR(CLASS) typename Ptr<CLASS>::type
#define DISPATCH(CLASS) \
  return static_cast<ImplClass*>(this)->Visit##CLASS(static_cast<PTR(CLASS)>(S))

#define OPENMP_CLAUSE(Name, Class)                              \
  RetTy Visit ## Class (PTR(Class) S) { DISPATCH(Class); }
#include "clang/Basic/OpenMPKinds.def"

  RetTy Visit(PTR(OMPClause) S) {
    // Top switch clause: visit each OMPClause.
    switch (S->getClauseKind()) {
    default: llvm_unreachable("Unknown stmt kind!");
#define OPENMP_CLAUSE(Name, Class)                              \
    case OMPC_ ## Name : return Visit ## Class(static_cast<PTR(Class)>(S));
#include "clang/Basic/OpenMPKinds.def"
    }
  }
  // Base case, ignore it. :)
  RetTy VisitOMPClause(PTR(OMPClause) Node) { return RetTy(); }
#undef PTR
#undef DISPATCH
};

template<class ImplClass, typename RetTy=void>
class OMPClauseVisitor :
      public OMPClauseVisitorBase <ImplClass, make_ptr_clause, RetTy> {};
template<class ImplClass, typename RetTy=void>
class ConstOMPClauseVisitor :
      public OMPClauseVisitorBase <ImplClass, make_const_ptr_clause, RetTy> {};

//===----------------------------------------------------------------------===//
// AST classes for directives.
//===----------------------------------------------------------------------===//

/// \brief This is a basic class for representing single OpenMP executable
/// directive.
///
class OMPExecutableDirective : public Stmt {
  friend class ASTStmtReader;
  /// \brief Kind of the directive.
  OpenMPDirectiveKind Kind;
  /// \brief Starting location of the directive kind.
  SourceLocation StartLoc;
  /// \brief Ending location of the directive.
  SourceLocation EndLoc;
  /// \brief Number of clauses.
  unsigned NumClauses;
  /// \brief Pointer to the list of clauses.
  OMPClause ** const Clauses;
  /// \brief Number of associated expressions and statements.
  unsigned NumStmts;
  /// \brief Has associated statement.
  bool AStmt;
protected:
  /// \brief Build instance of directive of class \a K.
  ///
  /// \param SC Statement class.
  /// \param K Kind of OpenMP directive.
  /// \param SL Starting location of the directive kind.
  /// \param EL Ending location of the directive.
  /// \param N Number of clauses.
  /// \param ClausesAndStmt A pointer to the buffer for clauses.
  ///
  OMPExecutableDirective(StmtClass SC, OpenMPDirectiveKind K,
                         SourceLocation StartLoc, SourceLocation EndLoc,
                         unsigned N, OMPClause **CL, bool AStmt, unsigned NumStmts)
    : Stmt(SC), Kind(K), StartLoc(StartLoc), EndLoc(EndLoc),
      NumClauses(N), Clauses(CL), NumStmts(NumStmts), AStmt(AStmt) { }

  /// \brief Fetches the list of clauses associated with this directive.
  llvm::MutableArrayRef<OMPClause *> getClauses() {
    return llvm::MutableArrayRef<OMPClause *>(Clauses, NumClauses);
  }

  /// \brief Fetches the list of clauses associated with this directive.
  ArrayRef<OMPClause *> getClauses() const {
    return ArrayRef<OMPClause *>(Clauses, NumClauses);
  }

  /// \brief Sets the list of variables for this clause.
  ///
  /// \brief Clauses The list of clauses for the directive.
  ///
  void setClauses(ArrayRef<OMPClause *> CL);

  /// \brief Set the associated statement for the directive.
  ///
  /// /param S Associated statement.
  ///
  void setAssociatedStmt(Stmt *S) {
    assert(AStmt && "No associated stmt allowed.");
    *reinterpret_cast<Stmt **>(&Clauses[NumClauses]) = S;
  }

public:
  /// \brief Return starting location of directive kind.
  SourceLocation getLocStart() const { return StartLoc; }
  /// \brief Return ending location of directive.
  SourceLocation getLocEnd() const { return EndLoc; }

  /// \brief Set starting location of directive kind.
  ///
  /// \brief Loc New starting location of directive.
  ///
  void setLocStart(SourceLocation Loc) { StartLoc = Loc; }
  /// \brief Set ending location of directive.
  ///
  /// \brief Loc New ending location of directive.
  ///
  void setLocEnd(SourceLocation Loc) { EndLoc = Loc; }

  /// \brief Get number of clauses.
  unsigned getNumClauses() const { return NumClauses; }

  /// \brief Fetches specified clause.
  ///
  /// \param i Number of clause.
  ///
  OMPClause *getClause(unsigned i) {
    assert(i < NumClauses && "Wrong number of clause!");
    return getClauses()[i];
  }

  /// \brief Fetches specified clause.
  ///
  /// \param i Number of clause.
  ///
  OMPClause *getClause(unsigned i) const {
    assert(i < NumClauses && "Wrong number of clause!");
    return getClauses()[i];
  }

  /// \brief Return statement associated with the directive.
  Stmt *getAssociatedStmt() {
    return AStmt ? *reinterpret_cast<Stmt **>(&Clauses[NumClauses]) : 0;
  }

  /// \brief Return statement associated with the directive.
  Stmt *getAssociatedStmt() const {
    return AStmt ? *reinterpret_cast<Stmt **>(&Clauses[NumClauses]) : 0;
  }

  bool hasAssociatedStmt() const { return AStmt; }

  OpenMPDirectiveKind getDirectiveKind() const { return Kind; }

  static bool classof(const Stmt *S) {
    return S->getStmtClass() >= firstOMPExecutableDirectiveConstant &&
           S->getStmtClass() <= lastOMPExecutableDirectiveConstant;
  }

  child_range children() {
    return child_range(reinterpret_cast<Stmt **>(&Clauses[NumClauses]),
                       reinterpret_cast<Stmt **>(&Clauses[NumClauses]) + NumStmts);
  }

  ArrayRef<OMPClause *> clauses() {
    return getClauses();
  }
  ArrayRef<OMPClause *> clauses() const {
    return getClauses();
  }
};

/// \brief This represents '#pragma omp parallel' directive.
///
/// \code
/// #pragma omp parallel private(a,b) reduction(+: c,d)
/// \endcode
/// In this example directive '#pragma omp parallel' has clauses 'private'
/// with the variables 'a' and 'b' and 'reduction' with operator '+' and
/// variables 'c' and 'd'.
///
class OMPParallelDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPParallelDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                       unsigned N)
    : OMPExecutableDirective(OMPParallelDirectiveClass, OMPD_parallel,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPParallelDirective(unsigned N)
    : OMPExecutableDirective(OMPParallelDirectiveClass, OMPD_parallel,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPParallelDirective *Create(ASTContext &C,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc,
                                      ArrayRef<OMPClause *> Clauses,
                                      Stmt *AssociatedStmt);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPParallelDirective *CreateEmpty(ASTContext &C, unsigned N,
                                           EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPParallelDirectiveClass;
  }
};

/// \brief This represents '#pragma omp for' directive.
///
/// \code
/// #pragma omp for private(a,b) reduction(+: c,d) ordered
/// \endcode
/// In this example directive '#pragma omp for' has clauses 'private'
/// with the variables 'a' and 'b', 'reduction' with operator '+' and
/// variables 'c' and 'd' and 'ordered'.
///
class OMPForDirective : public OMPExecutableDirective {
  friend class ASTStmtReader;
  unsigned CollapsedNum;
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPForDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                  unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPForDirectiveClass, OMPD_for,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true,
                             4 + CollapsedNum),
      CollapsedNum(CollapsedNum) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPForDirective(unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPForDirectiveClass, OMPD_for,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1),
                             true, 4 + CollapsedNum),
      CollapsedNum(CollapsedNum) { }
  void setNewIterVar(Expr *V) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[1] = V;
  }
  void setNewIterEnd(Expr *E) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[2] = E;
  }
  void setInit(Expr *I) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[3] = I;
  }
  void setCounters(ArrayRef<Expr *> VL) {
    assert(VL.size() == CollapsedNum &&
           "Number of variables is not the same as the number of collapsed loops.");
    std::copy(VL.begin(), VL.end(),
              &(reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[4]));
  }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPForDirective *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<OMPClause *> Clauses,
                                 Stmt *AssociatedStmt, Expr *NewIterVar,
                                 Expr *NewIterEnd, Expr *Init,
                                 ArrayRef<Expr *> VarCnts);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPForDirective *CreateEmpty(ASTContext &C, unsigned CollapsedNum,
                                      unsigned N, EmptyShell);

  Expr *getNewIterVar() const {
    return cast<Expr>(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() const {
    return cast<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[2]);
  }
  Expr *getInit() const {
    return cast<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[3]);
  }
  ArrayRef<Expr *> getCounters() const {
    return llvm::makeArrayRef(reinterpret_cast<Expr * const *>(&(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[4])),
                              CollapsedNum);
  }
  unsigned getCollapsedNumber() const { return CollapsedNum; }
  Expr *getNewIterVar() {
    return cast<Expr>(reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() {
    return cast<Expr>(reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[2]);
  }
  Expr *getInit() {
    return cast<Expr>(reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[3]);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPForDirectiveClass;
  }
};

/// \brief This represents '#pragma omp sections' directive.
///
/// \code
/// #pragma omp sections private(a,b) reduction(+: c,d) nowait
/// \endcode
/// In this example directive '#pragma omp sections' has clauses 'private'
/// with the variables 'a' and 'b', 'reduction' with operator '+' and
/// variables 'c' and 'd' and 'nowait'.
///
class OMPSectionsDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPSectionsDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                       unsigned N)
    : OMPExecutableDirective(OMPSectionsDirectiveClass, OMPD_sections,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPSectionsDirective(unsigned N)
    : OMPExecutableDirective(OMPSectionsDirectiveClass, OMPD_sections,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPSectionsDirective *Create(ASTContext &C,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc,
                                      ArrayRef<OMPClause *> Clauses,
                                      Stmt *AssociatedStmt);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPSectionsDirective *CreateEmpty(ASTContext &C, unsigned N,
                                           EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPSectionsDirectiveClass;
  }
};

/// \brief This represents '#pragma omp section' directive.
///
/// \code
/// #pragma omp section
/// \endcode
/// In this example directive '#pragma omp section' is used.
///
class OMPSectionDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPSectionDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPSectionDirectiveClass, OMPD_section,
                             StartLoc, EndLoc, 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPSectionDirective()
    : OMPExecutableDirective(OMPSectionDirectiveClass, OMPD_section,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPSectionDirective *Create(ASTContext &C,
                                     SourceLocation StartLoc,
                                     SourceLocation EndLoc,
                                     Stmt *AssociatedStmt);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  ///
  static OMPSectionDirective *CreateEmpty(ASTContext &C,
                                           EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPSectionDirectiveClass;
  }
};

/// \brief This represents '#pragma omp single' directive.
///
/// \code
/// #pragma omp single private(a,b) copyprivate(c,d)
/// \endcode
/// In this example directive '#pragma omp single' has clauses 'private'
/// with the variables 'a' and 'b', 'copyprivate' with variables 'c' and 'd'.
///
class OMPSingleDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPSingleDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                     unsigned N)
    : OMPExecutableDirective(OMPSingleDirectiveClass, OMPD_single,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPSingleDirective(unsigned N)
    : OMPExecutableDirective(OMPSingleDirectiveClass, OMPD_single,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPSingleDirective *Create(ASTContext &C,
                                    SourceLocation StartLoc,
                                    SourceLocation EndLoc,
                                    ArrayRef<OMPClause *> Clauses,
                                    Stmt *AssociatedStmt);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPSingleDirective *CreateEmpty(ASTContext &C, unsigned N,
                                         EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPSingleDirectiveClass;
  }
};

/// \brief This represents '#pragma omp task' directive.
///
/// \code
/// #pragma omp task private(a,b) firstprivate(c,d)
/// \endcode
/// In this example directive '#pragma omp task' has clauses 'private'
/// with the variables 'a' and 'b', 'firstprivate' with variables 'c' and 'd'.
///
class OMPTaskDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPTaskDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                   unsigned N)
    : OMPExecutableDirective(OMPTaskDirectiveClass, OMPD_task,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPTaskDirective(unsigned N)
    : OMPExecutableDirective(OMPTaskDirectiveClass, OMPD_task,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPTaskDirective *Create(ASTContext &C,
                                  SourceLocation StartLoc,
                                  SourceLocation EndLoc,
                                  ArrayRef<OMPClause *> Clauses,
                                  Stmt *AssociatedStmt);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPTaskDirective *CreateEmpty(ASTContext &C, unsigned N,
                                       EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPTaskDirectiveClass;
  }
};

/// \brief This represents '#pragma omp taskyield' directive.
///
/// \code
/// #pragma omp taskyield
/// \endcode
/// In this example directive '#pragma omp taskyield' is used.
///
class OMPTaskyieldDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPTaskyieldDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPTaskyieldDirectiveClass, OMPD_taskyield,
                             StartLoc, EndLoc, 0, 0, false, 0) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPTaskyieldDirective()
    : OMPExecutableDirective(OMPTaskyieldDirectiveClass, OMPD_taskyield,
                             SourceLocation(), SourceLocation(), 0, 0, false, 0) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  static OMPTaskyieldDirective *Create(ASTContext &C,
                                       SourceLocation StartLoc,
                                       SourceLocation EndLoc);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPTaskyieldDirective *CreateEmpty(ASTContext &C,
                                            EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPTaskyieldDirectiveClass;
  }
};

/// \brief This represents '#pragma omp master' directive.
///
/// \code
/// #pragma omp master
/// \endcode
/// In this example directive '#pragma omp master' is used.
///
class OMPMasterDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPMasterDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPMasterDirectiveClass, OMPD_master,
                             StartLoc, EndLoc, 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPMasterDirective()
    : OMPExecutableDirective(OMPMasterDirectiveClass, OMPD_master,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPMasterDirective *Create(ASTContext &C,
                                    SourceLocation StartLoc,
                                    SourceLocation EndLoc,
                                    Stmt *AssociatedStmt);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPMasterDirective *CreateEmpty(ASTContext &C,
                                         EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPMasterDirectiveClass;
  }
};

/// \brief This represents '#pragma omp critical' directive.
///
/// \code
/// #pragma omp critical
/// \endcode
/// In this example directive '#pragma omp critical' is used.
///
class OMPCriticalDirective : public OMPExecutableDirective {
  friend class ASTStmtReader;
  /// \brief Name of thee directive.
  DeclarationNameInfo DirName;
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPCriticalDirective(DeclarationNameInfo Name, SourceLocation StartLoc,
                       SourceLocation EndLoc)
    : OMPExecutableDirective(OMPCriticalDirectiveClass, OMPD_critical,
                             StartLoc, EndLoc, 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1),
                             DirName(Name) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPCriticalDirective()
    : OMPExecutableDirective(OMPCriticalDirectiveClass, OMPD_critical,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1),
      DirName() { }
  /// \brief Set name of the directive.
  ///
  /// \param Name Name of the directive.
  ///
  void setDirectiveName(const DeclarationNameInfo &Name) { DirName = Name; }

public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPCriticalDirective *Create(ASTContext &C,
                                      DeclarationNameInfo DirName,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc,
                                      Stmt *AssociatedStmt);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPCriticalDirective *CreateEmpty(ASTContext &C,
                                           EmptyShell);

  /// \brief Return name of the directive.
  ///
  DeclarationNameInfo getDirectiveName() const { return DirName; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPCriticalDirectiveClass;
  }
};

/// \brief This represents '#pragma omp barrier' directive.
///
/// \code
/// #pragma omp barrier
/// \endcode
/// In this example directive '#pragma omp barrier' is used.
///
class OMPBarrierDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPBarrierDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPBarrierDirectiveClass, OMPD_barrier,
                             StartLoc, EndLoc, 0, 0, false, 0) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPBarrierDirective()
    : OMPExecutableDirective(OMPBarrierDirectiveClass, OMPD_barrier,
                             SourceLocation(), SourceLocation(), 0, 0, false, 0) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  static OMPBarrierDirective *Create(ASTContext &C,
                                     SourceLocation StartLoc,
                                     SourceLocation EndLoc);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPBarrierDirective *CreateEmpty(ASTContext &C,
                                          EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPBarrierDirectiveClass;
  }
};

/// \brief This represents '#pragma omp taskwait' directive.
///
/// \code
/// #pragma omp taskwait
/// \endcode
/// In this example directive '#pragma omp taskwait' is used.
///
class OMPTaskwaitDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPTaskwaitDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPTaskwaitDirectiveClass, OMPD_taskwait,
                             StartLoc, EndLoc, 0, 0, false, 0) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPTaskwaitDirective()
    : OMPExecutableDirective(OMPTaskwaitDirectiveClass, OMPD_taskwait,
                             SourceLocation(), SourceLocation(), 0, 0, false, 0) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  static OMPTaskwaitDirective *Create(ASTContext &C,
                                      SourceLocation StartLoc,
                                      SourceLocation EndLoc);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPTaskwaitDirective *CreateEmpty(ASTContext &C,
                                           EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPTaskwaitDirectiveClass;
  }
};

/// \brief This represents '#pragma omp taskgroup' directive.
///
/// \code
/// #pragma omp taskgroup
/// \endcode
/// In this example directive '#pragma omp taskgroup' is used.
///
class OMPTaskgroupDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPTaskgroupDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPTaskgroupDirectiveClass, OMPD_taskgroup,
                             StartLoc, EndLoc, 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPTaskgroupDirective()
    : OMPExecutableDirective(OMPTaskgroupDirectiveClass, OMPD_taskgroup,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPTaskgroupDirective *Create(ASTContext &C,
                                       SourceLocation StartLoc,
                                       SourceLocation EndLoc,
                                       Stmt *AssociatedStmt);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPTaskgroupDirective *CreateEmpty(ASTContext &C,
                                            EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPTaskgroupDirectiveClass;
  }
};

/// \brief This represents '#pragma omp atomic' directive.
///
/// \code
/// #pragma omp atomic capture seq_cst
/// \endcode
/// In this example directive '#pragma omp atomic' has clauses 'capture' and
/// 'seq_cst'.
///
class OMPAtomicDirective : public OMPExecutableDirective {
  friend class ASTStmtReader;
  /// \brief Binary operator for atomic.
  BinaryOperatorKind BinOp;
  /// \brief Capture kind - true, if after expr, false, if before.
  bool CaptureAfter;
  /// \brief true, if operator for 'x' is reversed, false - otherwise.
  bool Reversed;
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPAtomicDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                     unsigned N)
    : OMPExecutableDirective(OMPAtomicDirectiveClass, OMPD_atomic,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 4),
      BinOp(BO_Assign), CaptureAfter(false), Reversed(false) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPAtomicDirective(unsigned N)
    : OMPExecutableDirective(OMPAtomicDirectiveClass, OMPD_atomic,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), true, 4),
      BinOp(BO_Assign), CaptureAfter(false), Reversed(false) { }

  /// \brief Sets binary operator for atomic.
  void setOperator(BinaryOperatorKind Op) { BinOp = Op; }

  /// \brief Sets 'v' parameter for atomic.
  void setV(Expr *V) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[1] = V;
  }

  /// \brief Sets 'x' parameter for atomic.
  void setX(Expr *X) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[2] = X;
  }

  /// \brief Sets 'expr' parameter for atomic.
  void setExpr(Expr *OpExpr) {
    reinterpret_cast<Stmt **>(&reinterpret_cast<OMPClause **>(this + 1)[getNumClauses()])[3] = OpExpr;
  }

  /// \brief Sets capture kind parameter for atomic.
  void setCaptureAfter(bool CaptureKind) {
    CaptureAfter = CaptureKind;
  }

  /// \brief Sets update rules for 'x' parameter for atomic.
  void setReversed(bool IsReversed) {
    Reversed = IsReversed;
  }

public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPAtomicDirective *Create(ASTContext &C,
                                    SourceLocation StartLoc,
                                    SourceLocation EndLoc,
                                    ArrayRef<OMPClause *> Clauses,
                                    Stmt *AssociatedStmt,
                                    Expr *V, Expr *X, Expr *OpExpr,
                                    BinaryOperatorKind Op,
                                    bool CaptureAfter, bool Reversed);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPAtomicDirective *CreateEmpty(ASTContext &C, unsigned N,
                                         EmptyShell);

  /// \brief Returns binary operator for atomic.
  BinaryOperatorKind getOperator() const { return BinOp; }

  /// \brief Returns 'v' parameter for atomic.
  Expr *getV() const {
    return reinterpret_cast<Expr * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[1];
  }

  /// \brief Returns 'x' parameter for atomic.
  Expr *getX() const {
    return reinterpret_cast<Expr * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[2];
  }

  /// \brief Returns 'expr' parameter for atomic.
  Expr *getExpr() const {
    return reinterpret_cast<Expr * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[3];
  }

  /// \brief Returns capture kind parameter for atomic.
  bool isCaptureAfter() const { return CaptureAfter; }

  /// \brief Returns update kind of 'x' parameter for atomic.
  bool isReversed() const { return Reversed; }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPAtomicDirectiveClass;
  }
};

/// \brief This represents '#pragma omp flush' directive.
///
/// \code
/// #pragma omp flush(a,b)
/// \endcode
/// In this example directive '#pragma omp flush' has list of variables 'a' and
/// 'b'.
///
class OMPFlushDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param N The number of clauses.
  ///
  OMPFlushDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                    unsigned N)
    : OMPExecutableDirective(OMPFlushDirectiveClass, OMPD_flush,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(this + 1), false, 0) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPFlushDirective(unsigned N)
    : OMPExecutableDirective(OMPFlushDirectiveClass, OMPD_flush,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(this + 1), false, 0) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param Clauses List of clauses.
  ///
  static OMPFlushDirective *Create(ASTContext &C,
                                   SourceLocation StartLoc,
                                   SourceLocation EndLoc,
                                   ArrayRef<OMPClause *> Clauses);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPFlushDirective *CreateEmpty(ASTContext &C, unsigned N,
                                        EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPFlushDirectiveClass;
  }
};

/// \brief This represents '#pragma omp ordered' directive.
///
/// \code
/// #pragma omp ordered
/// \endcode
/// In this example directive '#pragma omp ordered' is used.
///
class OMPOrderedDirective : public OMPExecutableDirective {
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  ///
  OMPOrderedDirective(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPExecutableDirective(OMPOrderedDirectiveClass, OMPD_ordered,
                             StartLoc, EndLoc, 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPOrderedDirective()
    : OMPExecutableDirective(OMPOrderedDirectiveClass, OMPD_ordered,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(this + 1), true, 1) { }
public:
  /// \brief Creates directive.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
  /// \param AssociatedStmt Statement, associated with the directive.
  ///
  static OMPOrderedDirective *Create(ASTContext &C,
                                     SourceLocation StartLoc,
                                     SourceLocation EndLoc,
                                     Stmt *AssociatedStmt);

  /// \brief Creates an empty directive.
  ///
  /// \param C AST context.
  ///
  static OMPOrderedDirective *CreateEmpty(ASTContext &C,
                                          EmptyShell);

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPOrderedDirectiveClass;
  }
};

}  // end namespace clang

#endif
