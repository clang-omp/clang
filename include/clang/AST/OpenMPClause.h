//===- OpenMPClause.h - Classes for OpenMP clauses --------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file defines OpenMP AST classes for clauses.
/// There are clauses for executable directives, clauses for declarative
/// directives and clauses which can be used in both kinds of directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_OPENMPCLAUSE_H
#define LLVM_CLANG_AST_OPENMPCLAUSE_H

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
  void setCondition(Expr *E) { Condition = E; }
public:
  /// \brief Build 'if' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPIfClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_if, StartLoc, EndLoc), Condition(E) { }

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
  void setCondition(Expr *E) { Condition = E; }
public:
  /// \brief Build 'if' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPFinalClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_final, StartLoc, EndLoc), Condition(E) { }

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
  void setNumThreads(Expr *E) { NumThreads = E; }
public:
  /// \brief Build 'num_threads' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPNumThreadsClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_num_threads, StartLoc, EndLoc), NumThreads(E) { }

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
  void setNumForLoops(Expr *E) { NumForLoops = E; }
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

/// \brief This represents 'proc_bind' clause in the '#pragma omp ...' directive.
///
/// \code
/// #pragma omp parallel proc_bind(master)
/// \endcode
/// In this example directive '#pragma omp parallel' has simple 'proc_bind'
/// clause with thread affinity 'master'.
///
class OMPProcBindClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Thread affinity defined in 'proc_bind' clause.
  OpenMPProcBindClauseKind ThreadAffinity;
  /// \brief Start location of the thread affinity in source code.
  SourceLocation ThreadAffinityLoc;

  /// \brief Set thread affinity of the clauses.
  ///
  /// \param K Argument of clause.
  ///
  void setThreadAffinity(OpenMPProcBindClauseKind K) { ThreadAffinity = K; }

  /// \brief Set argument location.
  ///
  /// \param KLoc Argument location.
  ///
  void setThreadAffinityLoc(SourceLocation KLoc) { ThreadAffinityLoc = KLoc; }
public:
  /// \brief Build 'proc_bind' clause with argument \a A ('master', 'close' or
  /// 'spread').
  ///
  /// \brief A Argument of the clause ('master', 'close' or 'spread').
  /// \brief ALoc Starting location of the argument.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  ///
  OMPProcBindClause(OpenMPProcBindClauseKind A, SourceLocation ALoc,
                    SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_proc_bind, StartLoc, EndLoc), ThreadAffinity(A),
      ThreadAffinityLoc(ALoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPProcBindClause()
    : OMPClause(OMPC_proc_bind, SourceLocation(), SourceLocation()),
      ThreadAffinity(OMPC_PROC_BIND_unknown),
      ThreadAffinityLoc(SourceLocation()) { }

  /// \brief Fetches thread affinity.
  ///
  OpenMPProcBindClauseKind getThreadAffinity() const { return ThreadAffinity; }

  /// \brief Fetches location of clause kind.
  ///
  SourceLocation getThreadAffinityLoc() const { return ThreadAffinityLoc; }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_proc_bind;
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
  /// \brief Name of custom operator.
  DeclarationName OperatorName;
  /// \brief Start location of the kind in cource code.
  SourceLocation OperatorLoc;

  /// \brief Set operator for the clause.
  ///
  /// \param Op Operator for the clause.
  ///
  void setOperator(OpenMPReductionClauseOperator Op) { Operator = Op; }

  /// \brief Set operator name for the clause.
  ///
  /// \param Op Operator name for the clause.
  ///
  void setOpName(DeclarationName OpName) { OperatorName = OpName; }

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
                     OpenMPReductionClauseOperator Op, DeclarationName OpName,
                     SourceLocation OpLoc)
    : OMPClause(OMPC_reduction, StartLoc, EndLoc),
      OMPVarList<OMPReductionClause>(N), Operator(Op), OperatorName(OpName),
      OperatorLoc(OpLoc) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPReductionClause(unsigned N)
    : OMPClause(OMPC_reduction, SourceLocation(), SourceLocation()),
      OMPVarList<OMPReductionClause>(N), Operator(OMPC_REDUCTION_unknown),
      OperatorName(), OperatorLoc(SourceLocation()) { }

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
                                    DeclarationName OpName,
                                    SourceLocation OpLoc);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPReductionClause *CreateEmpty(ASTContext &C, unsigned N);

  /// \brief Fetches operator for the clause.
  OpenMPReductionClauseOperator getOperator() const { return Operator; }

  /// \brief Fetches operator name for the clause.
  DeclarationName getOpName() const { return OperatorName; }

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
  void setChunkSize(Expr *E) { ChunkSize = E; }
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

/// \brief This represents 'dist_schedule' clause in the
/// '#pragma omp ...' directive.
///
/// \code
/// #pragma omp distribute dist_schedule(static, 3)
/// \endcode
/// In this example directive '#pragma omp distribute' has 'dist_schedule'
/// clause with arguments 'static' and '3'.
///
class OMPDistScheduleClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief A kind of the 'dist_schedule' clause.
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
  void setChunkSize(Expr *E) { ChunkSize = E; }
public:
  /// \brief Build 'dist_schedule' clause with argument \a Kind and
  /// an expression \a E.
  ///
  /// \brief K Argument of the clause.
  /// \brief KLoc Starting location of the argument.
  /// \brief E Chunk size.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  ///
  OMPDistScheduleClause(OpenMPScheduleClauseKind K, SourceLocation KLoc, Expr *E,
                        SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_dist_schedule, StartLoc, EndLoc), Kind(K), KindLoc(KLoc),
      ChunkSize(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPDistScheduleClause()
    : OMPClause(OMPC_dist_schedule, SourceLocation(), SourceLocation()),
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
    return T->getClauseKind() == OMPC_dist_schedule;
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

/// \brief This represents 'inbranch' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp declare simd inbranch
/// \endcode
/// In this example directive '#pragma omp declare simd' has clause 'inbranch'.
///
class OMPInBranchClause : public OMPClause {
public:
  /// \brief Build 'inbranch' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPInBranchClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_inbranch, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPInBranchClause()
    : OMPClause(OMPC_inbranch, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_inbranch;
  }

  StmtRange children() {
    return StmtRange();
  }
};

/// \brief This represents 'notinbranch' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp declare simd notinbranch
/// \endcode
/// In this example directive '#pragma omp declare simd' has
/// clause 'notinbranch'.
///
class OMPNotInBranchClause : public OMPClause {
public:
  /// \brief Build 'notinbranch' clause.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPNotInBranchClause(SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_notinbranch, StartLoc, EndLoc) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPNotInBranchClause()
    : OMPClause(OMPC_notinbranch, SourceLocation(), SourceLocation()) { }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_notinbranch;
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

/// \brief This represents clause 'uniform' in the '#pragma omp ...' directives.
///
/// \code
/// #pragma omp declare simd uniform(a,b)
/// \endcode
/// In this example directive '#pragma omp declare simd' has clause 'uniform'
/// with the variables 'a' and 'b'.
///
class OMPUniformClause : public OMPClause,
                         public OMPVarList<OMPUniformClause> {
  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  ///
  OMPUniformClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N)
    : OMPClause(OMPC_uniform, StartLoc, EndLoc), OMPVarList<OMPUniformClause>(N) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPUniformClause(unsigned N)
    : OMPClause(OMPC_uniform, SourceLocation(), SourceLocation()),
      OMPVarList<OMPUniformClause>(N) { }
public:
  /// \brief Creates clause with a list of variables \a VL.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  ///
  static OMPUniformClause *Create(ASTContext &C,
                                  SourceLocation StartLoc,
                                  SourceLocation EndLoc,
                                  ArrayRef<Expr *> VL);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPUniformClause *CreateEmpty(ASTContext &C, unsigned N);

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_uniform;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(varlist_end()));
  }
};

/// \brief This represents 'safelen' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp simd safelen(4)
/// \endcode
/// In this example directive '#pragma omp simd' has clause 'safelen'
/// with single expression '4'.
/// If the safelen clause is used then no two iterations executed
/// concurrently with SIMD instructions can have a greater distance
/// in the logical iteration space than its value. The parameter of
/// the safelen clause must be a constant positive integer expression.
///
class OMPSafelenClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Safe iteration space distance.
  Stmt *Safelen;
  /// \brief Set the safe iteration space distance.
  ///
  /// \param E safe iteration space distance.
  ///
  void setSafelen(Expr *E) { Safelen = E; }
public:
  /// \brief Build 'safelen' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPSafelenClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_safelen, StartLoc, EndLoc), Safelen(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPSafelenClause()
    : OMPClause(OMPC_safelen, SourceLocation(), SourceLocation()),
      Safelen(0) { }

  /// \brief Return safe iteration space distance.
  ///
  Expr *getSafelen() { return dyn_cast_or_null<Expr>(Safelen); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_safelen;
  }

  StmtRange children() {
    return StmtRange(&Safelen, &Safelen + 1);
  }
};

/// \brief This represents 'simdlen' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp declare simd simdlen(4)
/// \endcode
/// In this example directive '#pragma omp declare simd' has clause 'simdlen'
/// with single expression '4'.
///
class OMPSimdlenClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Safe iteration space distance.
  Stmt *Simdlen;
  /// \brief Set the safe iteration space distance.
  ///
  /// \param E safe iteration space distance.
  ///
  void setSimdlen(Expr *E) { Simdlen = E; }
public:
  /// \brief Build 'simdlen' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPSimdlenClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_simdlen, StartLoc, EndLoc), Simdlen(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPSimdlenClause()
    : OMPClause(OMPC_simdlen, SourceLocation(), SourceLocation()),
      Simdlen(0) { }

  /// \brief Return safe iteration space distance.
  ///
  Expr *getSimdlen() { return dyn_cast_or_null<Expr>(Simdlen); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_simdlen;
  }

  StmtRange children() {
    return StmtRange(&Simdlen, &Simdlen + 1);
  }
};

/// \brief This represents 'num_teams' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp teams num_teams(4)
/// \endcode
/// In this example directive '#pragma omp teams' has clause 'num_teams'
/// with single expression '4'.
///
class OMPNumTeamsClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Number of teams.
  Stmt *NumTeams;
  /// \brief Set the number of teams.
  ///
  /// \param E number of teams.
  ///
  void setNumTeams(Expr *E) { NumTeams = E; }
public:
  /// \brief Build 'num_teams' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPNumTeamsClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_num_teams, StartLoc, EndLoc), NumTeams(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPNumTeamsClause()
    : OMPClause(OMPC_num_teams, SourceLocation(), SourceLocation()),
      NumTeams(0) { }

  /// \brief Return the number of teams.
  ///
  Expr *getNumTeams() { return dyn_cast_or_null<Expr>(NumTeams); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_num_teams;
  }

  StmtRange children() {
    return StmtRange(&NumTeams, &NumTeams + 1);
  }
};

/// \brief This represents 'thread_limit' clause in the '#pragma omp ...'
/// directive.
///
/// \code
/// #pragma omp teams thread_limit(4)
/// \endcode
/// In this example directive '#pragma omp teams' has clause 'thread_limit'
/// with single expression '4'.
///
class OMPThreadLimitClause : public OMPClause {
  friend class OMPClauseReader;
  /// \brief Thread limit.
  Stmt *ThreadLimit;
  /// \brief Set the thread limit.
  ///
  /// \param E thread limit.
  ///
  void setThreadLimit(Expr *E) { ThreadLimit = E; }
public:
  /// \brief Build 'thread_limit' clause.
  ///
  /// \param E Expression associated with this clause.
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  ///
  OMPThreadLimitClause(Expr *E, SourceLocation StartLoc, SourceLocation EndLoc)
    : OMPClause(OMPC_thread_limit, StartLoc, EndLoc), ThreadLimit(E) { }

  /// \brief Build an empty clause.
  ///
  explicit OMPThreadLimitClause()
    : OMPClause(OMPC_thread_limit, SourceLocation(), SourceLocation()),
      ThreadLimit(0) { }

  /// \brief Return the number of teams.
  ///
  Expr *getThreadLimit() { return dyn_cast_or_null<Expr>(ThreadLimit); }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_thread_limit;
  }

  StmtRange children() {
    return StmtRange(&ThreadLimit, &ThreadLimit + 1);
  }
};

/// \brief This represents clause 'linear' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp simd linear(a,b : 2)
/// \endcode
/// In this example directive '#pragma omp simd' has clause 'linear'
/// with variables 'a', 'b' and linear step '2'.
///
class OMPLinearClause : public OMPClause,
                        public OMPVarList<OMPLinearClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;

  /// \brief Start location of the linear step in cource code.
  SourceLocation StepLoc;

  /// \brief Set step for the clause.
  ///
  /// \param E step for the clause.
  ///
  void setStep(Expr *E) {
    *(reinterpret_cast<Stmt **>(varlist_end())) = cast_or_null<Stmt>(E);
  }

  /// \brief Set step location.
  ///
  /// \param StLoc step location.
  ///
  void setStepLoc(SourceLocation StLoc) { StepLoc = StLoc; }

  /// \brief Build clause with number of variables \a N and a step \a St.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  /// \param StLoc Location of the linear step.
  ///
  OMPLinearClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N,
                  SourceLocation StLoc)
    : OMPClause(OMPC_linear, StartLoc, EndLoc),
      OMPVarList<OMPLinearClause>(N), StepLoc(StLoc) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPLinearClause(unsigned N)
    : OMPClause(OMPC_linear, SourceLocation(), SourceLocation()),
      OMPVarList<OMPLinearClause>(N),
      StepLoc(SourceLocation()) { }

public:
  /// \brief Creates clause with a list of variables \a VL and a step
  /// \a St.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  /// \param St Linear step.
  /// \param StLoc Location of the linear step.
  ///
  static OMPLinearClause *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<Expr *> VL,
                                 Expr *St,
                                 SourceLocation StLoc);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPLinearClause *CreateEmpty(ASTContext &C, unsigned N);

  /// \brief Fetches the linear step.
  Expr *getStep() {
    return dyn_cast_or_null<Expr>(*(reinterpret_cast<Stmt **>(varlist_end())));
  }

  /// \brief Fetches location of linear step.
  SourceLocation getStepLoc() const { return StepLoc; }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_linear;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(varlist_end() + 1));
  }
};

/// \brief This represents clause 'aligned' in the '#pragma omp ...'
/// directives.
///
/// \code
/// #pragma omp simd aligned(a,b : 8)
/// \endcode
/// In this example directive '#pragma omp simd' has clause 'aligned'
/// with variables 'a', 'b' and alignment '8'.
///
class OMPAlignedClause : public OMPClause,
                         public OMPVarList<OMPAlignedClause> {
  friend class OMPClauseReader;
  friend class OMPClauseWriter;

  /// \brief Start location of the alignment in cource code.
  SourceLocation AlignmentLoc;

  /// \brief Set alignment for the clause.
  ///
  /// \param E alignment for the clause.
  ///
  void setAlignment(Expr *E) {
    *(reinterpret_cast<Stmt **>(varlist_end())) = cast_or_null<Stmt>(E);
  }

  /// \brief Set alignment location.
  ///
  /// \param ALoc alignment location.
  ///
  void setAlignmentLoc(SourceLocation ALoc) { AlignmentLoc = ALoc; }

  /// \brief Build clause with number of variables \a N.
  ///
  /// \param StartLoc Starting location of the clause.
  /// \param EndLoc Ending location of the clause.
  /// \param N Number of the variables in the clause.
  /// \param ALoc Location of the alignment.
  ///
  OMPAlignedClause(SourceLocation StartLoc, SourceLocation EndLoc, unsigned N,
                   SourceLocation ALoc)
    : OMPClause(OMPC_aligned, StartLoc, EndLoc),
      OMPVarList<OMPAlignedClause>(N), AlignmentLoc(ALoc) { }

  /// \brief Build an empty clause.
  ///
  /// \param N Number of variables.
  ///
  explicit OMPAlignedClause(unsigned N)
    : OMPClause(OMPC_aligned, SourceLocation(), SourceLocation()),
      OMPVarList<OMPAlignedClause>(N),
      AlignmentLoc(SourceLocation()) { }

public:
  /// \brief Creates clause with a list of variables \a VL and an alignment
  /// \a A.
  ///
  /// \param C AST context.
  /// \brief StartLoc Starting location of the clause.
  /// \brief EndLoc Ending location of the clause.
  /// \param VL List of references to the variables.
  /// \param A Alignment.
  /// \param ALoc Location of the alignment.
  ///
  static OMPAlignedClause *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<Expr *> VL,
                                 Expr *A,
                                 SourceLocation ALoc);
  /// \brief Creates an empty clause with the place for \a N variables.
  ///
  /// \param C AST context.
  /// \param N The number of variables.
  ///
  static OMPAlignedClause *CreateEmpty(ASTContext &C, unsigned N);

  /// \brief Fetches the alignment.
  Expr *getAlignment() {
    return dyn_cast_or_null<Expr>(*(reinterpret_cast<Stmt **>(varlist_end())));
  }

  /// \brief Fetches location of the alignment.
  SourceLocation getAlignmentLoc() const { return AlignmentLoc; }

  static bool classof(const OMPClause *T) {
    return T->getClauseKind() == OMPC_aligned;
  }

  StmtRange children() {
    return StmtRange(reinterpret_cast<Stmt **>(varlist_begin()),
                     reinterpret_cast<Stmt **>(varlist_end() + 1));
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

}  // end namespace clang

#endif
