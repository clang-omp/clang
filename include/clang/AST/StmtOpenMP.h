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
#include "clang/AST/OpenMPClause.h"

namespace clang {

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

  OMPClause ** getClausesStorage() const { return Clauses; }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPParallelDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPParallelDirective(unsigned N)
    : OMPExecutableDirective(OMPParallelDirectiveClass, OMPD_parallel,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPParallelDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPForDirective),
                                                                                     sizeof(OMPClause *))),
                             true,
                             5 + CollapsedNum),
      CollapsedNum(CollapsedNum) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPForDirective(unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPForDirectiveClass, OMPD_for,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPForDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 5 + CollapsedNum),
                             CollapsedNum(CollapsedNum) { }
  // 5 is for AssociatedStmt, NewIterVar, NewIterEnd, Init, Final
  // and CollapsedNum is for Counters.
  void setNewIterVar(Expr *V) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1] = V;
  }
  void setNewIterEnd(Expr *E) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2] = E;
  }
  void setInit(Expr *I) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3] = I;
  }
  void setFinal(Expr *F) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4] = F;
  }
  void setCounters(ArrayRef<Expr *> VL) {
    assert(VL.size() == CollapsedNum &&
           "Number of variables is not the same as the number of collapsed loops.");
    std::copy(VL.begin(), VL.end(),
              &(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[5]));
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
                                 Expr *NewIterEnd, Expr *Init, Expr *Final,
                                 ArrayRef<Expr *> VarCnts);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPForDirective *CreateEmpty(ASTContext &C, unsigned CollapsedNum,
                                      unsigned N, EmptyShell);

  Expr *getNewIterVar() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt * const *>(&getClausesStorage()[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&getClausesStorage()[getNumClauses()])[2]);
  }
  Expr *getInit() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&getClausesStorage()[getNumClauses()])[3]);
  }
  Expr *getFinal() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&getClausesStorage()[getNumClauses()])[4]);
  }
  ArrayRef<Expr *> getCounters() const {
    return llvm::makeArrayRef(reinterpret_cast<Expr * const *>(&(reinterpret_cast<Stmt * const *>(&getClausesStorage()[getNumClauses()])[5])),
                              CollapsedNum);
  }
  unsigned getCollapsedNumber() const { return CollapsedNum; }
  Expr *getNewIterVar() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2]);
  }
  Expr *getInit() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3]);
  }
  Expr *getFinal() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4]);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPForDirectiveClass;
  }
};

/// \brief This represents '#pragma omp simd' directive.
///
/// \code
/// #pragma omp simd private(a,b) linear(i,j:s) reduction(+:c,d)
/// \endcode
/// In this example directive '#pragma omp simd' has clauses 'private'
/// with the variables 'a' and 'b', 'linear' with variables 'i', 'j' and
/// linear step 's', 'reduction' with operator '+' and variables 'c' and 'd'.
///
class OMPSimdDirective : public OMPExecutableDirective {
  friend class ASTStmtReader;
  unsigned CollapsedNum;
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending location of the directive.
  /// \param N The number of clauses.
  ///
  OMPSimdDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                  unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPSimdDirectiveClass, OMPD_simd,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSimdDirective),
                                                                                     sizeof(OMPClause *))),
                             true,
                             5 + CollapsedNum),
      CollapsedNum(CollapsedNum) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPSimdDirective(unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPSimdDirectiveClass, OMPD_simd,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSimdDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 5 + CollapsedNum),
                             CollapsedNum(CollapsedNum) { }
  void setNewIterVar(Expr *V) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1] = V;
  }
  void setNewIterEnd(Expr *E) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2] = E;
  }
  void setInit(Expr *I) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3] = I;
  }
  void setFinal(Expr *F) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4] = F;
  }
  void setCounters(ArrayRef<Expr *> VL) {
    assert(VL.size() == CollapsedNum &&
           "Number of variables is not the same as the number of collapsed loops.");
    std::copy(VL.begin(), VL.end(),
              &(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[5]));
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
  static OMPSimdDirective *Create(ASTContext &C,
                                 SourceLocation StartLoc,
                                 SourceLocation EndLoc,
                                 ArrayRef<OMPClause *> Clauses,
                                 Stmt *AssociatedStmt, Expr *NewIterVar,
                                 Expr *NewIterEnd, Expr *Init, Expr *Final,
                                 ArrayRef<Expr *> VarCnts);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPSimdDirective *CreateEmpty(ASTContext &C, unsigned CollapsedNum,
                                      unsigned N, EmptyShell);

  Expr *getNewIterVar() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[2]);
  }
  Expr *getInit() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[3]);
  }
  Expr *getFinal() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[4]);
  }
  ArrayRef<Expr *> getCounters() const {
    return llvm::makeArrayRef(reinterpret_cast<Expr * const *>(&(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[5])),
                              CollapsedNum);
  }
  unsigned getCollapsedNumber() const { return CollapsedNum; }
  Expr *getNewIterVar() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2]);
  }
  Expr *getInit() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3]);
  }
  Expr *getFinal() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4]);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPSimdDirectiveClass;
  }
};

/// \brief This represents '#pragma omp for simd' directive.
///
/// \code
/// #pragma omp for simd private(a,b) linear(i,j:s) reduction(+:c,d)
/// \endcode
/// In this example directive '#pragma omp for simd' has clauses 'private'
/// with the variables 'a' and 'b', 'linear' with variables 'i', 'j' and
/// linear step 's', 'reduction' with operator '+' and variables 'c' and 'd'.
///
class OMPForSimdDirective : public OMPExecutableDirective {
  friend class ASTStmtReader;
  unsigned CollapsedNum;
  /// \brief Build directive with the given start and end location.
  ///
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending location of the directive.
  /// \param N The number of clauses.
  ///
  OMPForSimdDirective(SourceLocation StartLoc, SourceLocation EndLoc,
                      unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPForSimdDirectiveClass, OMPD_for_simd,
                             StartLoc, EndLoc, N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPForSimdDirective),
                                                                                     sizeof(OMPClause *))),
                             true,
                             5 + CollapsedNum),
      CollapsedNum(CollapsedNum) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPForSimdDirective(unsigned CollapsedNum, unsigned N)
    : OMPExecutableDirective(OMPForSimdDirectiveClass, OMPD_for_simd,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPForSimdDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 5 + CollapsedNum),
                             CollapsedNum(CollapsedNum) { }
  void setNewIterVar(Expr *V) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1] = V;
  }
  void setNewIterEnd(Expr *E) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2] = E;
  }
  void setInit(Expr *I) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3] = I;
  }
  void setFinal(Expr *F) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4] = F;
  }
  void setCounters(ArrayRef<Expr *> VL) {
    assert(VL.size() == CollapsedNum &&
           "Number of variables is not the same as the number of collapsed loops.");
    std::copy(VL.begin(), VL.end(),
              &(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[5]));
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
  static OMPForSimdDirective *Create(ASTContext &C,
                                     SourceLocation StartLoc,
                                     SourceLocation EndLoc,
                                     ArrayRef<OMPClause *> Clauses,
                                     Stmt *AssociatedStmt, Expr *NewIterVar,
                                     Expr *NewIterEnd, Expr *Init, Expr *Final,
                                     ArrayRef<Expr *> VarCnts);

  /// \brief Creates an empty directive with the place for \a N clauses.
  ///
  /// \param C AST context.
  /// \param N The number of clauses.
  ///
  static OMPForSimdDirective *CreateEmpty(ASTContext &C, unsigned CollapsedNum,
                                          unsigned N, EmptyShell);

  Expr *getNewIterVar() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[2]);
  }
  Expr *getInit() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[3]);
  }
  Expr *getFinal() const {
    return cast_or_null<Expr>(reinterpret_cast<Stmt *const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[4]);
  }
  ArrayRef<Expr *> getCounters() const {
    return llvm::makeArrayRef(reinterpret_cast<Expr * const *>(&(reinterpret_cast<Stmt * const *>(&reinterpret_cast<OMPClause * const *>(this + 1)[getNumClauses()])[5])),
                              CollapsedNum);
  }
  unsigned getCollapsedNumber() const { return CollapsedNum; }
  Expr *getNewIterVar() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1]);
  }
  Expr *getNewIterEnd() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2]);
  }
  Expr *getInit() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3]);
  }
  Expr *getFinal() {
    return cast_or_null<Expr>(reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[4]);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == OMPForSimdDirectiveClass;
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSectionsDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPSectionsDirective(unsigned N)
    : OMPExecutableDirective(OMPSectionsDirectiveClass, OMPD_sections,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSectionsDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSectionDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPSectionDirective()
    : OMPExecutableDirective(OMPSectionDirectiveClass, OMPD_section,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSectionDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSingleDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPSingleDirective(unsigned N)
    : OMPExecutableDirective(OMPSingleDirectiveClass, OMPD_single,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPSingleDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPTaskDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPTaskDirective(unsigned N)
    : OMPExecutableDirective(OMPTaskDirectiveClass, OMPD_task,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPTaskDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPMasterDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPMasterDirective()
    : OMPExecutableDirective(OMPMasterDirectiveClass, OMPD_master,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPMasterDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPCriticalDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1),
                             DirName(Name) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPCriticalDirective()
    : OMPExecutableDirective(OMPCriticalDirectiveClass, OMPD_critical,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPCriticalDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1),
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPTaskgroupDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPTaskgroupDirective()
    : OMPExecutableDirective(OMPTaskgroupDirectiveClass, OMPD_taskgroup,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPTaskgroupDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPAtomicDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 4),
      BinOp(BO_Assign), CaptureAfter(false), Reversed(false) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPAtomicDirective(unsigned N)
    : OMPExecutableDirective(OMPAtomicDirectiveClass, OMPD_atomic,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPAtomicDirective),
                                                                                     sizeof(OMPClause *))),
                             true, 4),
      BinOp(BO_Assign), CaptureAfter(false), Reversed(false) { }

  /// \brief Sets binary operator for atomic.
  void setOperator(BinaryOperatorKind Op) { BinOp = Op; }

  /// \brief Sets 'v' parameter for atomic.
  void setV(Expr *V) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[1] = V;
  }

  /// \brief Sets 'x' parameter for atomic.
  void setX(Expr *X) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[2] = X;
  }

  /// \brief Sets 'expr' parameter for atomic.
  void setExpr(Expr *OpExpr) {
    reinterpret_cast<Stmt **>(&getClausesStorage()[getNumClauses()])[3] = OpExpr;
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPFlushDirective),
                                                                                     sizeof(OMPClause *))),
                             false, 0) { }

  /// \brief Build an empty directive.
  ///
  /// \param N Number of clauses.
  ///
  explicit OMPFlushDirective(unsigned N)
    : OMPExecutableDirective(OMPFlushDirectiveClass, OMPD_flush,
                             SourceLocation(), SourceLocation(), N,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPFlushDirective),
                                                                                     sizeof(OMPClause *))),
                             false, 0) { }
public:
  /// \brief Creates directive with a list of \a Clauses.
  ///
  /// \param C AST context.
  /// \param StartLoc Starting location of the directive kind.
  /// \param EndLoc Ending Location of the directive.
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
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPOrderedDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }

  /// \brief Build an empty directive.
  ///
  explicit OMPOrderedDirective()
    : OMPExecutableDirective(OMPOrderedDirectiveClass, OMPD_ordered,
                             SourceLocation(), SourceLocation(), 0,
                             reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) +
                                                            llvm::RoundUpToAlignment(sizeof(OMPOrderedDirective),
                                                                                     sizeof(Stmt *))),
                             true, 1) { }
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
