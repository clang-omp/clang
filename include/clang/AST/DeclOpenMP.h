//===- DeclOpenMP.h - Classes for representing OpenMP directives -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// \brief This file defines OpenMP nodes for declarative directives.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_AST_OPENMP_H
#define LLVM_CLANG_AST_OPENMP_H

#include "clang/AST/Decl.h"
#include "clang/AST/OpenMPClause.h"
#include "llvm/ADT/ArrayRef.h"

namespace clang {

/// \brief This represents '#pragma omp threadprivate ...' directive.
/// For example, in the following, both 'a' and 'A::b' are threadprivate:
///
/// \code
/// int a;
/// #pragma omp threadprivate(a)
/// struct A {
///   static int b;
/// #pragma omp threadprivate(b)
/// };
/// \endcode
///
class OMPThreadPrivateDecl : public Decl {
  friend class ASTDeclReader;
  unsigned NumVars;

  virtual void anchor();

  OMPThreadPrivateDecl(Kind DK, DeclContext *DC, SourceLocation L) :
    Decl(DK, DC, L), NumVars(0) { }

  ArrayRef<const Expr *> getVars() const {
    return ArrayRef<const Expr *>(
                   reinterpret_cast<const Expr * const *>(this + 1),
                   NumVars);
  }

  llvm::MutableArrayRef<Expr *> getVars() {
    return llvm::MutableArrayRef<Expr *>(
                                 reinterpret_cast<Expr **>(this + 1),
                                 NumVars);
  }

  void setVars(ArrayRef<Expr *> VL);

public:
  static OMPThreadPrivateDecl *Create(ASTContext &C, DeclContext *DC,
                                      SourceLocation L,
                                      ArrayRef<Expr *> VL);
  static OMPThreadPrivateDecl *CreateDeserialized(ASTContext &C,
                                                  unsigned ID, unsigned N);

  typedef llvm::MutableArrayRef<Expr *>::iterator varlist_iterator;
  typedef ArrayRef<const Expr *>::iterator varlist_const_iterator;

  unsigned varlist_size() const { return NumVars; }
  bool varlist_empty() const { return NumVars == 0; }
  varlist_iterator varlist_begin() { return getVars().begin(); }
  varlist_iterator varlist_end() { return getVars().end(); }
  varlist_const_iterator varlist_begin() const { return getVars().begin(); }
  varlist_const_iterator varlist_end() const { return getVars().end(); }

  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == OMPThreadPrivate; }
};

/// \brief This represents '#pragma omp declare reduction ...' directive.
/// For example, in the following, declared reduction 'foo':
///
/// \code
/// #pragma omp declare reduction (foo : int,float : omp_out += omp_in) initializer (omp_priv = 0)
/// \endcode
///
class OMPDeclareReductionDecl : public NamedDecl, public DeclContext {
public:
  struct ReductionData {
    ReductionData(QualType QTy, SourceRange TyRange, Expr *Combiner, Expr *Init)
      : QTy(QTy), TyRange(TyRange), CombinerFunction(Combiner), InitFunction(Init) { }
    QualType QTy;
    SourceRange TyRange;
    Expr *CombinerFunction;
    Expr *InitFunction;
  };
private:
  friend class ASTDeclReader;
  unsigned NumTypes;

  virtual void anchor();

  OMPDeclareReductionDecl(Kind DK, DeclContext *DC, SourceLocation L,
                          DeclarationName Name) :
    NamedDecl(DK, DC, L, Name), DeclContext(DK), NumTypes(0) {
      setModulePrivate();
    }

  static unsigned getFirstElementOffset();

  ArrayRef<ReductionData> getData() const {
    return ArrayRef<ReductionData>(
                   reinterpret_cast<const ReductionData *>(
                                reinterpret_cast<const char *>(this) +
                                getFirstElementOffset()),
                   NumTypes);
  }

  llvm::MutableArrayRef<ReductionData> getData() {
    return llvm::MutableArrayRef<ReductionData>(
                   reinterpret_cast<ReductionData *>(
                                reinterpret_cast<char *>(this) +
                                getFirstElementOffset()),
                   NumTypes);
  }

public:
  static OMPDeclareReductionDecl *Create(ASTContext &C, DeclContext *DC,
                                         SourceLocation L,
                                         DeclarationName Name,
                                         unsigned N);
  static OMPDeclareReductionDecl *CreateDeserialized(ASTContext &C,
                                                     unsigned ID, unsigned N);

  void setData(ArrayRef<ReductionData> RD);

  typedef llvm::MutableArrayRef<ReductionData>::iterator datalist_iterator;
  typedef ArrayRef<ReductionData>::iterator datalist_const_iterator;

  unsigned datalist_size() const { return NumTypes; }
  bool datalist_empty() const { return NumTypes == 0; }
  datalist_iterator datalist_begin() { return getData().begin(); }
  datalist_iterator datalist_end() { return getData().end(); }
  datalist_const_iterator datalist_begin() const { return getData().begin(); }
  datalist_const_iterator datalist_end() const { return getData().end(); }

  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == OMPDeclareReduction; }
  static DeclContext *castToDeclContext(const OMPDeclareReductionDecl *D) {
    return static_cast<DeclContext *>(const_cast<OMPDeclareReductionDecl*>(D));
  }
  static OMPDeclareReductionDecl *castFromDeclContext(const DeclContext *DC) {
    return static_cast<OMPDeclareReductionDecl *>(const_cast<DeclContext*>(DC));
  }
};
}  // end namespace clang

#endif
