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

/// \brief This represents '#pragma omp declare simd ...' directive.
/// Here is an example, where two simd-variants are declared for a function:
///
/// #pragma omp declare simd inbranch uniform(a) linear(b:4) simdlen(8)
/// #pragma omp declare simd inbranch uniform(a) linear(b:4) simdlen(16)
/// void func(float *a, float *b);
///
class OMPDeclareSimdDecl : public Decl {
public:
  /// \brief SimdVariant refers to a list of clauses which describe some
  ///        variant of the function that will need to be instantiated.
  struct SimdVariant {
    SimdVariant(SourceRange SR, unsigned BI, unsigned EI)
      :SrcRange(SR), BeginIdx(BI), EndIdx(EI) { }
    SourceRange SrcRange;
    unsigned BeginIdx;
    unsigned EndIdx;
  };
private:
  friend class ASTDeclReader;
  unsigned NumVariants;
  unsigned NumClauses;
  Decl *FuncDecl;

  virtual void anchor();

  OMPDeclareSimdDecl(Kind DK, DeclContext *DC, SourceLocation L,
                     unsigned NV, unsigned NC)
    :Decl(DK, DC, L), NumVariants(NV), NumClauses(NC), FuncDecl(0) { }

  static unsigned getFirstVariantOffset();
  static unsigned getFirstClauseOffset(unsigned NV);
  static unsigned getTotalSize(unsigned NV, unsigned NC);

public:
  // Getters for the array of simd variants.
  ArrayRef<SimdVariant> getVariants() const {
    return ArrayRef<SimdVariant>(
                   reinterpret_cast<const SimdVariant *>(
                     reinterpret_cast<const char *>(this) +
                     getFirstVariantOffset()),
                   NumVariants);
  }

  llvm::MutableArrayRef<SimdVariant> getVariants() {
    return llvm::MutableArrayRef<SimdVariant>(
                   reinterpret_cast<SimdVariant *>(
                     reinterpret_cast<char *>(this) +
                     getFirstVariantOffset()),
                   NumVariants);
  }

  // Getters for the array of clauses.
  ArrayRef<OMPClause *> getClauses() const {
    return ArrayRef<OMPClause *>(
                   reinterpret_cast<OMPClause * const *>(
                     reinterpret_cast<const char *>(this) +
                     getFirstClauseOffset(NumVariants)),
                   NumClauses);
  }

  llvm::MutableArrayRef<OMPClause *> getClauses() {
    return llvm::MutableArrayRef<OMPClause *>(
                   reinterpret_cast<OMPClause **>(
                     reinterpret_cast<char *>(this) +
                     getFirstClauseOffset(NumVariants)),
                   NumClauses);
  }

public:
  static OMPDeclareSimdDecl *Create(ASTContext &C, DeclContext *DC,
                                    SourceLocation L, Decl *FuncDecl,
                                    unsigned NV,
                                    ArrayRef<OMPClause *> CL);
  static OMPDeclareSimdDecl *CreateDeserialized(ASTContext &C, unsigned ID,
                                                unsigned NV, unsigned NC);

  Decl *getFunction() const { return FuncDecl; }
  void  setFunction(Decl *FD) { FuncDecl = FD; }
  unsigned getNumVariants() const { return NumVariants; }
  unsigned getNumClauses()  const { return NumClauses;  }

  // Stuff to work with variants
  void setVariants(ArrayRef<SimdVariant> SV);

  typedef llvm::MutableArrayRef<SimdVariant>::iterator simd_variants_iterator;
  typedef ArrayRef<SimdVariant>::iterator simd_variants_const_iterator;

  unsigned simd_variants_size() const { return NumVariants; }
  bool simds_variant_empty() const { return NumVariants == 0; }
  simd_variants_iterator simd_variants_begin() { return getVariants().begin(); }
  simd_variants_iterator simd_variants_end()   { return getVariants().end(); }
  simd_variants_const_iterator simd_variants_begin() const { return getVariants().begin(); }
  simd_variants_const_iterator simd_variants_end()   const { return getVariants().end();   }

  // Stuff to work with clauses
  void setClauses(ArrayRef<OMPClause *> CL);

  typedef llvm::MutableArrayRef<OMPClause *>::iterator clauses_iterator;
  typedef ArrayRef<OMPClause *>::iterator clauses_const_iterator;

  unsigned clauses_size() const { return NumClauses; }
  bool clauses_empty() const { return NumClauses == 0; }
  clauses_iterator clauses_begin() { return getClauses().begin(); }
  clauses_iterator clauses_end() { return getClauses().end(); }
  clauses_const_iterator clauses_begin() const { return getClauses().begin(); }
  clauses_const_iterator clauses_end() const { return getClauses().end(); }

  static bool classof(const Decl *D) { return classofKind(D->getKind()); }
  static bool classofKind(Kind K) { return K == OMPDeclareSimd; }
};

}  // end namespace clang

#endif
