//===--- DeclOpenMP.cpp - Declaration OpenMP AST Node Implementation ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
/// \brief This file implements OMPThreadPrivateDecl, OMPDeclareReduction,
/// OMPDeclareTarget classes.
///
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclOpenMP.h"
#include "clang/AST/Expr.h"

using namespace clang;

//===----------------------------------------------------------------------===//
// OMPThreadPrivateDecl Implementation.
//===----------------------------------------------------------------------===//

void OMPThreadPrivateDecl::anchor() {}

OMPThreadPrivateDecl *OMPThreadPrivateDecl::Create(ASTContext &C,
                                                   DeclContext *DC,
                                                   SourceLocation L,
                                                   ArrayRef<Expr *> VL) {
  OMPThreadPrivateDecl *D = new (C, DC, VL.size() * sizeof(Expr *))
      OMPThreadPrivateDecl(OMPThreadPrivate, DC, L);
  D->NumVars = VL.size();
  D->setVars(VL);
  return D;
}

OMPThreadPrivateDecl *OMPThreadPrivateDecl::CreateDeserialized(ASTContext &C,
                                                               unsigned ID,
                                                               unsigned N) {
  OMPThreadPrivateDecl *D = new (C, ID, N * sizeof(Expr *))
      OMPThreadPrivateDecl(OMPThreadPrivate, 0, SourceLocation());
  D->NumVars = N;
  return D;
}

void OMPThreadPrivateDecl::setVars(ArrayRef<Expr *> VL) {
  assert(VL.size() == NumVars &&
         "Number of variables is not the same as the preallocated buffer");
  Expr **Vars = reinterpret_cast<Expr **>(this + 1);
  std::copy(VL.begin(), VL.end(), Vars);
}

//===----------------------------------------------------------------------===//
// OMPDeclareSimd Implementation.
//===----------------------------------------------------------------------===//

void OMPDeclareSimdDecl::anchor() {}

unsigned OMPDeclareSimdDecl::getFirstVariantOffset() {
  unsigned Size = sizeof(OMPDeclareSimdDecl);
  Size = llvm::RoundUpToAlignment(
      Size, llvm::alignOf<OMPDeclareSimdDecl::SimdVariant>());
  return Size;
}

unsigned OMPDeclareSimdDecl::getFirstClauseOffset(unsigned NV) {
  unsigned ESize = llvm::RoundUpToAlignment(
      sizeof(OMPDeclareSimdDecl::SimdVariant),
      llvm::alignOf<OMPDeclareSimdDecl::SimdVariant>());
  unsigned Size = getFirstVariantOffset() + ESize * NV;
  Size = llvm::RoundUpToAlignment(Size, llvm::alignOf<OMPClause *>());
  return Size;
}

unsigned OMPDeclareSimdDecl::getTotalSize(unsigned NV, unsigned NC) {
  unsigned ESize = llvm::RoundUpToAlignment(sizeof(OMPClause *),
                                            llvm::alignOf<OMPClause *>());
  unsigned Size = getFirstClauseOffset(NV) + ESize * NC;
  Size = llvm::RoundUpToAlignment(Size, llvm::alignOf<OMPDeclareSimdDecl>());
  return Size;
}

void
OMPDeclareSimdDecl::setVariants(ArrayRef<OMPDeclareSimdDecl::SimdVariant> SV) {
  assert(SV.size() == NumVariants &&
         "Number of variants is not the same as the preallocated buffer");
  unsigned Offset = getFirstVariantOffset();
  OMPDeclareSimdDecl::SimdVariant *Data =
      reinterpret_cast<OMPDeclareSimdDecl::SimdVariant *>(
          reinterpret_cast<char *>(this) + Offset);
  for (unsigned i = 0; i < NumVariants; ++i)
    Data[i] = SV[i];
}

void OMPDeclareSimdDecl::setClauses(ArrayRef<OMPClause *> CL) {
  assert(CL.size() == NumClauses &&
         "Number of clauses is not the same as the preallocated buffer");
  unsigned Offset = getFirstClauseOffset(NumVariants);
  OMPClause **Data =
      reinterpret_cast<OMPClause **>(reinterpret_cast<char *>(this) + Offset);
  for (unsigned i = 0; i < NumClauses; ++i)
    Data[i] = CL[i];
}

OMPDeclareSimdDecl *OMPDeclareSimdDecl::Create(ASTContext &C, DeclContext *DC,
                                               SourceLocation L, Decl *FuncDecl,
                                               unsigned NV,
                                               ArrayRef<OMPClause *> CL) {
  unsigned NC = CL.size();
  unsigned Size = getTotalSize(NV, NC);
  OMPDeclareSimdDecl *D = new (C, DC, Size - sizeof(OMPDeclareSimdDecl))
      OMPDeclareSimdDecl(OMPDeclareSimd, DC, L, NV, NC);
  D->FuncDecl = FuncDecl;
  D->setClauses(CL);
  return D;
}

OMPDeclareSimdDecl *OMPDeclareSimdDecl::CreateDeserialized(ASTContext &C,
                                                           unsigned ID,
                                                           unsigned NV,
                                                           unsigned NC) {
  unsigned Size = getTotalSize(NV, NC);
  OMPDeclareSimdDecl *D = new (C, ID, Size - sizeof(OMPDeclareSimdDecl))
      OMPDeclareSimdDecl(OMPDeclareSimd, 0, SourceLocation(), NV, NC);
  D->FuncDecl = 0;
  return D;
}

//===----------------------------------------------------------------------===//
// OMPDeclareReductionDecl Implementation.
//===----------------------------------------------------------------------===//

void OMPDeclareReductionDecl::anchor() {}

unsigned OMPDeclareReductionDecl::getFirstElementOffset() {
  unsigned Size = sizeof(OMPDeclareReductionDecl);
  // Realign
  Size = llvm::RoundUpToAlignment(
      Size, llvm::alignOf<OMPDeclareReductionDecl::ReductionData>());
  return Size;
}

OMPDeclareReductionDecl *OMPDeclareReductionDecl::Create(ASTContext &C,
                                                         DeclContext *DC,
                                                         SourceLocation L,
                                                         DeclarationName Name,
                                                         unsigned N) {
  unsigned Size = getFirstElementOffset() +
                  N * sizeof(OMPDeclareReductionDecl::ReductionData);

  OMPDeclareReductionDecl *D =
      new (C, DC, Size - sizeof(OMPDeclareReductionDecl))
          OMPDeclareReductionDecl(OMPDeclareReduction, DC, L, Name);
  D->NumTypes = N;
  return D;
}

OMPDeclareReductionDecl *
OMPDeclareReductionDecl::CreateDeserialized(ASTContext &C, unsigned ID,
                                            unsigned N) {
  unsigned Size = getFirstElementOffset() +
                  N * sizeof(OMPDeclareReductionDecl::ReductionData);

  OMPDeclareReductionDecl *D =
      new (C, ID, Size - sizeof(OMPDeclareReductionDecl))
          OMPDeclareReductionDecl(OMPDeclareReduction, 0, SourceLocation(),
                                  DeclarationName());
  D->NumTypes = N;
  return D;
}

void OMPDeclareReductionDecl::setData(
    ArrayRef<OMPDeclareReductionDecl::ReductionData> RD) {
  assert(RD.size() == NumTypes &&
         "Number of inits is not the same as the preallocated buffer");
  unsigned Size = getFirstElementOffset();
  OMPDeclareReductionDecl::ReductionData *Data =
      reinterpret_cast<OMPDeclareReductionDecl::ReductionData *>(
          reinterpret_cast<char *>(this) + Size);
  for (unsigned i = 0; i < NumTypes; ++i)
    Data[i] = RD[i];
  // std::copy(RD.begin(), RD.end(), Data);
}

//===----------------------------------------------------------------------===//
// OMPDeclareTargetDecl Implementation.
//===----------------------------------------------------------------------===//

void OMPDeclareTargetDecl::anchor() {}

OMPDeclareTargetDecl *
OMPDeclareTargetDecl::Create(ASTContext &C, DeclContext *DC, SourceLocation L) {
  OMPDeclareTargetDecl *D =
      new (C, DC) OMPDeclareTargetDecl(OMPDeclareTarget, DC, L);
  return D;
}

OMPDeclareTargetDecl *OMPDeclareTargetDecl::CreateDeserialized(ASTContext &C,
                                                               unsigned ID) {
  // Realign
  OMPDeclareTargetDecl *D =
      new (C, ID) OMPDeclareTargetDecl(OMPDeclareTarget, 0, SourceLocation());
  return D;
}
