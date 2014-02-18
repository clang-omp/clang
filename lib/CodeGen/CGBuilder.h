//===-- CGBuilder.h - Choose IRBuilder implementation  ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_CODEGEN_CGBUILDER_H
#define CLANG_CODEGEN_CGBUILDER_H

#include "llvm/IR/IRBuilder.h"

namespace clang {
namespace CodeGen {

class CodeGenFunction;

/// IRBuilder inserter which forwards to CodeGenFunction::InsertHelper.
template <bool PreserveNames>
class CGBuilderInserter
  : protected llvm::IRBuilderDefaultInserter<PreserveNames> {
public:
  CGBuilderInserter() : CGF(0) {}
  explicit CGBuilderInserter(CodeGenFunction *CGF) : CGF(CGF) {}

protected:
  void InsertHelper(llvm::Instruction *I, const llvm::Twine &Name,
                    llvm::BasicBlock *BB,
                    llvm::BasicBlock::iterator InsertPt) const;
private:
  void operator=(const CGBuilderInserter &) LLVM_DELETED_FUNCTION;

  CodeGenFunction *CGF;
};

// Don't preserve names on values in an optimized build.
#ifdef NDEBUG
typedef CGBuilderInserter<false> CGBuilderInserterTy;
typedef llvm::IRBuilder<false, llvm::ConstantFolder, CGBuilderInserterTy>
  CGBuilderTy;
#else
typedef CGBuilderInserter<true> CGBuilderInserterTy;
typedef llvm::IRBuilder<true, llvm::ConstantFolder, CGBuilderInserterTy>
  CGBuilderTy;
#endif

}  // end namespace CodeGen
}  // end namespace clang

#endif
