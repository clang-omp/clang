//===---- CGLoopInfo.h - LLVM CodeGen for loop metadata -*- C++ -*---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This is the internal state used for llvm translation for loop statement
// metadata.
//
//===----------------------------------------------------------------------===//

#ifndef CLANG_CODEGEN_CGLOOPINFO_H
#define CLANG_CODEGEN_CGLOOPINFO_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Compiler.h"
#include "llvm/IR/Value.h"

namespace llvm {
class BasicBlock;
class Instruction;
class MDNode;
} // end namespace llvm

namespace clang {
namespace CodeGen {

/// LoopAttributes - Per loop attributes.
struct LoopAttributes {
  explicit LoopAttributes(bool IsParallel = false);
  void Clear();

  /// llvm.loop.parallel metadata generation for loads and stores.
  bool IsParallel;

  /// llvm.vectorizer.enable value:
  enum LVEnableState {
    LVEC_UNSPECIFIED,
    LVEC_ENABLE,
    LVEC_DISABLE
  };

  LVEnableState VectorizerEnable;

  /// llvm.vectorizer.width value
  unsigned VectorizerWidth;
};

/// LoopInfo - Information used when generating a structured loop.
class LoopInfo {
public:
  /// Construct a new LoopInfo for the loop with entry Header.
  LoopInfo(llvm::BasicBlock *Header, const LoopAttributes &Attrs);

  /// Construct a new LoopInfo with a given loop id metadata.
  LoopInfo(llvm::MDNode *LoopID, const LoopAttributes &Attrs);

  /// Get the loop id metadata for this loop.
  llvm::MDNode *GetLoopID() const { return LoopID; }

  /// Get the header block of this loop.
  llvm::BasicBlock *GetHeader() const { return Header; }

  /// Get the set of attributes active for this loop.
  const LoopAttributes &GetAttributes() const { return Attrs; }

private:
  /// Loop ID metadata.
  mutable llvm::MDNode *LoopID;
  /// Header block of this loop.
  llvm::BasicBlock *Header;
  /// The attributes for this loop.
  LoopAttributes Attrs;
};

/// LoopInfoStack - A stack of loop information corresponding to loop 
/// nesting levels. This stack can be used to prepare attributes which are
/// applied when a loop is emitted.
class LoopInfoStack {
  LoopInfoStack(const LoopInfoStack &) LLVM_DELETED_FUNCTION;
  void operator=(const LoopInfoStack &) LLVM_DELETED_FUNCTION;
public:
  LoopInfoStack() {}

  /// Begin a new structured loop. The set of applied attributes will be applied
  /// to the loop and the attributes will be cleared.
  void Push(llvm::BasicBlock *Header);

  /// Extend the code region as part of a parallel loop which might be inside
  /// another llvm function.
  void Push(llvm::MDNode *LoopID);

  /// End the current loop.
  void Pop();

  /// Return the top loop id metadata.
  llvm::MDNode *GetCurLoopID() const { return GetInfo().GetLoopID(); }

  /// Function called by the CodeGenFunction when an instruction is created.
  void InsertHelper(llvm::Instruction *I) const;

  /// Set the next pushed loop as parallel.
  void SetParallel() { StagedAttrs.IsParallel = true; }

  /// Set the next pushed loop 'vectorizer.enable'
  void SetVectorizerEnable(bool Enable = true) {
    StagedAttrs.VectorizerEnable = Enable ? LoopAttributes::LVEC_ENABLE :
                                            LoopAttributes::LVEC_DISABLE;
  }

  /// Set the vectorizer width for the next loop pushed.
  void SetVectorizerWidth(unsigned W) { StagedAttrs.VectorizerWidth = W; }

  /// Add an aligned variable for 'aligned' clause.
  void AddAligned(const llvm::Value *Val, int Align);

  /// Get alignment of given pointer based on 'aligned' clause.
  int GetAligned(const llvm::Value *Val) const;

private:
  /// Returns true if there is LoopInfo on the stack.
  bool HasInfo() const { return !Active.empty(); }
  /// Return the LoopInfo for the current loop. HasInfo should be called first
  /// to ensure LoopInfo is present.
  const LoopInfo &GetInfo() const { return Active.back(); }
  /// The set of attributes that will be applied to the next pushed loop.
  LoopAttributes StagedAttrs;
  /// Stack of active loops.
  llvm::SmallVector<LoopInfo, 4> Active;
  // 'Aligned' information.
  llvm::DenseMap<const llvm::Value *, int> Aligneds;
};

} // end namespace CodeGen
} // end namespace clang

#endif // CLANG_CODEGEN_CGLOOPINFO_H
