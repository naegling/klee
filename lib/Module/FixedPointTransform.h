
#pragma once

#include <cstdint>
#include <map>
#include <set>
#include <vector>
#include <string>

namespace llvm {

class LLVMContext;
class Module;
class FunctionType;
class Function;
class Type;
class MDNode;
class Value;

}


namespace klee {

class FixedPointTransform {

  llvm::Module *module;
  llvm::LLVMContext &ctx;
  llvm::Type *dbl_t;
  llvm::Type *flt_t;
  llvm::Type *i32_t;
  llvm::Type *i64_t;
  llvm::FunctionType *binop_t;
  llvm::FunctionType *uniop_t;
  static const std::map<uint64_t, uint64_t> map_predicates;
  const uint64_t fix32_one = 0x0000000100000000;          /*!< fix32_t value of 1 */

  std::map<uint64_t,std::pair<std::string,llvm::Function*>> map_binops;
  llvm::Function *fn_sqrt;
  std::set<llvm::MDNode *> visitedMetadata;

  unsigned countIndirection(llvm::Type *base, llvm::Type *src) const;
  llvm::Type *getIndirect(llvm::Type *base, unsigned count) const;
  llvm::Type *getEquivalentIndirect(llvm::Type *src, llvm::Type *old_base, llvm::Type *new_base) const;
  llvm::Type *transformFP(llvm::Type *src) const;
  void traverseMDNode(llvm::MDNode *node);
  void traverseValue(llvm::Value *value);
  bool isFP(const llvm::Type *ty) { return ty == dbl_t || ty == flt_t; }


public:
  FixedPointTransform(llvm::Module *M);
  bool run();

};


}
