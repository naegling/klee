
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
class StructType;
class MDNode;
class Value;
}


namespace klee {

class FixedPointTransform {

  llvm::Module *module;
  std::map<uint64_t, uint64_t> map_predicates;
  std::map<llvm::Type*,llvm::Type*> map_basic_ty;
  const uint64_t fix32_one = 0x0000000100000000;          /*!< fix32_t value of 1 */

  std::map<uint64_t,llvm::Function*> map_ins2fns;
  std::map<llvm::Function*,llvm::Function*> map_fns2fns;
  std::map<llvm::StructType*,llvm::StructType*> map_struct_ty;

  std::pair<llvm::Type*,unsigned> unwrap_type(llvm::Type* ty) const;
  llvm::Type *rewrap_type(llvm::Type *ty, unsigned cnt) const;
  llvm::Type *transformFP(llvm::Type *ty) const;
  bool isFP(llvm::Type *ty) const;

  std::set<llvm::MDNode *> visitedMetadata;
  void traverseMDNode(llvm::MDNode *node);
  void traverseValue(llvm::Value *value);
  void reset_types();

public:
  FixedPointTransform(llvm::Module *M);
  bool run();

};


}
