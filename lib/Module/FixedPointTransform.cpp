
#include "FixedPointTransform.h"

#include <llvm/ADT/APSInt.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DebugInfoMetadata.h>

#include <set>

using namespace llvm;

namespace klee {

// FCMP_FALSE
// FCMP_OEQ    EQ    (no nans)
// FCMP_OGT    SGT
// FCMP_OGE    SGE
// FCMP_OLT    SLT
// FCMP_OLE    SLE
// FCMP_ONE    NE
// FCMP_ORD    true if no nans
// FCMP_UNO    true if either nan
// FCMP_UEQ    EQ    (either nan)
// FCMP_UGT    SGT
// FCMP_UGE    SGE
// FCMP_ULT    SLT
// FCMP_ULE    SLE
// FCMP_UNE    NE
// FCMP_TRUE

const std::map<uint64_t, uint64_t> FixedPointTransform::map_predicates = {
    //    { CmpInst::Predicate::FCMP_FALSE, CmpInst::Predicate::ICMP_FALSE },
    {CmpInst::Predicate::FCMP_OEQ, CmpInst::Predicate::ICMP_EQ},
    {CmpInst::Predicate::FCMP_OGT, CmpInst::Predicate::ICMP_SGT},
    {CmpInst::Predicate::FCMP_OGE, CmpInst::Predicate::ICMP_SGE},
    {CmpInst::Predicate::FCMP_OLT, CmpInst::Predicate::ICMP_SLT},
    {CmpInst::Predicate::FCMP_OLE, CmpInst::Predicate::ICMP_SLE},
    {CmpInst::Predicate::FCMP_ONE, CmpInst::Predicate::ICMP_NE},
    //    { CmpInst::Predicate::FCMP_ORD, CmpInst::Predicate::ICMP_EQ },
    //    { CmpInst::Predicate::FCMP_UNO, CmpInst::Predicate::ICMP_EQ },
    {CmpInst::Predicate::FCMP_UEQ, CmpInst::Predicate::ICMP_EQ},
    {CmpInst::Predicate::FCMP_UGT, CmpInst::Predicate::ICMP_SGT},
    {CmpInst::Predicate::FCMP_UGE, CmpInst::Predicate::ICMP_SGE},
    {CmpInst::Predicate::FCMP_ULT, CmpInst::Predicate::ICMP_SLT},
    {CmpInst::Predicate::FCMP_ULE, CmpInst::Predicate::ICMP_SLE},
    {CmpInst::Predicate::FCMP_UNE, CmpInst::Predicate::ICMP_NE}
    //    { CmpInst::Predicate::FCMP_TRUE, CmpInst::Predicate::ICMP_NE }
};

FixedPointTransform::FixedPointTransform(Module *M)
    : module(M), ctx(M->getContext()) {

  dbl_t = Type::getDoubleTy(ctx);
  flt_t = Type::getFloatTy(ctx);
  i32_t = Type::getInt32Ty(ctx);
  i64_t = Type::getInt64Ty(ctx);

  std::vector<Type*> binop_args = { i64_t, i64_t};
  std::vector<Type*> uniop_args = { i64_t};
  binop_t = FunctionType::get(i64_t, binop_args, false);
  uniop_t = FunctionType::get(i64_t, uniop_args, false);

  map_binops = {
                 {Instruction::FAdd, {"fix32_add", nullptr}},
                 {Instruction::FSub, {"fix32_sub", nullptr}},
                 {Instruction::FMul, {"fix32_mul", nullptr}},
                 {Instruction::FDiv, {"fix32_div", nullptr}},
               };
  for (auto itr = map_binops.begin(), end=map_binops.end(); itr != end; ++itr) {
    std::string name = itr->second.first;
    Function *fn = Function::Create(binop_t, Function::ExternalLinkage, name, module);
    fn->setDSOLocal(true);
    fn->addAttribute(AttributeList::FunctionIndex, Attribute::NoInline);
    itr->second.second = fn;
  }
  fn_sqrt = Function::Create(uniop_t, Function::ExternalLinkage, "fix32_sqrt", module);
}

unsigned FixedPointTransform::countIndirection(Type *base, Type *src) const {

  unsigned cnt = 0;
  while (src->isPointerTy()) {
    src = src->getPointerElementType();
    cnt++;
  }
  return (base == src) ? cnt + 1 : 0;
}

Type *FixedPointTransform::getIndirect(Type *base, unsigned count) const {

  assert(base != nullptr);

  while (count > 1) {
    base = base->getPointerTo();
    count--;
  }
  return (count == 0 ? nullptr : base);
}

Type *FixedPointTransform::getEquivalentIndirect(Type *src, Type *old_base, Type *new_base) const {

  Type *result = nullptr;
  unsigned cnt = countIndirection(old_base, src);
  if (cnt > 0) {
    result = getIndirect(new_base, cnt);
  }
  return result;
}

Type *FixedPointTransform::transformFP(Type *src) const {

  Type *result = getEquivalentIndirect(src, dbl_t, i64_t);
  if (result == nullptr) {
    result = getEquivalentIndirect(src, flt_t, i32_t);
  }
  return result;
}

bool FixedPointTransform::run() {

  // global variables

  // aliases

  // ? structures/unions/etc ?

  // cannot remove replaced instructions during iteration, so keep a set to drop
  // later
  std::set<Instruction *> to_remove;

  for (Function &fn : *module) {

    // operands

    // arguments

    // instructions
    for (BasicBlock &bb : fn) {
      for (Instruction &in : bb) {

        // check the operands for fp constants
        for (unsigned idx = 0, end = in.getNumOperands(); idx < end; ++idx) {
          Value *v = in.getOperand(idx);
          if (auto CFP = dyn_cast<ConstantFP>(v)) {
            APSInt i_value;
            const APFloat &fv = CFP->getValueAPF();
            double dbl = fv.convertToDouble() * fix32_one;
            in.setOperand(idx, ConstantInt::get(i64_t, (uint64_t)dbl));
          }
        }

        auto opcode = in.getOpcode();
        auto itr = map_binops.find(opcode);
        if (itr != map_binops.end()) {

          // this is a fp binop, we can handle this generically
          if (auto BO = dyn_cast<BinaryOperator>(&in)) {

            Function *fn = itr->second.second;
            std::vector<Value*> args = { BO->getOperand(0), BO->getOperand(1) };
            Instruction *call = CallInst::Create(fn, args, "", BO);

            // preserve any metadata
            SmallVector<std::pair<unsigned, MDNode *>, 4> mds;
            BO->getAllMetadata(mds);
            for (const auto &md : mds) {
              call->setMetadata(md.first, md.second);
            }
            BO->replaceAllUsesWith(call);
            to_remove.insert(BO);
          } else {
            // well, someone certainly f'ed up ...
            assert(false);
          }

        } else {

          // per instruction updates
          switch (opcode) {
          case Instruction::Alloca: {
            auto *AI = static_cast<AllocaInst *>(&in);
            if (Type *new_t = transformFP(AI->getAllocatedType())) {
              AI->setAllocatedType(new_t);
            }
          } break;

          case Instruction::BitCast: {
            auto *BI = static_cast<BitCastInst *>(&in);
            Value *v = BI->getOperand(0);
            if (Type *new_t = transformFP(v->getType())) {
              v->mutateType(new_t);
            }
            if (Type *new_t = transformFP(BI->getType())) {
              BI->mutateType(new_t);
            }
          } break;

          case Instruction::Load: {
            auto *LD = static_cast<LoadInst *>(&in);
            if (Type *new_t = transformFP(LD->getType())) {
              LD->mutateType(new_t);
            }
            auto v = LD->getPointerOperand();
            if (Type *new_t = transformFP(v->getType())) {
              v->mutateType(new_t);
            }
          } break;

          case Instruction::Store: {
            auto *SR = static_cast<StoreInst *>(&in);
            if (Type *new_type = transformFP(SR->getType())) {
              SR->mutateType(new_type);
            }
          } break;

          case Instruction::FCmp: {
            auto *FC = static_cast<FCmpInst *>(&in);
            auto pred = FC->getPredicate();
            auto itr = map_predicates.find(pred);
            if (itr != map_predicates.end()) {
              pred = (CmpInst::Predicate)itr->second;
            }
            Instruction *cmp =
                ICmpInst::Create(Instruction::ICmp, pred, FC->getOperand(0),
                                 FC->getOperand(1), "", FC);
            FC->replaceAllUsesWith(cmp);
            to_remove.insert(FC);
          } break;

          case Instruction::Call: {
            auto CI = static_cast<CallInst *>(&in);
            Function *fn = CI->getCalledFunction();
            if (fn->getName() == "sqrt") {
              CI->mutateType(i64_t);
              CI->setCalledFunction(fn_sqrt);
            }
          } break;

          }

          // instruction metadata
          SmallVector<std::pair<unsigned, MDNode *>, 4> MDForInst;
          in.getAllMetadata(MDForInst);
          for (const auto &MD : MDForInst)
            traverseMDNode(MD.second);
          MDForInst.clear();

        }
      }
    }
  }

  // named metadata
  for (auto &NMD : module->named_metadata()) {
    for (auto *MDOp : NMD.operands()) {
      traverseMDNode(MDOp);
    }
  }

  // remove replaced instructions
  for (auto i : to_remove) {
    i->eraseFromParent();
  }
  return true;
}

void FixedPointTransform::traverseValue(Value *value) {

  if (auto *M = dyn_cast<MetadataAsValue>(value)) {
    if (auto *N = dyn_cast<MDNode>(M->getMetadata())) {
      return traverseMDNode(N);
    }
    if (auto *MDV = dyn_cast<ValueAsMetadata>(M->getMetadata()))
      return traverseValue(MDV->getValue());
    return;
  }
  if (isFP(value->getType())) {
//    outs() << "found a float\n";
  }
}

void FixedPointTransform::traverseMDNode(MDNode *node) {

  if (!visitedMetadata.insert(node).second) {
    return;
  }

  if (auto dibt = dyn_cast<DIBasicType>(node)) {
//    outs() << "Found a DIBasicType: " << dibt->getName() << ", " << dibt->getSizeInBits() << ", " << dibt->getEncoding() << '\n';
    outs().flush();
  }

  // Look in operands for types.
  for (Metadata *Op : node->operands()) {
    if (Op != nullptr) {
      if (auto *N = dyn_cast<MDNode>(Op)) {
        traverseMDNode(N);
      } else if (auto *cv = dyn_cast<ConstantAsMetadata>(Op)) {
        traverseValue(cv->getValue());
      }
    }
  }
}

} // namespace klee
