
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
using namespace std;

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

const map<uint64_t, uint64_t> FixedPointTransform::map_predicates {
    //    { CmpInst::Predicate::FCMP_FALSE, CmpInst::Predicate::ICMP_FALSE },
    {CmpInst::Predicate::FCMP_OEQ, CmpInst::Predicate::ICMP_EQ},
    {CmpInst::Predicate::FCMP_OGT, CmpInst::Predicate::ICMP_SGT},
    {CmpInst::Predicate::FCMP_OGE, CmpInst::Predicate::ICMP_SGE},
    {CmpInst::Predicate::FCMP_OLT, CmpInst::Predicate::ICMP_SLT},
    {CmpInst::Predicate::FCMP_OLE, CmpInst::Predicate::ICMP_SLE},
    {CmpInst::Predicate::FCMP_ONE, CmpInst::Predicate::ICMP_NE},
//    {CmpInst::Predicate::FCMP_ORD, CmpInst::Predicate::ICMP_TRUE },
//    {CmpInst::Predicate::FCMP_UNO, CmpInst::Predicate::ICMP_FALSE },
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

  vector<Type*> binop_args = { i64_t, i64_t};
  vector<Type*> uniop_args = { i64_t};
  FunctionType *binop_t = FunctionType::get(i64_t, binop_args, false);
  FunctionType *uniop_t = FunctionType::get(i64_t, uniop_args, false);
  FunctionType *i32_uniop_t = FunctionType::get(i32_t, uniop_args, false);

  static const vector<pair<uint64_t,pair<string,FunctionType*>>> sub_ins {
      {Instruction::FAdd, {"fix32_add", binop_t}},
      {Instruction::FSub, {"fix32_sub", binop_t}},
      {Instruction::FMul, {"fix32_mul", binop_t}},
      {Instruction::FDiv, {"fix32_div", binop_t}}
  };

  static const vector<pair<string,pair<string,FunctionType*>>> sub_fns {
      {"sqrt", {"fix32_sqrt", uniop_t}},
      {"sin", {"fix32_sin", uniop_t}},
      {"llvm.fabs.f64", {"fix32_abs", uniop_t}},
      {"__isnan__", {"fix32_isnan", i32_uniop_t}},
      {"__isinf__", {"fix32_isinf", i32_uniop_t}}
  };

  for (auto itr = sub_ins.begin(), end = sub_ins.end(); itr != end; ++itr) {
    uint64_t op = itr->first;
    string name = itr->second.first;
    Function *fn = Function::Create(itr->second.second, Function::ExternalLinkage, name, module);
    fn->setDSOLocal(true);
    fn->addAttribute(AttributeList::FunctionIndex, Attribute::NoInline);
    map_ins2fns.insert(make_pair(op, fn));
  }

  for (auto itr = sub_fns.begin(), end = sub_fns.end(); itr != end; ++itr) {
    string old_name = itr->first;
    if (Function *old_fn = module->getFunction(old_name)) {
      string new_name = itr->second.first;
      Function *new_fn = Function::Create(itr->second.second, Function::ExternalLinkage, new_name, module);
      new_fn->setDSOLocal(true);
      new_fn->addAttribute(AttributeList::FunctionIndex, Attribute::NoInline);
      map_fns2fns.insert(make_pair(old_fn, new_fn));
    }
  }
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

  // cannot remove replaced instructions during iteration, so keep a set to drop
  // later
  set<Instruction *> remove_ins;
  set<Function *> remove_fns;

  // global variables, cannot add or delete during iteration, so just create a record of gvs to replace
  map<GlobalVariable*,Type*> map_gvs;
  for (auto itr = module->global_begin(), end = module->global_end(); itr != end; ++itr) {
    GlobalVariable *gv = &(*itr);
    Type *old_t = gv->getValueType();
    if (Type *new_t = transformFP(old_t)) {
      map_gvs.insert(make_pair(gv, new_t));
    }
  }

  // replace each of these global variables with a new one of the correct type
  for (auto itr = map_gvs.begin(), end = map_gvs.end(); itr != end; ++itr) {
    GlobalVariable *old_gv = itr->first;
    Type *new_t = itr->second;

    Constant *new_init = nullptr;
    if (old_gv->hasInitializer()) {
      if (Constant *old_init = old_gv->getInitializer()) {
        if (auto fp_init = dyn_cast<ConstantFP>(old_init)) {
          APSInt i_value;
          const APFloat &fv = fp_init->getValueAPF();
          double dbl = fv.convertToDouble() * fix32_one;
          new_init = ConstantInt::get(new_t, (uint64_t) dbl);
        } else {
          new_init = old_init;
        }
      }
    }

    // rename the old gv so we can bequeath its name to the new gv
    string gv_name = old_gv->getName().str();
    old_gv->setName(gv_name + ".delete.me");
    GlobalVariable *new_gv = new GlobalVariable(*module,
                                                new_t,
                                                old_gv->isConstant(),
                                                old_gv->getLinkage(),
                                                new_init,
                                                gv_name,
                                                old_gv,
                                                old_gv->getThreadLocalMode(),
                                                old_gv->getAddressSpace(),
                                                old_gv->isExternallyInitialized());
    old_gv->replaceAllUsesWith(new_gv);
    old_gv->eraseFromParent();
  }

  // aliases

  // ? structures/unions/etc ?

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
        auto itr = map_ins2fns.find(opcode);
        if (itr != map_ins2fns.end()) {

          // this is a fp binop, we can handle this generically
          if (auto BO = dyn_cast<BinaryOperator>(&in)) {

            Function *fn = itr->second;
            vector<Value*> args = { BO->getOperand(0), BO->getOperand(1) };
            Instruction *call = CallInst::Create(fn, args, "", BO);

            // preserve any metadata
            SmallVector<pair<unsigned, MDNode *>, 4> mds;
            BO->getAllMetadata(mds);
            for (const auto &md : mds) {
              call->setMetadata(md.first, md.second);
            }
            BO->replaceAllUsesWith(call);
            remove_ins.insert(BO);
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
              pred = (CmpInst::Predicate) itr->second;
            } else if (pred == CmpInst::Predicate::FCMP_UNO) {
              Value *op1 = FC->getOperand(0);
              Value *op2 = FC->getOperand(1);
              if (op1 == op2) {
                pred = llvm::CmpInst::ICMP_NE;
              }
            } else if (pred == CmpInst::Predicate::FCMP_ORD) {

            }
            Instruction *cmp = ICmpInst::Create(Instruction::ICmp, pred, FC->getOperand(0), FC->getOperand(1), "", FC);
            FC->replaceAllUsesWith(cmp);
            remove_ins.insert(FC);
          } break;

          case Instruction::Call: {
            auto CI = static_cast<CallInst *>(&in);
            Function *old_fn = CI->getCalledFunction();
            auto itr = map_fns2fns.find(old_fn);
            if (itr != map_fns2fns.end()) {
              Function *new_fn = itr->second;
              CI->mutateType(new_fn->getReturnType());
              CI->setCalledFunction(new_fn);
              remove_fns.insert(old_fn);
            }
          } break;

          }

          // instruction metadata
          SmallVector<pair<unsigned, MDNode *>, 4> MDForInst;
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

  // remove replaced instructions and functions
  for (auto in : remove_ins) {
    in->eraseFromParent();
  }
  for (auto fn : remove_fns) {
    fn->eraseFromParent();
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

//  if (auto dibt = dyn_cast<DIBasicType>(node)) {
//    outs() << "Found a DIBasicType: " << dibt->getName() << ", " << dibt->getSizeInBits() << ", " << dibt->getEncoding() << '\n';
//    outs().flush();
//  }

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
