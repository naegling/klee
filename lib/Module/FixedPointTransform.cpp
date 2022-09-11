
#include "FixedPointTransform.h"

#include <llvm/ADT/APSInt.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/TypeFinder.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

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

FixedPointTransform::FixedPointTransform(Module *M) : module(M) {

  LLVMContext &ctx = module->getContext();
  Type *dbl_t = Type::getDoubleTy(ctx);
  Type *i64_t = Type::getInt64Ty(ctx);
  Type *flt_t = Type::getFloatTy(ctx);
  Type *i32_t = Type::getInt32Ty(ctx);

  map_predicates = {
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

  map_basic_ty = {{dbl_t, i64_t}, {flt_t, i32_t}};

  vector<Type*> binop_args = {i64_t, i64_t};
  vector<Type*> uniop_args = {i64_t};
  vector<Type*> zerop_args = {};
  vector<Type*> uniop_i32_args = {i32_t};

  FunctionType *binop_t = FunctionType::get(i64_t, binop_args, false);
  FunctionType *uniop_t = FunctionType::get(i64_t, uniop_args, false);
  FunctionType *i32_uniop_t = FunctionType::get(i32_t, uniop_args, false);
  FunctionType *i32_zerop_t = FunctionType::get(i32_t, zerop_args, false);
  FunctionType *i32_uniop_i32_t = FunctionType::get(i32_t, uniop_i32_args, false);

  static const vector<pair<uint64_t,pair<string,FunctionType*>>> sub_ins {
      {Instruction::FAdd, {"fix32_add", binop_t}},
      {Instruction::FSub, {"fix32_sub", binop_t}},
      {Instruction::FMul, {"fix32_mul", binop_t}},
      {Instruction::FDiv, {"fix32_div", binop_t}},
      {Instruction::SIToFP, {"fix32_SI2FP", uniop_t}},
      {Instruction::UIToFP, {"fix32_UI2FP", uniop_t}}
  };

  static const vector<pair<string,pair<string,FunctionType*>>> sub_fns {
      {"sqrt", {"fix32_sqrt", uniop_t}},
      {"sin", {"fix32_sin", uniop_t}},
      {"llvm.fabs.f64", {"fix32_abs", uniop_t}},
      {"__isnan__", {"fix32_isnan", i32_uniop_t}},
      {"__isinf__", {"fix32_isinf", i32_uniop_t}},
      {"__isfinite__", {"fix32_isfinite", i32_uniop_t}},
      {"fegetround", {"fix32_fegetround", i32_zerop_t}},
      {"fesetround", {"fix32_fesetround", i32_uniop_i32_t}}
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
    if (Function *old_fn = module->getFunction(itr->first)) {
      Function *new_fn = Function::Create(itr->second.second, Function::ExternalLinkage, itr->second.first, module);
      new_fn->setDSOLocal(true);
      new_fn->addAttribute(AttributeList::FunctionIndex, Attribute::NoInline);
      map_fns2fns.insert(make_pair(old_fn, new_fn));
    }
  }
}

void FixedPointTransform::reset_types() {
  map_struct_ty.clear();
}

std::pair<llvm::Type*,unsigned> FixedPointTransform::unwrap_type(llvm::Type* ty) const {

  unsigned cnt = 0;
  while (ty->isPointerTy()) {
    ty = ty->getPointerElementType();
    cnt++;
  }
  return make_pair(ty, cnt + 1);
}

llvm::Type *FixedPointTransform::rewrap_type(llvm::Type *ty, unsigned cnt) const {

  assert(ty != nullptr);
  while (cnt > 1) {
    ty = ty->getPointerTo();
    cnt--;
  }
  return ty;
}

Type *FixedPointTransform::transformFP(Type *ty) const {

  Type *result = nullptr;

  auto pr = unwrap_type(ty);
  auto itr = map_basic_ty.find(pr.first);
  if (itr != map_basic_ty.end()) {
    result = rewrap_type(itr->second, pr.second);
  } else if (StructType *sty = dyn_cast<StructType>(pr.first)) {
    auto itr = map_struct_ty.find(sty);
    if (itr != map_struct_ty.end()) {
      result = rewrap_type(itr->second, pr.second);
    }
  } else if (ArrayType *aty = dyn_cast<ArrayType>(pr.first)) {
    if (auto new_type = transformFP(aty->getElementType())) {
      result = rewrap_type(ArrayType::get(new_type, aty->getNumElements()), pr.second);
    }
  }
  return result;
}

bool FixedPointTransform::run() {

  LLVMContext &ctx = module->getContext();

  // cannot remove replaced instructions during iteration, so keep a set to drop
  // later
  set<Instruction *> remove_ins;
  set<Function *> remove_fns;

  // find all fp types embedded in structs and unions

  // get a set of all structures
  TypeFinder finder;
  finder.run(*module, false);
  set<StructType*> all_structs;
  for (StructType *sty : finder) {
    all_structs.insert(sty);
  }

  // this has to be iteratively updated until all affected structs are identified
  // i.e. we need the transitive closure
  unsigned num_prior = 0;
  while (num_prior < map_struct_ty.size() + 1) {
    num_prior = map_struct_ty.size() + 1;
    for (StructType *sty : all_structs) {
      for (unsigned idx = 0, end = sty->getNumElements(); idx < end; ++idx) {
        if (isFP(sty->getElementType(idx))) {
          // sty will need to be recreated due to changed struct
          map_struct_ty.insert(make_pair(sty, StructType::create(ctx)));
        }
      }
    }
  }

  // now have a complete list of structures to be re-written
  // create the new structures
  for (auto pr : map_struct_ty) {
    StructType *old_sty = pr.first;
    StructType *new_sty = pr.second;
    string st_name = old_sty->getName().str();
    old_sty->setName(st_name + ".delete.me");
    new_sty->setName(st_name);

    vector<Type*> members;
    for (unsigned idx = 0, end = old_sty->getNumElements(); idx < end; ++idx) {
      Type *mem_ty = old_sty->getElementType(idx);
      if (Type *new_ty = transformFP(mem_ty)) {
        members.push_back(new_ty);
      } else {
        members.push_back(mem_ty);
      }
    }
    new_sty->setBody(members, old_sty->isPacked());
  }

  // global variables, cannot add or delete during iteration, so just create a record of gvs to replace
  map<GlobalVariable*,Type*> map_gvs;
  for (auto itr = module->global_begin(), end = module->global_end(); itr != end; ++itr) {
    GlobalVariable *gv = &(*itr);
    Type *old_ty = gv->getValueType();
    if (Type *new_ty = transformFP(old_ty)) {
      map_gvs.insert(make_pair(gv, new_ty));
    }
  }

  // replace each of these global variables with a new one of the correct type
  for (auto itr = map_gvs.begin(), end = map_gvs.end(); itr != end; ++itr) {
    GlobalVariable *old_gv = itr->first;
    Type *new_t = itr->second;

    // RLR TODO: this only sets the new global to have the default initializer.
    Constant *init = ConstantAggregateZero::get(new_t);
    GlobalVariable *new_gv = new GlobalVariable(*module, new_t, old_gv->isConstant(), old_gv->getLinkage(), init, old_gv->getName(), old_gv);
    new_gv->copyAttributesFrom(old_gv);
    new_gv->takeName(old_gv);

    // replace uses of the old with the new, and drop the old
    // the old structs cannot be explicitly dropped, they drop from context with
    // the last instance does.
    old_gv->replaceAllUsesWith(new_gv);
    old_gv->eraseFromParent();
  }

  // aliases

  // find all functions that have to be recreated
  // i.e. those that have FP in signiture
  map<Function*,FunctionType*> map_func_ty;
  for (Function &fn : *module) {

    // operands
    bool fty_modified = false;

    if (!fn.isDeclaration()) {

      // construct a new function type
      FunctionType *old_fty = fn.getFunctionType();
      Type *new_rty = old_fty->getReturnType();
      if (Type *new_ty = transformFP(new_rty)) {
        new_rty = new_ty;
        fty_modified = true;
      }
      vector<Type*> parameters;
      for (unsigned idx = 0, end = old_fty->getNumParams(); idx < end; ++idx) {
        Type *param_ty = old_fty->getParamType(idx);
        if (Type *new_ty = transformFP(param_ty)) {
          parameters.push_back(new_ty);
          fty_modified = true;
        } else {
          parameters.push_back(param_ty);
        }
      }
      if (fty_modified) {
        FunctionType *new_fty = FunctionType::get(new_rty, parameters, old_fty->isVarArg());
        map_func_ty.insert(make_pair(&fn, new_fty));
      }
    }
  }

  set<Function*> modified_fns;
  for (auto pr : map_func_ty) {

    Function *old_fn = pr.first;
    FunctionType *new_fty = pr.second;
    Function *new_fn = Function::Create(new_fty, old_fn->getLinkage(), old_fn->getAddressSpace(), old_fn->getName(), module);
    ValueToValueMapTy vmap;
    for (unsigned idx = 0, end = new_fty->getNumParams(); idx < end; ++idx) {
      const Argument *old_arg = old_fn->getArg(idx);
      Argument *new_arg = new_fn->getArg(idx);
      if (old_arg->hasName()) {
        new_arg->setName(old_arg->getName());
      }
      vmap[old_arg] = new_arg;
    }
    SmallVector<ReturnInst*, 8> rets;
    CloneFunctionInto(new_fn, old_fn, vmap, old_fn->getSubprogram() != nullptr, rets);
    new_fn->takeName(old_fn);
    old_fn->replaceAllUsesWith(new_fn);

    // if this function was in the map of fns translations, then update it.
    auto itr = map_fns2fns.find(old_fn);
    if (itr != map_fns2fns.end()) {
      Function *trg_fn = itr->second;
      map_fns2fns.erase(itr);
      map_fns2fns.insert(make_pair(new_fn, trg_fn));
    }
    old_fn->eraseFromParent();
    modified_fns.insert(new_fn);
  }

  for (Function &fn : *module) {

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
            in.setOperand(idx, ConstantInt::get(Type::getInt64Ty(ctx), (uint64_t) dbl));
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
          } else if (auto UI = dyn_cast<UnaryInstruction>(&in)) {
            Function *fn = itr->second;
            vector<Value*> args = { UI->getOperand(0) };
            Instruction *call = CallInst::Create(fn, args, "", UI);

            // preserve any metadata
            SmallVector<pair<unsigned, MDNode *>, 4> mds;
            UI->getAllMetadata(mds);
            for (const auto &md : mds) {
              call->setMetadata(md.first, md.second);
            }
            UI->replaceAllUsesWith(call);
            remove_ins.insert(UI);

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
            auto v = SR->getPointerOperand();
            if (Type *new_t = transformFP(v->getType())) {
              v->mutateType(new_t);
            }
          } break;

          case Instruction::GetElementPtr: {
            auto GEP = static_cast<GetElementPtrInst *>(&in);
            Value *v = GEP->getPointerOperand();
            if (Type *new_type = transformFP(GEP->getSourceElementType())) {
              GEP->setSourceElementType(new_type);
            }
            if (Type *new_type = transformFP(GEP->getResultElementType())) {
              GEP->setResultElementType(new_type);
            }
            if (Type *new_type = transformFP(GEP->getType())) {
              GEP->mutateType(new_type);
            }
            if (Type *new_type = transformFP(v->getType())) {
              v->mutateType(new_type);
            }
          } break;

          case Instruction::ExtractValue: {
            auto EV = static_cast<ExtractValueInst *>(&in);
            Value *v = EV->getAggregateOperand();
            if (Type *new_type = transformFP(v->getType())) {
              v->mutateType(new_type);
            }
            if (Type *new_type = transformFP(EV->getType())) {
              EV->mutateType(new_type);
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
            Function *fn = CI->getCalledFunction();
            auto itr = map_fns2fns.find(fn);
            if (itr != map_fns2fns.end()) {
              Function *new_fn = itr->second;
              CI->mutateType(new_fn->getReturnType());
              CI->mutateFunctionType(new_fn->getFunctionType());
              CI->setCalledFunction(new_fn);
              remove_fns.insert(fn);
            } else if (modified_fns.count(fn) > 0) {
              CI->mutateFunctionType(fn->getFunctionType());
              CI->mutateType(fn->getReturnType());
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

  // RLR TODO: hack for some annoying llvm intrinsics
  set<string> annoying_hack{ "__isnan__", "__isinf__", "__isfinite__" };

  for (auto fn : remove_fns) {
    if (annoying_hack.count(fn->getName().str()) == 0) {
      fn->eraseFromParent();
    }
  }

  for (auto &fn_name : annoying_hack) {
    if (Function *fn = module->getFunction(fn_name)) {
      fn->deleteBody();
    }
  }

  static set<string> i_know{ "__assert_fail", "klee_make_symbolic", "printf", "klee_range", "fflush" };
  for (Function &fn : *module) {
    if (fn.isDeclaration() && fn.getNumUses() > 0) {
      string fn_name = fn.getName().str();
      if (fn_name.rfind("fix32_", 0) != 0) {
        if (fn_name.rfind("llvm.", 0) != 0) {
          if (i_know.count(fn_name) == 0) {
            outs() << "!!!!! used fn: " << fn_name << '\n';
          }
        }
      }
    }
  }
  return true;
}

bool FixedPointTransform::stripUnused(Module *module) {

  bool done = false;
  while (!done) {
    done = true;
    vector<Function*> fns_to_remove;
    for (Function &fn : *module) {
      if (fn.hasName() && fn.getName().str().rfind("fix32_", 0) == 0) {
        if (fn.getNumUses() == 0 && !fn.hasAddressTaken()) {
          fns_to_remove.push_back(&fn);
        }
      }
    }
    for (Function *fn : fns_to_remove) {
      fn->eraseFromParent();
    }

    vector<GlobalVariable*> gvs_to_remove;
    for (GlobalVariable &gv : module->globals()) {
      if (gv.hasName() && gv.getName().str().rfind("fix32_", 0) == 0) {
        if (gv.getNumUses() == 0) {
          gvs_to_remove.push_back(&gv);
        }
      }
    }
    for (GlobalVariable *gv : gvs_to_remove) {
      gv->eraseFromParent();
    }
    done = fns_to_remove.empty() && gvs_to_remove.empty();
    outs().flush();
  }
  return true;
}

bool FixedPointTransform::isFP(Type *ty) const {
  bool result = false;
  auto pr = unwrap_type(ty);
  if (map_basic_ty.count(pr.first) > 0) {
    result = true;
  } else if (StructType *sty = dyn_cast<StructType>(pr.first)) {
    result = map_struct_ty.count(sty) > 0;
  }
  return result;
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
