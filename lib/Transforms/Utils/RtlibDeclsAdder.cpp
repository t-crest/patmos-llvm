//===-- RtlibDeclsAdder.cpp - Add declarations for rtlib ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass adds declarations for rtlib.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "rtlibdeclsadder"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
using namespace llvm;

STATISTIC(NumCalls, "Number of handled calls");


namespace {

  class RtlibDeclsAdder : public ModulePass,
                           public InstVisitor<RtlibDeclsAdder> {

  protected:
    friend class InstVisitor<RtlibDeclsAdder>;
    virtual void visitCallInst(CallInst &CI);

    const TargetData *TD;

    virtual void AddIntrinsicPrototypes(Module &M);
    virtual void LowerIntrinsicCall(CallInst *CI);

  public:
    static char ID; // Pass identification, replacement for typeid
    RtlibDeclsAdder() : ModulePass(ID) {
      initializeRtlibDeclsAdderPass(*PassRegistry::getPassRegistry());
    }
    virtual ~RtlibDeclsAdder() { /*delete IL;*/ }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<TargetData>();
    }
    virtual void print(raw_ostream &O, const Module *M) const {}

  };
}

char RtlibDeclsAdder::ID = 0;
INITIALIZE_PASS(RtlibDeclsAdder, "rtlibdeclsadder",
                "Add declarations for rtlib", false, false)

ModulePass *
llvm::createRtlibDeclsAdderPass() { return new RtlibDeclsAdder(); }

//-----------------------------------------------------------------------------



template <class ArgIt>
static void EnsureFunctionExists(Module &M, const char *Name,
                                 ArgIt ArgBegin, ArgIt ArgEnd,
                                 Type *RetTy) {
  // Insert a correctly-typed definition now.
  std::vector<Type *> ParamTys;
  for (ArgIt I = ArgBegin; I != ArgEnd; ++I)
    ParamTys.push_back(I->getType());
  M.getOrInsertFunction(Name, FunctionType::get(RetTy, ParamTys, false));
}


static void EnsureFPIntrinsicsExist(Module &M, Function *Fn,
                                    const char *FName,
                                    const char *DName, const char *LDName) {
  // Insert definitions for all the floating point types.
  switch((int)Fn->arg_begin()->getType()->getTypeID()) {
  case Type::FloatTyID:
    EnsureFunctionExists(M, FName, Fn->arg_begin(), Fn->arg_end(),
                         Type::getFloatTy(M.getContext()));
    break;
  case Type::DoubleTyID:
    EnsureFunctionExists(M, DName, Fn->arg_begin(), Fn->arg_end(),
                         Type::getDoubleTy(M.getContext()));
    break;
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
    EnsureFunctionExists(M, LDName, Fn->arg_begin(), Fn->arg_end(),
                         Fn->arg_begin()->getType());
    break;
  default:
    llvm_unreachable("Unhandled floating point type for intrinsic function");
  }
}


/// ReplaceCallWith - This function is used when we want to lower an intrinsic
/// call to a call of an external function.  This handles hard cases such as
/// when there was already a prototype for the external function, and if that
/// prototype doesn't match the arguments we expect to pass in.
template <class ArgIt>
static CallInst *ReplaceCallWith(const char *NewFn, CallInst *CI,
                                 ArgIt ArgBegin, ArgIt ArgEnd,
                                 Type *RetTy) {
  // If we haven't already looked up this function, check to see if the
  // program already contains a function with this name.
  Module *M = CI->getParent()->getParent()->getParent();
  // Get or insert the definition now.
  std::vector<Type *> ParamTys;
  for (ArgIt I = ArgBegin; I != ArgEnd; ++I)
    ParamTys.push_back((*I)->getType());
  Constant* FCache = M->getOrInsertFunction(NewFn,
                                  FunctionType::get(RetTy, ParamTys, false));

  IRBuilder<> Builder(CI->getParent(), CI);
  SmallVector<Value *, 8> Args(ArgBegin, ArgEnd);
  CallInst *NewCI = Builder.CreateCall(FCache, Args);
  NewCI->setName(CI->getName());
  if (!CI->use_empty())
    CI->replaceAllUsesWith(NewCI);
  return NewCI;
}

static void ReplaceFPIntrinsicWithCall(CallInst *CI, const char *Fname,
                                       const char *Dname,
                                       const char *LDname) {
  CallSite CS(CI);
  switch (CI->getArgOperand(0)->getType()->getTypeID()) {
  default: llvm_unreachable("Invalid type in intrinsic");
  case Type::FloatTyID:
    ReplaceCallWith(Fname, CI, CS.arg_begin(), CS.arg_end(),
                  Type::getFloatTy(CI->getContext()));
    break;
  case Type::DoubleTyID:
    ReplaceCallWith(Dname, CI, CS.arg_begin(), CS.arg_end(),
                  Type::getDoubleTy(CI->getContext()));
    break;
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
    ReplaceCallWith(LDname, CI, CS.arg_begin(), CS.arg_end(),
                  CI->getArgOperand(0)->getType());
    break;
  }
}

//-----------------------------------------------------------------------------

bool RtlibDeclsAdder::runOnModule(Module &M) {
  TD = &getAnalysis<TargetData>();
  AddIntrinsicPrototypes(M);
  visit(M);
  //delete IL;
  return true;
}

void RtlibDeclsAdder::visitCallInst(CallInst &CI) {
  dbgs() << "Call instruction " << CI << '\n';
  if (CI.getCalledFunction()->isIntrinsic()) {
    LowerIntrinsicCall(&CI);
  }
  NumCalls++;
}



// VisualStudio defines setjmp as _setjmp
#if defined(_MSC_VER) && defined(setjmp) && \
                         !defined(setjmp_undefined_for_msvc)
#  pragma push_macro("setjmp")
#  undef setjmp
#  define setjmp_undefined_for_msvc
#endif

void RtlibDeclsAdder::AddIntrinsicPrototypes(Module &M) {
  LLVMContext &Context = M.getContext();
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I)
    if (I->isDeclaration() && !I->use_empty())
      switch (I->getIntrinsicID()) {
      default: break;
      case Intrinsic::setjmp:
        EnsureFunctionExists(M, "setjmp", I->arg_begin(), I->arg_end(),
                             Type::getInt32Ty(M.getContext()));
        break;
      case Intrinsic::longjmp:
        EnsureFunctionExists(M, "longjmp", I->arg_begin(), I->arg_end(),
                             Type::getVoidTy(M.getContext()));
        break;
      case Intrinsic::siglongjmp:
        EnsureFunctionExists(M, "abort", I->arg_end(), I->arg_end(),
                             Type::getVoidTy(M.getContext()));
        break;
      case Intrinsic::memcpy:
        M.getOrInsertFunction("memcpy",
          Type::getInt8PtrTy(Context),
                              Type::getInt8PtrTy(Context),
                              Type::getInt8PtrTy(Context),
                              TD->getIntPtrType(Context), (Type *)0);
        break;
      case Intrinsic::memmove:
        M.getOrInsertFunction("memmove",
          Type::getInt8PtrTy(Context),
                              Type::getInt8PtrTy(Context),
                              Type::getInt8PtrTy(Context),
                              TD->getIntPtrType(Context), (Type *)0);
        break;
      case Intrinsic::memset:
        M.getOrInsertFunction("memset",
          Type::getInt8PtrTy(Context),
                              Type::getInt8PtrTy(Context),
                              Type::getInt32Ty(M.getContext()),
                              TD->getIntPtrType(Context), (Type *)0);
        break;
      case Intrinsic::sqrt:
        EnsureFPIntrinsicsExist(M, I, "sqrtf", "sqrt", "sqrtl");
        break;
      case Intrinsic::sin:
        EnsureFPIntrinsicsExist(M, I, "sinf", "sin", "sinl");
        break;
      case Intrinsic::cos:
        EnsureFPIntrinsicsExist(M, I, "cosf", "cos", "cosl");
        break;
      case Intrinsic::pow:
        EnsureFPIntrinsicsExist(M, I, "powf", "pow", "powl");
        break;
      case Intrinsic::log:
        EnsureFPIntrinsicsExist(M, I, "logf", "log", "logl");
        break;
      case Intrinsic::log2:
        EnsureFPIntrinsicsExist(M, I, "log2f", "log2", "log2l");
        break;
      case Intrinsic::log10:
        EnsureFPIntrinsicsExist(M, I, "log10f", "log10", "log10l");
        break;
      case Intrinsic::exp:
        EnsureFPIntrinsicsExist(M, I, "expf", "exp", "expl");
        break;
      case Intrinsic::exp2:
        EnsureFPIntrinsicsExist(M, I, "exp2f", "exp2", "exp2l");
        break;
      }
}



void RtlibDeclsAdder::LowerIntrinsicCall(CallInst *CI) {
  IRBuilder<> Builder(CI->getParent(), CI);
  LLVMContext &Context = CI->getContext();

  const Function *Callee = CI->getCalledFunction();
  assert(Callee && "Cannot lower an indirect call!");

  CallSite CS(CI);
  switch (Callee->getIntrinsicID()) {
  case Intrinsic::not_intrinsic:
    report_fatal_error("Cannot lower a call to a non-intrinsic function '"+
                      Callee->getName() + "'!");
  default:
    report_fatal_error("Code generator does not support intrinsic function '"+
                      Callee->getName()+"'!");

  case Intrinsic::expect: {
    // Just replace __builtin_expect(exp, c) with EXP.
    Value *V = CI->getArgOperand(0);
    CI->replaceAllUsesWith(V);
    break;
  }

    // The setjmp/longjmp intrinsics should only exist in the code if it was
    // never optimized (ie, right out of the CFE), or if it has been hacked on
    // by the lowerinvoke pass.  In both cases, the right thing to do is to
    // convert the call to an explicit setjmp or longjmp call.
  case Intrinsic::setjmp: {
    Value *V = ReplaceCallWith("setjmp", CI, CS.arg_begin(), CS.arg_end(),
                               Type::getInt32Ty(Context));
    if (!CI->getType()->isVoidTy())
      CI->replaceAllUsesWith(V);
    break;
  }
  case Intrinsic::sigsetjmp:
     if (!CI->getType()->isVoidTy())
       CI->replaceAllUsesWith(Constant::getNullValue(CI->getType()));
     break;

  case Intrinsic::longjmp: {
    ReplaceCallWith("longjmp", CI, CS.arg_begin(), CS.arg_end(),
                    Type::getVoidTy(Context));
    break;
  }

  case Intrinsic::siglongjmp: {
    // Insert the call to abort
    ReplaceCallWith("abort", CI, CS.arg_end(), CS.arg_end(), 
                    Type::getVoidTy(Context));
    break;
  }
  case Intrinsic::ctpop:
    //CI->replaceAllUsesWith(LowerCTPOP(Context, CI->getArgOperand(0), CI));
    break;

  case Intrinsic::bswap:
    //CI->replaceAllUsesWith(LowerBSWAP(Context, CI->getArgOperand(0), CI));
    break;

  case Intrinsic::ctlz:
    //CI->replaceAllUsesWith(LowerCTLZ(Context, CI->getArgOperand(0), CI));
    break;

  case Intrinsic::cttz: {
    // cttz(x) -> ctpop(~X & (X-1))
    /*
    Value *Src = CI->getArgOperand(0);
    Value *NotSrc = Builder.CreateNot(Src);
    NotSrc->setName(Src->getName() + ".not");
    Value *SrcM1 = ConstantInt::get(Src->getType(), 1);
    SrcM1 = Builder.CreateSub(Src, SrcM1);
    Src = LowerCTPOP(Context, Builder.CreateAnd(NotSrc, SrcM1), CI);
    CI->replaceAllUsesWith(Src);
    */
    break;
  }

  case Intrinsic::stacksave:
  case Intrinsic::stackrestore: {
    errs() << "WARNING: this target does not support the llvm.stack"
           << (Callee->getIntrinsicID() == Intrinsic::stacksave ?
             "save" : "restore") << " intrinsic.\n";
    CI->replaceAllUsesWith(Constant::getNullValue(CI->getType()));
    break;
  }

  case Intrinsic::returnaddress:
  case Intrinsic::frameaddress:
    errs() << "WARNING: this target does not support the llvm."
           << (Callee->getIntrinsicID() == Intrinsic::returnaddress ?
             "return" : "frame") << "address intrinsic.\n";
    CI->replaceAllUsesWith(ConstantPointerNull::get(
                                            cast<PointerType>(CI->getType())));
    break;

  case Intrinsic::prefetch:
    break;    // Simply strip out prefetches on unsupported architectures

  case Intrinsic::pcmarker:
    break;    // Simply strip out pcmarker on unsupported architectures
  case Intrinsic::readcyclecounter: {
    errs() << "WARNING: this target does not support the llvm.readcyclecoun"
           << "ter intrinsic.  It is being lowered to a constant 0\n";
    CI->replaceAllUsesWith(ConstantInt::get(Type::getInt64Ty(Context), 0));
    break;
  }

  case Intrinsic::dbg_declare:
    break;    // Simply strip out debugging intrinsics

  case Intrinsic::eh_typeid_for:
    // Return something different to eh_selector.
    CI->replaceAllUsesWith(ConstantInt::get(CI->getType(), 1));
    break;

  case Intrinsic::var_annotation:
    break;   // Strip out annotate intrinsic
    
  case Intrinsic::memcpy: {
    IntegerType *IntPtr = TD->getIntPtrType(Context);
    Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                        /* isSigned */ false);
    Value *Ops[3];
    Ops[0] = CI->getArgOperand(0);
    Ops[1] = CI->getArgOperand(1);
    Ops[2] = Size;
    ReplaceCallWith("memcpy", CI, Ops, Ops+3, CI->getArgOperand(0)->getType());
    break;
  }
  case Intrinsic::memmove: {
    IntegerType *IntPtr = TD->getIntPtrType(Context);
    Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                        /* isSigned */ false);
    Value *Ops[3];
    Ops[0] = CI->getArgOperand(0);
    Ops[1] = CI->getArgOperand(1);
    Ops[2] = Size;
    ReplaceCallWith("memmove", CI, Ops, Ops+3, CI->getArgOperand(0)->getType());
    break;
  }
  case Intrinsic::memset: {
    IntegerType *IntPtr = TD->getIntPtrType(Context);
    Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                        /* isSigned */ false);
    Value *Ops[3];
    Ops[0] = CI->getArgOperand(0);
    // Extend the amount to i32.
    Ops[1] = Builder.CreateIntCast(CI->getArgOperand(1),
                                   Type::getInt32Ty(Context),
                                   /* isSigned */ false);
    Ops[2] = Size;
    ReplaceCallWith("memset", CI, Ops, Ops+3, CI->getArgOperand(0)->getType());
    break;
  }
  case Intrinsic::sqrt: {
    ReplaceFPIntrinsicWithCall(CI, "sqrtf", "sqrt", "sqrtl");
    break;
  }
  case Intrinsic::log: {
    ReplaceFPIntrinsicWithCall(CI, "logf", "log", "logl");
    break;
  }
  case Intrinsic::log2: {
    ReplaceFPIntrinsicWithCall(CI, "log2f", "log2", "log2l");
    break;
  }
  case Intrinsic::log10: {
    ReplaceFPIntrinsicWithCall(CI, "log10f", "log10", "log10l");
    break;
  }
  case Intrinsic::exp: {
    ReplaceFPIntrinsicWithCall(CI, "expf", "exp", "expl");
    break;
  }
  case Intrinsic::exp2: {
    ReplaceFPIntrinsicWithCall(CI, "exp2f", "exp2", "exp2l");
    break;
  }
  case Intrinsic::pow: {
    ReplaceFPIntrinsicWithCall(CI, "powf", "pow", "powl");
    break;
  }
  case Intrinsic::flt_rounds:
     // Lower to "round to the nearest"
     if (!CI->getType()->isVoidTy())
       CI->replaceAllUsesWith(ConstantInt::get(CI->getType(), 1));
     break;
  case Intrinsic::invariant_start:
  case Intrinsic::lifetime_start:
    // Discard region information.
    CI->replaceAllUsesWith(UndefValue::get(CI->getType()));
    break;
  case Intrinsic::invariant_end:
  case Intrinsic::lifetime_end:
    // Discard region information.
    break;
  }

  assert(CI->use_empty() &&
         "Lowering should have eliminated any uses of the intrinsic call!");
  CI->eraseFromParent();
}
