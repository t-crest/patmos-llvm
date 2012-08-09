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
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"
using namespace llvm;


STATISTIC(NumDeclsAdded, "Number of declarations added");

STATISTIC(NumIntrinsicsLowered, "Number of lowered intrinsic calls");
STATISTIC(NumIntrinsicsIgnored, "Number of ignored intrinsic calls");

static SmallPtrSet<Value *, 4> DeclsSet;



namespace {

  class RtlibDeclsAdder : public ModulePass,
                           public InstVisitor<RtlibDeclsAdder> {

  protected:
    friend class InstVisitor<RtlibDeclsAdder>;
    virtual void visitCallInst(CallInst &CI);

    const TargetData *TD;

    virtual void AddIntrinsicPrototypes(Module &M);
    /// Lower an intrinsic call.
    /// Returns true if the intrinsic was handled and lowered.
    virtual bool LowerIntrinsicCall(CallInst *CI);

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
INITIALIZE_PASS(RtlibDeclsAdder, "add-rtlib-decls",
                "Add declarations for runtime library functions for the linker", false, false)

ModulePass *
llvm::createRtlibDeclsAdderPass() { return new RtlibDeclsAdder(); }

//-----------------------------------------------------------------------------



template <class ArgIt>
static Constant *EnsureFunctionExists(Module &M, const char *Name,
                                 ArgIt ArgBegin, ArgIt ArgEnd,
                                 Type *RetTy) {
  // Insert a correctly-typed definition now.
  std::vector<Type *> ParamTys;
  for (ArgIt I = ArgBegin; I != ArgEnd; ++I)
    ParamTys.push_back(I->getType());
  return M.getOrInsertFunction(Name, FunctionType::get(RetTy, ParamTys, false));
}


static Constant *EnsureFPIntrinsicsExist(Module &M, Function *Fn,
                                    const char *FName,
                                    const char *DName, const char *LDName) {
  Constant *F = NULL;
  // Insert definitions for all the floating point types.
  switch((int)Fn->arg_begin()->getType()->getTypeID()) {
  case Type::FloatTyID:
    F = EnsureFunctionExists(M, FName, Fn->arg_begin(), Fn->arg_end(),
                         Type::getFloatTy(M.getContext()));
    break;
  case Type::DoubleTyID:
    F = EnsureFunctionExists(M, DName, Fn->arg_begin(), Fn->arg_end(),
                         Type::getDoubleTy(M.getContext()));
    break;
  case Type::X86_FP80TyID:
  case Type::FP128TyID:
  case Type::PPC_FP128TyID:
    F = EnsureFunctionExists(M, LDName, Fn->arg_begin(), Fn->arg_end(),
                         Fn->arg_begin()->getType());
    break;
  default:
    report_fatal_error("Unhandled floating point type for intrinsic function '"
                     +Fn->getName()+"'!");
  }
  return F;
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
  NumDeclsAdded = DeclsSet.size();
  visit(M);
  return true;
}

void RtlibDeclsAdder::visitCallInst(CallInst &CI) {
  const Function *Callee = CI.getCalledFunction();
  dbgs() << "Call instruction " << CI << '\n';
  if (Callee && CI.getCalledFunction()->isIntrinsic()) {
    bool wasLowered = LowerIntrinsicCall(&CI);
    if (wasLowered) {
      NumIntrinsicsLowered++;
    } else {
      NumIntrinsicsIgnored++;
    }
  }
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
  for (Module::iterator I = M.begin(), E = M.end(); I != E; ++I) {
    if (I->isDeclaration() && !I->use_empty()) {
      Constant *F = NULL;
      switch (I->getIntrinsicID()) {
      default: break;
      // mem*
      case Intrinsic::memcpy:
        F = M.getOrInsertFunction("memcpy",
            Type::getInt8PtrTy(Context),
            Type::getInt8PtrTy(Context),
            Type::getInt8PtrTy(Context),
            TD->getIntPtrType(Context), (Type *)0);
        break;
      case Intrinsic::memmove:
        F = M.getOrInsertFunction("memmove",
            Type::getInt8PtrTy(Context),
            Type::getInt8PtrTy(Context),
            Type::getInt8PtrTy(Context),
            TD->getIntPtrType(Context), (Type *)0);
        break;
      case Intrinsic::memset:
        F = M.getOrInsertFunction("memset",
          Type::getInt8PtrTy(Context),
          Type::getInt8PtrTy(Context),
          Type::getInt32Ty(M.getContext()),
          TD->getIntPtrType(Context), (Type *)0);
        break;
      // setjmp, longjmp and friends, and trap
      case Intrinsic::setjmp:
        F = EnsureFunctionExists(M, "setjmp", I->arg_begin(), I->arg_end(),
                             Type::getInt32Ty(M.getContext()));
        break;
      case Intrinsic::longjmp:
        F = EnsureFunctionExists(M, "longjmp", I->arg_begin(), I->arg_end(),
                             Type::getVoidTy(M.getContext()));
        break;
      case Intrinsic::sigsetjmp:
        // do nothing
        break;
      case Intrinsic::siglongjmp:
      case Intrinsic::trap:
        F = EnsureFunctionExists(M, "abort", I->arg_end(), I->arg_end(),
                             Type::getVoidTy(M.getContext()));
        break;
      case Intrinsic::sqrt:
        F = EnsureFPIntrinsicsExist(M, I, "sqrtf", "sqrt", "sqrtl");
        break;
      case Intrinsic::sin:
        F = EnsureFPIntrinsicsExist(M, I, "sinf", "sin", "sinl");
        break;
      case Intrinsic::cos:
        F = EnsureFPIntrinsicsExist(M, I, "cosf", "cos", "cosl");
        break;
      case Intrinsic::log:
        F = EnsureFPIntrinsicsExist(M, I, "logf", "log", "logl");
        break;
      case Intrinsic::log2:
        F = EnsureFPIntrinsicsExist(M, I, "log2f", "log2", "log2l");
        break;
      case Intrinsic::log10:
        F = EnsureFPIntrinsicsExist(M, I, "log10f", "log10", "log10l");
        break;
      case Intrinsic::exp:
        F = EnsureFPIntrinsicsExist(M, I, "expf", "exp", "expl");
        break;
      case Intrinsic::exp2:
        F = EnsureFPIntrinsicsExist(M, I, "exp2f", "exp2", "exp2l");
        break;
      case Intrinsic::pow:
        F = EnsureFPIntrinsicsExist(M, I, "powf", "pow", "powl");
        break;
      } // end switch
      if (F) DeclsSet.insert(F);
    } // end if
  } // end for
}



bool RtlibDeclsAdder::LowerIntrinsicCall(CallInst *CI) {
  IRBuilder<> Builder(CI->getParent(), CI);
  LLVMContext &Context = CI->getContext();

  bool wasLowered = true;

  CallSite CS(CI);
  switch (CI->getCalledFunction()->getIntrinsicID()) {
    case Intrinsic::memcpy:
      {
        IntegerType *IntPtr = TD->getIntPtrType(Context);
        Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                            /* isSigned */ false);
        Value *Ops[3];
        Ops[0] = CI->getArgOperand(0);
        Ops[1] = CI->getArgOperand(1);
        Ops[2] = Size;
        ReplaceCallWith("memcpy", CI, Ops, Ops+3, CI->getArgOperand(0)->getType());
      }
      break;
    case Intrinsic::memmove:
      {
        IntegerType *IntPtr = TD->getIntPtrType(Context);
        Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                            /* isSigned */ false);
        Value *Ops[3];
        Ops[0] = CI->getArgOperand(0);
        Ops[1] = CI->getArgOperand(1);
        Ops[2] = Size;
        ReplaceCallWith("memmove", CI, Ops, Ops+3, CI->getArgOperand(0)->getType());
      }
      break;
    case Intrinsic::memset:
      {
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
      }
      break;
    // The setjmp/longjmp intrinsics should only exist in the code if it was
    // never optimized (ie, right out of the CFE), or if it has been hacked on
    // by the lowerinvoke pass.  In both cases, the right thing to do is to
    // convert the call to an explicit setjmp or longjmp call.
    case Intrinsic::setjmp:
      {
        Value *V = ReplaceCallWith("setjmp", CI, CS.arg_begin(), CS.arg_end(),
                                   Type::getInt32Ty(Context));
        if (!CI->getType()->isVoidTy())
          CI->replaceAllUsesWith(V);
      }
      break;
    case Intrinsic::longjmp:
      ReplaceCallWith("longjmp", CI, CS.arg_begin(), CS.arg_end(),
                      Type::getVoidTy(Context));
      break;
    case Intrinsic::sigsetjmp:
      // is removed
      if (!CI->getType()->isVoidTy())
        CI->replaceAllUsesWith(Constant::getNullValue(CI->getType()));
      break;
    case Intrinsic::siglongjmp:
    case Intrinsic::trap:
      // Insert the call to abort
      ReplaceCallWith("abort", CI, CS.arg_end(), CS.arg_end(),
                      Type::getVoidTy(Context));
      break;


    case Intrinsic::sqrt:
      ReplaceFPIntrinsicWithCall(CI, "sqrtf", "sqrt", "sqrtl");
      break;
    case Intrinsic::sin:
      ReplaceFPIntrinsicWithCall(CI, "sinf", "sin", "sinl");
      break;
    case Intrinsic::cos:
      ReplaceFPIntrinsicWithCall(CI, "cosf", "cos", "cosl");
      break;
    case Intrinsic::log:
      ReplaceFPIntrinsicWithCall(CI, "logf", "log", "logl");
      break;
    case Intrinsic::log2:
      ReplaceFPIntrinsicWithCall(CI, "log2f", "log2", "log2l");
      break;
    case Intrinsic::log10:
      ReplaceFPIntrinsicWithCall(CI, "log10f", "log10", "log10l");
      break;
    case Intrinsic::exp:
      ReplaceFPIntrinsicWithCall(CI, "expf", "exp", "expl");
      break;
    case Intrinsic::exp2:
      ReplaceFPIntrinsicWithCall(CI, "exp2f", "exp2", "exp2l");
      break;
    case Intrinsic::pow:
      ReplaceFPIntrinsicWithCall(CI, "powf", "pow", "powl");
      break;
    /*
    case Intrinsic::flt_rounds:
       // Lower to "round to the nearest"
       if (!CI->getType()->isVoidTy())
         CI->replaceAllUsesWith(ConstantInt::get(CI->getType(), 1));
       break;
    */
    default:
      /* ignore call to intrinsic function */
      wasLowered = false;
  }

  if (wasLowered) {
    assert(CI->use_empty() && "Lowering should have eliminated any uses of the intrinsic call!");
    CI->eraseFromParent();
  }
  return wasLowered;
}
