//===-- AddRuntimeDependencies.cpp - Add declarations for rtlib ------------------===//
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
// TODO:
// The pass is currently unused, since it does not really work this way.
// Additional instructions must still be handled, and ideally the instructions
// must be lowered down to calls, but this needs to handle various special
// cases that are already handled by the backend (partially constant arguments,
// ...).
// The pass might still be useful in the future however, when full program
// analysis should be performed, including code in the runtime libs.
//
// TODO The options stuff is a mess, also the pass registration, but that's
// the way it is in LLVM obviously.
//
// TODO The pass needs to be executed precisely at the right time in the tool
// chain, i.e., after all optimizations that a) introduce new code that might
// result in runtime calls, or b) use the semantics of the instructions that
// are removed by this pass, but before any optimizations that do whole-program
// analyses. And it needs to be automatically executed by the driver, but not
// for libraries, as they are linked in and optimized at a later time.
//
// TODO floats need to be handled more fine-grained, i.e., distinguish between
// no floating point support at all, only SP floats, full float hardware but no
// pow/log/.. functions..
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "addruntimedeps"

#include "llvm/Transforms/Scalar.h"
#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Constants.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Intrinsics.h"
#include "llvm/DataLayout.h"
#include "llvm/IRBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/InstVisitor.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/Statistic.h"

using namespace llvm;

STATISTIC(NumDeclsAdded, "Number of declarations added");

STATISTIC(NumIntrinsicsLowered, "Number of lowered intrinsic calls");
STATISTIC(NumIntrinsicsIgnored, "Number of ignored intrinsic calls");

static SmallPtrSet<Value *, 4> DeclsSet;


/* This pass is currently unused, so we hide the options for now
 * TODO use the -float-abi option from llc somehow, and there is some sort of
 * target machine in clang too (!?)
static cl::opt<std::string>
FloatABI("-fpatmos-float-abi", cl::value_desc("none|all"),
        cl::desc("Lower floating point instructions to calls"),
        cl::init("all"));

static cl::opt<bool>
LowerCalls("lower-runtime-calls", cl::init(false),
        cl::desc("Lower some operations to runtime library calls (requires -add-runtime-deps)"));
*/
static std::string FloatABI = "all";
static bool LowerCalls = true;

// VisualStudio defines setjmp as _setjmp
#if defined(_MSC_VER) && defined(setjmp) && \
                         !defined(setjmp_undefined_for_msvc)
#  pragma push_macro("setjmp")
#  undef setjmp
#  define setjmp_undefined_for_msvc
#endif

namespace {

  class CallConfig;

  class RuntimeInstructionVisitor {
    bool LowerCalls;
    Module *M;
    LLVMContext *Context;
    const DataLayout *TD;

    SmallPtrSet<Constant*, 8> Used;
    SmallPtrSet<Constant*, 8> Lowered;

  public:
    RuntimeInstructionVisitor() : LowerCalls(false) {}

    void setLowerCalls(bool Lower) {
      LowerCalls = Lower;
    }

    bool doLowerCalls() {
      return LowerCalls;
    }

    void setDataLayout(Module &M, const DataLayout *TD) {
      this->M = &M;
      this->Context = &M.getContext();
      this->TD = TD;
    }

    /// return the referenced function declaration, if any
    Constant *processInstruction(Instruction *I, CallConfig &Config, CallInst **NewCI);

    /// Update llvm.used GlobalVariable with the added declarations
    void updateLLVMUsed();

  private:
    /// Lower the given instruction. Returns the new call instruction if lowering was performed, else NULL.
    CallInst *replaceInstruction(Instruction *I, CallConfig &Config, Constant *F);

  };


  struct TypeCache {
    Type *FloatTy;
    Type *DoubleTy;
    Type *IntTy;
  };


  class AddRuntimeDependencies : public ModulePass,
                          public InstVisitor<AddRuntimeDependencies>
  {
  protected:
    friend class InstVisitor<AddRuntimeDependencies>;

    class RuntimeInstructionVisitor RIC;
    bool HandleFloats;

    TypeCache TyCache;
  public:
    static char ID; // Pass identification, replacement for typeid

    AddRuntimeDependencies(bool _LowerCalls = LowerCalls,
                           StringRef _FloatABI = FloatABI)
    : ModulePass(ID)
    {
      initializeAddRuntimeDependenciesPass(*PassRegistry::getPassRegistry());

      RIC.setLowerCalls(_LowerCalls);

      // TODO maybe handle floats depending on type of FPU (only some/all float instructions, ..)
      HandleFloats = (_FloatABI != "none" && _FloatABI != "hard");
    }

    virtual ~AddRuntimeDependencies() { }

    virtual bool runOnModule(Module &M);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<DataLayout>();
    }

    virtual void print(raw_ostream &O, const Module *M) const {}

  protected:
    void visitCallInst(CallInst &CI);

    void visitFPTruncInst(FPTruncInst &I);
    void visitFPExtInst(FPExtInst &I);
    void visitFPToUIInst(FPToUIInst &I);
    void visitFPToSIInst(FPToSIInst &I);
    void visitUIToFPInst(UIToFPInst &I);
    void visitSIToFPInst(SIToFPInst &I);

    void visitFCmpInst(FCmpInst &I);
    void visitBinaryOperator(BinaryOperator &I);
    void visitUnaryInstruction(UnaryInstruction &I);

    void visitIntrinsicCall(CallInst *CI);

    const char *getFPFunctionName(Instruction *I, const char *FName, const char *DName, const char *LName) {
      return getFPFunctionName(I->getOperand(0)->getType(), FName, DName, LName);
    }

    const char *getFPFunctionName(Value *V, const char *FName, const char *DName, const char *LName) {
      return getFPFunctionName(V->getType(), FName, DName, LName);
    }

    const char *getFPFunctionName(Type *T, const char *FName, const char *DName, const char *LName) {
      switch (T->getTypeID()) {
      case Type::FloatTyID:
        return FName;
      case Type::DoubleTyID:
        return DName;
      case Type::X86_FP80TyID:
      case Type::FP128TyID:
      case Type::PPC_FP128TyID:
        return LName;
      default: llvm_unreachable("Invalid type in float instruction.");
        return 0; // To make eclipse happy..
      }
    }

    const char *getIntFunctionName(Instruction *I, const char *SName, const char *DName) {
      switch (I->getOperand(0)->getType()->getIntegerBitWidth()) {
      case 64:
        return DName;
      case 32:
        return SName;
      default: llvm_unreachable("Invalid type size in integer instruction.");
        return 0; // To make eclipse happy..
      }
    }

  };

  class CallConfig {
  public:
    CallConfig() {}
    virtual ~CallConfig() {}

    virtual Constant* getPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) = 0;

    /// Called when addCallArguments returns false or lowering is disabled.
    virtual Constant* getAdditionalPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      return 0;
    }

    /// return false when the instruction cannot be lowered.
    virtual bool addCallArguments(LLVMContext &Context, Module &M, const DataLayout &TD,
                                  IRBuilder<> &Builder, SmallVectorImpl<Value*> &Args) = 0;

  };

  class SimpleCallConfig: public CallConfig {
    const char *FnName;
    User::const_op_iterator ArgBegin, ArgEnd;
    Type *RetTy;
    bool AllowLower;
  public:
    SimpleCallConfig(const char *FnName, User::const_op_iterator ArgBegin, User::const_op_iterator ArgEnd,
                             Type *RetTy, bool AllowLower)
     : CallConfig(), FnName(FnName), ArgBegin(ArgBegin), ArgEnd(ArgEnd),
       RetTy(RetTy), AllowLower(AllowLower) { }

    SimpleCallConfig(const char *FnName, CallSite &CS, Type* RetTy, bool AllowLower = true)
     : CallConfig(), FnName(FnName), ArgBegin(CS.arg_begin()), ArgEnd(CS.arg_end()),
       RetTy(RetTy), AllowLower(AllowLower) { }

    SimpleCallConfig(const char *FnName, Instruction &I, bool AllowLower = true)
     : CallConfig(), FnName(FnName), ArgBegin(I.op_begin()), ArgEnd(I.op_end()),
       RetTy(I.getType()), AllowLower(AllowLower) { }


    virtual Constant* getPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      std::vector<Type *> ParamTys;
      for (User::const_op_iterator I = ArgBegin; I != ArgEnd; ++I)
        ParamTys.push_back((*I)->getType());
      return M.getOrInsertFunction(FnName, FunctionType::get(RetTy, ParamTys, false));
    }

    virtual bool addCallArguments(LLVMContext &Context, Module &M, const DataLayout &TD,
                                  IRBuilder<> &Builder, SmallVectorImpl<Value*> &Args)
    {
      if (!AllowLower) return false;
      Args.append(ArgBegin, ArgEnd);
      return true;
    }
  };

  class DivModCallConfig: public CallConfig {
    const char *FnName;
    Type *ITy;
  public:
    DivModCallConfig(const char *FnName, Instruction &I)
     : CallConfig(), FnName(FnName), ITy(I.getOperand(0)->getType()) { }

    virtual Constant* getPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      std::vector<Type *> ParamTys;
      ParamTys.push_back(ITy);
      ParamTys.push_back(ITy);
      ParamTys.push_back(ITy->getPointerTo(0));
      return M.getOrInsertFunction(FnName, FunctionType::get(ITy, ParamTys, false));
    }

    virtual bool addCallArguments(LLVMContext &Context, Module &M, const DataLayout &TD,
                                  IRBuilder<> &Builder, SmallVectorImpl<Value*> &Args)
    {
      return false;
    }
  };

  class MemCallConfig: public CallConfig {
    const char *FnName;
    CallInst *CI;
    bool isMemset;
  public:
    MemCallConfig(const char *FnName, CallInst *CI, bool isMemset)
     : CallConfig(), FnName(FnName), CI(CI), isMemset(isMemset) { }

    virtual Constant* getPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      return M.getOrInsertFunction(FnName,
            Type::getInt8PtrTy(Context),
            Type::getInt8PtrTy(Context),
            isMemset ? (Type*) Type::getInt32Ty(Context) : (Type*) Type::getInt8PtrTy(Context),
            TD.getIntPtrType(Context), (Type *)0);
    }

    virtual bool addCallArguments(LLVMContext &Context, Module &M, const DataLayout &TD,
                                  IRBuilder<> &Builder, SmallVectorImpl<Value*> &Args)
    {
      IntegerType *IntPtr = TD.getIntPtrType(Context);
      Value *Size = Builder.CreateIntCast(CI->getArgOperand(2), IntPtr,
                                          /* isSigned */ false);

      Args.push_back( CI->getArgOperand(0) );
      if ( isMemset ) {
        Args.push_back( Builder.CreateIntCast(CI->getArgOperand(1),
                                               Type::getInt32Ty(Context),
                                               /* isSigned */ false) );
      } else {
        Args.push_back( CI->getArgOperand(1) );
      }
      Args.push_back( Size );

      return true;
    }
  };

  class FPCallConfig: public CallConfig {
    const char *FName, *DName, *LName;
    Instruction &I;
    TypeCache &TyCache;
    bool UseRetTy;
    bool AllowLower;
  public:
    FPCallConfig(const char *FName, const char *DName, const char *LName,
                 Instruction &I, TypeCache &TyCache, bool UseRetTy = false, bool AllowLower = true)
     : CallConfig(), FName(FName), DName(DName), LName(LName), I(I), TyCache(TyCache),
       UseRetTy(UseRetTy), AllowLower(AllowLower) { }

    virtual Constant* getPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      std::vector<Type *> ParamTys;
      for (User::const_op_iterator it = I.op_begin(); it != I.op_end(); ++it)
        ParamTys.push_back((*it)->getType());

      const char *FnName;
      switch (getFPType()->getTypeID()) {
      case Type::FloatTyID: FnName = FName; break;
      case Type::DoubleTyID: FnName = DName; break;
      case Type::FP128TyID: FnName = LName; break;
      default:
        llvm_unreachable("Unsupported floating point type");
      }
      return M.getOrInsertFunction(FnName, FunctionType::get(getRetType(), ParamTys, false));
    }

    virtual Constant* getAdditionalPrototype(LLVMContext &Context, Module &M, const DataLayout &TD) {
      // If we have some sort of double instruction, add the float variant too, since the backend
      // sometimes optimizes the types..
      if (getFPType()->isFloatTy()) {
        return 0;
      }

      std::vector<Type *> ParamTys;
      for (User::const_op_iterator it = I.op_begin(); it != I.op_end(); ++it)
        ParamTys.push_back(UseRetTy ? (*it)->getType() : TyCache.FloatTy);

      return M.getOrInsertFunction(FName, FunctionType::get(UseRetTy ? TyCache.FloatTy : getRetType(),
                                                            ParamTys, false));
    }

    virtual bool addCallArguments(LLVMContext &Context, Module &M, const DataLayout &TD,
                                  IRBuilder<> &Builder, SmallVectorImpl<Value*> &Args)
    {
      // for compares, we would need to cast correctly and handle unordered stuff, skip it
      if (!AllowLower || isCompare()) return false;
      Args.append(I.op_begin(), I.op_end());
      return true;
    }

    Type *getFPType() {
      return UseRetTy ? I.getType() : I.getOperand(0)->getType();
    }

    bool isCompare() {
      return I.getType()->isIntegerTy(1);
    }

    Type *getRetType() {
      if (UseRetTy) return I.getType();
      // compare instructions return i32 instead of boolean in runtime-lib
      if (isCompare()) {
        return TyCache.IntTy;
      }
      return I.getType();
    }
  };


}



char AddRuntimeDependencies::ID = 0;
INITIALIZE_PASS(AddRuntimeDependencies, "add-runtime-deps",
                "Add declarations, calls and dependencies for runtime library functions", false, false)

ModulePass *
llvm::createAddRuntimeDependenciesPass() {
  return new AddRuntimeDependencies();
}
ModulePass *
llvm::createAddRuntimeDependenciesPass(bool LowerCalls, std::string FloatABI) {
  return new AddRuntimeDependencies(LowerCalls, FloatABI);
}

//-----------------------------------------------------------------------------

bool AddRuntimeDependencies::runOnModule(Module &M) {
  RIC.setDataLayout(M, &getAnalysis<DataLayout>());

  TyCache.DoubleTy = Type::getDoubleTy(M.getContext());
  TyCache.FloatTy = Type::getFloatTy(M.getContext());
  TyCache.IntTy = Type::getInt32Ty(M.getContext());

  visit(M);

  RIC.updateLLVMUsed();

  NumDeclsAdded = DeclsSet.size();
  return true;
}

void AddRuntimeDependencies::visitCallInst(CallInst &CI) {
  const Function *Callee = CI.getCalledFunction();

  if (Callee && CI.getCalledFunction()->isIntrinsic()) {
    visitIntrinsicCall(&CI);
  }
}

void AddRuntimeDependencies::visitIntrinsicCall(CallInst *CI) {
  IRBuilder<> Builder(CI->getParent(), CI);
  LLVMContext &Context = CI->getContext();

  bool wasHandled = true;
  Constant *F = NULL;
  CallInst *V;
  CallSite CS(CI);

  switch (CI->getCalledFunction()->getIntrinsicID()) {
    // Other intrinsics:
    // - va_start, va_end, va_copy
    // - gcroot, gcread, gcwrite
    // Not needed:
    // - flt_rounds, cttz, ctlz, ctpop: expanded to instructions
    case Intrinsic::memcpy:
    {
      MemCallConfig CC("memcpy", CI, false);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::memmove:
    {
      MemCallConfig CC("memmove", CI, false);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::memset:
    {
      MemCallConfig CC("memset", CI, true);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    // The setjmp/longjmp intrinsics should only exist in the code if it was
    // never optimized (ie, right out of the CFE), or if it has been hacked on
    // by the lowerinvoke pass.  In both cases, the right thing to do is to
    // convert the call to an explicit setjmp or longjmp call.
    case Intrinsic::setjmp:
    {
      SimpleCallConfig CC("setjmp", CS, Type::getInt32Ty(Context));
      F = RIC.processInstruction(CI, CC, &V);

      if (V && !CI->getType()->isVoidTy())
        CI->replaceAllUsesWith(V);
      break;
    }
    case Intrinsic::longjmp:
    {
      SimpleCallConfig CC("longjmp", CS, Type::getVoidTy(Context));
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::sigsetjmp:
      // is removed
      if (!CI->getType()->isVoidTy())
        CI->replaceAllUsesWith(Constant::getNullValue(CI->getType()));
      break;
    case Intrinsic::siglongjmp:
    case Intrinsic::trap:
    {
      // Insert the call to abort
      SimpleCallConfig CC("abort", CS.arg_end(), CS.arg_end(), Type::getVoidTy(Context), false);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }

    case Intrinsic::sqrt:
    {
      FPCallConfig CC("sqrtf", "sqrt", "sqrtl", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::sin:
    {
      FPCallConfig CC("sinf", "sin", "sinl", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::cos:
    {
      FPCallConfig CC("cosf", "cos", "cosl", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::log:
    {
      FPCallConfig CC("logf", "log", "logl", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::log2:
    {
      FPCallConfig CC("log2f", "log2", "log2l", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::log10:
    {
      FPCallConfig CC("log10f", "log10", "log10l", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::exp:
    {
      FPCallConfig CC("expf", "exp", "expl", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::exp2:
    {
      FPCallConfig CC("exp2f", "exp2", "exp2l", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::pow:
    {
      FPCallConfig CC("powf", "pow", "powl", *CI , TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::powi:
    {
      FPCallConfig CC("__powisf2", "__powidf2", "__powixf2", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    case Intrinsic::fma: // fused multiply-add
    {
      FPCallConfig CC("fmaf", "fma", "fmal", *CI, TyCache);
      F = RIC.processInstruction(CI, CC, &V);
      break;
    }
    // TODO: (u|s)(mul|add|sub)_with_overflow ??

    default:
      /* ignore call to intrinsic function */
      wasHandled = false;
      break;
  }

  if (wasHandled) {
    if (F) DeclsSet.insert(F);

    if (CI->use_empty()) {
      CI->eraseFromParent();
      NumIntrinsicsLowered++;
    }
  } else {
    NumIntrinsicsIgnored++;
  }
}

void AddRuntimeDependencies::visitFPTruncInst(FPTruncInst &I) {
  if (!HandleFloats) return;

  if (I.getOperand(0)->getType()->isDoubleTy() && I.getType()->isFloatTy()) {
    SimpleCallConfig CC("__truncdfsf2", I);
    RIC.processInstruction(&I, CC, NULL);
  } else {
    // TODO handle FP80 ?
    llvm_unreachable("Unsupported floating point types for fptrunc, not yet implemented.");
  }
}

void AddRuntimeDependencies::visitFPExtInst(FPExtInst &I) {
  if (!HandleFloats) return;

  if (I.getOperand(0)->getType()->isFloatTy() && I.getType()->isDoubleTy()) {
    SimpleCallConfig CC("__extendsfdf2", I);
    RIC.processInstruction(&I, CC, NULL);
  } else {
    // TODO handle FP80 ?
    llvm_unreachable("Unsupported floating point types for extend, not yet implemented.");
  }
}

void AddRuntimeDependencies::visitFPToUIInst(FPToUIInst &I) {
  if (!HandleFloats) return;

  Type *ITy = I.getType();

  if (ITy->isIntegerTy(64)) {
    FPCallConfig CC("__fixunssfdi", "__fixunsdfdi", "__fixunsxfdi", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
  }
  else if (ITy->isIntegerTy() && ITy->getIntegerBitWidth() <= 32) {
    FPCallConfig CC("__fixunssfsi", "__fixunsdfsi", "__fixunsxfsi", I , TyCache);
    RIC.processInstruction(&I, CC, NULL);
  }
  else llvm_unreachable("Target type size for FPToUI not yet supported.");
}

void AddRuntimeDependencies::visitFPToSIInst(FPToSIInst &I) {
  if (!HandleFloats) return;

  Type *ITy = I.getType();

  if (ITy->isIntegerTy(64)) {
    FPCallConfig CC("__fixsfdi", "__fixdfdi", "__fixxfdi", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
  }
  else if (ITy->isIntegerTy() && ITy->getIntegerBitWidth() <= 32) {
    FPCallConfig CC("__fixsfsi", "__fixdfsi", "__fixxfsi", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
  }
  else llvm_unreachable("Target type size for FPToSI not yet supported.");

}

void AddRuntimeDependencies::visitUIToFPInst(UIToFPInst &I) {
  if (!HandleFloats) return;

  Type *ITy = I.getOperand(0)->getType();

  if (ITy->isIntegerTy(64)) {
    FPCallConfig CC("__floatundisf", "__floatundidf", "__floatundixf", I, TyCache, true);
    RIC.processInstruction(&I, CC, NULL);
  }
  else if (ITy->isIntegerTy() && ITy->getIntegerBitWidth() <= 32) {
    FPCallConfig CC("__floatunsisf", "__floatunsidf", "__floatunsixf", I, TyCache, true);
    RIC.processInstruction(&I, CC, NULL);
  }
  else llvm_unreachable("Source type size for UIToFP not yet supported.");

}

void AddRuntimeDependencies::visitSIToFPInst(SIToFPInst &I) {
  if (!HandleFloats) return;

  Type *ITy = I.getOperand(0)->getType();

  if (ITy->isIntegerTy(64)) {
    FPCallConfig CC("__floatdisf", "__floatdidf", "__floatdixf", I, TyCache, true);
    RIC.processInstruction(&I, CC, NULL);
  }
  else if (ITy->isIntegerTy() && ITy->getIntegerBitWidth() <= 32) {
    FPCallConfig CC("__floatsisf", "__floatsidf", "__floatsixf", I, TyCache, true);
    RIC.processInstruction(&I, CC, NULL);
  }
  else llvm_unreachable("Source type size for SIToFP not yet supported.");
}

void AddRuntimeDependencies::visitFCmpInst(FCmpInst &I) {
  if (!HandleFloats) return;

  // To avoid problems with the backend doing optimizations, we just link in all compare ops
  //switch (I.getPredicate()) {
  //case CmpInst::FCMP_UEQ:
  //case CmpInst::FCMP_OEQ:

  FPCallConfig CC1("__eqsf2", "__eqdf2", "__eqxf2", I, TyCache);
  RIC.processInstruction(&I, CC1, NULL);
  FPCallConfig CC2("__gesf2", "__gedf2", "__gexf2", I, TyCache);
  RIC.processInstruction(&I, CC2, NULL);
  FPCallConfig CC3("__gtsf2", "__gtdf2", "__gtxf2", I, TyCache);
  RIC.processInstruction(&I, CC3, NULL);
  FPCallConfig CC4("__lesf2", "__ledf2", "__lexf2", I, TyCache);
  RIC.processInstruction(&I, CC4, NULL);
  FPCallConfig CC5("__ltsf2", "__ltdf2", "__ltxf2", I, TyCache);
  RIC.processInstruction(&I, CC5, NULL);
  FPCallConfig CC6("__nesf2", "__nedf2", "__nexf2", I, TyCache);
  RIC.processInstruction(&I, CC6, NULL);

  // Just add them in any case
  FPCallConfig CC7("__unordsf2", "__unorddf2", "__unordxf2", I, TyCache, false, false);
  RIC.processInstruction(&I, CC7, NULL);
}

void AddRuntimeDependencies::visitBinaryOperator(BinaryOperator &I) {
  Type *OpTy = I.getOperand(0)->getType();

  // TODO configure which instructions will be lowered to a runtime call.. maybe get infos
  // from backend, including RTLIB names ???

  // TODO this might be too conservative: if one of the arguments is a constant,
  // it might be lowered to a simpler set of instructions (e.g. shladd instead of div)

  // For now we do not lower div and mod, so that the backend can do some optimizations

  switch (I.getOpcode()) {
  case Instruction::UDiv:
  {
    SimpleCallConfig CC(getIntFunctionName(&I, "__udivsi3", "__udivdi3"), I, false);
    RIC.processInstruction(&I, CC, NULL);
    // TODO we do not know if we actually need it, but just in case the selector uses it..
    DivModCallConfig DM(getIntFunctionName(&I, "__udivmodsi4", "__udivmoddi4"), I);
    RIC.processInstruction(&I, DM, NULL);
    return;
  }
  case Instruction::SDiv:
  {
    SimpleCallConfig CC(getIntFunctionName(&I, "__divsi3", "__divdi3"), I, false);
    RIC.processInstruction(&I, CC, NULL);
    // TODO we do not know if we actually need it, but just in case the selector uses it..
    DivModCallConfig DM(getIntFunctionName(&I, "__divmodsi4", "__divmoddi4"), I);
    RIC.processInstruction(&I, DM, NULL);
    return;
  }
  case Instruction::URem:
  {
    SimpleCallConfig CC(getIntFunctionName(&I, "__umodsi3", "__umoddi3"), I, false);
    RIC.processInstruction(&I, CC, NULL);
    // TODO we do not know if we actually need it, but just in case the selector uses it..
    DivModCallConfig DM(getIntFunctionName(&I, "__udivmodsi4", "__udivmoddi4"), I);
    RIC.processInstruction(&I, DM, NULL);
    return;
  }
  case Instruction::SRem:
  {
    SimpleCallConfig CC(getIntFunctionName(&I, "__modsi3", "__moddi3"), I, false);
    RIC.processInstruction(&I, CC, NULL);
    // TODO we do not know if we actually need it, but just in case the selector uses it..
    DivModCallConfig DM(getIntFunctionName(&I, "__divmodsi4", "__divmoddi4"), I);
    RIC.processInstruction(&I, DM, NULL);
    return;
  }
  default: break;
  }

  if (OpTy->isIntegerTy(64)) {
    switch (I.getOpcode()) {
    case Instruction::Shl:
    {
      SimpleCallConfig CC("__ashldi3", I);
      RIC.processInstruction(&I, CC, NULL);
      return;
    }
    case Instruction::LShr:
    {
      SimpleCallConfig CC("__lshrdi3", I);
      RIC.processInstruction(&I, CC, NULL);
      return;
    }
    case Instruction::AShr:
    {
      SimpleCallConfig CC("__ashrdi3", I);
      RIC.processInstruction(&I, CC, NULL);
      return;
    }
    default: return;
    }
  }

  if (!HandleFloats) return;

  switch (I.getOpcode()) {
  case Instruction::FAdd:
  {
    FPCallConfig CC("__addsf3", "__adddf3", "__addxf3", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
    return;
  }
  case Instruction::FSub:
  {
    FPCallConfig CC("__subsf3", "__subdf3", "__subxf3", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
    return;
  }
  case Instruction::FMul:
  {
    FPCallConfig CC("__mulsf3", "__muldf3", "__mulxf3", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
    return;
  }
  case Instruction::FDiv:
  {
    FPCallConfig CC("__divsf3", "__divdf3", "__divxf3", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
    return;
  }
  case Instruction::FRem:
  {
    FPCallConfig CC("fmodf", "fmod", "fmodl", I, TyCache);
    RIC.processInstruction(&I, CC, NULL);
    return;
  }
  default: break;
  }
}

void AddRuntimeDependencies::visitUnaryInstruction(UnaryInstruction &I) {
}


//-----------------------------------------------------------------------------

Constant* RuntimeInstructionVisitor::processInstruction(Instruction *I, CallConfig &Config, CallInst **NewCI) {

  Constant *F = Config.getPrototype(*Context, *M, *TD);
  CallInst *Result;

  bool lowered;

  if (LowerCalls) {
    Result = replaceInstruction(I, Config, F);
    lowered = (Result != NULL);
  } else {
    Result = NULL;
    lowered = false;
  }
  if (NewCI) {
    *NewCI = Result;
  }

  // Update llvm.used here
  if (!lowered && F) {
    Used.insert(F);
  } else if (lowered && F) {
    Lowered.insert(F);
  }

  if (!lowered) {
    // TODO allow for more than one additional prototype!
    F = Config.getAdditionalPrototype(*Context, *M, *TD);
    if (F != 0) {
      Used.insert(F);
    }
  }

  return F;
}

CallInst *RuntimeInstructionVisitor::replaceInstruction(Instruction *I, CallConfig &Config, Constant *F) {
  SmallVector<Value *, 8> Args;
  IRBuilder<> Builder(I->getParent(), I);

  if (!Config.addCallArguments(*Context, *M, *TD, Builder, Args)) {
    return NULL;
  }

  CallInst *NewCI = Builder.CreateCall(F, Args);
  NewCI->setName(I->getName());
  if (!I->use_empty())
    I->replaceAllUsesWith(NewCI);

  return NewCI;
}

void RuntimeInstructionVisitor::updateLLVMUsed() {

  GlobalVariable *LLVMUsed = M->getGlobalVariable("llvm.used");

  if (Used.empty() && (!LLVMUsed || Lowered.empty())) {
    return;
  }

  // We always mark as used what we did not lower in this pass, even if another use was lowered
  SmallVector<Constant*, 8> UsedArray(Used.begin(), Used.end());

  // Read out llvm.used, merge with Used
  ConstantArray *Init = 0;
  SmallPtrSet<Constant*,8> OldUsed;
  if (LLVMUsed) {
    Init = dyn_cast<ConstantArray>(LLVMUsed->getInitializer());
    if (Init) {
      for (unsigned i = 0, e = Init->getNumOperands(); i != e; ++i) {
        Constant *GV = dyn_cast<Constant>(Init->getOperand(i)->stripPointerCasts());
        if (GV == 0 || Used.count(GV) != 0) continue;

        // TODO skip all values that have been lowered now. This may be unsafe, so make it optional.

        // Do not add duplicate entries (happens due to appending linkage, llvm-link does not remove dups)
        if (OldUsed.insert(GV)) {
          UsedArray.push_back(GV);
        }
      }
    }
  }

  PointerType *Int8PtrTy = Type::getInt8PtrTy(*Context, 0);
  ArrayType *ATy = llvm::ArrayType::get(Int8PtrTy, UsedArray.size());

  for (unsigned i = 0, e = UsedArray.size(); i != e; ++i) {
    UsedArray[i] = ConstantExpr::getBitCast(UsedArray[i], Int8PtrTy);
  }

  Constant* NewInit = ConstantArray::get(ATy, UsedArray);

  // Cannot change the type of the variable, so create a new one, replace and erase old one
  GlobalVariable *NewUsed = new GlobalVariable(*M, ATy, false, GlobalValue::AppendingLinkage,
                                NewInit, "llvm.used");
  NewUsed->setSection("llvm.metadata");

  if (LLVMUsed) {
    NewUsed->takeName(LLVMUsed);

    if (Init) {
      // TODO No idea how to remove the initializer properly ..
      /*
      Init->replaceAllUsesWith(ConstantExpr::getBitCast(NewInit, Init->getType()));
      assert(Init->use_empty());
      delete Init;
      */
    }
    LLVMUsed->replaceAllUsesWith(ConstantExpr::getBitCast(NewUsed, LLVMUsed->getType()));

    LLVMUsed->eraseFromParent();
  }


}

