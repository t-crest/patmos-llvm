//===-- PatmosSPMark.cpp - Remove unused function declarations ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass marks functions for single-path conversion on machine code level.
// Functions were cloned on bitcode level (PatmosSPClone), including functions
// which could be potentially called.  These functions are library
// functions (soft floats, lowered intrinsics, etc), and calls to these were
// not visible on bitcode level. Now the respective calls are present,
// and need to be rewritten to the cloned "..._sp_" variants, which were marked
// with attribute 'sp-maybe'.
// The final decision (single-path or not) is set in PatmosMachineFunctionInfo
// (setSinglePath()). Any functions marked as 'sp-maybe' but not finally
// in the PatmosMachineFunctionInfo are "removed" again.
//
// The removal is done by erasing all basic blocks and inserting a single
// basic block with a single return instruction, the least required to
// make the compiler happy.
//
// Ideally, they should vanish completely from the final executable, but it
// seems that this cannot easily be done.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSinglePathInfo.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
//#include "llvm/IR/Attributes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <deque>

using namespace llvm;

STATISTIC(NumSPTotal,   "Total number of functions marked as single-path");
STATISTIC(NumSPMaybe,   "Number of 'used' functions marked as single-path");
STATISTIC(NumSPCleared, "Number of functions cleared again");


namespace {

class PatmosSPMark : public MachineModulePass {
private:
  typedef std::deque<MachineFunction*> Worklist;

  PatmosTargetMachine &TM;
  MachineModuleInfo *MMI; // contains map Function -> MachineFunction

  /**
   * Get the bitcode function called by a given MachineInstr.
   */
  const Function *getCallTarget(const MachineInstr *MI) const;

  /**
   * Get the machine function called by a given MachineInstr.
   */
  MachineFunction *getCallTargetMF(const MachineInstr *MI) const;

  /**
   * Get the machine function called by a given MachineInstr, but if
   * no function can be found, abort with a relevant message.
   */
  MachineFunction *
  getCallTargetMFOrAbort(MachineBasicBlock::iterator MI, MachineFunction::iterator MBB);

  /**
   * Rewrite the call target of the given MachineInstr to the
   * _sp variant of the callee.
   * @pre MI is a call instruction and not already the _sp variant
   */
  void rewriteCall(MachineInstr *MI);

  /**
   * Go through all instructions of the given machine function and find
   * calls that do not call a single-path function.
   * These calls are rewritten with rewriteCall().
   * Any newly reachable 'sp-maybe' functions are put on the worklist W.
   */
  void scanAndRewriteCalls(MachineFunction *MF, Worklist &W);

  /**
   * Remove all cloned 'sp-maybe' machine functions that are not marked as
   * single-path in PatmosMachineFunctionInfo.
   */
  void removeUncalledSPFunctions(const Module &M);

  /**
   * Called on all SP functions. Adds an additional operand to all instructions
   * (also terminals?) that is used in subsequent passes.
   * The operand is used to track which branch in a scope the instruction is part of
   * after two basic blocks have been merged to make use of bundling.
   * This operand is always the last operand of the instruction.
   * Also, for consistency, the operand value is always 123400  plus
   * the operand number.
   * This pass will assign the value of the operands to 123399. This
   * means the operands have not been assigned yet, and must assigned by
   * the PatmosSinglePathInfo to a valid operand number, such that
   * its >= 123400.
   *
   * For normal instructions, the operand will be the last operand and can be
   * retrieved using: `MI->getOperand(MI->getNumOperands()-1)`.
   * For terminal instructions, the operand will be the first implicit operand
   * and can be retrieved using `MI->getOperand(MI->getNumExplicitOperands())`
   * The reasoning for this discrepancy between terminal and non-terminals
   * is LLVM specific but otherwise unknown.
   */
  void addSPPredicateToInstruction(MachineFunction &MF);

public:
  static char ID; // Pass identification, replacement for typeid

  PatmosSPMark(PatmosTargetMachine &tm)
    : MachineModulePass(ID), TM(tm) { (void) TM; }

  /// getPassName - Return the pass' name.
  virtual const char *getPassName() const {
    return "Patmos Single-Path Mark (machine code)";
  }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<MachineModuleInfo>();
    AU.setPreservesAll();
    MachineModulePass::getAnalysisUsage(AU);
  }

  virtual bool doInitialization(Module &M) {
    return false;
  }

  virtual bool doFinalization(Module &M) {
    return false;
  }

  virtual bool runOnMachineModule(const Module &M);
};

} // end anonymous namespace

char PatmosSPMark::ID = 0;

ModulePass *llvm::createPatmosSPMarkPass(PatmosTargetMachine &tm) {
  return new PatmosSPMark(tm);
}

///////////////////////////////////////////////////////////////////////////////


bool PatmosSPMark::runOnMachineModule(const Module &M) {
  DEBUG( dbgs() <<
         "[Single-Path] Mark functions reachable from single-path roots\n");

  MMI = &getAnalysis<MachineModuleInfo>();
  assert(MMI);

  Worklist W;
  // initialize the worklist with machine functions that have either
  // sp-root or sp-reachable function attribute
  for(Module::const_iterator F(M.begin()), FE(M.end()); F != FE; ++F) {
    if (F->hasFnAttribute("sp-root") || F->hasFnAttribute("sp-reachable")) {
      // get the machine-level function
      MachineFunction *MF = MMI->getMachineFunction(F);
      assert( MF );
      PatmosMachineFunctionInfo *PMFI =
        MF->getInfo<PatmosMachineFunctionInfo>();
      PMFI->setSinglePath();
      NumSPTotal++; // bump STATISTIC
      W.push_back(MF);
    }
  }

  // process worklist
  while (!W.empty()) {
    MachineFunction *MF = W.front();
    W.pop_front();
    scanAndRewriteCalls(MF, W);
    addSPPredicateToInstruction(*MF);
  }

  removeUncalledSPFunctions(M);

  // We either have rewritten calls or removed superfluous functions.
  return true;
}


const Function *PatmosSPMark::getCallTarget(const MachineInstr *MI) const {
  const MachineOperand &MO = MI->getOperand(2);
  const Function *Target = NULL;
  if (MO.isGlobal()) {
    Target = dyn_cast<Function>(MO.getGlobal());
  } else if (MO.isSymbol()) {
    const char *TargetName = MO.getSymbolName();
    const Module *M = MI->getParent()->getParent()->getFunction()->getParent();
    Target = M->getFunction(TargetName);
  }
  return Target;
}


MachineFunction *PatmosSPMark::getCallTargetMF(const MachineInstr *MI) const {
  const Function *F = getCallTarget(MI);
  MachineFunction *MF;
  if (F && (MF = MMI->getMachineFunction(F))) {
    return MF;
  }
  return NULL;
}

MachineFunction *
PatmosSPMark::getCallTargetMFOrAbort(MachineBasicBlock::iterator MI, MachineFunction::iterator MBB){
  MachineFunction *MF =  getCallTargetMF(MI);
  if (!MF) {
    errs() << "[Single-path] Cannot find ";
    bool foundSymbol = false;
    for(
      MachineInstr::mop_iterator op = MI->operands_begin();
      op != MI->operands_end();
      ++op
    ){
      if(op->isSymbol()){
        errs() << "function '" << op->getSymbolName() << "'";
        foundSymbol = true;
        break;
      }
    }
    if(!foundSymbol){
        errs() << "unknown function";
    }
    errs() << " to rewrite. Was called by '"
           << MBB->getParent()->getFunction()->getName()
           << "' (indirect call?)\n";
    abort();
  }
  return MF;
}

void PatmosSPMark::scanAndRewriteCalls(MachineFunction *MF, Worklist &W) {
  DEBUG(dbgs() << "In function '" << MF->getName() << "':\n");
  for (MachineFunction::iterator MBB = MF->begin(), MBBE = MF->end();
                                 MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                     ME = MBB->getFirstTerminator();
                                     MI != ME; ++MI) {
      if (MI->isCall()) {
        MachineFunction *MF = getCallTargetMFOrAbort(MI,MBB);

        const Function *Target = getCallTarget(MI);

        PatmosMachineFunctionInfo *PMFI =
          MF->getInfo<PatmosMachineFunctionInfo>();
        if (!PMFI->isSinglePath()) {
          if (!Target->hasFnAttribute("sp-reachable") &&
              !Target->hasFnAttribute("sp-maybe")) {
            // The call target is not a .._sp_ clone
            rewriteCall(MI);
          }
          // set _sp MF to single path in PMFI (MF might have changed)
          MachineFunction *MF = getCallTargetMF(MI);
          PatmosMachineFunctionInfo *PMFI =
            MF->getInfo<PatmosMachineFunctionInfo>();
          // we possibly have already marked the _sp variant as single-path
          // in an earlier call, if not, then set this final decision.
          if (!PMFI->isSinglePath()) {
            PMFI->setSinglePath();
            // add the new single-path function to the worklist
            W.push_back(MF);

            NumSPTotal++; // bump STATISTIC
            NumSPMaybe++; // bump STATISTIC
          }
        }
      }
    }
  }
}


void PatmosSPMark::rewriteCall(MachineInstr *MI) {
  // get current MBB and call target
  MachineBasicBlock *MBB = MI->getParent();
  const Function *Target = getCallTarget(MI);
  // get the same function with _sp_ suffix
  SmallVector<char, 64> buf;
  const StringRef SPFuncName = Twine(
      Twine(Target->getName()) + Twine("_sp_")
      ).toNullTerminatedStringRef(buf);

  const Function *SPTarget = Target->getParent()->getFunction(SPFuncName);
  if (!SPTarget) {
    errs() <<  "[Single-path] function '" << SPFuncName << "' missing!\n";
    abort();
  }

  // Remove the call target operand and add a new target operand
  // with an MachineInstrBuilder. In this case, it is inserted at
  // the right place, before the implicit defs of the call.
  MI->RemoveOperand(2);
  MachineInstrBuilder MIB(*MBB->getParent(), MI);
  MIB.addGlobalAddress(SPTarget);
  DEBUG( dbgs() << "  Rewrite call: " << Target->getName()
                << " -> " << SPFuncName << "\n" );
}


void PatmosSPMark::removeUncalledSPFunctions(const Module &M) {
  for(Module::const_iterator F(M.begin()), FE(M.end()); F != FE; ++F) {
    if (F->hasFnAttribute("sp-maybe")) {
      // get the machine-level function
      MachineFunction *MF = MMI->getMachineFunction(F);
      assert( MF );
      PatmosMachineFunctionInfo *PMFI =
        MF->getInfo<PatmosMachineFunctionInfo>();

      if (!PMFI->isSinglePath()) {
        DEBUG(dbgs() << "  Remove function: " << F->getName() << "\n");
        // delete all MBBs
        while (MF->begin() != MF->end()) {
          MF->begin()->eraseFromParent();
        }
        // insert a new single MBB with a single return instruction
        MachineBasicBlock *EmptyMBB = MF->CreateMachineBasicBlock();
        MF->push_back(EmptyMBB);

        DebugLoc DL;
        AddDefaultPred(BuildMI(*EmptyMBB, EmptyMBB->end(), DL,
            TM.getInstrInfo()->get(Patmos::RET)));
        NumSPCleared++; // bump STATISTIC
      };
    }
  }
}

void printFunction(MachineFunction &MF) {
  outs() << "Bundle function '" << MF.getFunction()->getName() << "'\n";
  outs() << "Block list:\n";
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                   MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->getFirstTerminator();
                                         MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
    }
    outs() << "Terminators:\n";
    for( MachineBasicBlock::iterator MI = MBB->getFirstTerminator(),
                                             ME = MBB->end();
                                             MI != ME; ++MI) {
      outs() << "\t";
      MI->print(outs(), &(MF.getTarget()), false);
    }
    outs() << "\n";
  }

  outs() <<"\n";

}

void PatmosSPMark::addSPPredicateToInstruction(MachineFunction &MF) {
  for (MachineFunction::iterator MBB = MF.begin(), MBBE = MF.end();
                                     MBB != MBBE; ++MBB) {
    for( MachineBasicBlock::iterator MI = MBB->begin(),
                                         ME = MBB->end();
                                         MI != ME; ++MI) {
      LLVMContext& C = MF.getFunction()->getContext();
      MDNode* N = MDNode::get(C, MDString::get(C, "SPPred:-1"));
      MachineOperand op = MachineOperand::CreateMetadata(N);
      MachineOperand *newOp = new MachineOperand(op);
      MI->addOperand(*newOp);
    }
  }
}
