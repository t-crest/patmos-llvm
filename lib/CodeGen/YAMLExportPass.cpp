//===-- YAMLExportPass.cpp -----------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// YAMLExportPass implementation.
//
//===----------------------------------------------------------------------===//

#include "llvm/Function.h"
#include "llvm/Instructions.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLExport.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"

using namespace llvm;

namespace {

/// YAMLExportPass - This is a pass to export a machine function to
/// YAML (using the PML schema define at (TODO: cite report))
struct YAMLExportPass : public MachineFunctionPass {

  static char ID;

  TargetMachine *TM;
  std::string OutFileName;
  tool_output_file *OutFile;
  yaml::Output *Output;

  // loop info for machine code
  MachineLoopInfo *LI;

  YAMLExportPass(std::string& filename, TargetMachine *tm)
      : MachineFunctionPass(ID), TM(tm), OutFileName(filename) {}

  const char *getPassName() const { return "YAML/PML Export"; }

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.setPreservesAll();
    AU.addRequired<MachineLoopInfo>();
    MachineFunctionPass::getAnalysisUsage(AU);
  }
  virtual bool doInitialization(Module &M) {

      std::string ErrorInfo;
      OutFile = new tool_output_file(OutFileName.c_str(), ErrorInfo, 0);
      if(! ErrorInfo.empty()) {
         delete OutFile;
         errs() << "Opening Export File failed: " << OutFileName << "\n";
         errs() << "Reason: " << ErrorInfo;
         OutFile = 0;
      } else {
         Output = new yaml::Output(OutFile->os());
      }
      return false;
 }

 virtual bool doFinalization(Module &M) {
     if(OutFile) {
         OutFile->keep();
         delete Output;
         delete OutFile;
     }
     return false;
  }
  typedef yaml::Doc<yaml::GenericArchitecture> YAMLDoc;
  bool runOnMachineFunction(MachineFunction &MF) {
     // In this first implementation, we provide generic
     // MachineFunction and Function serialization
     LI = &getAnalysis<MachineLoopInfo>();
     YAMLDoc YDoc("pml-0.1");
     serializeMachineFunction(MF, YDoc);
     serializeFunction(MF.getFunction(), YDoc);
     *Output << YDoc;
     return false;
  }
    void serializeMachineFunction(MachineFunction &MF, YAMLDoc& doc) {
        yaml::GenericArchitecture::MachineFunction *F = new yaml::GenericArchitecture::MachineFunction(MF.getFunctionNumber());
        F->MapsTo = yaml::Name(MF.getFunction()->getName());
        F->Level = yaml::level_machinecode;
        yaml::GenericArchitecture::MachineBlock *B;
        for (MachineFunction::iterator BB = MF.begin(), E = MF.end(); BB != E; ++BB) {
            B = F->addBlock(new yaml::GenericArchitecture::MachineBlock(BB->getNumber()));
            for (MachineBasicBlock::const_pred_iterator BBPred = BB->pred_begin(), E = BB->pred_end(); BBPred != E; ++BBPred)
                B->Predecessors.push_back(yaml::Name((*BBPred)->getNumber()));
            for (MachineBasicBlock::const_succ_iterator BBSucc = BB->succ_begin(), E = BB->succ_end(); BBSucc != E; ++BBSucc)
                B->Successors.push_back(yaml::Name((*BBSucc)->getNumber()));
            B->MapsTo = yaml::Name(BB->getName());
            // export loop information
            MachineLoop *Loop = LI->getLoopFor(BB);
            while(Loop) {
                B->Loops.push_back(yaml::Name(Loop->getHeader()->getNumber()));
                Loop = Loop->getParentLoop();
            }
            // export instruction and branch Information
            MachineBasicBlock *TrueSucc = 0, *FalseSucc = 0;
            SmallVector<MachineOperand,4> Conditions;
            const TargetInstrInfo *TII = TM->getInstrInfo();
            bool HasBranchInfo = ! TII->AnalyzeBranch(*BB, TrueSucc, FalseSucc, Conditions, false);
            unsigned Index = 0;
            for (MachineBasicBlock::iterator Ins = BB->begin(), E = BB->end(); Ins != E; ++Ins) {
                yaml::GenericMachineInstruction *I =
                  B->addInstruction(new yaml::GenericMachineInstruction(Index++));
              I->Size = Ins->getDesc().getSize();
              I->Opcode = Ins->getOpcode();
              // FIXME: this is still the hackish implementation from the OTAP prototype
              if(Ins->getDesc().isCall()) {
                  std::string CalleeDesc;
                  for(MachineInstr::const_mop_iterator Op = Ins->operands_begin(), E = Ins->operands_end(); Op != E; ++Op) {
                      if(Op->isGlobal()) {
                          I->addCallee(Op->getGlobal()->getName());
                      }
                  }
              }
              if(Ins->getDesc().isBranch()) {
                  if(Ins->getDesc().isConditionalBranch()) {
                      I->BranchType = yaml::branch_conditional;
                      if(HasBranchInfo && TrueSucc) I->BranchTarget = TrueSucc->getNumber();
                  } else if(Ins->getDesc().isUnconditionalBranch()) {
                      I->BranchType = yaml::branch_unconditional;
                      MachineBasicBlock *USucc = Conditions.empty() ? TrueSucc : FalseSucc;
                      if(HasBranchInfo && USucc) I->BranchTarget = USucc->getNumber();
                  } else {
                      I->BranchType = yaml::branch_any;
                  }
              } else {
                  I->BranchType = yaml::branch_none;
              }
          }
      }
      // TODO: we do not compute a hash yet
      F->Hash = StringRef("0");
      doc.addMachineFunction(F);
  }

  void serializeFunction(const Function *const_function, YAMLDoc &doc) {
    Function *function = const_cast<Function*>(const_function);
    yaml::BitcodeFunction *F = new yaml::BitcodeFunction(function->getName());

    F->Level = yaml::level_bitcode;
    yaml::BitcodeBlock *B;
    DenseMap<BasicBlock*, unsigned> BlockIds;
    unsigned BlockIndex = 0;
    for(Function::iterator BI = function->begin(), BE= function->end(); BI != BE; ++BI, ++BlockIndex) {
        BlockIds.insert(std::make_pair(&*BI, BlockIndex));
    }
    BlockIndex = 0;
    for(Function::iterator BI = function->begin(), BE= function->end(); BI != BE; ++BI, ++BlockIndex) {
        B = F->addBlock(new yaml::BitcodeBlock(BlockIndex));
        B->MapsTo = yaml::Name(BI->getName());
        for (pred_iterator PI = pred_begin(&*BI), PE = pred_end(&*BI); PI != PE; ++PI) {
            B->Predecessors.push_back(yaml::Name(BlockIds[*PI]));
        }
        for (succ_iterator SI = succ_begin(&*BI), SE = succ_end(&*BI); SI != SE; ++SI) {
            B->Successors.push_back(yaml::Name(BlockIds[*SI]));
        }
        unsigned Index = 0;
        for (BasicBlock::iterator II = BI->begin(), IE = BI->end(); II != IE; ++II) {
            yaml::Instruction *I = B->addInstruction(new yaml::Instruction(Index++));
            I->Opcode = II->getOpcode();
            if(CallInst *CI = dyn_cast<CallInst>(II)) {
                if(Function *F = CI->getCalledFunction()) {
                    I->addCallee(F->getName());
                } else {
                    // TODO: we still have no information about indirect calls
                    I->addCallee(StringRef("*"));
                }
            }
        }
    }
    // TODO: we do not compute a hash yet
    F->Hash = StringRef("0");
    doc.addFunction(F);
  }
};

char YAMLExportPass::ID = 0;
} // end namespace <anonymous>

namespace llvm {

/// Returns a newly-created YAML export pass.
MachineFunctionPass *createYAMLExportPass(std::string& FileName, TargetMachine *TM) {
    return new YAMLExportPass(FileName,TM);
}

} // end namespace llvm
