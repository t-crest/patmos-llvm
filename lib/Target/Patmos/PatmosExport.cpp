//===-- PatmosFunctionSplitter.cpp - Split functions to fit into the cache ===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains a Patmos-customized PML export driver and -pass.
// 
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-export"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/PMLExport.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <sstream>
#include <iostream>

using namespace llvm;

// TODO should we make this a generic option of PMLExport?
static cl::opt<bool> SkipSerializeInstructions (
  "mpatmos-serialize-skip-instructions",
  cl::init(false),
  cl::desc("Only export interesting instructions, such as branches."),
  cl::Hidden);

namespace llvm {

  class PatmosPMLInstrInfo : public PMLInstrInfo {
    virtual std::vector<StringRef> getCallee(MachineFunction &Caller,
                                             const MachineInstr *Instr)
    {
      std::vector<StringRef> Callees;

      assert(Instr->isCall());

      // read call (patmos specific: operand[2] of call)
      const MachineOperand &MO(Instr->getOperand(2));

      if (MO.isGlobal()) {
        // is the global value a function?
        Callees.push_back(MO.getGlobal()->getName());
      }
      else if (MO.isSymbol()) {
        // find the function in the current module
        Callees.push_back(MO.getSymbolName());
      }

      return Callees;
    }

    virtual const std::vector<MachineBasicBlock*> getJumpTargets(
                                    MachineFunction &MF,
                                    const MachineInstr *Instr)
    {
      // read jump table (patmos specific: operand[3] of BR(CF)?Tu?)
      switch (Instr->getOpcode()) {
      case Patmos::BRT:
      case Patmos::BRTu:
      case Patmos::BRCFT:
      case Patmos::BRCFTu:
        assert(Instr->getNumOperands() == 4);

        unsigned index = Instr->getOperand(3).getIndex();
        MachineJumpTableInfo *MJTI = MF.getJumpTableInfo();

        typedef const std::vector<MachineBasicBlock*> JTEntries;
        JTEntries &JTBBs(MJTI->getJumpTables()[index].MBBs);

        return JTBBs;
      }

      std::vector<MachineBasicBlock*> targets;
      return targets;
    }
  };

  class PatmosFunctionExport : public PMLFunctionExport {
  public:
    PatmosFunctionExport(PatmosTargetMachine &tm) : PMLFunctionExport(tm) {}

    virtual bool doExportInstruction(const Instruction* Instr) {
      if (SkipSerializeInstructions) {
        if (!dyn_cast<const CallInst>(Instr)) return false;
      }
      return true;
    }

  };

  class PatmosMachineFunctionExport : public PMLMachineFunctionExport {
  public:
    PatmosMachineFunctionExport(PatmosTargetMachine &tm)
      : PMLMachineFunctionExport(tm, new PatmosPMLInstrInfo()) {}

    virtual bool doExportInstruction(const MachineInstr *Ins) {
      if (SkipSerializeInstructions) {
        if (!Ins->getDesc().isCall() && !Ins->getDesc().isBranch())
          return false;
      }
      return true;
    }
  };


  /// createPatmosExportPass - Returns a new PatmosExportPass
  /// \see PatmosExportPass
  FunctionPass *createPatmosExportPass(std::string& filename,
                                       PatmosTargetMachine &tm)
  {
    PMLExportPass *PEP = new PMLExportPass(filename, &tm, false);

    // Add our own export passes
    PEP->addExporter( new PatmosMachineFunctionExport(tm) );
    PEP->addExporter( new PMLBitcodeExportAdapter(
                          new PatmosFunctionExport(tm) ));
    PEP->addExporter( new PMLRelationGraphExport(tm) );

    return PEP;
  }

} // end namespace llvm

