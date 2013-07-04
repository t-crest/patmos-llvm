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
#include "PatmosCallGraphBuilder.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/PMLExport.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManagers.h"

#include <map>
#include <sstream>
#include <iostream>

using namespace llvm;

static cl::opt<bool> SkipSerializeInstructions (
  "mpatmos-serialize-skip-instructions",
  cl::init(false),
  cl::value_desc("names"),
  cl::desc("Only export interesting instructions, such as branches."),
  cl::Hidden, cl::CommaSeparated);


namespace llvm {

  class PatmosPMLInstrInfo : public PMLInstrInfo {

    PatmosTargetMachine &TM;
    MCallGraph *MCG;

  public:
    PatmosPMLInstrInfo(PatmosTargetMachine &tm) : PMLInstrInfo(), TM(tm), MCG(0) {}

    void setCallGraph(MCallGraph *mcg) { MCG = mcg; }

    virtual std::vector<StringRef> getCalleeNames(MachineFunction &Caller,
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
      else if (MCG) {
        // TODO should we use the callgraph in any case if we have one?
        // (but for immediate calls, this is not much of an advantage..)

        MCGNode *node = MCG->makeMCGNode(&Caller);
        MCGSite *site = node->findSite(Instr);

        if (site && site->getCallee()) {
          MCGNode *Callee = site->getCallee();
          // TODO for unknown nodes, try to resolve using type??
          if (Callee->getMF()) {
            MachineFunction *MF = Callee->getMF();
            if (MF && MF->getFunction()) {
              Callees.push_back(MF->getFunction()->getName());
            }
          }
        }
      }

      return Callees;
    }

    virtual MFList getCallees(const Module &M, MachineModuleInfo &MMI,
                              MachineFunction &MF, const MachineInstr *Instr)
    {
      if (MCG) {
        MCGNode *node = MCG->makeMCGNode(&MF);
        MCGSite *site = node->findSite(Instr);
        MFList Callees;
        if (site && site->getCallee() && site->getCallee()->getMF()) {
          Callees.push_back( site->getCallee()->getMF() );
        }
        return Callees;
      }
      return PMLInstrInfo::getCallees(M, MMI, MF, Instr);
    }

    virtual const std::vector<MachineBasicBlock*> getBranchTargets(
                                    MachineFunction &MF,
                                    const MachineInstr *Instr)
    {
      std::vector<MachineBasicBlock*> targets;

      switch (Instr->getOpcode()) {
      case Patmos::BR:
      case Patmos::BRu:
      case Patmos::BRCF:
      case Patmos::BRCFu: {
        // handle normal branches, return single branch target
        const MachineOperand &MO(Instr->getOperand(2));

        if (MO.isMBB()) {
          targets.push_back( MO.getMBB() );
        }

        break;
        }
      case Patmos::BRT:
      case Patmos::BRTu:
      case Patmos::BRCFT:
      case Patmos::BRCFTu: {
        // read jump table (patmos specific: operand[3] of BR(CF)?Tu?)
        assert(Instr->getNumOperands() == 4);

        unsigned index = Instr->getOperand(3).getIndex();
        MachineJumpTableInfo *MJTI = MF.getJumpTableInfo();

        typedef const std::vector<MachineBasicBlock*> JTEntries;
        JTEntries &JTBBs(MJTI->getJumpTables()[index].MBBs);

        return JTBBs;
        }
      }

      return targets;
    }

    virtual MFList getCalledFunctions(const Module &M,
                                      MachineModuleInfo &MMI,
                                      MachineFunction &MF)
    {
      if (MCG) {
        // take the shortcut using the callgraph
        MCGNode *node = MCG->makeMCGNode(&MF);
        if (node) {
          MFList Callees;
          const MCGSites &Sites = node->getSites();
          for (MCGSites::const_iterator it = Sites.begin(), ie = Sites.end();
               it != ie; ++it)
          {
            MCGNode *Callee = (*it)->getCallee();
            if (Callee && Callee->getMF()) {
              Callees.push_back(Callee->getMF());
            }
          }
          return Callees;
        }
      }
      return PMLInstrInfo::getCalledFunctions(M, MMI, MF);
    }

    virtual int getSize(const MachineInstr *Instr)
    {
      return TM.getInstrInfo()->getInstrSize(Instr);
    }

  };

  class PatmosBitcodeExport : public PMLBitcodeExport {
  public:
    PatmosBitcodeExport(PatmosTargetMachine &tm, ModulePass &mp)
      : PMLBitcodeExport(tm, mp) {}

    virtual bool doExportInstruction(const Instruction* Instr) {
      if (SkipSerializeInstructions) {
        if (!dyn_cast<const CallInst>(Instr)) return false;
      }
      return true;
    }

  };

  class PatmosMachineExport : public PMLMachineExport {
  public:
    PatmosMachineExport(PatmosTargetMachine &tm, ModulePass &mp,
                        PMLInstrInfo *PII)
      : PMLMachineExport(tm, mp, PII) {}

    virtual bool doExportInstruction(const MachineInstr *Ins) {
      if (SkipSerializeInstructions) {
        if (!Ins->getDesc().isCall() && !Ins->getDesc().isBranch() && !Ins->getDesc().isReturn())
          return false;
      }
      return true;
    }
  };


  class PatmosModuleExportPass : public PMLModuleExportPass {
    static char ID;

    PatmosPMLInstrInfo PII;
  public:
    PatmosModuleExportPass(PatmosTargetMachine &tm, StringRef filename,
                           ArrayRef<std::string> roots)
      : PMLModuleExportPass(ID, tm, filename, roots), PII(tm)
    {
      initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    PMLInstrInfo *getInstrInfo() { return &PII; }

    virtual const char *getPassName() const {
      return "Patmos YAML/PML Module Export";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      AU.addRequired<PatmosCallGraphBuilder>();
      PMLModuleExportPass::getAnalysisUsage(AU);
    }

    virtual bool runOnMachineModule(const Module &M) {
      PatmosCallGraphBuilder &PCGB( getAnalysis<PatmosCallGraphBuilder>() );
      PII.setCallGraph( PCGB.getCallGraph() );
      return PMLModuleExportPass::runOnMachineModule(M);
    }
  };

  char PatmosModuleExportPass::ID = 0;


  /// createPatmosExportPass - Returns a new PatmosExportPass
  /// \see PatmosExportPass
  ModulePass *createPatmosModuleExportPass(PatmosTargetMachine &TM,
                                           std::string& Filename,
                                           std::string& BitcodeFilename,
                                           ArrayRef<std::string> Roots)
  {
    PatmosModuleExportPass *PEP =
                      new PatmosModuleExportPass(TM, Filename, Roots);

    // Add our own export passes
    PEP->addExporter( new PatmosMachineExport(TM, *PEP, PEP->getInstrInfo()));
    PEP->addExporter( new PatmosBitcodeExport(TM, *PEP) );
    PEP->addExporter( new PMLRelationGraphExport(TM, *PEP) );
    if (! BitcodeFilename.empty())
      PEP->writeBitcode(BitcodeFilename);
    return PEP;
  }

} // end namespace llvm

