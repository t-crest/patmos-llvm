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

static cl::opt<bool> SkipSerializeInstructions (
  "mpatmos-serialize-skip-instructions",
  cl::init(false),
  cl::value_desc("names"),
  cl::desc("Only export interesting instructions, such as branches."),
  cl::Hidden, cl::CommaSeparated);


namespace llvm {

  class PatmosPMLInstrInfo : public PMLInstrInfo {

    MCallGraph *MCG;

  public:
    PatmosPMLInstrInfo() : PMLInstrInfo(), MCG(0) {}

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

    virtual MFList getCallees(Module &M, MachineModuleInfo &MMI,
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

    virtual MFList getCalledFunctions(Module &M,
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


  class PatmosExportPass : public PMLExportPass {
    static char ID;

    PatmosPMLInstrInfo PII;
  public:
    PatmosExportPass(PatmosTargetMachine &tm, StringRef filename)
     : PMLExportPass(ID, tm, filename)
    {
      //initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Patmos YAML/PML Module Export";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      //AU.addRequired<PatmosCallGraphBuilder>();
      PMLExportPass::getAnalysisUsage(AU);
    }

    virtual bool doInitialization(Module &M) {
      //PatmosCallGraphBuilder *PCG = &getAnalysis<PatmosCallGraphBuilder>();
      //PII.setCallGraph( PCG->getCallGraph() );
      return PMLExportPass::doInitialization(M);
    }
  };

  char PatmosExportPass::ID = 0;


  class PatmosModuleExportPass : public PMLModuleExportPass {
    static char ID;

    PatmosPMLInstrInfo PII;
  public:
    PatmosModuleExportPass(PatmosTargetMachine &tm, StringRef filename,
                           ArrayRef<std::string> roots)
     : PMLModuleExportPass(ID, tm, filename, roots)
    {
      //initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Patmos YAML/PML Module Export";
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesAll();
      //AU.addRequired<PatmosCallGraphBuilder>();
      PMLModuleExportPass::getAnalysisUsage(AU);
    }

    virtual bool runOnModule(Module &M) {
      //PatmosCallGraphBuilder *PCG = &getAnalysis<PatmosCallGraphBuilder>();
      //PII.setCallGraph( PCG->getCallGraph() );
      return PMLModuleExportPass::runOnModule(M);
    }
  };

  char PatmosModuleExportPass::ID = 0;




  /// createPatmosExportPass - Returns a new PatmosExportPass
  /// \see PatmosExportPass
  FunctionPass *createPatmosExportPass(PatmosTargetMachine &tm,
                                       std::string& filename)
  {
    PMLExportPass *PEP = new PatmosExportPass(tm, filename);

    // Add our own export passes
    PEP->addExporter( new PatmosMachineFunctionExport(tm) );
    PEP->addExporter( new PatmosFunctionExport(tm) );
    PEP->addExporter( new PMLRelationGraphExport(tm) );

    return PEP;
  }

  /// createPatmosExportPass - Returns a new PatmosExportPass
  /// \see PatmosExportPass
  ModulePass *createPatmosModuleExportPass(PatmosTargetMachine &tm,
                                           std::string& filename,
                                           ArrayRef<std::string> roots)
  {
    PatmosModuleExportPass *PEP =
                      new PatmosModuleExportPass(tm, filename, roots);

    // Add our own export passes
    PEP->addExporter( new PatmosMachineFunctionExport(tm) );
    PEP->addExporter( new PatmosFunctionExport(tm) );
    PEP->addExporter( new PMLRelationGraphExport(tm) );

    return PEP;
  }

} // end namespace llvm

