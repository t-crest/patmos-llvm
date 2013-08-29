//===-- PatmosExport.cpp - Target-specific PML exporter for Patmos --------===//
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
#include "InstPrinter/PatmosInstPrinter.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/CallingConvLower.h"
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

    virtual unsigned getBranchDelaySlots(const MachineInstr *Instr) {
      switch (Instr->getOpcode()) {
      case Patmos::BR:
      case Patmos::BRu: 
      case Patmos::BRT:
      case Patmos::BRTu:
	return 2;
      case Patmos::BRCF:
      case Patmos::BRCFu:
      case Patmos::BRCFT:
      case Patmos::BRCFTu:
      case Patmos::CALL:
      case Patmos::CALLR:
      case Patmos::RET:
	return 3;
      default:
	return 0;
      }
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

  // we need information about the calling conventions!
  #include "PatmosGenCallingConv.inc"

  class PatmosMachineExport : public PMLMachineExport {
  public:
    PatmosMachineExport(PatmosTargetMachine &tm, ModulePass &mp,
                        PMLInstrInfo *PII)
      : PMLMachineExport(tm, mp, PII) {
        // silence compiler warning
        (void)RetCC_Patmos;
      }

    virtual bool doExportInstruction(const MachineInstr *Ins) {
      if (Ins->isDebugValue()) return false;
      if (SkipSerializeInstructions) {
        if (!Ins->getDesc().isCall() && !Ins->getDesc().isBranch() &&
            !Ins->getDesc().isReturn() && !Ins->getDesc().mayLoad())
          return false;
      }
      return true;
    }

    /// exportArgumentRegisterMapping
    /// see below for implementation
    virtual void exportArgumentRegisterMapping(
                                  yaml::MachineFunction *PMF,
                                  const MachineFunction &MF);
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



///////////////////////////////////////////////////////////////////////////////


  void PatmosMachineExport::
  exportArgumentRegisterMapping(yaml::MachineFunction *PMF,
                                const MachineFunction &MF)
  {
      const PatmosTargetLowering *TLI =
        static_cast<const PatmosTargetLowering *>(TM.getTargetLowering());
      const TargetRegisterInfo *TRI = TM.getRegisterInfo();
      const Function *F = MF.getFunction();
      LLVMContext &Ctx = F->getParent()->getContext();

      if (F->isVarArg()) {
        // we are passing arguments on stack, can't handle this for now
        // TODO
        return;
      }

      // following code segment was copied in large parts from
      // SelectionDAGISel::LowerArguments(), which can be found in
      // lib/CodeGen/SelectionDAG/SelectionDAGBuilder.cpp:6628
      /////////////////

      SmallVector<ISD::InputArg, 16> Ins;
      const DataLayout *TD = TLI->getDataLayout();
      ISD::ArgFlagsTy Flags;

      // For Patmos, PatmosISelLowering does not overload CanLowerReturn(),
      // which returns true by default and is queried by FunctionInfo
      // Check whether the function can return without sret-demotion.
      SmallVector<ISD::OutputArg, 4> Outs;
      GetReturnInfo(F->getReturnType(), F->getAttributes().getRetAttributes(),
          Outs, *TLI);
      assert(TLI->CanLowerReturn(F->getCallingConv(),
            const_cast<MachineFunction&>(MF), F->isVarArg(), Outs, Ctx));

      // Set up the incoming argument description vector.
      unsigned Idx = 1;
      for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end();
          I != E; ++I, ++Idx) {
        SmallVector<EVT, 4> ValueVTs;
        ComputeValueVTs(*TLI, I->getType(), ValueVTs);
        bool isArgValueUsed = !I->use_empty();
        for (unsigned Value = 0, NumValues = ValueVTs.size();
            Value != NumValues; ++Value) {
          EVT VT = ValueVTs[Value];
          Type *ArgTy = VT.getTypeForEVT(Ctx);
          ISD::ArgFlagsTy Flags;
          unsigned OriginalAlignment =
            TD->getABITypeAlignment(ArgTy);

          if (F->getAttributes().hasAttribute(Idx, Attribute::ZExt))
            Flags.setZExt();
          if (F->getAttributes().hasAttribute(Idx, Attribute::SExt))
            Flags.setSExt();
          if (F->getAttributes().hasAttribute(Idx, Attribute::InReg))
            Flags.setInReg();
          if (F->getAttributes().hasAttribute(Idx, Attribute::StructRet))
            Flags.setSRet();
          if (F->getAttributes().hasAttribute(Idx, Attribute::ByVal)) {
            Flags.setByVal();
            PointerType *Ty = cast<PointerType>(I->getType());
            Type *ElementTy = Ty->getElementType();
            Flags.setByValSize(TD->getTypeAllocSize(ElementTy));
            // For ByVal, alignment should be passed from FE.  BE will guess if
            // this info is not there but there are cases it cannot get right.
            unsigned FrameAlign;
            if (F->getParamAlignment(Idx))
              FrameAlign = F->getParamAlignment(Idx);
            else
              FrameAlign = TLI->getByValTypeAlignment(ElementTy);
            Flags.setByValAlign(FrameAlign);
          }
          if (F->getAttributes().hasAttribute(Idx, Attribute::Nest))
            Flags.setNest();
          Flags.setOrigAlign(OriginalAlignment);

          EVT RegisterVT = TLI->getRegisterType(Ctx, VT);
          unsigned NumRegs = TLI->getNumRegisters(Ctx, VT);
          for (unsigned i = 0; i != NumRegs; ++i) {
            ISD::InputArg MyFlags(Flags, RegisterVT, isArgValueUsed,
                Idx-1, i*RegisterVT.getStoreSize());
            if (NumRegs > 1 && i == 0)
              MyFlags.Flags.setSplit();
            // if it isn't first piece, alignment must be 1
            else if (i > 0)
              MyFlags.Flags.setOrigAlign(1);
            Ins.push_back(MyFlags);
          }
        }
      }

      /////////////////
      // see
      // PatmosTargetLowering::LowerCCCArguments() in PatmosISelLowering.cpp
      /////////////////
      // Assign locations to all of the incoming arguments.
      SmallVector<CCValAssign, 16> ArgLocs;
      CCState CCInfo(F->getCallingConv(), false/*isVarArg*/,
                     const_cast<MachineFunction&>(MF), TM, ArgLocs, Ctx);
      CCInfo.AnalyzeFormalArguments(Ins, CC_Patmos);
      /////////////////


      // Lowered arguments are computed, now we can match them back again
      // and do the actual exporting

      unsigned FAIdx = 0, // formal argument index
               LAIdx = 0; // lowered argument index

      // we have a 1-1 mapping of Ins to ArgLocs, we access them via LAIdx
      for (Function::const_arg_iterator I = F->arg_begin(), E = F->arg_end();
          I != E; ++I, ++FAIdx) {

        // Place LAIdx to the position where the lowered arguments for the
        // corresponding formal argument start
        while(Ins[LAIdx].OrigArgIndex < FAIdx) LAIdx++;

        if (IntegerType *ITy = dyn_cast<IntegerType>(I->getType())) {
          // being an integer, this might be an interesting parameter

          // TODO Do we need to test special flags (zero-/sign-extend)
          //      and output more information?
          // Note that we start with the low-part registers first
          EVT VT = TLI->getValueType(ITy);
          assert(VT.isSimple());

          DEBUG( dbgs() << FAIdx << " " << *I << ": [" );

          // FIXME
          // we don't add it immediately to PMF, as we only support
          // arguments in registers at this point
          std::string ArgName = ("\"%" + I->getName() + "\"").str();
          yaml::Argument *Arg = new yaml::Argument(ArgName, FAIdx);
          bool allInRegs = true;

          // get all registers with OrigArgIndex == FAIdx
          for( ; LAIdx < Ins.size() && Ins[LAIdx].OrigArgIndex == FAIdx;
              LAIdx++) {
            CCValAssign &VA = ArgLocs[LAIdx];

            if (!VA.isRegLoc()) {
              // no support yet, bailout
              allInRegs = false;
              break;
            }

            // we prefer the name of the register as is printed in assembly
            Arg->addReg(PatmosInstPrinter::getRegisterName(VA.getLocReg()));

            DEBUG( dbgs() <<  TRI->getName(VA.getLocReg()) << " " );
          }

          DEBUG( dbgs() <<  "]\n" );

          if (allInRegs) {
            PMF->addArgument(Arg);
          } else {
            delete Arg;
          }
        }
      }
    }



} // end namespace llvm

