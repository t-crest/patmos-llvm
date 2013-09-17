//===-- PatmosBypassFromPML.cpp - Patmos bypass from PML ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass imports a PML file with annotated aiT analysis results,
// scans them for memory accesses with unknown (large) address ranges
// and rewrites the memory instructions to bypassed loads.
// The rationale is to avoid destroying the state of the data cache by
// unanalyzable accesses, such that the analysis becomes more precise.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-bypass-from-pml"
#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/PMLImport.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"



using namespace llvm;

STATISTIC( Rewritten, "Number of instructions rewritten to bypass");


static cl::opt<bool> EnableBypassFromPML(
  "mpatmos-enable-bypass-from-pml",
  cl::init(false),
  cl::desc("Enable rewriting unanalyzable (aiT) memory accesses "
           "to bypass the cache."),
  cl::Hidden);


static cl::opt<int> BypassRangeThreshold(
    "mpatmos-bypass-threshold",
    cl::init(24),
    cl::desc("Bypass if a range is wider than 2^THRESHOLD"),
    cl::Hidden, cl::Optional);

namespace {


  class PatmosBypassFromPML : public MachineFunctionPass {
  private:
    /// Pass ID
    static char ID;

    /// bypassCacheLoads - Rewrite the loads in a given MBB for which
    /// there exist value facts classifiying the load as load
    /// from an unpredictable address
    bool bypassCacheLoads(MachineBasicBlock &MBB, PMLQuery *Q,
                          PMLQuery::ValueFactList &MemFacts);


    /// hasLargeRange - Returns true if at least one range of the
    /// value fact is above a certain threshold
    bool hasLargeRange(const yaml::ValueFact *VF) const;

    //
    /// rewriteInstruction - Rewrite a diven instruction to bypass the cache
    /// NB: only applies to load instructions for now
    bool rewriteInstruction(MachineInstr &MI);

  public:
    /// Target machine description which we query for reg. names, data
    /// layout, etc.
    ///
    PatmosTargetMachine &TM;
    const PatmosInstrInfo *TII;
    const TargetRegisterInfo *TRI;

    PatmosBypassFromPML(PatmosTargetMachine &tm)
      : MachineFunctionPass(ID), TM(tm),
        TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
        TRI(tm.getRegisterInfo()) {
      // we have to initialize the PMLImport Pass
      initializePMLImportPass(*PassRegistry::getPassRegistry());
    }

    virtual const char *getPassName() const {
      return "Patmos Bypass From PML";
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<PMLImport>();
      AU.setPreservesAll();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    bool runOnMachineFunction(MachineFunction &MF);
  };


  char PatmosBypassFromPML::ID = 0;
} // end of anonymous namespace

///////////////////////////////////////////////////////////////////////////////

/// createPatmosBypassFromPMLPass - Returns a pass that fills in delay
/// slots in Patmos MachineFunctions
///
FunctionPass *llvm::createPatmosBypassFromPMLPass(PatmosTargetMachine &tm) {
  return new PatmosBypassFromPML(tm);
}

///////////////////////////////////////////////////////////////////////////////


bool PatmosBypassFromPML::hasLargeRange(const yaml::ValueFact *VF) const {
  for (unsigned i = 0; i < VF->Values.size(); i++) {
    yaml::Value Val = VF->Values[i];
    if (Val.Symbol.empty() &&
        (Val.Max-Val.Min > (1<<BypassRangeThreshold))) {
      return true;
    }
  }
  return false;
}


bool PatmosBypassFromPML::rewriteInstruction(MachineInstr &MI) {
  unsigned opc = 0;
  switch (MI.getOpcode()) {
    case Patmos::LWC:  opc = Patmos::LWM;  break;
    case Patmos::LHC:  opc = Patmos::LHM;  break;
    case Patmos::LBC:  opc = Patmos::LBM;  break;
    case Patmos::LHUC: opc = Patmos::LHUM; break;
    case Patmos::LBUC: opc = Patmos::LBUM; break;
                       // decoupled loads
    case Patmos::DLWC:  opc = Patmos::DLWM;  break;
    case Patmos::DLHC:  opc = Patmos::DLHM;  break;
    case Patmos::DLBC:  opc = Patmos::DLBM;  break;
    case Patmos::DLHUC: opc = Patmos::DLHUM; break;
    case Patmos::DLBUC: opc = Patmos::DLBUM; break;
    default: /*ignore*/;
  }

  if (opc) {
    DEBUG( dbgs() << "  - rewrite: " << MI );
    MI.setDesc(TII->get(opc));
    Rewritten++; // bump stats
    return true;
  }
  return false;
}


bool PatmosBypassFromPML::bypassCacheLoads(MachineBasicBlock &MBB,
    PMLQuery *Q, PMLQuery::ValueFactList &MemFacts) {

  SmallPtrSet<const yaml::ProgramPoint *, 32> RewritePPs;
  bool changed = false;

  // go through all VFs and check if they contain large ranges
  // - if so, put VF->PP into a set
  // - skip PPs already contained in the set
  for (PMLQuery::ValueFactList::const_iterator I = MemFacts.begin(),
        E = MemFacts.end(); I != E; ++I) {
    const yaml::ValueFact *VF = *I;

    if (RewritePPs.count(VF->PP)) continue;

    if (hasLargeRange(VF)) RewritePPs.insert(VF->PP);
  }

  // get the labels of the program points
  SmallSet<uint64_t, 32> MemInstrLabels;
  for( SmallPtrSet<const yaml::ProgramPoint *, 32>::const_iterator
        I = RewritePPs.begin(), E = RewritePPs.end(); I != E; ++I) {

    yaml::Name L = Q->getMemInstrLabel(*I);
    // we assume that the labels are in integer form,
    // with label n decribing the n-th memory access in a basic block
    assert(L.isInteger() && "Mem instr label is not an integer!");
    MemInstrLabels.insert(L.getNameAsInteger());
  }

  // for each MachineInstr keep counting the mem instrs
  // - if it is an index in the set, rewrite the instruction
  uint64_t memidx = 0;
  for (MachineBasicBlock::iterator MI = MBB.begin(), ME = MBB.end();
      MI != ME; ++MI) {
    if (MI->mayLoad() || MI->mayStore()) {
      if (MemInstrLabels.count(memidx)) {
        changed |= rewriteInstruction(*MI);
      }
      memidx++;
    }
  }

  return changed;
}


bool PatmosBypassFromPML::runOnMachineFunction(MachineFunction &MF) {

  if (!EnableBypassFromPML) return false;

  PMLImport &PI = getAnalysis<PMLImport>();
  PMLMCQuery *Query = PI.createMCQuery(*this, MF);

  bool Changed = false;

  PMLQuery::ValueFactsMap LoadFacts;

  if (Query && Query->getMemFacts(MF, LoadFacts)) {

    DEBUG( dbgs() << "[BypassFromPML] "
        << MF.getFunction()->getName() << "\n");

    for (MachineFunction::iterator FI = MF.begin(), FE = MF.end();
        FI != FE; ++FI) {
      PMLQuery::ValueFactList &VFL = Query->getBBMemFacts(LoadFacts, *FI);
      if (!VFL.empty()) {

        DEBUG( dbgs() << "  MBB#" << FI->getNumber() << "\n" );
        Changed |= bypassCacheLoads(*FI, Query, VFL);
      }

    }
    delete Query;
  }
  return Changed;
}

