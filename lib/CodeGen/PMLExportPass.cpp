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
#include "llvm/CodeGen/PMLExport.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"

using namespace llvm;

namespace llvm
{

void
PMLExport::initialize(std::string filename)
{
  std::string ErrorInfo;
  OutFile = new tool_output_file(filename.c_str(), ErrorInfo, 0);
  if (!ErrorInfo.empty()) {
    delete OutFile;
    errs() << "[mc2yml] Opening Export File failed: " << filename << "\n";
    errs() << "[mc2yml] Reason: " << ErrorInfo;
    OutFile = 0;
  }
  else {
    Output = new yaml::Output(OutFile->os());
  }
}

void
PMLExport::finalize()
{
  if (OutFile) {
    OutFile->keep();
    delete Output;
    delete OutFile;
  }
}

void PMLExport::buildEventMaps(MachineFunction &MF,
      std::map<const BasicBlock*, StringRef> &BitcodeEventMap,
      std::map<MachineBasicBlock*, StringRef> &MachineEventMap,
      std::set<StringRef> &TabuList)
{
  BitcodeEventMap.clear();
  MachineEventMap.clear();
  for (MachineFunction::iterator BlockI = MF.begin(), BlockE = MF.end();
      BlockI != BlockE; ++BlockI) {
    /// Check predecessors, ignoring backedges, unmapped nodes and nodes mapped
    /// to the entry node
    if (!BlockI->getBasicBlock())
      continue;
    if (BlockI->getBasicBlock() == BlockI->getBasicBlock()->getParent()->begin())
      continue;
    if (TabuList.count(BlockI->getBasicBlock()->getName()) > 0)
      continue;

    bool IsSubNode = false;
    for (MachineBasicBlock::const_pred_iterator PredI = BlockI->pred_begin(),
        PredE = BlockI->pred_end(); PredI != PredE; ++PredI)
    {
      if (isBackEdge(*PredI, BlockI))
        continue;
      if ((*PredI)->getBasicBlock() == BlockI->getBasicBlock())
        IsSubNode = true;
    }
    if (IsSubNode)
      continue;

    StringRef Event = BlockI->getBasicBlock()->getName();
    MachineEventMap.insert(std::make_pair(BlockI, Event));
    BitcodeEventMap.insert(std::make_pair(BlockI->getBasicBlock(), Event));
  }
  // errs() << "EventMaps Bitcode\n";
  // for(std::map<const BasicBlock*,StringRef>::iterator I =
  //             BitcodeEventMap.begin(), E = BitcodeEventMap.end();I!=E;++I) {
  //   errs() << I->first->getName() << "," << I->second << "\n";
  // }
  // errs() << "EventMaps Machinecode\n";
  // for(std::map<MachineBasicBlock*,StringRef>::iterator I =
  //             MachineEventMap.begin(), E = MachineEventMap.end();I!=E;++I) {
  //   errs() << I->first->getNumber();
  //   if(const BasicBlock *BB = I->first->getBasicBlock()) {
  //     errs() << "," << BB->getName();
  //   }
  //   errs() << "," << I->second << "\n";
  // }
}


/// Add progress nodes after expanding the bitcode and machine code subgraphs
void PMLExport::addProgressNodes(yaml::RelationGraph *RG,
      EventQueueMap<const BasicBlock*> &BitcodeEvents,
      EventQueueMap<MachineBasicBlock*> &MachineEvents,
      std::map<ProgressID, yaml::RelationNode*>& RMap,
      std::vector<std::pair<ProgressID, yaml::RelationNode*> >& RTodo,
      std::set<StringRef> &UnmatchedEvents)
{
  for (EventQueueMap<MachineBasicBlock*>::iterator I = MachineEvents.begin(),
      E = MachineEvents.end(); I != E; ++I)
  {
    const StringRef& Event = I->first;
    EventQueue<MachineBasicBlock*>* MQueue = I->second;
    EventQueue<const BasicBlock*>* IQueue = BitcodeEvents.remove(Event);
    if (IQueue == 0) {
      UnmatchedEvents.insert(Event);
      continue;
    }
    for (EventQueue<MachineBasicBlock*>::iterator MQI = MQueue->begin(), MQE =
        MQueue->end(); MQI != MQE; ++MQI) {
      for (EventQueue<const BasicBlock*>::iterator IQI = IQueue->begin(),
          IQE = IQueue->end(); IQI != IQE; ++IQI)
      {
        yaml::RelationNode *RN;
        ProgressID PNID(IQI->first, MQI->first);
        if (RMap.count(PNID) == 0) {
          // create progress node (MBlock, IBlock)
          if (!IQI->first) {
          }
          else {
            RN = RG->addNode(yaml::rnt_progress);
            RN->setSrcBlock(yaml::Name(IQI->first->getName()));
            RN->setDstBlock(yaml::Name(MQI->first->getNumber()));
            RMap.insert(std::make_pair(PNID, RN));
            RTodo.push_back(std::make_pair(PNID, RN));
          }
        }
        RN = RMap[PNID];
        // connect MPreds and IPreds to progress node
        std::vector<yaml::RelationNode*> *MPreds = MQI->second, *IPreds =
            IQI->second;
        for (std::vector<yaml::RelationNode*>::iterator PI = MPreds->begin(),
            PE = MPreds->end(); PI != PE; ++PI)
          (*PI)->addSuccessor(RN, false);
        for (std::vector<yaml::RelationNode*>::iterator PI = IPreds->begin(),
            PE = IPreds->end(); PI != PE; ++PI)
          (*PI)->addSuccessor(RN, true);
      }
    }
    delete IQueue;
  }
  yaml::RelationNode *RN = RG->getExitNode();
  for (std::vector<yaml::RelationNode*>::iterator PI =
      BitcodeEvents.getExitPredecessors().begin(), PE =
      BitcodeEvents.getExitPredecessors().end(); PI != PE; ++PI)
    (*PI)->addSuccessor(RN, false);
  for (std::vector<yaml::RelationNode*>::iterator PI =
      MachineEvents.getExitPredecessors().begin(), PE =
      MachineEvents.getExitPredecessors().end(); PI != PE; ++PI)
    (*PI)->addSuccessor(RN, true);
  if (BitcodeEvents.begin() != BitcodeEvents.end()) {
    // unmatched events (bitcode side)
    for (EventQueueMap<const BasicBlock*>::iterator I = BitcodeEvents.begin(),
        E = BitcodeEvents.end(); I != E; ++I) {
      UnmatchedEvents.insert(I->first);
    }
  }
  if (BitcodeEvents.hasExitPredecessors()
      != MachineEvents.hasExitPredecessors())
  {
    // record inconsistency, no action for tabu list
    UnmatchedEvents.insert("__exit__");
    for (std::vector<yaml::RelationNode*>::iterator PI =
        BitcodeEvents.getExitPredecessors().begin(), PE =
        BitcodeEvents.getExitPredecessors().end(); PI != PE; ++PI)
      ; // errs() << "Exit Predecessors (only src) "
        //        << (*PI)->NodeName.getName() << "\n";
    for (std::vector<yaml::RelationNode*>::iterator PI =
        MachineEvents.getExitPredecessors().begin(), PE =
        MachineEvents.getExitPredecessors().end(); PI != PE; ++PI)
      ; // errs() << "Exit Predecessors (only dst) "
        //        << (*PI)->NodeName.getName() << "\n";
  }
}

/// Check whether Source -> Target is a backedge
bool PMLExport::isBackEdge(MachineBasicBlock *Source, MachineBasicBlock *Target)
{
  if (!LI->isLoopHeader(Target))
    return false;
  MachineLoop *HeaderLoop = LI->getLoopFor(Target);
  MachineLoop *LatchLoop = LI->getLoopFor(Source);
  if (!LatchLoop)
    return false;
  while (LatchLoop->getLoopDepth() > HeaderLoop->getLoopDepth())
    LatchLoop = LatchLoop->getParentLoop();
  return (LatchLoop == HeaderLoop);
}


/// PMLExportPass - This is a pass to export a machine function to
/// YAML (using the PML schema define at (TODO: cite report))

PMLExportPass::~PMLExportPass()
{
  if (PE) delete PE;
}

void
PMLExportPass::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.setPreservesAll();
  AU.addRequired<MachineLoopInfo>();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool
PMLExportPass::doInitialization(Module &M)
{
  PE->initialize(OutFileName);
  return false;
}

bool
PMLExportPass::doFinalization(Module &M)
{
  PE->finalize();
  return false;
}

bool
PMLExportPass::runOnMachineFunction(MachineFunction &MF)
{
  MachineLoopInfo *LI = &getAnalysis<MachineLoopInfo>();
  yaml::Doc<yaml::GenericArchitecture> YDoc("pml-0.1");
  PE->serialize(MF, LI, YDoc);
  return false;
}

char PMLExportPass::ID = 0;


/// Returns a newly-created PML export pass.
MachineFunctionPass *
createPMLExportPass(std::string& FileName, TargetMachine *TM)
{
  return new PMLExportPass(FileName, TM);
}

} // end namespace llvm
