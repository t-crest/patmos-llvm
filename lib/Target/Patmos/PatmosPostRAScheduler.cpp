//===----- SchedulePostRAList.cpp - list scheduler ------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// PatmosPostRAScheduler implements a bottom-up scheduler based on
// ScheduleDAGInstr. This is similar to the PostRASchedulerList pass, but
// uses the ScheduleDAGPostRA.
//
// TODO This code is mostly similar to MachineScheduler and ScheduleDAGMI.
// In fact, most of it is copied from there, merged with the
// PostRASchedulerList code and adapted for VLIW scheduling. This should be
// merged with the MachineScheduler code into one generic base framework that
// gets specialized for pre-RA scheduling with reg-pressure tracking and for
// post-RA scheduling with AntiDepBreaking and VLIW.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "post-RA-sched"
#include "Patmos.h"
#include "PatmosPostRAScheduler.h"
#include "PatmosSchedStrategy.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/CodeGen/AggressiveAntiDepBreaker.h"
#include "llvm/CodeGen/AntiDepBreaker.h"
#include "llvm/CodeGen/CriticalAntiDepBreaker.h"
#include "llvm/CodeGen/LatencyPriorityQueue.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineInstrBundle.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/RegisterClassInfo.h"
#include "llvm/CodeGen/ScheduleDAGInstrs.h"
#include "llvm/CodeGen/ScheduleDFS.h"
#include "llvm/CodeGen/ScheduleHazardRecognizer.h"
#include "llvm/CodeGen/SchedulerRegistry.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetLowering.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetRegisterInfo.h"
#include "llvm/Target/TargetSubtargetInfo.h"
using namespace llvm;

STATISTIC(NumNoops, "Number of noops inserted");
STATISTIC(NumFixedAnti, "Number of fixed anti-dependencies");
STATISTIC(NumBundled, "Number of bundles with size > 1");
STATISTIC(NumNotBundled, "Number of instructions not bundled");
STATISTIC(NumRescheduled, "Number of rescheduled instructions");

static cl::opt<bool> ViewPostRASchedDAGs("view-postra-sched-dags", cl::Hidden,
  cl::desc("Pop up a window to show PostRASched dags after they are processed"));

static cl::opt<std::string>
EnableAntiDepBreaking("mpatmos-break-anti-dependencies",
                      cl::desc("Override default post-RA scheduling "
                               "anti-dependencies breaking mode: "
                               "\"critical\", \"all\", or \"none\""),
                      cl::Hidden);


// DAG subtrees must have at least this many nodes.
static const unsigned MinSubtreeSize = 8;


namespace {

  class PatmosPostRAScheduler : public PostRASchedContext,
                                public MachineFunctionPass {

  public:
    static char ID;
    PatmosPostRAScheduler() : MachineFunctionPass(ID) {
      initializePatmosPostRASchedulerPass(*PassRegistry::getPassRegistry());
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.setPreservesCFG();
      AU.addRequired<AliasAnalysis>();
      AU.addRequired<TargetPassConfig>();
      AU.addRequired<MachineDominatorTree>();
      AU.addPreserved<MachineDominatorTree>();
      AU.addRequired<MachineLoopInfo>();
      AU.addPreserved<MachineLoopInfo>();
      MachineFunctionPass::getAnalysisUsage(AU);
    }

    bool runOnMachineFunction(MachineFunction &Fn);
  };
  char PatmosPostRAScheduler::ID = 0;

}

char &llvm::PatmosPostRASchedulerID = PatmosPostRAScheduler::ID;

INITIALIZE_PASS(PatmosPostRAScheduler, "patmos-post-RA-sched",
                "Patmos Post RA scheduler", false, false)

/// Decrement this iterator until reaching the top or a non-debug instr.
static MachineBasicBlock::iterator
priorNonDebug(MachineBasicBlock::iterator I, MachineBasicBlock::iterator Beg) {
  assert(I != Beg && "reached the top of the region, cannot decrement");
  while (--I != Beg) {
    if (!I->isDebugValue())
      break;
  }
  return I;
}

/// If this iterator is a debug value, increment until reaching the End or a
/// non-debug instruction.
static MachineBasicBlock::iterator
nextIfDebug(MachineBasicBlock::iterator I, MachineBasicBlock::iterator End) {
  for(; I != End; ++I) {
    if (!I->isDebugValue())
      break;
  }
  return I;
}


bool PatmosPostRAScheduler::runOnMachineFunction(MachineFunction &mf) {
  // Initialize the context of the pass.
  MF = &mf;
  MLI = &getAnalysis<MachineLoopInfo>();
  MDT = &getAnalysis<MachineDominatorTree>();
  PassConfig = &getAnalysis<TargetPassConfig>();
  AA = &getAnalysis<AliasAnalysis>();

  RegClassInfo->runOnMachineFunction(*MF);

  AntiDepMode = TargetSubtargetInfo::ANTIDEP_NONE;
  CriticalPathRCs.clear();

  // Check that post-RA scheduling is enabled for this target.
  // This may upgrade the AntiDepMode.
  const TargetSubtargetInfo &ST =
                             mf.getTarget().getSubtarget<TargetSubtargetInfo>();
  if (!ST.enablePostRAScheduler(PassConfig->getOptLevel(), AntiDepMode,
                                CriticalPathRCs))
    return false;

  // Check for antidep breaking override...
  if (EnableAntiDepBreaking.getPosition() > 0) {
    AntiDepMode = (EnableAntiDepBreaking == "all")
      ? TargetSubtargetInfo::ANTIDEP_ALL
      : ((EnableAntiDepBreaking == "critical")
         ? TargetSubtargetInfo::ANTIDEP_CRITICAL
         : TargetSubtargetInfo::ANTIDEP_NONE);
  }

  // TODO this should be created by some factory..
  const PatmosTargetMachine *PTM =
                      static_cast<const PatmosTargetMachine*>(&mf.getTarget());
  PostRASchedStrategy *S = new PatmosPostRASchedStrategy(*PTM);

  OwningPtr<ScheduleDAGPostRA> Scheduler(new ScheduleDAGPostRA(this, S));

  // Visit all machine basic blocks.
  for (MachineFunction::iterator MBB = MF->begin(), MBBEnd = MF->end();
       MBB != MBBEnd; ++MBB) {

    Scheduler->startBlock(MBB);

    // Break the block into scheduling regions [I, RegionEnd), and schedule each
    // region as soon as it is discovered. RegionEnd points the scheduling
    // boundary at the bottom of the region. The DAG does not include RegionEnd,
    // but the region does (i.e. the next RegionEnd is the previous
    // RegionBegin). If the current block has no terminator then RegionEnd ==
    // MBB->end() for the bottom region.
    // StartIndex is the index of the first instruction in the region (I).
    // EndIndex is the index of RegionEnd, i.e., the start of the previously
    // scheduled region.
    //
    // The Scheduler may insert instructions during either schedule() or
    // exitRegion(), even for empty regions. So the local iterators 'I' and
    // 'RegionEnd' are invalid across these calls.
    unsigned EndIndex = MBB->size();
    for(MachineBasicBlock::iterator RegionEnd = MBB->end();
        RegionEnd != MBB->begin(); RegionEnd = Scheduler->begin()) {

      // Skip the terminator instruction in case the scheduler cannot handle
      // them.
      if (!Scheduler->canHandleTerminators()) {
        if (RegionEnd != MBB->end() ||
            Scheduler->isSchedulingBoundary(RegionEnd, MBB, *MF))
        {
          RegionEnd = llvm::prior(RegionEnd);
          --EndIndex;

          Scheduler->observe(RegionEnd, EndIndex);
        }
      }

      // The next region starts above the previous region. Look backward in the
      // instruction stream until we find the nearest boundary.
      MachineBasicBlock::iterator I = llvm::prior(RegionEnd);
      unsigned StartIndex = EndIndex - 1;
      for(;I != MBB->begin(); --I, --StartIndex) {
        if (Scheduler->isSchedulingBoundary(llvm::prior(I), MBB, *MF))
          break;
        assert(!I->isBundled() && "Rescheduling bundled code is not supported.");
      }
      assert(!I->isBundled() && "Rescheduling bundled code is not supported.");

      // Notify the scheduler of the region, even if we may skip scheduling
      // it. Perhaps it still needs to be bundled.
      Scheduler->enterRegion(MBB, I, RegionEnd, EndIndex);

      // Skip empty scheduling regions.
      if (I == RegionEnd) {
        // Close the current region. Bundle the terminator if needed.
        // This invalidates 'RegionEnd' and 'I'.
        Scheduler->exitRegion();
        continue;
      }

      DEBUG(dbgs() << "********** PostRA MI Scheduling **********\n");
      DEBUG(dbgs() << MF->getName()
            << ":BB#" << MBB->getNumber() << " " << MBB->getName()
            << "\n  From: " << *I << "    To: ";
            if (RegionEnd != MBB->end()) dbgs() << *RegionEnd;
            else dbgs() << "End";
            dbgs() << " [" << StartIndex << ", " << EndIndex << ")\n");

      // Schedule a region: possibly reorder instructions.
      // This invalidates 'RegionEnd' and 'I'.
      Scheduler->schedule();

      // Close the current region.
      Scheduler->exitRegion();

      // Scheduling has invalidated the current iterator 'I'. Ask the
      // scheduler for the top of it's scheduled region.
      RegionEnd = Scheduler->begin();
      EndIndex = StartIndex;
    }
    assert(EndIndex == 0 && "Instruction count mismatch!");
    Scheduler->finishBlock();
  }
  Scheduler->finalizeSchedule();
  return true;
}


PostRASchedContext::PostRASchedContext():
    MF(0), MLI(0), MDT(0), PassConfig(0), AA(0),
    AntiDepMode(TargetSubtargetInfo::ANTIDEP_NONE) {
  RegClassInfo = new RegisterClassInfo();
}

PostRASchedContext::~PostRASchedContext() {
  delete RegClassInfo;
}



ScheduleDAGPostRA::ScheduleDAGPostRA(PostRASchedContext *C,
                                     PostRASchedStrategy *S)
  : ScheduleDAGInstrs(*C->MF, *C->MLI, *C->MDT, /*IsPostRA=*/true),
    SchedImpl(S), DFSResult(0), Topo(SUnits, &ExitSU), EndIndex(0), AA(C->AA),
    LiveRegs(TRI->getNumRegs())
{
  // We need liveness infos for accurate latencies between schedule regions
  assert(MRI.tracksLiveness() &&
         "Live-ins must be accurate for post-RA scheduling");

  AntiDepBreak = NULL;
  if (C->AntiDepMode == TargetSubtargetInfo::ANTIDEP_ALL) {
    AntiDepBreak = new AggressiveAntiDepBreaker(MF, *C->RegClassInfo,
                                                C->CriticalPathRCs);
  }
  else if (C->AntiDepMode == TargetSubtargetInfo::ANTIDEP_CRITICAL) {
    AntiDepBreak = new CriticalAntiDepBreaker(MF, *C->RegClassInfo);
  }

  CanHandleTerminators = S->canHandleTerminators();
}

ScheduleDAGPostRA::~ScheduleDAGPostRA() {
  for (MutationList::iterator it = Mutations.begin(), ie = Mutations.end();
       it != ie; it++)
  {
    delete &*it;
  }
  delete AntiDepBreak;
  delete SchedImpl;
}

void ScheduleDAGPostRA::addMutation(ScheduleDAGPostRAMutation *M)
{
  Mutations.push_back(M);
}

bool ScheduleDAGPostRA::canAddEdge(SUnit *SuccSU, SUnit *PredSU) {
  return SuccSU == &ExitSU || !Topo.IsReachable(PredSU, SuccSU);
}

bool ScheduleDAGPostRA::addEdge(SUnit *SuccSU, const SDep &PredDep) {
  if (SuccSU != &ExitSU) {
    // Do not use WillCreateCycle, it assumes SD scheduling.
    // If Pred is reachable from Succ, then the edge creates a cycle.
    if (Topo.IsReachable(PredDep.getSUnit(), SuccSU))
      return false;
    Topo.AddPred(SuccSU, PredDep.getSUnit());
  }
  SuccSU->addPred(PredDep, /*Required=*/!PredDep.isArtificial());
  // Return true regardless of whether a new edge needed to be inserted.
  return true;
}

bool ScheduleDAGPostRA::isSchedulingBoundary(const MachineInstr *MI,
                                            const MachineBasicBlock *MBB,
                                            const MachineFunction &MF)
{
  return SchedImpl->isSchedulingBoundary(MI, MBB, MF);
}

void ScheduleDAGPostRA::postprocessDAG()
{
  for (MutationList::iterator it = Mutations.begin(), ie = Mutations.end();
         it != ie; it++)
  {
    (*it)->apply(this);
  }
}

/// StartBlock - Initialize register live-range state for scheduling in
/// this block.
///
void ScheduleDAGPostRA::startBlock(MachineBasicBlock *BB) {
  // Call the superclass.
  ScheduleDAGInstrs::startBlock(BB);

  // Reset the anti-dep breaker.
  if (AntiDepBreak != NULL)
    AntiDepBreak->StartBlock(BB);
}

/// Initialize state associated with the next scheduling region.
void ScheduleDAGPostRA::enterRegion(MachineBasicBlock *bb,
                 MachineBasicBlock::iterator begin,
                 MachineBasicBlock::iterator end,
                 unsigned endcount) {
  EndIndex = endcount;

  ScheduleDAGInstrs::enterRegion(bb, begin, end, endcount);
}

/// Observe - Update liveness information to account for the current
/// instruction, which will not be scheduled.
///
void ScheduleDAGPostRA::observe(MachineInstr *MI, unsigned Count)
{
  // Call observe for a skipped instruction
  if (AntiDepBreak != NULL)
    AntiDepBreak->Observe(MI, Count, EndIndex);
}


/// Schedule - Schedule the instruction range using list scheduling.
///
void ScheduleDAGPostRA::schedule() {
  // Build the scheduling graph.
  buildSchedGraph(AA);

  if (AntiDepBreak != NULL) {
    unsigned Broken =
      AntiDepBreak->BreakAntiDependencies(SUnits, RegionBegin, RegionEnd,
                                          EndIndex, DbgValues);

    if (Broken != 0) {
      // We made changes. Update the dependency graph.
      // Theoretically we could update the graph in place:
      // When a live range is changed to use a different register, remove
      // the def's anti-dependence *and* output-dependence edges due to
      // that register, and add new anti-dependence and output-dependence
      // edges based on the next live range of the register.
      ScheduleDAG::clearDAG();
      buildSchedGraph(AA);

      NumFixedAnti += Broken;
    }
  }

  Topo.InitDAGTopologicalSorting();

  postprocessDAG();

  SchedImpl->postprocessDAG(this);

  SmallVector<SUnit*, 8> TopRoots, BotRoots;
  findRootsAndBiasEdges(TopRoots, BotRoots);

  // Initialize the strategy before modifying the DAG.
  SchedImpl->initialize(this);

  DEBUG(dbgs() << "********** List Scheduling **********\n");
  DEBUG(for (unsigned su = 0, e = SUnits.size(); su != e; ++su)
          SUnits[su].dumpAll(this));
  if (ViewPostRASchedDAGs) viewGraph();

  // Initialize ready queues now that the DAG and priority data are finalized.
  initQueues(TopRoots, BotRoots);

  bool IsTopNode = false;
  bool IsBundled = false;
  SUnit *SU;

  while (SchedImpl->pickNode(SU, IsTopNode, IsBundled))
  {
    scheduleMI(SU, IsTopNode, IsBundled);

    if (SU && SU->isScheduled) {
      SchedImpl->reschedNode(SU, IsTopNode, IsBundled);

      NumRescheduled++;
    }
    else if (SU) {
      updateQueues(SU, IsTopNode, IsBundled);

      SchedImpl->schedNode(SU, IsTopNode, IsBundled);
    }
    else {
      SchedImpl->schedNoop(IsTopNode);

      NumNoops++;
    }
  }

  finishTopBundle();
  finishBottomBundle();

  assert(CurrentTop == CurrentBottom && "Nonempty unscheduled zone.");

  SchedImpl->finalize(this);

  placeDebugValues();

  DEBUG({
      unsigned BBNum = begin()->getParent()->getNumber();
      dbgs() << "*** Final schedule for BB#" << BBNum << " ***\n";
      dumpSchedule();
      dbgs() << '\n';
    });
}

void ScheduleDAGPostRA::finalizeSchedule()
{
  ScheduleDAGInstrs::finalizeSchedule();

  // Finalize all bundles
  // TODO should we do this by calling the finalize pass instead?
  finalizeBundles(MF);
}

/// This is normally called from the main scheduler loop but may also be invoked
/// by the scheduling strategy to perform additional code motion.
void ScheduleDAGPostRA::moveInstruction(MachineInstr *MI,
                                    MachineBasicBlock::iterator InsertPos) {
  // Advance RegionBegin if the first instruction moves down.
  if (&*RegionBegin == MI)
    ++RegionBegin;

  bool InsideBundle = MI->isBundledWithPred() && MI->isBundledWithSucc();
  MachineInstr *PrevMI = InsideBundle ? MI->getPrevNode() : NULL;

  // update statistics when rescheduling nodes
  if (!InsideBundle &&
      ((MI->isBundledWithPred() && !MI->getPrevNode()->isBundledWithPred()) ||
       (MI->isBundledWithSucc() && !MI->getNextNode()->isBundledWithSucc())))
  {
    // We remove the second-last instruction from a bundle, update stats
    NumBundled--;
    NumNotBundled++;
  }

  // Remove the instruction from a bundle, if it is inside one
  if (MI->isBundledWithPred()) MI->unbundleFromPred();
  if (MI->isBundledWithSucc()) MI->unbundleFromSucc();

  // Update the instruction stream.
  BB->splice(InsertPos, BB, MI);

  if (InsideBundle) {
    // reconnect the old bundle
    PrevMI->bundleWithSucc();
  }

  // Recede RegionBegin if an instruction moves above the first.
  if (RegionBegin == InsertPos)
    RegionBegin = MI;
}

void ScheduleDAGPostRA::computeDFSResult() {
  if (!DFSResult)
    DFSResult = new SchedDFSResult(/*BottomU*/true, MinSubtreeSize);
  DFSResult->clear();
  ScheduledTrees.clear();
  DFSResult->resize(SUnits.size());
  DFSResult->compute(SUnits);
  ScheduledTrees.resize(DFSResult->getNumSubtrees());
}

void ScheduleDAGPostRA::findRootsAndBiasEdges(SmallVectorImpl<SUnit*> &TopRoots,
                                          SmallVectorImpl<SUnit*> &BotRoots) {
  for (std::vector<SUnit>::iterator
         I = SUnits.begin(), E = SUnits.end(); I != E; ++I) {
    SUnit *SU = &(*I);
    assert(!SU->isBoundaryNode() && "Boundary node should not be in SUnits");

    // Order predecessors so DFSResult follows the critical path.
    SU->biasCriticalPath();

    // A SUnit is ready to top schedule if it has no predecessors.
    if (!I->NumPredsLeft)
      TopRoots.push_back(SU);
    // A SUnit is ready to bottom schedule if it has no successors.
    if (!I->NumSuccsLeft)
      BotRoots.push_back(SU);
  }
  ExitSU.biasCriticalPath();
}

/// Identify DAG roots and setup scheduler queues.
void ScheduleDAGPostRA::initQueues(ArrayRef<SUnit*> TopRoots,
                                   ArrayRef<SUnit*> BotRoots)
{
  // Release all DAG roots for scheduling, not including EntrySU/ExitSU.
  //
  // Nodes with unreleased weak edges can still be roots.
  // Release top roots in forward order.
  for (SmallVectorImpl<SUnit*>::const_iterator
         I = TopRoots.begin(), E = TopRoots.end(); I != E; ++I) {
    SchedImpl->releaseTopNode(*I);
  }
  // Release bottom roots in reverse order so the higher priority nodes appear
  // first. This is more natural and slightly more efficient.
  for (SmallVectorImpl<SUnit*>::const_reverse_iterator
         I = BotRoots.rbegin(), E = BotRoots.rend(); I != E; ++I) {
    SchedImpl->releaseBottomNode(*I);
  }

  releaseSuccessors(&EntrySU);
  releasePredecessors(&ExitSU);

  SchedImpl->registerRoots();

  // Advance past initial DebugValues.
  CurrentTop = nextIfDebug(RegionBegin, RegionEnd);

  CurrentBottom = RegionEnd;

  TopBundleMIs.clear();
  BottomBundleMIs.clear();
}

/// Move an instruction and update register pressure.
void ScheduleDAGPostRA::scheduleMI(SUnit *SU, bool IsTopNode, bool IsBundled) {
  // Move the instruction to its new location in the instruction stream.
  MachineInstr *MI = SU ? SU->getInstr() : NULL;

  if (IsTopNode) {
    assert((!SU || SU->isTopReady()) &&
           "node still has unscheduled dependencies");

    if (!IsBundled)
      finishTopBundle();
    if (MI) {
      DEBUG(dbgs() << "Pick top node SU(" << SU->NodeNum << ") ");
      DEBUG( if (DFSResult) dbgs()
            << " ILP: " << DFSResult->getILP(SU)
            << " Tree: " << DFSResult->getSubtreeID(SU) << " @"
            << DFSResult->getSubtreeLevel(DFSResult->getSubtreeID(SU)) << '\n');
      DEBUG(dbgs() << "Scheduling " << *SU->getInstr());

      TopBundleMIs.push_back(MI);
    } else {
      DEBUG(dbgs() << "Scheduling NOOP at top\n");

      TII->insertNoop(*BB, CurrentTop);
      // set ReginBegin to the NOP if we inserted a NOP at the region start
      if (CurrentTop == RegionBegin) {
	RegionBegin = llvm::prior(CurrentTop);
      }
    }
  }
  else {
    assert((!SU || SU->isBottomReady()) &&
           "node still has unscheduled dependencies");

    if (!IsBundled)
      finishBottomBundle();
    if (MI) {
      DEBUG(dbgs() << "Pick bottom node SU(" << SU->NodeNum << ") ");
      DEBUG( if (DFSResult) dbgs()
            << " ILP: " << DFSResult->getILP(SU)
            << " Tree: " << DFSResult->getSubtreeID(SU) << " @"
            << DFSResult->getSubtreeLevel(DFSResult->getSubtreeID(SU)) << '\n');
      DEBUG(dbgs() << "Scheduling " << *SU->getInstr());

      BottomBundleMIs.push_back(MI);
    } else {
      DEBUG(dbgs() << "Scheduling NOOP at bottom\n");

      TII->insertNoop(*BB, CurrentBottom);
      // recede Top as well if we insert a NOP at the top
      if (CurrentBottom == CurrentTop) {
	CurrentTop = llvm::prior(CurrentTop);
      }
      // set ReginBegin to the NOP if we inserted a NOP at the region start
      if (CurrentBottom == RegionBegin) {
	RegionBegin = llvm::prior(CurrentBottom);
      }
      CurrentBottom = llvm::prior(CurrentBottom);
    }
  }
}

void ScheduleDAGPostRA::finishTopBundle()
{
  if (TopBundleMIs.empty()) return;

  DEBUG(dbgs() << "Finishing top bundle of size "
               << TopBundleMIs.size() << "\n");

  for (size_t i = 0; i < TopBundleMIs.size(); i++) {
    MachineInstr *MI = TopBundleMIs[i];

    if (&*CurrentTop == MI)
      CurrentTop = nextIfDebug(++CurrentTop, CurrentBottom);
    else {
      moveInstruction(MI, CurrentTop);
    }
  }

  // Bundle instructions, including all debug instructions
  if (TopBundleMIs.size() > 1) {
    MachineBasicBlock::instr_iterator MI = llvm::next(TopBundleMIs[0]);
    for (;MI != CurrentTop; MI++) {
      MI->bundleWithPred();
    }
    NumBundled++;
  } else {
    NumNotBundled++;
  }

  TopBundleMIs.clear();
}

void ScheduleDAGPostRA::finishBottomBundle()
{
  if (BottomBundleMIs.empty()) return;

  DEBUG(dbgs() << "Finishing bottom bundle of size "
               << BottomBundleMIs.size() << "\n");

  for (int i = (int)BottomBundleMIs.size() - 1; i >= 0; i--) {
    MachineInstr *MI = BottomBundleMIs[i];

    MachineBasicBlock::iterator priorII =
      priorNonDebug(CurrentBottom, CurrentTop);
    if (&*priorII == MI)
      CurrentBottom = priorII;
    else {
      if (&*CurrentTop == MI) {
        CurrentTop = nextIfDebug(++CurrentTop, priorII);
      }
      moveInstruction(MI, CurrentBottom);
      CurrentBottom = MI;
    }
  }

  // Bundle instructions, including all debug instructions
  if (BottomBundleMIs.size() > 1) {
    MachineBasicBlock::instr_iterator MI = *CurrentBottom;
    for (;MI != *BottomBundleMIs.back(); MI++) {
      MI->bundleWithSucc();
    }
    NumBundled++;
  } else {
    NumNotBundled++;
  }

  BottomBundleMIs.clear();
}

/// Update scheduler queues after scheduling an instruction.
void ScheduleDAGPostRA::updateQueues(SUnit *SU, bool IsTopNode, bool IsBundled)
{
  // Release dependent instructions for scheduling.
  if (IsTopNode)
    releaseSuccessors(SU);
  else
    releasePredecessors(SU);

  SU->isScheduled = true;

  if (DFSResult) {
    unsigned SubtreeID = DFSResult->getSubtreeID(SU);
    if (!ScheduledTrees.test(SubtreeID)) {
      ScheduledTrees.set(SubtreeID);
      DFSResult->scheduleTree(SubtreeID);
      SchedImpl->scheduleTree(SubtreeID);
    }
  }
}

/// Reinsert any remaining debug_values, just like the PostRA scheduler.
void ScheduleDAGPostRA::placeDebugValues() {
  // If first instruction was a DBG_VALUE then put it back.
  if (FirstDbgValue) {
    BB->splice(RegionBegin, BB, FirstDbgValue);
    RegionBegin = FirstDbgValue;
  }

  for (std::vector<std::pair<MachineInstr *, MachineInstr *> >::iterator
         DI = DbgValues.end(), DE = DbgValues.begin(); DI != DE; --DI) {
    std::pair<MachineInstr *, MachineInstr *> P = *prior(DI);
    MachineInstr *DbgValue = P.first;
    MachineBasicBlock::instr_iterator OrigPrevMI = P.second;

    while (OrigPrevMI->isBundledWithSucc()) {
      // Move the pointer to the end of the bundle so that the debug value is
      // placed after the bundle, not inside the bundle.
      // TODO we might want to put the debug value inside he bundle as close as
      // possible to the original instruction (?) Then we probably need to split
      // the bundle first and connect it afterwards, since splice does not
      // handle bundles.
      OrigPrevMI++;
    }

    moveInstruction(DbgValue, ++OrigPrevMI);
  }

  DbgValues.clear();
  FirstDbgValue = NULL;
}

/// FinishBlock - Clean up register live-range state.
///
void ScheduleDAGPostRA::finishBlock() {
  if (AntiDepBreak != NULL)
    AntiDepBreak->FinishBlock();

  // Update register kills
  fixupKills(BB);

  // Call the superclass.
  ScheduleDAGInstrs::finishBlock();
}


#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void ScheduleDAGPostRA::dumpSchedule() const {
  for (MachineBasicBlock::iterator MI = begin(), ME = end(); MI != ME; ++MI) {
    if (SUnit *SU = getSUnit(&(*MI)))
      SU->dump(this);
    else if (MI->isDebugValue())
      dbgs() << "Debug Value\n";
    else
      dbgs() << "Missing SUnit\n";
  }
}
#endif


/// StartBlockForKills - Initialize register live-range state for updating kills
///
void ScheduleDAGPostRA::startBlockForKills(MachineBasicBlock *BB) {
  // Start with no live registers.
  LiveRegs.reset();

  // Examine the live-in regs of all successors.
  for (MachineBasicBlock::succ_iterator SI = BB->succ_begin(),
       SE = BB->succ_end(); SI != SE; ++SI) {
    for (MachineBasicBlock::livein_iterator I = (*SI)->livein_begin(),
         E = (*SI)->livein_end(); I != E; ++I) {
      unsigned Reg = *I;
      LiveRegs.set(Reg);
      // Repeat, for all subregs.
      for (MCSubRegIterator SubRegs(Reg, TRI); SubRegs.isValid(); ++SubRegs)
        LiveRegs.set(*SubRegs);
    }
  }
}

bool ScheduleDAGPostRA::toggleKillFlag(MachineInstr *MI,
                                          MachineOperand &MO) {
  // Setting kill flag...
  if (!MO.isKill()) {
    MO.setIsKill(true);
    return false;
  }

  // If MO itself is live, clear the kill flag...
  if (LiveRegs.test(MO.getReg())) {
    MO.setIsKill(false);
    return false;
  }

  // If any subreg of MO is live, then create an imp-def for that
  // subreg and keep MO marked as killed.
  MO.setIsKill(false);
  bool AllDead = true;
  const unsigned SuperReg = MO.getReg();
  MachineInstrBuilder MIB(MF, MI);
  for (MCSubRegIterator SubRegs(SuperReg, TRI); SubRegs.isValid(); ++SubRegs) {
    if (LiveRegs.test(*SubRegs)) {
      MIB.addReg(*SubRegs, RegState::ImplicitDefine);
      AllDead = false;
    }
  }

  if(AllDead)
    MO.setIsKill(true);
  return false;
}

/// FixupKills - Fix the register kill flags, they may have been made
/// incorrect by instruction reordering.
///
void ScheduleDAGPostRA::fixupKills(MachineBasicBlock *MBB) {
  DEBUG(dbgs() << "Fixup kills for BB#" << MBB->getNumber() << '\n');

  BitVector killedRegs(TRI->getNumRegs());

  startBlockForKills(MBB);

  // Examine block from end to start...
  unsigned Count = MBB->size();
  for (MachineBasicBlock::iterator I = MBB->end(), E = MBB->begin();
       I != E; --Count) {
    MachineInstr *MI = --I;
    if (MI->isDebugValue())
      continue;

    // Update liveness.  Registers that are defed but not used in this
    // instruction are now dead. Mark register and all subregs as they
    // are completely defined.
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (MO.isRegMask())
        LiveRegs.clearBitsNotInMask(MO.getRegMask());
      if (!MO.isReg()) continue;
      unsigned Reg = MO.getReg();
      if (Reg == 0) continue;
      if (!MO.isDef()) continue;
      // Ignore two-addr defs.
      if (MI->isRegTiedToUseOperand(i)) continue;

      LiveRegs.reset(Reg);

      // Repeat for all subregs.
      for (MCSubRegIterator SubRegs(Reg, TRI); SubRegs.isValid(); ++SubRegs)
        LiveRegs.reset(*SubRegs);
    }

    // Examine all used registers and set/clear kill flag. When a
    // register is used multiple times we only set the kill flag on
    // the first use.
    killedRegs.reset();
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || !MO.isUse()) continue;
      unsigned Reg = MO.getReg();
      if ((Reg == 0) || MRI.isReserved(Reg)) continue;

      bool kill = false;
      if (!killedRegs.test(Reg)) {
        kill = true;
        // A register is not killed if any subregs are live...
        for (MCSubRegIterator SubRegs(Reg, TRI); SubRegs.isValid(); ++SubRegs) {
          if (LiveRegs.test(*SubRegs)) {
            kill = false;
            break;
          }
        }

        // If subreg is not live, then register is killed if it became
        // live in this instruction
        if (kill)
          kill = !LiveRegs.test(Reg);
      }

      if (MO.isKill() != kill) {
        DEBUG(dbgs() << "Fixing " << MO << " in ");
        // Warning: ToggleKillFlag may invalidate MO.
        toggleKillFlag(MI, MO);
        DEBUG(MI->dump());
      }

      killedRegs.set(Reg);
    }

    // Mark any used register (that is not using undef) and subregs as
    // now live...
    for (unsigned i = 0, e = MI->getNumOperands(); i != e; ++i) {
      MachineOperand &MO = MI->getOperand(i);
      if (!MO.isReg() || !MO.isUse() || MO.isUndef()) continue;
      unsigned Reg = MO.getReg();
      if ((Reg == 0) || MRI.isReserved(Reg)) continue;

      LiveRegs.set(Reg);

      for (MCSubRegIterator SubRegs(Reg, TRI); SubRegs.isValid(); ++SubRegs)
        LiveRegs.set(*SubRegs);
    }
  }
}

/// ReleaseSucc - Decrement the NumPredsLeft count of a successor. When
/// NumPredsLeft reaches zero, release the successor node.
///
/// FIXME: Adjust SuccSU height based on MinLatency.
void ScheduleDAGPostRA::releaseSucc(SUnit *SU, SDep *SuccEdge) {
  SUnit *SuccSU = SuccEdge->getSUnit();

  if (SuccEdge->isWeak()) {
    --SuccSU->WeakPredsLeft;
    return;
  }
#ifndef NDEBUG
  if (SuccSU->NumPredsLeft == 0) {
    dbgs() << "*** Scheduling failed! ***\n";
    SuccSU->dump(this);
    dbgs() << " has been released too many times!\n";
    llvm_unreachable(0);
  }
#endif
  --SuccSU->NumPredsLeft;
  if (SuccSU->NumPredsLeft == 0 && SuccSU != &ExitSU)
    SchedImpl->releaseTopNode(SuccSU);
}

/// releaseSuccessors - Call releaseSucc on each of SU's successors.
void ScheduleDAGPostRA::releaseSuccessors(SUnit *SU) {
  for (SUnit::succ_iterator I = SU->Succs.begin(), E = SU->Succs.end();
       I != E; ++I) {
    releaseSucc(SU, &*I);
  }
}

/// ReleasePred - Decrement the NumSuccsLeft count of a predecessor. When
/// NumSuccsLeft reaches zero, release the predecessor node.
///
/// FIXME: Adjust PredSU height based on MinLatency.
void ScheduleDAGPostRA::releasePred(SUnit *SU, SDep *PredEdge) {
  SUnit *PredSU = PredEdge->getSUnit();

  if (PredEdge->isWeak()) {
    --PredSU->WeakSuccsLeft;
    return;
  }
#ifndef NDEBUG
  if (PredSU->NumSuccsLeft == 0) {
    dbgs() << "*** Scheduling failed! ***\n";
    PredSU->dump(this);
    dbgs() << " has been released too many times!\n";
    llvm_unreachable(0);
  }
#endif
  --PredSU->NumSuccsLeft;
  if (PredSU->NumSuccsLeft == 0 && PredSU != &EntrySU)
    SchedImpl->releaseBottomNode(PredSU);
}

/// releasePredecessors - Call releasePred on each of SU's predecessors.
void ScheduleDAGPostRA::releasePredecessors(SUnit *SU) {
  for (SUnit::pred_iterator I = SU->Preds.begin(), E = SU->Preds.end();
       I != E; ++I) {
    releasePred(SU, &*I);
  }
}

//===----------------------------------------------------------------------===//
// GraphWriter support for ScheduleDAGMI.
//===----------------------------------------------------------------------===//

namespace llvm {

template<> struct GraphTraits<
  ScheduleDAGPostRA*> : public GraphTraits<ScheduleDAG*> {};

template<>
struct DOTGraphTraits<ScheduleDAGPostRA*> : public DefaultDOTGraphTraits {

  DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

  static std::string getGraphName(const ScheduleDAG *G) {
    return G->MF.getName();
  }

  static bool renderGraphFromBottomUp() {
    return true;
  }

  static bool isNodeHidden(const SUnit *Node, const ScheduleDAG *Graph) {
    return (Node->NumPreds > 10 || Node->NumSuccs > 10);
  }

  static bool hasNodeAddressLabel(const SUnit *Node,
                                  const ScheduleDAG *Graph) {
    return false;
  }

  /// If you want to override the dot attributes printed for a particular
  /// edge, override this method.
  static std::string getEdgeAttributes(const SUnit *Node,
                                       SUnitIterator EI,
                                       const ScheduleDAG *Graph) {
    if (EI.isArtificialDep())
      return "color=cyan,style=dashed";
    if (EI.isCtrlDep())
      return "color=blue,style=dashed";
    return "";
  }

  static std::string getNodeLabel(const SUnit *SU, const ScheduleDAG *G) {
    std::string Str;
    raw_string_ostream SS(Str);
    SS << "SU(" << SU->NodeNum << ")[" << SU->Latency << "]";
    return SS.str();
  }
  static std::string getNodeDescription(const SUnit *SU, const ScheduleDAG *G) {
    return G->getGraphNodeLabel(SU);
  }

  static std::string getNodeAttributes(const SUnit *N,
                                       const ScheduleDAG *Graph) {
    std::string Str("shape=Mrecord");
    const SchedDFSResult *DFS =
      static_cast<const ScheduleDAGPostRA*>(Graph)->getDFSResult();
    if (DFS) {
      Str += ",style=filled,fillcolor=\"#";
      Str += DOT::getColorString(DFS->getSubtreeID(N));
      Str += '"';
    }
    return Str;
  }
};
} // namespace llvm

/// viewGraph - Pop up a ghostview window with the reachable parts of the DAG
/// rendered using 'dot'.
///
void ScheduleDAGPostRA::viewGraph(const Twine &Name, const Twine &Title) {
  ViewGraph(this, Name, false, Title);
}

/// Out-of-line implementation with no arguments is handy for gdb.
void ScheduleDAGPostRA::viewGraph() {
  viewGraph(getDAGName(), "Scheduling-Units Graph for " + getDAGName());
}
