//===-- PMLExport.cpp -----------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Export Internal Compiler Information to PML
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "pml-export"

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineMemOperand.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PML.h"
#include "llvm/CodeGen/PMLExport.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"


using namespace llvm;

namespace llvm {
STATISTIC( NumConstantBounds, "Number of constant header bounds exported");
STATISTIC( NumSymbolicBounds, "Number of symbolic header bounds exported");

STATISTIC( NumMemOp,   "Number of mem ops with pointer type");
}

/// Unfortunately, the interface for accessing successors differs
/// between machine block and bitcode block, therefore we need this
/// trait in order to avoid code duplication
namespace llvm {
namespace yaml {

/// Utility class to generalize parts of the relation graph construction
template <typename Block> struct FlowGraphTrait {};
template <> struct FlowGraphTrait<const BasicBlock> {
  typedef llvm::succ_const_iterator succ_iterator;
  inline static succ_iterator succ_begin(const BasicBlock *BB) { return llvm::succ_begin(BB); }
  inline static succ_iterator succ_end(const BasicBlock *BB) { return llvm::succ_end(BB); }
  static Name getName(const BasicBlock *BB) {
    return Name(BB->getName());
  }
};
template <> struct FlowGraphTrait<MachineBasicBlock> {
  typedef MachineBasicBlock::succ_iterator succ_iterator;
  inline static succ_iterator succ_begin(MachineBasicBlock *MBB) { return MBB->succ_begin(); }
  inline static succ_iterator succ_end(MachineBasicBlock *MBB) { return MBB->succ_end(); }
  static Name getName(MachineBasicBlock*BB) {
    return Name(BB->getNumber());
  }
};

bool operator==(const Name n1, const Name n2) {
  return n1.NameStr == n2.NameStr;
}

} // end namespace yaml
} // end namespace llvm

///////////////////////////////////////////////////////////////////////////////

namespace llvm
{


PMLInstrInfo::StringList PMLInstrInfo::getCalleeNames(MachineFunction &Caller,
                                                 const MachineInstr *Ins)
{
  StringList Callees;
  for (MachineInstr::const_mop_iterator Op = Ins->operands_begin(),
      E = Ins->operands_end(); Op != E; ++Op) {
    if (Op->isGlobal()) {
      Callees.push_back( Op->getGlobal()->getName() );
    }
    else if (Op->isSymbol()) {
      Callees.push_back( Op->getSymbolName() );
    }
  }
  return Callees;
}

PMLInstrInfo::MFList PMLInstrInfo::getCallees(const Module &M,
                                              MachineModuleInfo &MMI,
                                              MachineFunction &MF,
                                              const MachineInstr *Ins)
{
  MFList Callees;

  // Using the names found earlier to find functions.
  // This will not work if a temp function has no name, but can this be anyway?
  StringList CalleeNames = getCalleeNames(MF, Ins);

  for (StringList::iterator it = CalleeNames.begin(), ie = CalleeNames.end();
       it != ie; ++it)
  {
    Function *F = M.getFunction(*it);
    if (!F) continue;

    MachineFunction *MF = MMI.getMachineFunction(F);
    if (!MF) continue;

    Callees.push_back(MF);
  }

  return Callees;
}


PMLInstrInfo::MBBList PMLInstrInfo::getBranchTargets(
                                      MachineFunction &MF,
                                      const MachineInstr *Instr)
{
  MBBList targets;
  return targets;
}

PMLInstrInfo::MFList PMLInstrInfo::getCalledFunctions(const Module &M,
                                              MachineModuleInfo &MMI,
                                              MachineFunction &MF)
{
  MFList CalledFunctions;

  // Iterate over all instructions, get callees for all call sites
  for (MachineFunction::iterator BB = MF.begin(), BE = MF.end(); BB != BE;
       ++BB)
  {
    for (MachineBasicBlock::iterator II = BB->begin(), IE = BB->end();
         II != IE; ++II)
    {
      if (!II->getDesc().isCall()) continue;

      MFList Callees = getCallees(M, MMI, MF, II);
      for (MFList::iterator CI = Callees.begin(), CE = Callees.end();
           CI != CE; ++CI)
      {
        // TODO make this unique!!
        CalledFunctions.push_back(*CI);
      }
    }
  }

  return CalledFunctions;
}

int PMLInstrInfo::getSize(const MachineInstr *Instr)
{
  return Instr->getDesc().getSize();
}


///////////////////////////////////////////////////////////////////////////////

namespace {
  // Check whether all LLVM values are function formal arguments.
  // Implements SCEVTraversal::Visitor.
  struct SCEVCheckFormalArgs {
    bool OnlyArg;

    SCEVCheckFormalArgs() : OnlyArg(true) {}

    bool follow(const SCEV *S) {
      if(const SCEVUnknown *Val = dyn_cast<SCEVUnknown>(S)) {
        OnlyArg = isa<Argument>(Val->getValue());
      }
      return OnlyArg;
    }

    bool isDone() const { return !OnlyArg; }
  };
}


yaml::FlowFact *PMLBitcodeExport::createLoopFact(const BasicBlock *BB,
                                                 yaml::Name RHS) const {
  const Function *Fn = BB->getParent();

  yaml::FlowFact *FF = new yaml::FlowFact();

  FF->Scope = FF->createLoop(yaml::Name(Fn->getName()),
                             yaml::Name(BB->getName()));

  yaml::ProgramPoint *Block =
    FF->createBlock(yaml::Name(Fn->getName()),
                    yaml::Name(BB->getName()));
  FF->addTermLHS(Block, 1LL);
  FF->RHS = RHS;
  FF->Comparison = yaml::cmp_less_equal;
  FF->Level = yaml::level_bitcode;
  FF->Origin = "llvm.bc";
  FF->Classification = "loop-global";

  return FF;
}


void PMLBitcodeExport::serialize(MachineFunction &MF)
{
  const Function *Fn = MF.getFunction();

  if (!Fn) return;
  LoopInfo &LI = P.getAnalysis<LoopInfo>(*const_cast<Function*>(Fn));
  ScalarEvolution &SE = P.getAnalysis<ScalarEvolution>(*const_cast<Function*>(Fn));

  // create PML bitcode function
  yaml::BitcodeFunction *F = new yaml::BitcodeFunction(Fn->getName());
  F->Level = yaml::level_bitcode;
  yaml::BitcodeBlock *B;
  for (Function::const_iterator BI = Fn->begin(), BE = Fn->end(); BI != BE;
      ++BI) {
    B = F->addBlock(new yaml::BitcodeBlock(BI->getName()));

    // export loop information
    Loop *Loop = LI.getLoopFor(BI);
    if (Loop) {
      if(SE.hasLoopInvariantBackedgeTakenCount(Loop)) {

        // check for constant loop bound
        if(const SCEVConstant* BEBound =
                dyn_cast<SCEVConstant>(SE.getMaxBackedgeTakenCount(Loop))) {
          uint64_t ConstHeaderBound = BEBound->getValue()->getZExtValue() + 1;
          if(ConstHeaderBound < 0xFFFFFFFFu) {
            YDoc.addFlowFact(createLoopFact(BI, ConstHeaderBound));
            // bump statistic counter
            NumConstantBounds++;
          }
        }
        // check for non-constant, symbolic loop bound
        const SCEV *BECount = SE.getBackedgeTakenCount(Loop);
        if (!isa<SCEVCouldNotCompute>(BECount)) {
          // check for Arguments using visitor
          SCEVCheckFormalArgs CheckFA;
          visitAll(BECount, CheckFA);
          if (CheckFA.OnlyArg) {
            // add 1 to get header bound from backedge count
            const SCEV *SymbolicHeaderBound = SE.getAddExpr(BECount,
                SE.getConstant(BECount->getType(), (uint64_t)1));

            // FIXME maybe we want something more structured/processed
            // than a dump
            std::string s;
            raw_string_ostream os(s);
            os << "\"" << *SymbolicHeaderBound << "\"";

            YDoc.addFlowFact(createLoopFact(BI, StringRef(os.str())));
            // bump statistic counter
            NumSymbolicBounds++;
          }
        }
      }
    }

    while (Loop) {
      B->Loops.push_back(yaml::Name(Loop->getHeader()->getName()));
      Loop = Loop->getParentLoop();
    }

    /// B->MapsTo = (maybe C-source debug info?)
    for (const_pred_iterator PI = pred_begin(&*BI), PE = pred_end(&*BI); PI != PE;
        ++PI) {
      B->Predecessors.push_back(yaml::Name((*PI)->getName()));
    }
    for (succ_const_iterator SI = succ_begin(&*BI), SE = succ_end(&*BI); SI != SE;
        ++SI) {
      B->Successors.push_back(yaml::Name((*SI)->getName()));
    }

    unsigned Index = 0;
    for (BasicBlock::const_iterator II = BI->begin(), IE = BI->end(); II != IE;
        ++II)
    {
      if (!doExportInstruction(II)) { Index++; continue; }

      yaml::Instruction *I = B->addInstruction(
          new yaml::Instruction(Index++));
      exportInstruction(I, II);
    }
  }
  // TODO: we do not compute a hash yet
  F->Hash = StringRef("0");
  YDoc.addFunction(F);
}

void PMLBitcodeExport::exportInstruction(yaml::Instruction* I,
                                          const Instruction *II)
{
  I->Opcode = II->getOpcode();
  if (const CallInst *CI = dyn_cast<const CallInst>(II)) {
    if (const Function *F = CI->getCalledFunction()) {
      I->addCallee(F->getName());
    }
    else {
      // TODO: we still have no information about indirect calls
      // TODO: use PMLInstrInfo to try to get call info about bitcode calls
      I->addCallee(StringRef("__any__"));
    }
  }
}

void PMLMachineExport::serialize(MachineFunction &MF)
{
  Function* F = const_cast<Function*>(MF.getFunction());
  MachineLoopInfo &MLI(P.getAnalysis<MachineLoopInfo>(*F));

  yaml::GenericFormat::MachineFunction *PMF =
     new yaml::GenericFormat::MachineFunction(MF.getFunctionNumber());

  PMF->MapsTo = yaml::Name(MF.getFunction()->getName());
  PMF->Level = yaml::level_machinecode;
  yaml::GenericFormat::MachineBlock *B;

  // export argument-register mapping if available
  exportArgumentRegisterMapping(PMF, MF);

  for (MachineFunction::iterator BB = MF.begin(), E = MF.end(); BB != E; ++BB)
  {
    B = PMF->addBlock(
        new yaml::GenericFormat::MachineBlock(BB->getNumber()));

    for (MachineBasicBlock::const_pred_iterator BBPred = BB->pred_begin(),
        E = BB->pred_end(); BBPred != E; ++BBPred)
      B->Predecessors.push_back(yaml::Name((*BBPred)->getNumber()));
    for (MachineBasicBlock::const_succ_iterator BBSucc = BB->succ_begin(),
        E = BB->succ_end(); BBSucc != E; ++BBSucc)
      B->Successors.push_back(yaml::Name((*BBSucc)->getNumber()));

    B->MapsTo = yaml::Name(BB->getName());

    // export loop information
    MachineLoop *Loop = MLI.getLoopFor(BB);
    while (Loop) {
      B->Loops.push_back(yaml::Name(Loop->getHeader()->getNumber()));
      Loop = Loop->getParentLoop();
    }

    // export instruction and branch Information
    MachineBasicBlock *TrueSucc = 0, *FalseSucc = 0;
    SmallVector<MachineOperand, 4> Conditions;
    const TargetInstrInfo *TII = TM.getInstrInfo();
    bool HasBranchInfo = !TII->AnalyzeBranch(*BB, TrueSucc, FalseSucc,
        Conditions, false);

    unsigned Index = 0;
    for (MachineBasicBlock::iterator Ins = BB->begin(), E = BB->end();
        Ins != E; ++Ins)
    {
      if (!doExportInstruction(Ins)) { Index++; continue; }

      yaml::GenericMachineInstruction *I = B->addInstruction(
          new yaml::GenericMachineInstruction(Index++));
      exportInstruction(MF, I, Ins, Conditions, HasBranchInfo,
                        TrueSucc, FalseSucc);
    }
  }

  // TODO: we do not compute a hash yet
  PMF->Hash = StringRef("0");
  YDoc.addMachineFunction(PMF);
}

void PMLMachineExport::
exportInstruction(MachineFunction &MF, yaml::GenericMachineInstruction *I,
                  const MachineInstr *Ins,
                  SmallVector<MachineOperand, 4> &Conditions,
                  bool HasBranchInfo, MachineBasicBlock *TrueSucc,
                  MachineBasicBlock *FalseSucc)
{
  I->Opcode = Ins->getOpcode();
  I->Size = PII->getSize(Ins);
  I->BranchDelaySlots = PII->getBranchDelaySlots(Ins);

  if (Ins->getDesc().isCall()) {
    I->BranchType = yaml::branch_call;
    exportCallInstruction(MF, I, Ins);
  } else if (Ins->getDesc().isReturn()) {
    I->BranchType = yaml::branch_return;
  } else if (Ins->getDesc().isBranch()) {
    exportBranchInstruction(MF, I, Ins, Conditions, HasBranchInfo,
                            TrueSucc, FalseSucc);
  } else if (Ins->getDesc().mayLoad()) {
    exportLoadInstruction(MF, I, Ins);
    I->BranchType = yaml::branch_none;
  } else {
    I->BranchType = yaml::branch_none;
  }
  // XXX: maybe a good idea (descriptions)
  // raw_string_ostream ss(I->Descr);
  // Ins->print(ss);
  // ss.flush();
}

void PMLMachineExport::
exportCallInstruction(MachineFunction &MF, yaml::GenericMachineInstruction *I,
                      const MachineInstr *Ins)
{
  std::vector<StringRef> Callees = PII->getCalleeNames(MF, Ins);

  for (std::vector<StringRef>::iterator it = Callees.begin(),ie = Callees.end();
       it != ie; ++it) {
    I->addCallee(*it);
  }

  if (!I->hasCallees()) {
    // errs() << "[mc2yml] Warning: no known callee for MC instruction ";
    // errs() << "(opcode " << Ins->getOpcode() << ")";
    // errs() << " in " << MF.getFunction()->getName() << "\n";
    // TODO shouldn't we just leave this empty??
    I->addCallee(StringRef("__any__"));
  }
}

void PMLMachineExport::
exportBranchInstruction(MachineFunction &MF,
                        yaml::GenericMachineInstruction *I,
                        const MachineInstr *Ins,
                        SmallVector<MachineOperand, 4> &Conditions,
                        bool HasBranchInfo, MachineBasicBlock *TrueSucc,
                        MachineBasicBlock *FalseSucc)
{
  // Should we check the PMLInstrInfo for branch targets?
  bool LookupBranchTargets = true;

  if (Ins->getDesc().isConditionalBranch()) {
    I->BranchType = yaml::branch_conditional;
    if (HasBranchInfo && TrueSucc) {
      I->BranchTargets.push_back(yaml::Name(TrueSucc->getNumber()));
      LookupBranchTargets = false;
    }
  }
  else if (Ins->getDesc().isUnconditionalBranch()) {
    I->BranchType = yaml::branch_unconditional;
    MachineBasicBlock *USucc =
        Conditions.empty() ? TrueSucc : FalseSucc;
    if (HasBranchInfo && USucc) {
      I->BranchTargets.push_back(yaml::Name(USucc->getNumber()));
      LookupBranchTargets = false;
    }
  }
  else if (Ins->getDesc().isIndirectBranch()) {
    I->BranchType = yaml::branch_indirect;
  }
  else {
    I->BranchType = yaml::branch_any;
  }

  if (LookupBranchTargets) {
    typedef const std::vector<MachineBasicBlock*> BTVector;
    const BTVector& targets = PII->getBranchTargets(MF, Ins);

    for (BTVector::const_iterator it = targets.begin(),ie=targets.end();
          it != ie; ++it) {
      I->BranchTargets.push_back(yaml::Name((*it)->getNumber()));
    }
  }
}

void PMLMachineExport::
exportLoadInstruction(MachineFunction &MF, yaml::GenericMachineInstruction *I,
                      const MachineInstr *Ins)
{
  // TODO attach the information to the PML instruction
  //
  // FIXME maybe there should be only one memoperand here anyway? bundles?
  for(MachineInstr::mmo_iterator I=Ins->memoperands_begin(),
                                 E=Ins->memoperands_end(); I!=E; ++I) {
    MachineMemOperand *MO = *I;
    assert(MO->isLoad());
    const Value *V = MO->getValue();
    if (V) {
      assert(V->getType()->getTypeID()==Type::PointerTyID &&
            "Value referenced by a MachineMemOperand is not a pointer!");
      NumMemOp++; // STATISTICS

      // Debug output to inspect types, grep "^[GCIAPO]:" <err>
      if (const GlobalValue *G = dyn_cast<GlobalValue>(V)) {
          DEBUG( dbgs() << "G: "; G->dump() );
          // A subclass of Constant
          // Global value of which we know the address EXACTLY

      } else if (const Constant *C = dyn_cast<Constant>(V)) {
          DEBUG( dbgs() << "C: "; C->dump() );
          // this also captures getelementptr with constant operands
          // we know the address EXACTLY

      } else if (const Instruction *I = dyn_cast<Instruction>(V)) {
          DEBUG( dbgs() << "I: "; I->dump() );
          // This should be either getelementptr or inttoptr
          // at least one of the operands of getelementptr is variable:
          // - base (e.g. if passed as argument)
          // - index (for accesses to known arrays)
          //
          // If idx0 == 0 and only the other indices is variable,
          // we can export the RANGE (probably the most interesting case)
          // TODO focus on this and see if exporting the others gives any
          //      improvement

      } else if (const Argument *A = dyn_cast<Argument>(V)) {
          DEBUG( dbgs() << "A: "; A->dump() );
          // pointers passed as function arguments

      } else if (const PseudoSourceValue *P =
                            dyn_cast<PseudoSourceValue>(V)) {
          DEBUG( dbgs() << "P: "; P->dump() );
          // this captures locations on the stack frame

      } else {
          DEBUG( dbgs() << "O: "; V->dump() );
          // ??? anything we forgot?
      }
    }
  }

}

void PMLMachineExport::
exportArgumentRegisterMapping(yaml::GenericFormat::MachineFunction *F,
                              const MachineFunction &MF) {
  // must be implemented entirely by target-specific exporters at this stage
}

///////////////////////////////////////////////////////////////////////////////

// TODO maybe move RelationGraph utility stuff into its own (internal) class.

/// RelationGraph utility class to manage maps from Blocks to Predecessor Lists
template <typename Block>
class EventQueue {
  typedef std::vector<yaml::RelationNode*> PredList;
  std::map<Block, PredList* > KeyedQueues;
public:
  ~EventQueue() {
    for(iterator I = KeyedQueues.begin(), E = KeyedQueues.end(); I!=E; ++I)
    {
      delete I->second;
    }
  }
  void addItem(Block& Key, yaml::RelationNode *Pred) {
    if(KeyedQueues.count(Key) == 0) {
      KeyedQueues.insert( make_pair(Key, new PredList()) );
    }
    KeyedQueues[Key]->push_back(Pred);
  }
  typedef typename std::map<Block,PredList*>::iterator iterator;
  iterator begin() { return KeyedQueues.begin(); }
  iterator end() { return   KeyedQueues.end(); }
};


/// RelationGraph utility class to maintain candidates for progress nodes
template <typename Block>
class EventQueueMap {
  std::vector<yaml::RelationNode*> ExitPreds;
  std::map<StringRef, EventQueue<Block>*> EMap;
public:
  ~EventQueueMap() {
    for(iterator I = EMap.begin(), E = EMap.end(); I!=E; ++I) {
      delete I->second;
    }
  }
  void addItem(StringRef Event, Block& Key, yaml::RelationNode *Pred) {
    if(EMap.count(Event) == 0) {
      EMap.insert( std::make_pair(Event,new EventQueue<Block>()) );
    }
    EMap[Event]->addItem(Key, Pred);
  }
  void addExitPredecessor(yaml::RelationNode *Pred) {
    ExitPreds.push_back(Pred);
  }
  std::vector<yaml::RelationNode*>& getExitPredecessors() {
    return ExitPreds;
  }
  bool hasExitPredecessors() {
    return !ExitPreds.empty();
  }
  // Caller takes ownership of removed queue
  EventQueue<Block>* remove(const StringRef& Event) {
    iterator It = EMap.find(Event);
    if(It == EMap.end())
      return 0;
    EventQueue<Block>* Queue = It->second;
    EMap.erase(It);
    return Queue;
  }
  typedef typename std::map<StringRef,EventQueue<Block>*>::iterator iterator;
  iterator begin() { return EMap.begin(); }
  iterator end()   { return EMap.end(); }
};

/// Mapping from T to events (strings)
template <typename T>
struct EventMap {
    typedef std::map<T,StringRef> type;
};

/// Progress nodes are characterized by a pair of bitcode/machine block
typedef std::pair <const BasicBlock*,MachineBasicBlock*> ProgressID;

/// Expand progress node N either at the machine code (Block=MachineBasicBlock) or bitcode (Block=BasicBlock)
/// level. The RelationGraphHelperTrait<BlockType> class provides the machine/bitcode specific
/// functionality
template<typename Block>
void
expandProgressNode(yaml::RelationGraph *RG,
    yaml::RelationNode *ProgressNode, yaml::RelationNodeType type,
    Block* StartBlock, typename EventMap<Block*>::type EventMap,
    EventQueueMap<Block*>& Events)
{
  typedef yaml::FlowGraphTrait<Block> Trait;
  std::vector<std::pair<yaml::RelationNode*, Block*> > Queue;
  std::map<Block*, yaml::RelationNode*> Created;
  std::set<Block*> Visited, Black;
  Queue.push_back(std::make_pair(ProgressNode, StartBlock));
  while (!Queue.empty()) {
    // expand unexpanded, queued items
    std::pair<yaml::RelationNode*, Block*> Item = Queue.back();
    yaml::RelationNode* RN = Item.first;
    Block* BB = Item.second;
    if (Visited.count(BB) > 0) { // visited before
      Black.insert(BB); // black: all successors done
      Queue.pop_back();
      continue;
    }
    Visited.insert(BB);
    for (typename Trait::succ_iterator I = Trait::succ_begin(BB), E =
        Trait::succ_end(BB); I != E; ++I) {
      Block *BB2 = *I;
      if (EventMap.count(BB2) == 0) {
        // successor generates no event -> add 'type' relation node, queue successor
        if (Created.count(BB2) == 0) {
          DEBUG(dbgs() << "Internal node for "
                       << Trait::getName(BB2).getName() << "("
                       << ((type==yaml::rnt_src) ? "src" : "dst")
                       << ") created\n");
          yaml::RelationNode *NewRelationNode = RG->addNode(type);
          NewRelationNode->setBlock(Trait::getName(BB2),
              type == yaml::rnt_src);
          Created.insert(std::make_pair(BB2, NewRelationNode));
        }
        yaml::RelationNode *RN2 = Created[BB2];
        RN->addSuccessor(RN2, type == yaml::rnt_src);
        if(Visited.count(BB2) > 0) {
          if(Black.count(BB2) == 0) {
            // Detected cycle without progress
            RG->Status = yaml::rg_status_loop;
          }
        } else {
          Queue.push_back(std::make_pair(RN2, BB2));
        }
      }
      else {
        // successor generates event -> queue event
        Events.addItem(EventMap[BB2], BB2, RN);
      }
    }
    // No successors -> exit event
    if (Trait::succ_begin(BB) == Trait::succ_end(BB)) {
      Events.addExitPredecessor(RN);
    }
  }
}

/// Add progress nodes after expanding the bitcode and machine code subgraphs
void addProgressNodes(yaml::RelationGraph *RG,
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
      DEBUG(dbgs() << "Unmatched Machine Event: " << Event << "\n");
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

  // add src-edges from src-labeled nodes to exit
  for (std::vector<yaml::RelationNode*>::iterator PI =
      BitcodeEvents.getExitPredecessors().begin(), PE =
      BitcodeEvents.getExitPredecessors().end(); PI != PE; ++PI)
    (*PI)->addSuccessor(RN, true);

  // add dst-edges from dst-labeled nodes to exit
  for (std::vector<yaml::RelationNode*>::iterator PI =
      MachineEvents.getExitPredecessors().begin(), PE =
      MachineEvents.getExitPredecessors().end(); PI != PE; ++PI)
    (*PI)->addSuccessor(RN, false);
  if (BitcodeEvents.begin() != BitcodeEvents.end()) {
    // unmatched events (bitcode side)
    for (EventQueueMap<const BasicBlock*>::iterator I = BitcodeEvents.begin(),
        E = BitcodeEvents.end(); I != E; ++I) {
      DEBUG(dbgs() << "Unmatched Event (Bitcode): " << I->first << "\n");
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


void PMLRelationGraphExport::serialize(MachineFunction &MF)
{
  Function *BF = const_cast<Function*>(MF.getFunction());
  if (!BF)
    return;

  // unmatched events, used as tabu list, and for error reporting
  std::set<StringRef> TabuEvents;
  std::set<StringRef> UnmatchedEvents;

  // As the LLVM mapping is not always good enough, we might have had unmatched events
  // In this case, we use the list of unmatched machinecode events as Tabu List, and
  // retry (with a retry limit)
  unsigned TriesLeft = 3;
  yaml::RelationGraph *RG = 0;
  yaml::RelationGraphStatus Status = yaml::rg_status_valid;
  while (TriesLeft-- > 0) {
    if (RG)
      delete RG; // also deletes scopes and nodes
    /// Create Graph
    yaml::RelationScope *DstScope = new yaml::RelationScope(
        yaml::Name(MF.getFunctionNumber()), yaml::level_machinecode);
    yaml::RelationScope *SrcScope = new yaml::RelationScope(
        yaml::Name(BF->getName()), yaml::level_bitcode);
    RG = new yaml::RelationGraph(SrcScope, DstScope);
    RG->getEntryNode()->setSrcBlock(
        yaml::FlowGraphTrait<const BasicBlock>::getName(
            &BF->getEntryBlock()));
    RG->getEntryNode()->setDstBlock(
        yaml::FlowGraphTrait<MachineBasicBlock>::getName(&MF.front()));
    UnmatchedEvents.clear();

    /// Event Maps
    EventMap<const BasicBlock*>::type IEventMap;
    EventMap<MachineBasicBlock*>::type MEventMap;

    /// Known and visited relation nodes
    std::map<ProgressID, yaml::RelationNode*> RMap;
    std::set<yaml::RelationNode*> RVisited;
    std::vector<std::pair<ProgressID, yaml::RelationNode*> > RTodo;

    /// Build event maps using RUnmatched as tabu list
    buildEventMaps(MF, IEventMap, MEventMap, TabuEvents);

    /// We first queue the entry node
    RTodo.push_back(
        std::make_pair(std::make_pair(&BF->getEntryBlock(), &MF.front()),
            RG->getEntryNode()));

    /// while there is an unprocessed progress node (n -> IBB,MBB)
    while (!RTodo.empty()) {
      std::pair<ProgressID, yaml::RelationNode*> Item = RTodo.back();
      RTodo.pop_back();
      yaml::RelationNode *RN = Item.second;
      if (RVisited.count(RN) > 0)
        continue;
      const BasicBlock *IBB = Item.first.first;
      MachineBasicBlock *MBB = Item.first.second;
      EventQueueMap<const BasicBlock*> IEvents;
      EventQueueMap<MachineBasicBlock*> MEvents;

      DEBUG(errs() << "Expanding node " << IBB->getName() << " / " << MBB->getNumber() << "\n");
      /// Expand both at the bitcode and machine level (starting with IBB and MBB, resp.),
      /// which results in new src/dst nodes being created, and two bitcode and machinecode-level maps from events
      /// to a list of (bitcode/machine block, list of RG predecessor blocks) pairs
      expandProgressNode(RG, RN, yaml::rnt_src, IBB, IEventMap, IEvents);
      expandProgressNode(RG, RN, yaml::rnt_dst, MBB, MEventMap, MEvents);

      /// For each event and corresponding bitcode list IList and machinecode MList, create a progress
      /// node (iblock,mblock) for every pair ((iblock,ipreds),(mblock,mpreds)) \in (IList x MList) and add
      /// edges from all ipreds and mpreds to that progress node
      addProgressNodes(RG, IEvents, MEvents, RMap, RTodo, UnmatchedEvents);
    }
    if (UnmatchedEvents.empty()) { // No unmatched events this time
      break;
    } else if (TabuEvents.empty()) {
      DEBUG(errs() << "[mc2yml] Warning: inconsistent initial mapping for "
            << MF.getFunction()->getName() << " (retrying)\n");
      Status = yaml::rg_status_corrected;
    }
    TabuEvents.insert(UnmatchedEvents.begin(), UnmatchedEvents.end());
  }
  if (!UnmatchedEvents.empty()) {
    DEBUG(errs() << "[mc2yml] Error: failed to find a correct event mapping for " << MF.getFunction()->getName() << "\n");
    Status = yaml::rg_status_incomplete;
  }
  RG->Status = Status;
  YDoc.addRelationGraph(RG);
}

void PMLRelationGraphExport::buildEventMaps(MachineFunction &MF,
      std::map<const BasicBlock*, StringRef> &BitcodeEventMap,
      std::map<MachineBasicBlock*, StringRef> &MachineEventMap,
      std::set<StringRef> &TabuList)
{


  Function* F = const_cast<Function*>(MF.getFunction());
  BackedgeInfo BI(P.getAnalysis<MachineLoopInfo>(*F));

  BitcodeEventMap.clear();
  MachineEventMap.clear();
  DEBUG(dbgs() << "buildEventMaps() " << MF.begin()->getParent()->getFunction()->getName() << "\n");
  std::map<MachineBasicBlock*, StringRef> InitialEvents;
  for (MachineFunction::iterator BlockI = MF.begin(), BlockE = MF.end();
      BlockI != BlockE; ++BlockI) {
    /// No mapping if there is no information on the original basic block
    if (!BlockI->getBasicBlock()) {
      DEBUG(dbgs() << "Not mapping " << BlockI->getNumber() << ": no mapping information\n");
      continue;
    }
    /// No mapping if it maps to the entry node
    if (BlockI->getBasicBlock() == BlockI->getBasicBlock()->getParent()->begin()) {
      DEBUG(dbgs() << "Not mapping " << BlockI->getNumber() << ": entry node\n");
      continue;
    }
    /// XXX: No mapping if the loop nest levels do not match ?
    /// No mapping if the block is tabu
    if (TabuList.count(BlockI->getBasicBlock()->getName()) > 0) {
      DEBUG(dbgs() << "Not mapping " << BlockI->getNumber() << ": TABU\n");
      continue;
    }

    StringRef Event = BlockI->getBasicBlock()->getName();

    bool IsSubNode = false;
    /// No mapping if predecessor (excluding loop latches) is mapped to the same block
    for (MachineBasicBlock::const_pred_iterator PredI = BlockI->pred_begin(),
        PredE = BlockI->pred_end(); PredI != PredE; ++PredI)
    {
      if (BI.isBackEdge(*PredI, BlockI))
          continue;
      if ((*PredI)->getBasicBlock() == BlockI->getBasicBlock()) {
        IsSubNode = true;
        break;
      }
    }
    if (IsSubNode) {
      DEBUG(dbgs() << "Not mapping " << BlockI->getNumber() << ": subgraph node\n");
      continue;
    }
    DEBUG(dbgs() << "MachineEvent " << BlockI->getNumber() << " -> " << Event << "\n");
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


/// Check whether Source -> Target is a backedge
bool PMLRelationGraphExport::BackedgeInfo::
isBackEdge(MachineBasicBlock *Source, MachineBasicBlock *Target)
{
  if (!MLI.isLoopHeader(Target))
    return false;
  MachineLoop *HeaderLoop = MLI.getLoopFor(Target);
  MachineLoop *LatchLoop = MLI.getLoopFor(Source);
  if (!LatchLoop)
    return false;
  while (LatchLoop->getLoopDepth() > HeaderLoop->getLoopDepth())
    LatchLoop = LatchLoop->getParentLoop();
  return (LatchLoop == HeaderLoop);
}


///////////////////////////////////////////////////////////////////////////////


PMLModuleExportPass::PMLModuleExportPass(char &id, TargetMachine &TM,
                              StringRef filename,
                              ArrayRef<std::string> roots, PMLInstrInfo *pii)
  : MachineModulePass(id), OutFileName(filename), Roots(roots)
{
  PII = pii ? pii : new PMLInstrInfo();
}

PMLModuleExportPass::PMLModuleExportPass(TargetMachine &TM, StringRef filename,
                              ArrayRef<std::string> roots, PMLInstrInfo *pii)
  : MachineModulePass(ID), OutFileName(filename), Roots(roots)
{
  PII = pii ? pii : new PMLInstrInfo();
}


void PMLModuleExportPass::getAnalysisUsage(AnalysisUsage &AU) const
{
  AU.setPreservesAll();
  AU.addRequired<MachineModuleInfo>();
  AU.addRequired<MachineLoopInfo>();
  AU.addRequired<LoopInfo>();
  AU.addRequired<ScalarEvolution>();
  MachineModulePass::getAnalysisUsage(AU);
}

bool PMLModuleExportPass::runOnMachineModule(const Module &M)
{
  // get the machine-level module information.
  MachineModuleInfo &MMI(getAnalysis<MachineModuleInfo>());

  // Queue roots
  FoundFunctions.clear();
  Queue.clear();
  for (size_t i=0; i < Roots.size(); i++) {
    addToQueue(M, MMI, Roots[i]);
  }

  // follow roots until no new methods are found
  while (!Queue.empty()) {
    MachineFunction *MF = Queue.front();
    Queue.pop_front();

    for (size_t i=0; i < Exporters.size(); i++) {
      Exporters[i]->serialize(*MF);
    }

    addCalleesToQueue(M, MMI, *MF);
  }

  return false;
}

void PMLModuleExportPass::addCalleesToQueue(const Module &M, MachineModuleInfo &MMI,
                                            MachineFunction &MF)
{
  PMLInstrInfo::MFList Callees = PII->getCalledFunctions(M, MMI, MF);
  for (PMLInstrInfo::MFList::iterator it = Callees.begin(), ie = Callees.end();
       it != ie; ++it)
  {
    addToQueue(*it);
  }
}

bool PMLModuleExportPass::doInitialization(Module &M) {
  for (ExportList::iterator it = Exporters.begin(), ie = Exporters.end();
       it != ie; ++it)
  {
    (*it)->initialize(M);
  }
  return false;
}

bool PMLModuleExportPass::doFinalization(Module &M) {
  tool_output_file *OutFile;
  yaml::Output *Output;
  std::string ErrorInfo;

  OutFile = new tool_output_file(OutFileName.str().c_str(), ErrorInfo, 0);
  if (!ErrorInfo.empty()) {
    delete OutFile;
    errs() << "[mc2yml] Opening Export File failed: " << OutFileName << "\n";
    errs() << "[mc2yml] Reason: " << ErrorInfo;
    return false;
  }
  else {
    Output = new yaml::Output(OutFile->os());
  }

  for (ExportList::iterator it = Exporters.begin(), ie = Exporters.end();
       it != ie; ++it)
  {
    (*it)->finalize(M);
    (*it)->writeOutput(Output);
  }

  if (OutFile) {
    OutFile->keep();
    delete Output;
    delete OutFile;
  }

  if (!BitcodeFile.empty()) {
    std::string ErrorInfo;
    tool_output_file BitcodeStream(BitcodeFile.c_str(), ErrorInfo, 0);
    WriteBitcodeToFile(&M, BitcodeStream.os());
    if(! ErrorInfo.empty()) {
      errs() << "[mc2yml] Writing Bitcode File " << BitcodeFile << " failed: " << ErrorInfo <<" \n";
    } else {
      BitcodeStream.keep();
    }
  }
  return false;
}

void PMLModuleExportPass::addToQueue(const Module &M, MachineModuleInfo &MMI,
                                     std::string FnName)
{
  const Function *F = M.getFunction(FnName);
  if (!F) {
    errs() << "[mc2yml] Could not find function " << FnName << " in module.\n";
    return;
  }

  MachineFunction *MF = MMI.getMachineFunction(F);
  if (!MF) {
    return;
  }

  addToQueue(MF);
}

void PMLModuleExportPass::addToQueue(MachineFunction *MF) {
  if (!MF) return;

  if (FoundFunctions.find(MF) == FoundFunctions.end()) {
    Queue.push_back(MF);
    FoundFunctions.insert(MF);
  }
}

char PMLModuleExportPass::ID = 0;


/// Returns a newly-created PML export pass.
MachineModulePass *
createPMLExportPass(TargetMachine &TM, std::string& FileName, std::string& BitcodeFileName, ArrayRef<std::string> Roots)
{
  PMLModuleExportPass *PEP = new PMLModuleExportPass(TM, FileName, Roots, 0);

  PEP->addExporter( new PMLBitcodeExport(TM, *PEP) );
  PEP->addExporter( new PMLMachineExport(TM, *PEP) );
  PEP->addExporter( new PMLRelationGraphExport(TM, *PEP) );
  if (! BitcodeFileName.empty())
    PEP->writeBitcode(BitcodeFileName);
  return PEP;
}


} // end namespace llvm
