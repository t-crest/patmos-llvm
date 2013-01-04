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
#include "llvm/Support/CFG.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/YAMLExport.h"
#include "llvm/Support/YAMLTraits.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Target/TargetInstrInfo.h"

using namespace llvm;

/// Unfortunately, the interface for accessing successors differs
/// between machine block and bitcode block, therefore we need tis
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
}
}

namespace {

/// YAMLExportPass - This is a pass to export a machine function to
/// YAML (using the PML schema define at (TODO: cite report))
struct YAMLExportPass : public MachineFunctionPass {

  static char ID;

  TargetMachine *TM;
  std::string OutFileName;
  tool_output_file *OutFile;
  yaml::Output *Output;

  // Information for the current function
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
         errs() << "[mc2yml] Opening Export File failed: " << OutFileName << "\n";
         errs() << "[mc2yml] Reason: " << ErrorInfo;
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
     YAMLDoc YDoc("pml-0.1");
     LI = &getAnalysis<MachineLoopInfo>();
     serializeMachineFunction(MF, YDoc);
     serializeFunction(MF.getFunction(), YDoc);
     serializeRelationGraph(MF, YDoc);
     *Output << YDoc;
     return false;
  }

  void serializeMachineFunction(MachineFunction &MF, YAMLDoc& doc) {
        yaml::GenericArchitecture::MachineFunction *F = new yaml::GenericArchitecture::MachineFunction(MF.getFunctionNumber());
        F->MapsTo = yaml::Name(MF.getFunction()->getName());
        F->Level  = yaml::level_machinecode;
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
                for(MachineInstr::const_mop_iterator Op = Ins->operands_begin(), E = Ins->operands_end(); Op != E; ++Op) {
                  if(Op->isGlobal()) {
                    I->addCallee(Op->getGlobal()->getName());
                  } else if(Op->isSymbol()) {
                    I->addCallee(StringRef(Op->getSymbolName()));
                  }
                }
                if(! I->hasCallees()) {
                  errs() << "[mc2yml] Warning: no known callee for MC instruction\n";
                  I->addCallee(StringRef("__any__"));
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
    Function *BF = const_cast<Function*>(const_function);
    yaml::BitcodeFunction *F = new yaml::BitcodeFunction(BF->getName());

    F->Level = yaml::level_bitcode;
    yaml::BitcodeBlock *B;
    for(Function::iterator BI = BF->begin(), BE= BF->end(); BI != BE; ++BI) {
      B = F->addBlock(new yaml::BitcodeBlock(BI->getName()));
        /// B->MapsTo = (maybe C-source debug info?)
        for (pred_iterator PI = pred_begin(&*BI), PE = pred_end(&*BI); PI != PE; ++PI) {
          B->Predecessors.push_back(yaml::Name((*PI)->getName()));
        }
        for (succ_iterator SI = succ_begin(&*BI), SE = succ_end(&*BI); SI != SE; ++SI) {
          B->Successors.push_back(yaml::Name((*SI)->getName()));
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
                    I->addCallee(StringRef("__any__"));
                }
            }
        }
    }
    // TODO: we do not compute a hash yet
    F->Hash = StringRef("0");
    doc.addFunction(F);
  }

private:
  /// RelationGraph utility class to manage maps from Blocks to Predecessor Lists
  template <typename Block>
  class EventQueue {
    typedef std::vector<yaml::RelationNode*> PredList;
    std::map<Block, PredList* > KeyedQueues;
  public:
    ~EventQueue() {
      for(iterator I = KeyedQueues.begin(), E = KeyedQueues.end(); I!=E; ++I) {
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
    typedef typename std::map<StringRef, EventQueue<Block>*>::iterator iterator;
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

public:
  /// Build the Control-Flow Relation Graph connection machine code and bitcode
  void serializeRelationGraph(MachineFunction &MF, YAMLDoc &doc) {
    Function *BF = const_cast<Function*>(MF.getFunction());
    if(! BF) return;
    /// Create Graph
    yaml::RelationScope *DstScope = new yaml::RelationScope(yaml::Name(MF.getFunctionNumber()), yaml::level_machinecode);
    yaml::RelationScope *SrcScope = new yaml::RelationScope(yaml::Name(BF->getName()), yaml::level_bitcode);
    yaml::RelationGraph *RG = new yaml::RelationGraph(SrcScope,DstScope);
    RG->getEntryNode()->setSrcBlock(yaml::FlowGraphTrait<const BasicBlock>::getName(&BF->getEntryBlock()));
    RG->getEntryNode()->setDstBlock(yaml::FlowGraphTrait<MachineBasicBlock>::getName(&MF.front()));

    /// Event Maps
    EventMap<const BasicBlock*>::type  IEventMap;
    EventMap<MachineBasicBlock*>::type MEventMap;
    buildEventMaps(MF, IEventMap, MEventMap);

    /// Known and visited relation nodes
    std::map< ProgressID , yaml::RelationNode*> RMap;
    std::set< yaml::RelationNode* > RVisited;
    std::vector< std::pair< ProgressID, yaml::RelationNode*> > RTodo;

    /// We first queue the entry node
    RTodo.push_back( std::make_pair( std::make_pair(&BF->getEntryBlock(), &MF.front()),
                                     RG->getEntryNode()) );
    /// while there is an unprocessed progress node (n -> IBB,MBB)
    while(! RTodo.empty() ) {
      std::pair < ProgressID, yaml::RelationNode* > Item = RTodo.back();
      RTodo.pop_back();
      yaml::RelationNode *RN = Item.second;
      if(RVisited.count(RN) > 0) continue;
      const BasicBlock *IBB = Item.first.first;
      MachineBasicBlock *MBB = Item.first.second;
      EventQueueMap<const BasicBlock*> IEvents;
      EventQueueMap<MachineBasicBlock*> MEvents;

      /// Expand both at the bitcode and machine level (starting with IBB and MBB, resp.),
      /// which results in new src/dst nodes being created, and two bitcode and machinecode-level maps from events
      /// to a list of (bitcode/machine block, list of RG predecessor blocks) pairs
      expandProgressNode(RG, RN, yaml::rnt_src, IBB, IEventMap, IEvents);
      expandProgressNode(RG, RN, yaml::rnt_dst, MBB, MEventMap, MEvents);

      /// For each event and corresponding bitcode list IList and machinecode MList, create a progress
      /// node (iblock,mblock) for every pair ((iblock,ipreds),(mblock,mpreds)) \in (IList x MList) and add
      /// edges from all ipreds and mpreds to that progress node
      addProgressNodes(RG, IEvents, MEvents, RMap, RTodo);
    }
    doc.addRelationGraph(RG);
  }

private:

  /// Generate (heuristic) MachineBlock->EventName and IR-Block->EventName maps
  /// (1) if all forward-CFG predecessors of (MBB originating from BB) map to no or a different IR block,
  ///     MBB generates a BB event.
  /// (2) if there is a MBB generating a event BB, the basic block BB also generates this event
  void buildEventMaps(MachineFunction &MF,   std::map<const BasicBlock*,StringRef> &BitcodeEventMap,
                      std::map<MachineBasicBlock*,StringRef> &MachineEventMap) {
    BitcodeEventMap.clear();
    MachineEventMap.clear();
    for (MachineFunction::iterator BlockI = MF.begin(), BlockE = MF.end(); BlockI != BlockE; ++BlockI) {
      /// Check predecessors, ignoring backedges, unmapped nodes and nodes mapped to the entry node
      if(! BlockI->getBasicBlock()) continue;
      if(BlockI->getBasicBlock() == BlockI->getBasicBlock()->getParent()->begin()) continue;
      bool IsSubNode = false;
      for (MachineBasicBlock::const_pred_iterator PredI = BlockI->pred_begin(), PredE = BlockI->pred_end(); PredI!=PredE; ++PredI) {
        if(isBackEdge(*PredI,BlockI))
          continue;
        if((*PredI)->getBasicBlock() == BlockI->getBasicBlock())
          IsSubNode = true;
      }
      if(IsSubNode) continue;
      StringRef Event = BlockI->getBasicBlock()->getName();
      MachineEventMap.insert( std::make_pair(BlockI,Event) );
      BitcodeEventMap.insert( std::make_pair(BlockI->getBasicBlock(),Event) );
    }
    // errs() << "EventMaps Bitcode\n";
    // for(std::map<const BasicBlock*,StringRef>::iterator I = BitcodeEventMap.begin(), E = BitcodeEventMap.end();I!=E;++I) {
    //   errs() << I->first->getName() << "," << I->second << "\n";
    // }
    // errs() << "EventMaps Machinecode\n";
    // for(std::map<MachineBasicBlock*,StringRef>::iterator I = MachineEventMap.begin(), E = MachineEventMap.end();I!=E;++I) {
    //   errs() << I->first->getNumber();
    //   if(const BasicBlock *BB = I->first->getBasicBlock()) {
    //     errs() << "," << BB->getName();
    //   }
    //   errs() << "," << I->second << "\n";
    // }
  }

  /// Expand progress node N either at the machine code (Block=MachineBasicBlock) or bitcode (Block=BasicBlock)
  /// level. The RelationGraphHelperTrait<BlockType> class provides the machine/bitcode specific
  /// functionality
  template <typename Block>
  void expandProgressNode(yaml::RelationGraph *RG, yaml::RelationNode *ProgressNode, yaml::RelationNodeType type,
                          Block* StartBlock, typename EventMap<Block*>::type EventMap, EventQueueMap<Block*>& Events) {
    typedef yaml::FlowGraphTrait<Block> Trait;
    std::vector< std::pair< yaml::RelationNode*, Block* > > Queue;
    std::map< Block*, yaml::RelationNode* > Created;
    std::set< Block* > Visited;
    Queue.push_back( std::make_pair(ProgressNode, StartBlock) );
    while(! Queue.empty()) {
      // expand unexpanded, queued items
      std::pair< yaml::RelationNode*, Block* > Item = Queue.back();
      Queue.pop_back();
      yaml::RelationNode* RN = Item.first;
      Block* BB = Item.second;
      if(Visited.count(BB) > 0) continue;
      Visited.insert(BB);
      for(typename Trait::succ_iterator I = Trait::succ_begin(BB), E = Trait::succ_end(BB); I!=E; ++I) {
        Block *BB2 = *I;
        if(EventMap.count(BB2) == 0) {
          // successor generates no event -> add 'type' relation node, queue successor
          if(Created.count(BB2) == 0) {
            DEBUG(dbgs() << "Internal node for " << Trait::getName(BB2).getName() << "("
                  << ((type==yaml::rnt_src) ? "src" : "dst") << ") created\n");
            yaml::RelationNode *NewRelationNode = RG->addNode(type);
            NewRelationNode->setBlock(Trait::getName(BB2), type == yaml::rnt_src);
            Created.insert( std::make_pair (BB2,NewRelationNode) );
          }
          yaml::RelationNode *RN2 = Created[BB2];
          RN->addSuccessor(RN2, type == yaml::rnt_src);
          Queue.push_back( std::make_pair(RN2,BB2) );
        } else {
          // successor generates event -> queue event
          Events.addItem(EventMap[BB2], BB2, RN);
        }
      }
      // No successors -> exit event
      if(Trait::succ_begin(BB) == Trait::succ_end(BB)) {
        Events.addExitPredecessor(RN);
      }
    }
  }

  /// Add progress nodes after expanding the bitcode and machine code subgraphs
  void addProgressNodes(yaml::RelationGraph *RG,
                        EventQueueMap<const BasicBlock*> &BitcodeEvents,
                        EventQueueMap<MachineBasicBlock*> &MachineEvents,
                        std::map< ProgressID , yaml::RelationNode*>& RMap,
                        std::vector< std::pair< ProgressID, yaml::RelationNode*> >& RTodo) {
    for(EventQueueMap<MachineBasicBlock*>::iterator I = MachineEvents.begin(), E = MachineEvents.end();I!=E;++I) {
      const StringRef& Event = I->first;
      EventQueue<MachineBasicBlock*>* MQueue = I->second;
      EventQueue<const BasicBlock*>* IQueue = BitcodeEvents.remove(Event);
      if(IQueue == 0) {
        errs() << "[mc2yml] Inconsistent event mapping for relation graph: unmapped machinecode events\n";
        continue;
      }
      for(EventQueue<MachineBasicBlock*>::iterator MQI = MQueue->begin(), MQE = MQueue->end(); MQI!=MQE;++MQI) {
        for(EventQueue<const BasicBlock*>::iterator IQI = IQueue->begin(), IQE = IQueue->end(); IQI!=IQE;++IQI) {
          yaml::RelationNode *RN;
          ProgressID PNID(IQI->first, MQI->first);
          if(RMap.count(PNID) == 0) {
            // create progress node (MBlock, IBlock)
            if(! IQI->first) {
            } else {
              RN = RG->addNode(yaml::rnt_progress);
              RN->setSrcBlock(yaml::Name(IQI->first->getName()));
              RN->setDstBlock(yaml::Name(MQI->first->getNumber()));
              RMap.insert(std::make_pair(PNID,RN));
              RTodo.push_back(std::make_pair(PNID,RN));
            }
          }
          RN = RMap[PNID];
          // connect MPreds and IPreds to progress node
          std::vector<yaml::RelationNode*> *MPreds = MQI->second, *IPreds = IQI->second;
          for(std::vector<yaml::RelationNode*>::iterator PI = MPreds->begin(), PE = MPreds->end(); PI!=PE;++PI)
            (*PI)->addSuccessor(RN,false);
          for(std::vector<yaml::RelationNode*>::iterator PI = IPreds->begin(), PE = IPreds->end(); PI!=PE;++PI)
            (*PI)->addSuccessor(RN,true);
        }
      }
      delete IQueue;
    }
    yaml::RelationNode *RN = RG->getExitNode();
    for(std::vector<yaml::RelationNode*>::iterator PI = BitcodeEvents.getExitPredecessors().begin(),
          PE = BitcodeEvents.getExitPredecessors().end(); PI!=PE; ++PI)
      (*PI)->addSuccessor(RN,false);
    for(std::vector<yaml::RelationNode*>::iterator PI = MachineEvents.getExitPredecessors().begin(),
          PE = MachineEvents.getExitPredecessors().end(); PI!=PE; ++PI)
      (*PI)->addSuccessor(RN,true);
    if( BitcodeEvents.begin() != BitcodeEvents.end() ) {
      errs() << "[mc2yml] Inconsistent relation graph: unmapped bitcode events\n";
    }
    if( BitcodeEvents.hasExitPredecessors() != MachineEvents.hasExitPredecessors() ) {
      errs() << "[mc2yml] Inconsistent relation graph: unmatched exit event\n";
    }
  }


  /// Check whether Source -> Target is a backedge
  bool isBackEdge(MachineBasicBlock *Source, MachineBasicBlock *Target) {
    if(! LI->isLoopHeader(Target)) return false;
    MachineLoop *HeaderLoop  = LI->getLoopFor(Target);
    MachineLoop *LatchLoop = LI->getLoopFor(Source);
    if(! LatchLoop) return false;
    while(LatchLoop->getLoopDepth() > HeaderLoop->getLoopDepth())
      LatchLoop = LatchLoop->getParentLoop();
    return (LatchLoop==HeaderLoop);
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
