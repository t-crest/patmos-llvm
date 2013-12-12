//===- InputDataDependence.cpp - Dependency analysis using Thinned Gated Single Assignment Form -===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//                     Using The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "pidda"

#include "llvm/Analysis/InputDependenceAnalysis.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/Module.h"
#include <set>

using namespace llvm;

STATISTIC(MuNodes,  "Number of MU nodes identified");
STATISTIC(EtaNodes,  "Number of ETA nodes identified");
STATISTIC(NumGammaDeps,  "Number of GAMMA dependencies identified");

char InputDependenceAnalysis::ID = 0;

INITIALIZE_PASS_BEGIN(InputDependenceAnalysis, "pidda",
                      "Input Data Dependence Analysis Pass", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfo)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolution)
INITIALIZE_AG_DEPENDENCY(AliasAnalysis)
INITIALIZE_PASS_END(InputDependenceAnalysis, "pidda",
                      "Input Data Dependence Analysis Pass", false, false)

// Debugging
namespace llvm {

template class LoopInfoBase<BasicBlock, Loop>;

template<typename List>
raw_ostream& osAppendList(raw_ostream &O,
                                    const List &L) {
  O << "[";

  for (typename List::const_iterator B = L.begin(),
         E = L.end(); B != E; ) {
    O << "'" << (*B)->getName() << "'";
    if (++B != E) O << ", ";
  }

  return O << "]";
}
template<typename List>
raw_ostream& osAppendList(raw_ostream &O,const List &L);
raw_ostream& operator<<(raw_ostream &O, const BlockList &L) {
    return osAppendList(O,L);
}
raw_ostream& operator<<(raw_ostream &O, const PHINodeList &L) {
    return osAppendList(O,L);
}
}

static unsigned getSuccessorIndex(BasicBlock* BB, BasicBlock* Succ);
static inline Value* getBranchCondition(Instruction *Ins);

// Process Header, adding MU dependencies
bool InputDependenceAnalysis::processLoop(Loop *L) {
  BasicBlock* Header = L->getHeader();

  if(! L->getLoopPreheader()) return loopError(Header,"No preheader block");
  else if(! L->getLoopLatch()) return loopError(Header,"No latch block");
  else if(! L->hasDedicatedExits()) return loopError(Header, "No dedicated exit blocks");

  LoopDecisionBlocks.insert(LoopBlocksMap::value_type(L, BlockList()));
  computeDecisionBlocks(L, LoopDecisionBlocks[L]);
  array_pod_sort(LoopDecisionBlocks[L].begin(), LoopDecisionBlocks[L].end());

  ExitBlocks.insert(LoopBlocksMap::value_type(L, BlockList()));
  L->getUniqueExitBlocks(ExitBlocks[L]);
  array_pod_sort(ExitBlocks[L].begin(), ExitBlocks[L].end());

  // Dump Exit Blocks and DecisionBlocks
  DEBUG(dbgs() << "Exit Blocks of " << L->getHeader()->getName() << ": "
        << ExitBlocks[L] << "\n");
  DEBUG(dbgs() << "Decision Blocks of " << L->getHeader()->getName() << ": "
        << LoopDecisionBlocks[L] << "\n");

  // Count Mu Nodes
  PHINodeList LoopVars;
  getLoopVariantVars(L,LoopVars);
  MuNodes += LoopVars.size();

  return true;
}

// Get decision blocks (one successor outside, one inside) for a loop
void InputDependenceAnalysis::computeDecisionBlocks(Loop *L, SmallVectorImpl<BasicBlock*>& DecisionBlocks) {
  for(Loop::block_iterator BBI = L->block_begin(); BBI != L->block_end(); ++BBI) {
    BasicBlock* BB = *BBI;

    unsigned SuccsOutOfLoop = 0;
    unsigned SuccsInLoop = 0;
    for (succ_iterator PI = succ_begin(BB), E = succ_end(BB); PI != E; ++PI) {
       if (!L->contains(*PI)) ++SuccsOutOfLoop;
       else                   ++SuccsInLoop;
    }
    if(SuccsOutOfLoop > 0 && SuccsInLoop > 0) {
      DecisionBlocks.push_back(BB);
    }
  }
}

// First, add eta deps.
// Then, in reverse topological order up to the dominator of the phi node,
// propagate phi selectors along edges.
// Compute phi selector from the propagated reaching def info
// Add GAMMA dependencies extracted from the phi selector
bool InputDependenceAnalysis::processPHINode(PHINode* PN, BasicBlock* BB)  {

  assert(!LI->isLoopHeader(BB) && "processPHINode called on a loop header block");

  EtaDeps.insert(PHIBlocksMap::value_type(PN, BlockList()));
  // Eta deps
  bool HasEtaDep = false;
  DenseSet<Loop*> Visited;
  for(unsigned i = 0; i < PN->getNumIncomingValues (); ++i) {
    BasicBlock* Pred = PN->getIncomingBlock(i);
    Loop* PredLoop = LI->getLoopFor(Pred);
    if(Visited.count(PredLoop) > 0) {
      continue;
    } else {
      Visited.insert(PredLoop);
    }
    if(PredLoop && ! PredLoop->contains(BB)) { // Exit node, eta dep
      HasEtaDep = true;
      EtaDeps[PN].insert(EtaDeps[PN].begin(), LoopDecisionBlocks[PredLoop].begin(), LoopDecisionBlocks[PredLoop].end());
    }
  }
  if(HasEtaDep) ++EtaNodes;

  // Then, in reverse topological order up to the dominator of the phi node,
  // propagate phi selectors along edges.

  // Get Dominator
  if(! DT->getNode(BB)) {
      report_fatal_error("Non-header block with PHI-nodes but no dominator. Maybe dead code? Consider running -simplifycfg");
  }
  BasicBlock* IDOM = DT->getNode(BB)->getIDom()->getBlock();

  // propagate phi selectors (using white/grey reverse dfs)
  PHIDecisionMap SelectorMap;
  computePHIDecision(PN, IDOM, SelectorMap);
  return true;
}

// TODO: getSuccessorIndex could become a performance bottleneck
void InputDependenceAnalysis::computePHIDecision(PHINode *PN, BasicBlock *IDOM, PHIDecisionMap& SelectorMap) {
    BlockList Worklist, Topolist;
    DenseSet<BasicBlock*> Visited, Finished;

    InputDependenceAnalysis::dbgs() << "Compute PHI selector for ";
    PN->print(InputDependenceAnalysis::dbgs());
    InputDependenceAnalysis::dbgs() << "\n";

    // Initialize phi selectors for direct predecessors
    for(unsigned i = 0; i < PN->getNumIncomingValues (); ++i) {
      BasicBlock* BB = PN->getIncomingBlock(i);
      Worklist.push_back(BB);

      PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(BB));
      SelectorMap.insert(PHIDecisionMap::value_type(BB, NewNode));

      PHIDecisionNode::Ptr InitialReachingDef(new PHIDecisionNode(PN->getIncomingValue(i), true));
      SelectorMap[BB]->setChildSelector(getSuccessorIndex(BB,PN->getParent()),InitialReachingDef);

//      InputDependenceAnalysis::dbgs() << "Initial Decision DAG at Predecessor:\n";
//      SelectorMap[BB]->print(InputDependenceAnalysis::dbgs(),2);
//      InputDependenceAnalysis::dbgs() << '\n';
    }

    // DFS -> TOPO
    // FIXME: It would be nicer to use GraphTraits, but trait on BB will not respect
    //        that we should not follow latches using OrderIterator.
    while(! Worklist.empty()) {
      BasicBlock *BB = Worklist.back();

      // InputDependenceAnalysis::dbgs() << "Visiting " << BB->getName() << " with count " << Visited.count(BB) << "\n";
      // InputDependenceAnalysis::dbgs() << "Worklist: " << Worklist << '\n';
      if(BB == IDOM) {
        Visited.insert(BB);
      }
      if(Visited.count(BB) == 0) { // First visit: Push all predecessors, except latches
        Visited.insert(BB);
        for (pred_iterator PI = pred_begin(BB), PE = pred_end(BB); PI != PE; ++PI) {
          if(Visited.count(*PI) == 0) {
            Loop* PredLoop = LI->getLoopFor(*PI);
            if(PredLoop && PredLoop->getHeader() == BB && PredLoop->getLoopLatch() == *PI) {
                // InputDependenceAnalysis::dbgs() << "Ignoring Edge from Latch to Header: " << (*PI)->getName() << '\n';
                continue; // Ignore Latches
            }

            // InputDependenceAnalysis::dbgs() << "Add to worklist: " << (*PI)->getName() << '\n';
            Worklist.push_back(*PI);
          }
        }
      } else {
          Worklist.pop_back();
          if (Finished.count(BB) == 0) { // Second visit: Push on topological list
              Finished.insert(BB);
              PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(BB));
              SelectorMap.insert(PHIDecisionMap::value_type(BB, NewNode));
              Topolist.push_back(BB);
              // InputDependenceAnalysis::dbgs() << "Add to topo " << BB->getName() << '\n';
          }
      }
    }
    std::reverse(Topolist.begin(), Topolist.end());
    //     Compute block.phiselnode from (succblock, value) \in block.phisel
    //     For all predessors set predlbock.phisel[block] = block.phiselnode
    for(BlockList::iterator BBI = Topolist.begin(), BBE = Topolist.end(); BBI != BBE; ++BBI) {
      BasicBlock* BB = *BBI;

      InputDependenceAnalysis::dbgs() << "Simplify PHI Decision Node for " << BB->getName() << '\n';
      InputDependenceAnalysis::dbgs() << "Initial Decision DAG:\n";
      SelectorMap[BB]->print(InputDependenceAnalysis::dbgs(),2);
      InputDependenceAnalysis::dbgs() << '\n';

      // Try to simplify phiselnode
      PHIDecisionNode::Ptr PSN = SelectorMap[BB];
      PHIDecisionNode::Ptr Propagated;
      for(PHIDecisionNode::iterator PSNI = PSN->begin(), PSNE = PSN->end(); PSNI != PSNE; ++PSNI) {
        PHIDecisionNode::Ptr ChildNode = *PSNI;
        if(ChildNode) {
          if(! Propagated) {
            Propagated = ChildNode;
          } else if(*Propagated != *ChildNode) {
            Propagated = 0;
            break;
          } else {
            // Equivalent definition
          }
        }
      }
      if(Propagated) {
        PSN = (SelectorMap[BB] = Propagated);
      }

      // Propagate phiselnode
      // For inner loops, we ignore latch nodes
      if(BB == IDOM) break;
      for (pred_iterator PI = pred_begin(BB), PE = pred_end(BB); PI != PE; ++PI) {
       BasicBlock* PredBB = *PI;
       Loop* PredLoop = LI->getLoopFor(*PI);
       if(PredLoop && PredLoop->getLoopLatch() == *PI) continue; // Ignore Latches

//       InputDependenceAnalysis::dbgs() << "Set PHI Selector " << PredBB->getName() << "/"
//                           << getSuccessorIndex(PredBB,BB) << " = ";
//       PSN->print(InputDependenceAnalysis::dbgs());
//       InputDependenceAnalysis::dbgs() << '\n';

       SelectorMap[PredBB]->setChildSelector(getSuccessorIndex(PredBB,BB), PSN);
      }
    }
    Selectors[PN] = SelectorMap[IDOM];

    InputDependenceAnalysis::dbgs() << "PHI selector for ";
    PN->print(InputDependenceAnalysis::dbgs());
    InputDependenceAnalysis::dbgs() << "\n";
    Selectors[PN]->print(InputDependenceAnalysis::dbgs(),0);
    InputDependenceAnalysis::dbgs() << "\n";
}

void InputDependenceAnalysis::dump(const Function& F, raw_ostream& O) {
  for(Function::const_iterator BBI = F.begin(); BBI != F.end(); ++BBI) {
    const BasicBlock* _BB = BBI;
    BasicBlock* BB = const_cast<BasicBlock*>(_BB);
    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; ++I) {
      if (PHINode *PN = dyn_cast<PHINode>(I)) {
        O << "Dependencies for "; PN->dump();

        Loop *L = LI->getLoopFor(BB);

        // If the block is a header, print MU deps
        if(LI->isLoopHeader(BB)) {
          for(BlockList::iterator DBI = LoopDecisionBlocks[L].begin(), DBE = LoopDecisionBlocks[L].end();
              DBI != DBE; ++DBI) {
            O << "  (mu)  " << (*DBI)->getName() << " : ";
            getBranchCondition((*DBI)->getTerminator())->dump();
          }
          continue;
        }

        // Print eta deps
        for(BlockList::iterator EDI = EtaDeps[PN].begin(), EDE = EtaDeps[PN].end(); EDI != EDE; ++EDI) {
            O << "  (eta) " << (*EDI)->getName() << " : ";
            getBranchCondition((*EDI)->getTerminator())->dump();
        }

        // Print gamma deps
        BlockList GammaDeps;
        Selectors[PN]->getControlDependencies(GammaDeps);

        for(BlockList::iterator DBI = GammaDeps.begin(), DBE = GammaDeps.end();
        DBI != DBE; ++DBI) {
            ++NumGammaDeps;
            O << "  (gamma) " << (*DBI)->getName() << "\n";
        }
      }
    }
  }
}

inline static Value* getBranchCondition(Instruction *Ins) {
  if(BranchInst* BrIns = dyn_cast<BranchInst>(Ins)) {
    return BrIns->getCondition();
  } else if(IndirectBrInst* IBrIns = dyn_cast<IndirectBrInst>(Ins)) {
    return IBrIns->getAddress();
  } else if(SwitchInst* SwitchIns = dyn_cast<SwitchInst>(Ins)) {
    return SwitchIns->getCondition();
  } else { // No condition for invoke,return,unreachable and unwind
    return 0;
  }
}

static unsigned getSuccessorIndex(BasicBlock* BB, BasicBlock* Succ) {
  for(unsigned I = 0, E = BB->getTerminator()->getNumSuccessors(); I!=E; ++I) {
    if(BB->getTerminator()->getSuccessor(I) == Succ) {
        return I;
    }
  }
  assert(false && "getSuccessorIndex: No Index Found");
}


/* DEMO */
namespace {

  // PIDDA - Precise Input-Data Dependency Analysis
  struct PIDDA : public ModulePass {
    static char ID; // Pass identification, replacement for typeid
    PIDDA() : ModulePass(ID) {}

     virtual void getAnalysisUsage(AnalysisUsage &AU) const {
       AU.addRequired<DominatorTree>();
       AU.addRequired<LoopInfo>();
       AU.addRequired<ScalarEvolution>();
       AU.addRequired<InputDependenceAnalysis>();
     }
    static Value* getCondition(TerminatorInst* I) {
        if(BranchInst* BI = dyn_cast<BranchInst>(I)) {
            return BI->getCondition();
        } else if(SwitchInst* SI = dyn_cast<SwitchInst>(I)) {
            return SI->getCondition();
        } else {
            report_fatal_error("getCondition(TerminatorInst): not a branch or switch (unsupported)");
        }
    }

    virtual bool runOnModule(Module &M) {
      dbgs() << "* MODULE " << M.getModuleIdentifier() << "\n\n";

      for(Module::iterator F = M.begin(), FE = M.end(); F != FE; ++F) {
        if(F->isDeclaration()) continue;
        dbgs() << "\n=================================\n";
        dbgs() << "= FUNCTION " << F->getName() << "\n";
        dbgs() <<   "=================================\n\n";

        LoopInfo* LI = &getAnalysis<LoopInfo>(*F);
        InputDependenceAnalysis* IdDeps = &getAnalysis<InputDependenceAnalysis>(*F);
        ScalarEvolution* SE = &getAnalysis<ScalarEvolution>(*F);

        IdDeps->dump(*F, dbgs());

        std::map<Value*, std::set<Value*> > DepClosure;
        // Compute direct data, gamma and eta dependencies for all variables
        for(Function::iterator BI = F->begin(), BE = F->end(); BI != BE; ++BI) {
            for(BasicBlock::iterator II = BI->begin(), IE = BI->end(); II != IE; ++II) {
                Instruction* Ins = &*II;
                std::set<Value*> Dependencies;
                // Add data dependencies
                for(Instruction::op_iterator OI = Ins->op_begin(), OE = Ins->op_end(); OI != OE; ++OI) {
                    if(! isa<BasicBlock>(OI) && ! isa<ConstantInt>(OI)) {
                        Dependencies.insert(*OI);
                    }
                }
                // Add gamma and eta control dependencies, ignore mu nodes
                PHINode* PN;
                if ((PN = dyn_cast<PHINode>(Ins)) && !LI->isLoopHeader(&*BI)) {
                  BlockList EtaDeps = IdDeps->getEtaDeps(PN);
                  for(BlockList::iterator EDI = EtaDeps.begin(), EDE = EtaDeps.end(); EDI != EDE; ++EDI) {
                      dbgs () << "[pidda] Adding eta dependency to " << Ins->getName() << ": ";
                      (*EDI)->getTerminator()->print(dbgs()); dbgs() << "\n";
                      Dependencies.insert((*EDI)->getTerminator());
                  }
                  BlockList GammaDeps;
                  IdDeps->getGammaDeps(PN, GammaDeps);
                  for(BlockList::iterator GDI = GammaDeps.begin(), GDE = GammaDeps.end(); GDI != GDE; ++GDI) {
                      dbgs () << "[pidda] Adding gamma dependency to " << Ins->getName() << ": ";
                      (*GDI)->getTerminator()->print(dbgs()); dbgs() << "\n";
                      Dependencies.insert((*GDI)->getTerminator());
                  }
                } else {

                }
                DepClosure.insert(std::make_pair(Ins, Dependencies));

//                dbgs () << "* Direct dependencies for " << valueToString(Ins, true) << "\n";
//                for(std::set<Value*>::iterator DI = DepClosure[Ins].begin(), DE = DepClosure[Ins].end(); DI != DE; ++DI) {
//                    dbgs () << valueToString(Ins) << ", ";
//                }
//                dbgs () << "\n";
            }
        }
        // Compute dependency closure for all variables
        for(Function::iterator BI = F->begin(), BE = F->end(); BI != BE; ++BI) {
            for(BasicBlock::iterator II = BI->begin(), IE = BI->end(); II != IE; ++II) {
                Instruction* Ins = &*II;
                std::deque<Value*> Worklist;
                for(std::set<Value*>::iterator DI = DepClosure[Ins].begin(), DE = DepClosure[Ins].end(); DI != DE; ++DI) {
                    Worklist.push_back(*DI);
                }
                std::set<Value*> Deps;
                while (!Worklist.empty())
                {
                    Value* Dep = Worklist.front();
                    Worklist.pop_front();
                    // if(DepClosure.count(Dep) == 0) continue; // not a known instruction
                    if(Deps.count(Dep) == 0) {
                        if(! isa<TerminatorInst>(Dep)) // just add conditions/targets, not the instruction itself
                            Deps.insert(Dep);
                        for(std::set<Value*>::iterator DI = DepClosure[Dep].begin(), DE = DepClosure[Dep].end(); DI != DE; ++DI) {
                            Value* DepOfDep = *DI;
                            if(! Deps.count(DepOfDep)) {
                                Worklist.push_back(DepOfDep);
                            }
                        }
                    }
                }
                DepClosure[Ins] = Deps;
            }
        }
        dbgs () << "------------------------------------\n";
        dbgs () << "- Intraprocedural dependency closure\n";
        dbgs () << "------------------------------------\n";
        for(std::map<Value*, std::set<Value*> >::iterator DI = DepClosure.begin(), DE = DepClosure.end(); DI != DE; ++DI) {
            dbgs() << "  * Dependencies of " << valueToString(DI->first, true) << "\n";
            std::set<Value*> Deps = DI->second;
            for(std::set<Value*>::iterator SI = Deps.begin(), SE = Deps.end(); SI != SE; ++SI) {
                dbgs() << "    - " << valueToString(*SI, true) << "\n";
            }
        }
      }

      return false;
    }

  private:
    inline std::string valueToString(const Value* V, bool LongDescr = false) {
        std::string s;
        raw_string_ostream os(s);
        bool HasName = ! V->getName().empty();
        if(const Function *F = dyn_cast<Function>(V)) {
            os << "function " << F->getName();
        } else if(HasName) {
            os << "%" << V->getName();
            if(LongDescr) {
                os << " ["; V->print(os); os << " ]";
            }
        } else {
            os << " ["; V->print(os); os << " ]";
        }
        return os.str();
    }
  };
}

char PIDDA::ID = 0;
static RegisterPass<PIDDA> X("pidda-demo",
                             "Precise Input-Data Dependency Analysis - Demonstration",
                             false,
                             false);

