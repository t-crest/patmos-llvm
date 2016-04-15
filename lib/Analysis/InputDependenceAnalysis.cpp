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
                      "Input Data Dependence Analysis Pass", true, true)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_END(InputDependenceAnalysis, "pidda",
                      "Input Data Dependence Analysis Pass", true, true)

// Debugging
namespace {

Value* getBranchCondition(Instruction *Ins) {
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

unsigned getSuccessorIndex(BasicBlock* BB, BasicBlock* Succ) {
  for(unsigned I = 0, E = BB->getTerminator()->getNumSuccessors(); I!=E; ++I) {
    if(BB->getTerminator()->getSuccessor(I) == Succ) {
        return I;
    }
  }
  assert(false && "getSuccessorIndex: No Index Found");
}

}

// Process Header, adding MU dependencies
bool InputDependenceAnalysis::processLoop(Loop *L) {
  BasicBlock* Header = L->getHeader();

  // Check canonical form of reducible loop
  if(! Header) return loopError(Header, "No header block");
  if(! L->getLoopPreheader()) return loopError(Header,"No preheader block");
  else if(! L->getLoopLatch()) return loopError(Header,"No latch block");
  else if(! L->hasDedicatedExits()) return loopError(Header, "No dedicated exit blocks");

  ExitBlocks.insert(std::make_pair(L, BlockList()));
  L->getUniqueExitBlocks(ExitBlocks[L]);
  array_pod_sort(ExitBlocks[L].begin(), ExitBlocks[L].end());

  LoopDecisionBlocks.insert(std::make_pair(L, BlockList()));
  getLoopDecisionBlocks(L, LoopDecisionBlocks[L]);
  array_pod_sort(LoopDecisionBlocks[L].begin(), LoopDecisionBlocks[L].end());

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

// Get loop decision blocks (branches that decide whether the loop is exited, or another
// loop iteration is executed)
void InputDependenceAnalysis::getLoopDecisionBlocks(Loop *L, BlockList& DecisionBlocks) {
  PHIDecisionMap DecisionMap;
  computeLoopDecision(L, DecisionMap);
  LoopSelectors[L]->getControlDependencies(DecisionBlocks);
}

// First, add eta deps.
// Then, in reverse topological order up to the dominator of the phi node,
// propagate phi selectors along edges.
// Compute phi selector from the propagated reaching def info
// Add GAMMA dependencies extracted from the phi selector
bool InputDependenceAnalysis::processPHINode(PHINode* PN, BasicBlock* BB)  {

  assert(!LI->isLoopHeader(BB) && "processPHINode called on a loop header block");

  EtaDeps.insert(std::make_pair(PN, BlockList()));
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

// Compute decision diagram for the reaching definitions of phi node PN;
// IDOM is the immediate dominator of the PHI node; results are stored in
// SelectorMap
//
// TODO: getSuccessorIndex could become a performance bottleneck
void InputDependenceAnalysis::computePHIDecision(PHINode *PN, BasicBlock *IDOM, PHIDecisionMap& SelectorMap) {
  BlockList Worklist;
  
  // DEBUG(dbgs() << "Compute PHI selector for ");
  // DEBUG(PN->print(dbgs()));
  // DEBUG(dbgs() << "\n");
  
  // Initialize phi selectors for direct predecessors
  for(unsigned i = 0; i < PN->getNumIncomingValues (); ++i) {
    BasicBlock* BB = PN->getIncomingBlock(i);
    Worklist.push_back(BB);

    PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(BB));
    SelectorMap.insert(std::make_pair(BB, NewNode));

    PHIDecisionNode::Ptr InitialReachingDef(new PHIDecisionNode(PN->getIncomingValue(i), true));
    SelectorMap[BB]->setChildSelector(getSuccessorIndex(BB,PN->getParent()),InitialReachingDef);

    // DEBUG(dbgs() << "Initial Decision DAG at Predecessor:\n");
    // DEBUG(SelectorMap[BB]->print(dbgs(),2));
    // DEBUG(dbgs() << "\n");
  }

  buildDecisionDAG(Worklist, IDOM, SelectorMap);

  Selectors[PN] = SelectorMap[IDOM];

  DEBUG(dbgs() << "PHI selector for ");
  DEBUG(PN->print(dbgs()));
  DEBUG(dbgs() << "\n");
  DEBUG(Selectors[PN]->print(dbgs(),0));
  DEBUG(dbgs() << "\n");
}

// compute loop decision diagram
void InputDependenceAnalysis::computeLoopDecision(Loop *L, PHIDecisionMap& SelectorMap) {
  BlockList Worklist;
  
  // DEBUG(dbgs() << "Compute loop selector for ");
  // DEBUG(L->print(dbgs()));
  // DEBUG(dbgs() << "\n");
  
  // Initialize phi selectors for exit blocks
  for(BlockList::iterator I = ExitBlocks[L].begin(), E = ExitBlocks[L].end();
      I!=E; ++I) {
    BasicBlock* BB = *I;
    Worklist.push_back(BB);

    PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(BB));
    SelectorMap.insert(std::make_pair(BB, NewNode));

    ConstantInt *TrueValue = ConstantInt::getTrue(BB->getContext());
    PHIDecisionNode::Ptr ExitLeaf(new PHIDecisionNode(TrueValue,true));
    SelectorMap[BB]->setChildSelector(0,ExitLeaf);

    // DEBUG(dbgs() << "Initial Decision DAG at Exit Block:\n");
    // DEBUG(SelectorMap[BB]->print(dbgs(),2));
    // DEBUG(dbgs() << "\n");
  }
  // Initialize phi selectors for latch
  BasicBlock *Latch = L->getLoopLatch();
  Worklist.push_back(Latch);
  PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(Latch));
  SelectorMap.insert(std::make_pair(Latch, NewNode));
  ConstantInt *FalseValue = ConstantInt::getFalse(Latch->getContext());
  PHIDecisionNode::Ptr LatchLeaf(new PHIDecisionNode(FalseValue,true));
  SelectorMap[Latch]->setChildSelector(0,LatchLeaf);

  buildDecisionDAG(Worklist, L->getHeader(), SelectorMap);

  LoopSelectors[L] = SelectorMap[L->getHeader()];

  DEBUG(dbgs() << "Loop selector for ");
  DEBUG(L->print(dbgs()));
  DEBUG(dbgs() << "\n");
  DEBUG(LoopSelectors[L]->print(dbgs(),0));
  DEBUG(dbgs() << "\n");
}


void InputDependenceAnalysis::buildDecisionDAG(BlockList& Worklist, BasicBlock *IDOM,
					       PHIDecisionMap& SelectorMap) {
  BlockList Topolist;
  DenseSet<BasicBlock*> Visited, Finished;
  // Create a topologically sorted list of all nodes reachable from the PHI node
  // in the reverse forward CFG upto the immediate dominator of the PHI Node
  // Topological sort using Depth-First Earch (Tarjan, Cormen)
  while(! Worklist.empty()) {
    BasicBlock *BB = Worklist.back();

    // The IDOM of the PHI node (and any predecessors) do not influence which
    // definition reaches the PHI node
    if(BB == IDOM) {
      Visited.insert(BB);
    }

    if(Visited.count(BB) == 0) {
      // On the first visit, push all predecessor, except latches
      Visited.insert(BB);
      for (pred_iterator PI = pred_begin(BB), PE = pred_end(BB); PI != PE; ++PI) {
	if(Visited.count(*PI) == 0) {
	  Loop* PredLoop = LI->getLoopFor(*PI);
	  // Ignore Latches
	  if(PredLoop && PredLoop->getHeader() == BB && PredLoop->getLoopLatch() == *PI)
	    continue; 
	  Worklist.push_back(*PI);
	}
      }
    } else {
      // On the second visit, add to topologically-sorted list
      Worklist.pop_back();
      if (Finished.count(BB) == 0) { // Second visit: Push on topological list
	Finished.insert(BB);
	PHIDecisionNode::Ptr NewNode(new PHIDecisionNode(BB));
	SelectorMap.insert(std::make_pair(BB, NewNode));
	Topolist.push_back(BB);
      }
    }
  }
  std::reverse(Topolist.begin(), Topolist.end());

  //     Compute block.phiselnode from (succblock, value) \in block.phisel
  //     For all predessors set predlbock.phisel[block] = block.phiselnode
  for(BlockList::iterator BBI = Topolist.begin(), BBE = Topolist.end(); BBI != BBE; ++BBI) {
    BasicBlock* BB = *BBI;

    // DEBUG(dbgs() << "Simplify PHI Decision Node for " << BB->getName() << '\n');
    // DEBUG(dbgs() << "Initial Decision DAG:\n");
    // DEBUG(SelectorMap[BB]->print(dbgs(),2));
    // DEBUG(dbgs() << '\n');

    // Simplify this decision node: if the decision nodes of all successors are equivalent, replace
    // the decision node of this block with one of the successors' decision node
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
      // DEBUG(dbgs() << "All child nodes lead to the same reaching definition\n");
      PSN = (SelectorMap[BB] = Propagated);
    }
      
    // If we reached the immediate dominator, we're done
    if(BB == IDOM) break;
      
    // for all predcessors BB' of BB (except loop latch): 
    //  BB'.successors[i] == BB <=> decisionNode(BB').selector[i] = decsisionNode(BB)
    for (pred_iterator PI = pred_begin(BB), PE = pred_end(BB); PI != PE; ++PI) {
      BasicBlock* PredBB = *PI;
      Loop* PredLoop = LI->getLoopFor(*PI);
      if(PredLoop && PredLoop->getLoopLatch() == *PI) continue; // Ignore Latches
      SelectorMap[PredBB]->setChildSelector(getSuccessorIndex(PredBB,BB), PSN);
    }
  }
}

void InputDependenceAnalysis::dump(const Function& F, raw_ostream& O) {
  for(Function::const_iterator BBI = F.begin(); BBI != F.end(); ++BBI) {
    const BasicBlock* _BB = &*BBI;
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



/* DEMO */
namespace {

  // PIDDA - Precise Input-Data Dependency Analysis - DEMO
  struct PIDDA : public ModulePass {
    static char ID; // Pass identification, replacement for typeid
    PIDDA() : ModulePass(ID) {}

     virtual void getAnalysisUsage(AnalysisUsage &AU) const {
       AU.addRequired<DominatorTreeWrapperPass>();
       AU.addRequired<LoopInfoWrapperPass>();
       AU.addRequired<ScalarEvolutionWrapperPass>();
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

        InputDependenceAnalysis* IdDeps = &getAnalysis<InputDependenceAnalysis>(*F);
        LoopInfo* LI = &getAnalysis<LoopInfoWrapperPass>(*F).getLoopInfo();
        ScalarEvolution* SE = &getAnalysis<ScalarEvolutionWrapperPass>(*F).getSE();

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
            }
        }
	
        dependencyClosure(&*F, DepClosure);
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
    void dependencyClosure(Function *F, std::map<Value*, std::set<Value*> > &DepClosure) {
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
    }

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

