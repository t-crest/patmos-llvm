//===- PIDDA.cpp - Precise Input-Data Dependency analysis using Thinned Gated Single Assignment Form -===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//                     Using The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The purpose of this file is a simple demo of our precise input-data
// dependency analysis.
//
// It will traverse all MU-nodes, and get the module-global input-data
// dependencies.
#define DEBUG_TYPE "pidda"

#include "PHIDecisionNode.h"
#include "InputDataDependence.h"
#include "llvm/IR/Module.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include <set>

using namespace llvm;

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

//        // For all loops, process loop headers (MU deps) and compute decision blocks
//        for(po_iterator< Loop* > SubLoopI = loop_po_begin(LI), SubLoopE = loop_po_end(LI);
//                                 SubLoopI != SubLoopE; ++SubLoopI) {
//          Loop* TheLoop = *SubLoopI;
//          dbgs() << "- LOOP " << F->getName() << "." << TheLoop->getHeader()->getName() << '\n';
//
//          // All loop iteration variables are in header phi nodes
//          PHINodeList LoopVars;
//          IdDeps->getLoopVariantVars(TheLoop, LoopVars);
//          dbgs() << " - Variables defined in loop header(" << LoopVars.size() << "): " << LoopVars << '\n';
//
//          BlockList& LoopControlBlocks = IdDeps->getLoopControlBlocks(TheLoop);
//          dbgs() << " - Conditionals controlling loop iteration(" << LoopControlBlocks.size() << "):\n";
//          for(BlockList::iterator I = LoopControlBlocks.begin(),E= LoopControlBlocks.end(); I!=E; ++I) {
//              dbgs() << "  -- ";
//              (*I)->getTerminator()->print(dbgs());
//              dbgs() << " .. defined as .. ";
//              getCondition((*I)->getTerminator())->print(dbgs());
//              dbgs() << "\n";
//          }
//          if(TheLoop->getCanonicalInductionVariable()) {
//              dbgs() << " - llvm.getCanonicalInductionVariable: ";
//              TheLoop->getCanonicalInductionVariable()->print(dbgs());
//              dbgs() << '\n';
//          }
//          if(SE->hasLoopInvariantBackedgeTakenCount(TheLoop)) {
//              dbgs() << " - llvm.scalarevolution.getBackedgeTakenCount: ";
//              SE->getBackedgeTakenCount(TheLoop)->dump();
//          }
//        }

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
static RegisterPass<PIDDA> X("pidda-demo", "Precise Input-Data Dependency Analysis - Demonstration", false, false);

