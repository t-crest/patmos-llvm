//==-- PatmosSinglePathInfo.cpp - Analysis Pass for SP CodeGen -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines a pass to compute imformation for single-path converting
// seleced functions.
//
//===---------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-singlepath"

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>
#include <queue>


using namespace llvm;


/// SPFuncList - Option to enable single-path conversion.
static cl::list<std::string> SPFuncList(
    "mpatmos-spconv",
    cl::value_desc("list"),
    cl::desc("A list of functions to Single-Path convert (Patmos only)"),
    cl::CommaSeparated,
    cl::Hidden);


///////////////////////////////////////////////////////////////////////////////

char PatmosSinglePathInfo::ID = 0;

/// createPatmosSinglePathInfoPass - Returns a new PatmosSinglePathInfo pass
/// \see PatmosSinglePathInfo
FunctionPass *
llvm::createPatmosSinglePathInfoPass(const PatmosTargetMachine &tm) {
  return new PatmosSinglePathInfo(tm);
}

///////////////////////////////////////////////////////////////////////////////


bool PatmosSinglePathInfo::isEnabled() {
  return !SPFuncList.empty();
}



PatmosSinglePathInfo::PatmosSinglePathInfo(const PatmosTargetMachine &tm)
  : MachineFunctionPass(ID), TM(tm),
    STC(tm.getSubtarget<PatmosSubtarget>()),
    TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())),
    Funcs(SPFuncList.begin(), SPFuncList.end())   {}


bool PatmosSinglePathInfo::doInitialization(Module &M) {
  // fill the set of functions to convert as specified on command line
  FuncsRemain.insert( Funcs.begin(), Funcs.end() );
  return false;
}


bool PatmosSinglePathInfo::doFinalization(Module &M) {
  if (!FuncsRemain.empty()) {
    DEBUG( dbgs() << "Following functions not found to "
                     "single-path convert:\n'" );
    for (std::set<std::string>::iterator it=FuncsRemain.begin();
            it!=FuncsRemain.end(); ++it) {
      DEBUG( dbgs() << *it << "' ");
    }
    DEBUG( dbgs() << '\n');
    FuncsRemain.clear();
  }
  return false;
}

void PatmosSinglePathInfo::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<MachinePostDominatorTree>();
  AU.addRequired<MachineLoopInfo>();
  AU.setPreservesAll();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool PatmosSinglePathInfo::runOnMachineFunction(MachineFunction &MF) {
  // only consider function if specified on command line
  std::string curfunc = MF.getFunction()->getName();
  if ( isToConvert(MF) ) {
    DEBUG( dbgs() << "[Single-Path] Analyze '" << curfunc << "'\n" );
    analyzeFunction(MF);
    FuncsRemain.erase(curfunc);
  }
  // didn't modify anything
  return false;
}



void PatmosSinglePathInfo::print(raw_ostream &OS, const Module *M) const {
  // TODO implement
}


#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void PatmosSinglePathInfo::dump() const {
  print(dbgs());
}
#endif





bool PatmosSinglePathInfo::isToConvert(MachineFunction &MF) const {
  return Funcs.count(MF.getFunction()->getName()) > 0;
}

void PatmosSinglePathInfo::analyzeFunction(MachineFunction &MF) {

  SPNode *Root = createSPNodeTree(MF);
  Root->dump();

  // let's see if we get the order right
  std::vector<const MachineBasicBlock *> order;
  Root->getOrder(order);
  for(unsigned i=0; i<order.size(); i++) {
    dbgs() << " | " << order[i]->getNumber() << "\n";
  }

  delete Root;

  // CD: MBB -> set of edges
  CD_map_t CD;
  computeControlDependence(MF, CD);

  // decompose CD
  K_t K;
  R_t R;
  decomposeControlDependence(MF, CD, K, R);


  // "Augment K"
  BitVector pred_initialize = computeUpwardsExposedUses(MF, K, R);
  /*
  for (unsigned i=0; i<K.size(); i++) {
    if (pred_initialize.test(i)) {
      K[i].insert( std::make_pair((MachineBasicBlock *)NULL, &MF.front()) );
    }
  }
  */

  // XXX for debugging
  //MF.viewCFGOnly();

}



// TODO Maybe an artificial entry node?
void PatmosSinglePathInfo::computeControlDependence(MachineFunction &MF,
                                                    CD_map_t &CD) const {
  // for CD, we need the Postdom-Tree
  MachinePostDominatorTree &PDT = getAnalysis<MachinePostDominatorTree>();
  assert(PDT.getRoots().size()==1 && "Function must have a single exit node!");


  DEBUG_TRACE( dbgs() << "Post-dominator tree:\n" );
  DEBUG_TRACE( PDT.print(dbgs(), MF.getFunction()->getParent()) );

  // build control dependence
  for (MachineFunction::iterator FI=MF.begin(), FE=MF.end(); FI!=FE; ++FI) {
    MachineBasicBlock *MBB = FI;
    MachineDomTreeNode *ipdom = PDT[MBB]->getIDom();

    for(MachineBasicBlock::succ_iterator SI=MBB->succ_begin(),
                                         SE=MBB->succ_end(); SI!=SE; ++SI) {
      MachineBasicBlock *SMBB = *SI;
      // exclude edges to post-dominaing successors
      if (!PDT.dominates(SMBB, MBB)) {
        // insert the edge MBB->SMBB to all controlled blocks
        for (MachineDomTreeNode *t = PDT[SMBB]; t != ipdom; t = t->getIDom()) {
          CD[t->getBlock()].insert( std::make_pair(MBB,SMBB) );
        }
      }
    } // end for all successors
  } // end for each MBB

  // add control dependence for entry edge NULL -> BB0
  {
    MachineBasicBlock *entryMBB = &MF.front();
    for ( MachineDomTreeNode *t = PDT[entryMBB]; t != NULL; t = t->getIDom() ) {
      CD[t->getBlock()].insert( std::make_pair(
                                  (MachineBasicBlock*)NULL, entryMBB)
                                  );
    }
  }


  DEBUG_TRACE({
    // dump CD
    dbgs() << "Control dependence:\n";
    for (CD_map_t::iterator I=CD.begin(), E=CD.end(); I!=E; ++I) {
      dbgs() << "BB#" << I->first->getNumber() << ": { ";
      for (CD_map_entry_t::iterator EI=I->second.begin(), EE=I->second.end();
           EI!=EE; ++EI) {
        dbgs() << "(" << ((EI->first) ? EI->first->getNumber() : -1) << ","
                      << EI->second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });
}



void PatmosSinglePathInfo::decomposeControlDependence(MachineFunction &MF,
                                                      CD_map_t &CD, K_t &K,
                                                      R_t &R) const {
  int p = 0;
  for (MachineFunction::iterator FI=MF.begin(); FI!=MF.end(); ++FI) {
    MachineBasicBlock *MBB = FI;
    CD_map_entry_t t = CD[MBB];
    int q=-1;
    // try to lookup the control dependence
    for (unsigned int i=0; i<K.size(); i++) {
        if ( t == K[i] ) {
          q = i;
          break;
        }
    }
    if (q != -1) {
      // we already have handled this dependence
      R[MBB] = q;
    } else {
      // new dependence set:
      K.push_back(t);
      R[MBB] = p++;
    }
  } // end for each MBB

  DEBUG_TRACE({
    // dump R, K
    dbgs() << "Decomposed CD:\n";
    dbgs() << "map R: MBB -> pN\n";
    for (R_t::iterator RI=R.begin(), RE=R.end(); RI!=RE; ++RI) {
      dbgs() << "R(" << RI->first->getNumber() << ") = p" << RI->second << "\n";
    }
    dbgs() << "map K: pN -> t \\in CD\n";
    for (unsigned int i=0; i<K.size(); i++) {
      dbgs() << "K(p" << i << ") -> {";
      for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
            EI!=EE; ++EI) {
        dbgs() << "(" << ((EI->first) ? EI->first->getNumber() : -1) << ","
                      << EI->second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });
}



BitVector PatmosSinglePathInfo::
computeUpwardsExposedUses(MachineFunction &MF, K_t &K, R_t &R) const {
  // ... by solving data-flow equations (upwards-exposed uses)
  std::map<MachineBasicBlock*, BitVector> gen;
  std::map<MachineBasicBlock*, BitVector> kill;
  // fill gen/kill
  for (R_t::iterator EI=R.begin(), EE=R.end(); EI!=EE; ++EI) {
    MachineBasicBlock *MBB = EI->first;

    gen[MBB] = BitVector(K.size());
    kill[MBB] = BitVector(K.size());
    // put R(MBB) into gen (as use)
    gen[MBB].set( EI->second );
  }
  for (unsigned int i=0; i<K.size(); i++) {
    // each MBB defining a predicate kills a use
    for (CD_map_entry_t::iterator KI=K[i].begin(), KE=K[i].end();
         KI!=KE; ++KI) {
      if (!KI->first) continue; // skip entry edge
      kill[KI->first].set(i);
    }
  }
  DEBUG_TRACE({
    dbgs() << "Compute Upwards Exposed Uses\n";
    // dump gen/kill
    dbgs() << "DU: MBB -> gen/kill sets (bvlen " << K.size() << ")\n";
    for (MachineFunction::iterator FI=MF.begin(), FE=MF.end(); FI!=FE; ++FI) {
      MachineBasicBlock *MBB = FI;
      dbgs() << "  BB#" << MBB->getNumber() << " gen: {";
      for (unsigned i=0; i<gen[MBB].size(); i++) {
        if (gen[MBB].test(i)) dbgs() << " p" << i;
      }
      dbgs() << " }  kill: {";
      for (unsigned i=0; i<kill[MBB].size(); i++) {
        if (kill[MBB].test(i)) dbgs() << " p" << i;
      }
      dbgs() << " }\n";
    }
  });

  std::queue<MachineBasicBlock*> worklist;
  std::map<MachineBasicBlock*, BitVector> bvIn;
  // fill worklist initially in dfs postorder
  for (po_iterator<MachineBasicBlock *> POI = po_begin(&MF.front());
                                        POI != po_end(&MF.front());
                                        ++POI) {
      worklist.push(*POI);
      // initially, In = gen
      bvIn[*POI] = BitVector(gen[*POI]);
  }
  // first element is exit node. initialize properly (all)
  bvIn[worklist.front()].set();
  worklist.pop();
  // iterate.
  while (!worklist.empty()) {
    // pop first element
    MachineBasicBlock *MBB = worklist.front();
    worklist.pop();
    // effect
    BitVector bvOut = BitVector(K.size());
    for (MachineBasicBlock::succ_iterator SI=MBB->succ_begin();
         SI!=MBB->succ_end(); ++SI) {
      bvOut |= bvIn[*SI];
    }
    bvOut.reset(kill[MBB]);
    bvOut |= gen[MBB];
    if (bvOut != bvIn[MBB]) {
      DEBUG_TRACE({
          dbgs() << "  Update IN of BB#" << MBB->getNumber() << "{";
          for (unsigned i=0; i<bvOut.size(); i++) {
          if (bvOut.test(i)) dbgs() << " p" << i;
          }
          dbgs() << " }\n";
      });
      bvIn[MBB] = bvOut;
      // add predecessors to worklist
      for (MachineBasicBlock::pred_iterator PI=MBB->pred_begin();
           PI!=MBB->pred_end(); ++PI) {
          worklist.push(*PI);
      }
    }
  } // end of main iteration

  // Augmented elements
  BitVector pred_initialize = bvIn[&MF.front()];
  pred_initialize.reset(0); // entry edge
  DEBUG_TRACE({
    // dump pN to be initialized
    dbgs() << "Initialization with F:";
    for (unsigned i=0; i<pred_initialize.size(); i++) {
      if (pred_initialize.test(i)) dbgs() << " p" << i;
    }
    dbgs() << "\n";
  });
  return pred_initialize;
}





///////////////////////////////////////////////////////////////////////////////
// SPNode methods
///////////////////////////////////////////////////////////////////////////////


SPNode::SPNode(SPNode *parent, const MachineBasicBlock *header,
               const MachineBasicBlock *succ, unsigned int numbe)
               : Parent(parent), SuccMBB(succ), NumBackedges(numbe) {
  Level = 0;
  // add to parent's child list
  if (Parent) {
    Parent->Children[header] = this;
    Level = Parent->Level + 1;
  }
  // add header also to block list
  Blocks.push_back(header);
}

/// destructor - free the child nodes first, cleanup
SPNode::~SPNode() {
  for (std::map<const MachineBasicBlock*, SPNode*>::iterator
            I = Children.begin(), E = Children.end(); I != E; ++I) {
    delete I->second;
  }
  Children.clear();
}

void SPNode::addMBB(const MachineBasicBlock *MBB) {
  if (Blocks.front() != MBB) {
    Blocks.push_back(MBB);
  }
}


void SPNode::getOrder(std::vector<const MachineBasicBlock *> &list) {
  std::vector<const MachineBasicBlock *> S;
  std::vector<const MachineBasicBlock *> succs;
  std::map<const MachineBasicBlock *, int> deps;
  // for each block in SPNode excluding header,
  // store the number of preds
  for (unsigned i=1; i<Blocks.size(); i++) {
    const MachineBasicBlock *MBB = Blocks[i];
    deps[MBB] = MBB->pred_size();
    if (Children.count(MBB)) {
      SPNode *subloop = Children[MBB];
      deps[MBB] -= subloop->NumBackedges;
    }
  }
  // consider subloop headers
  for (child_iterator I = Children.begin(), E = Children.end(); I != E; ++I) {
      deps[I->first] = I->first->pred_size() - I->second->NumBackedges;
  }

  S.push_back(Blocks.front());
  while(!S.empty()) {
    const MachineBasicBlock *n = S.back();
    S.pop_back();
    // n is either a subloop header or a block of this SPNode
    if (Children.count(n)) {
      SPNode *loop = Children[n];
      loop->getOrder(list);
      succs.push_back(loop->getSuccMBB());
    } else {
      list.push_back(n);
      succs.insert( succs.end(), n->succ_begin(), n->succ_end() );
    }

    for (unsigned i=0; i<succs.size(); i++) {
      const MachineBasicBlock *succ = succs[i];
      // successors for which all preds were visited become available
      if (succ != getHeader()) {
        deps[succ]--;
        if (deps[succ] == 0)
          S.push_back(succ);
      }
    }
    succs.clear();
  }
}

static void indent(unsigned level) {
  for(unsigned i=0; i<level; i++)
    dbgs() << "  ";
}

void SPNode::dump() const {
  indent(Level);
  dbgs() <<  "[BB#" << Blocks.front()->getNumber() << "]";
  if (SuccMBB) {
    dbgs() << " -> BB#" << SuccMBB->getNumber();
  }
  dbgs() << "\n";

  for (unsigned i=1; i<Blocks.size(); i++) {
    indent(Level+1);
    dbgs() <<  " BB#" << Blocks[i]->getNumber() << "\n";
  }
  for (std::map<const MachineBasicBlock*, SPNode*>::const_iterator
            I = Children.begin(), E = Children.end(); I != E; ++I) {
    I->second->dump();
  }
}





// build the SPNode tree in DFS order, creating new SPNodes preorder
static
void createSPNodeSubtree(MachineLoop *loop, SPNode *parent,
                         std::map<const MachineLoop *, SPNode *> &M) {
  // We need to make some assumptions about the loops we can handle for now...
  // allow only one successor for SPNode
  assert( loop->getExitBlock() != NULL &&
          "Allow only one successor for loops!" );
  assert( loop->getExitingBlock() != NULL &&
          "Allow only exactly one exiting edge for loops!" );
  // for now, also:
  assert( loop->getHeader() == loop->getExitingBlock() &&
          "Allow only loops with Header == Exiting Block!" );

  SPNode *N = new SPNode(parent,
                         loop->getHeader(),
                         loop->getExitBlock(),
                         loop->getNumBackEdges()
                         );

  // update map: Loop -> SPNode
  M[loop] = N;

  for (MachineLoop::iterator I = loop->begin(), E = loop->end();
          I != E; ++I) {
    createSPNodeSubtree(*I, N, M);
  }
}



SPNode *
PatmosSinglePathInfo::createSPNodeTree(const MachineFunction &MF) const {
  // Get loop information
  MachineLoopInfo &LI = getAnalysis<MachineLoopInfo>();

  // First, create a SPNode tree
  std::map<const MachineLoop *, SPNode *> M;

  SPNode *Root = new SPNode(NULL, &MF.front(), NULL, 0);

  M[NULL] = Root;

  // iterate over top-level loops
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    createSPNodeSubtree(Loop, Root, M);
  }

  // Then, add MBBs to the corresponding SPNodes
  for (MachineFunction::const_iterator FI=MF.begin(), FE=MF.end();
          FI!=FE; ++FI) {
    const MachineBasicBlock *MBB = FI;
    const MachineLoop *Loop = LI[MBB]; // also accounts for NULL (no loop)
    M[Loop]->addMBB(MBB);
  }

  return Root;
}


