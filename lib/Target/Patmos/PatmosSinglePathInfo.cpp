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
#include "llvm/IR/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>
#include <queue>


using namespace llvm;


/// SPRootList - Option to enable single-path conversion.
static cl::list<std::string> SPRootList(
    "mpatmos-spconv",
    cl::value_desc("list"),
    cl::desc("Single-Path roots (Patmos only)"),
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
  return !SPRootList.empty();
}


bool PatmosSinglePathInfo::isEnabled(const MachineFunction &MF) {
  return isRoot(MF) || isReachable(MF);
}

bool PatmosSinglePathInfo::isRoot(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-root");
}

bool PatmosSinglePathInfo::isReachable(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-reachable");
}

void PatmosSinglePathInfo::getRootNames(std::set<std::string> &S) {
  S.insert( SPRootList.begin(), SPRootList.end() );
}

///////////////////////////////////////////////////////////////////////////////

PatmosSinglePathInfo::PatmosSinglePathInfo(const PatmosTargetMachine &tm)
  : MachineFunctionPass(ID), TM(tm),
    STC(tm.getSubtarget<PatmosSubtarget>()),
    TII(static_cast<const PatmosInstrInfo*>(tm.getInstrInfo())), Root(0) {}


bool PatmosSinglePathInfo::doInitialization(Module &M) {
  return false;
}


bool PatmosSinglePathInfo::doFinalization(Module &M) {
  if (Root) {
    delete Root;
    Root = NULL;
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
  if (Root) {
    delete Root;
    Root = NULL;
  }
  // only consider function if specified on command line
  std::string curfunc = MF.getFunction()->getName();
  if ( isEnabled(MF) ) {
    DEBUG( dbgs() << "[Single-Path] Analyze '" << curfunc << "'\n" );
    analyzeFunction(MF);
  }
  // didn't modify anything
  return false;
}


static void printBitVector(raw_ostream &OS, BitVector B) {
  for (int i=B.size()-1; i>=0; i--) {
    OS << ( (B.test(i)) ? "1" : "0" );
  }
}

void PatmosSinglePathInfo::print(raw_ostream &OS, const Module *M) const {
  assert(Root);
  Root->dump();
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void PatmosSinglePathInfo::dump() const {
  print(dbgs());
}
#endif



void PatmosSinglePathInfo::walkRoot(llvm::SPScopeWalker &walker) const {
  assert( Root != NULL );
  Root->walk(walker);
}


void PatmosSinglePathInfo::analyzeFunction(MachineFunction &MF) {

  // for CD, we need the Postdom-Tree
  MachinePostDominatorTree &PDT = getAnalysis<MachinePostDominatorTree>();

  assert(PDT.getRoots().size()==1 && "Function must have a single exit node!");

  DEBUG_TRACE( dbgs() << "Post-dominator tree:\n" );
  DEBUG_TRACE( PDT.print(dbgs(), MF.getFunction()->getParent()) );

  // build the SPScope tree
  Root = createSPScopeTree(MF);
  for (df_iterator<SPScope*> I = df_begin(Root), E = df_end(Root); I!=E; ++I) {
    SPScope *S = *I;
    // topologically sort the blocks and subscopes of each SPScope
    S->topoSort();

    CD_map_t CD;
    computeControlDependence(*S, PDT, CD);

    K_t K;
    R_t R;
    decomposeControlDependence(*S, CD, K, R);

    assignPredInfo(*S, K, R);
  }

  DEBUG( print(dbgs()) );

  // XXX for debugging
  //MF.viewCFGOnly();
}



void PatmosSinglePathInfo::computeControlDependence(SPScope &S,
                                              MachinePostDominatorTree &PDT,
                                              CD_map_t &CD) const {

  // build control dependence information
  for (unsigned i=0; i<S.Blocks.size(); i++) {
    MachineBasicBlock *MBB = S.Blocks[i];
    MachineDomTreeNode *ipdom = PDT[MBB]->getIDom();

    for(MachineBasicBlock::succ_iterator SI=MBB->succ_begin(),
                                         SE=MBB->succ_end(); SI!=SE; ++SI) {
      MachineBasicBlock *SMBB = *SI;
      // only consider members
      if (!S.isMember(SMBB))
        continue;
      // exclude edges to post-dominating (single) successors;
      // the second case catches the single-node loop case
      // (i==0 -> MBB is header, control dependent on itself)
      if (!PDT.dominates(SMBB, MBB) || (i==0 && SMBB==MBB)) {
        // insert the edge MBB->SMBB to all controlled blocks
        for (MachineDomTreeNode *t = PDT[SMBB]; t != ipdom; t = t->getIDom()) {
          CD[t->getBlock()].insert( std::make_pair(MBB,SMBB) );
        }
      }
    } // end for all successors
  } // end for each MBB

  // add control dependence for entry edge NULL -> BB0
  if (S.isTopLevel()) {
    MachineBasicBlock *entryMBB = S.getHeader();
    for (MachineDomTreeNode *t = PDT[entryMBB]; t != NULL; t = t->getIDom() ) {
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


void PatmosSinglePathInfo::decomposeControlDependence(SPScope &S,
                                                      const CD_map_t &CD,
                                                      K_t &K, R_t &R) const {
  int p = 0;
  for (unsigned i=0; i<S.Blocks.size(); i++) {
    const MachineBasicBlock *MBB = S.Blocks[i];
    CD_map_entry_t t = CD.at(MBB);
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


void PatmosSinglePathInfo::assignPredInfo(SPScope &S, const K_t &K,
                                          const R_t &R) const {
  // Properly assign the Uses/Defs
  S.PredCount = K.size();
  S.PredUse = R;
  // initialize number of defining edges to 0 for all predicates
  S.NumPredDefEdges = std::vector<unsigned>( K.size(), 0 );

  // For each predicate, compute defs
  for (unsigned int i=0; i<K.size(); i++) {
    // store number of defining edges
    S.NumPredDefEdges[i] = K[i].size();
    // for each definition edge
    for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
              EI!=EE; ++EI) {
      const MachineBasicBlock *MBBSrc = EI->first, *MBBDst = EI->second;
      if (!MBBSrc) {
        continue;
      }

      // get pred definition info of MBBSrc
      PredDefInfo &PredDef = getOrCreateDefInfo(S, MBBSrc);
      // insert definition for predicate i according to MBBDst
      PredDef.define(i, MBBDst);
    } // end for each definition edge
  }
//TODO
}
///////////////////////////////////////////////////////////////////////////////

PredDefInfo &
PatmosSinglePathInfo::getOrCreateDefInfo(SPScope &S,
                                         const MachineBasicBlock *MBB) const {

  if (!S.PredDefs.count(MBB)) {
    // for AnalyzeBranch
    MachineBasicBlock *TBB = NULL, *FBB = NULL;
    SmallVector<MachineOperand, 2> Cond;
    if (!TII->AnalyzeBranch(*const_cast<MachineBasicBlock*>(MBB),
          TBB, FBB, Cond)) {
      // According to AnalyzeBranch spec, at a conditional branch,
      // Cond will hold the branch conditions
      // Further, there are two cases for conditional branches:
      // 1. conditional+fallthrough:   TBB holds branch target
      // 2. conditional+unconditional: TBB holds target of conditional branch,
      //                               FBB the target of the unconditional one
      // Hence, the branch condition will always refer to the TBB edge.
      assert( !Cond.empty() && "AnalyzeBranch for SP-IfConversion failed; "
          "could not determine branch condition");
    } else {
      assert(0 && "AnalyzeBranch failed");
    }

    // Create new info
    S.PredDefs.insert(
      std::make_pair(MBB, PredDefInfo(S.PredCount, TBB, Cond)) );
  }

  return S.PredDefs.at(MBB);
}


///////////////////////////////////////////////////////////////////////////////
// SPScope methods
///////////////////////////////////////////////////////////////////////////////


SPScope::SPScope(SPScope *parent, MachineBasicBlock *header,
               MachineBasicBlock *succ, unsigned int numbe)
               : Parent(parent), SuccMBB(succ), NumBackedges(numbe),
                 LoopBound(-1) {
  Depth = 0;
  if (Parent) {
    // add to parent's child list
    Parent->HeaderMap[header] = this;
    Parent->Children.push_back(this);
    // add to parent's block list as well
    Parent->addMBB(header);
    Depth = Parent->Depth + 1;
  }
  // add header also to this SPScope's block list
  Blocks.push_back(header);
}

/// destructor - free the child scopes first, cleanup
SPScope::~SPScope() {
  for (unsigned i=0; i<Children.size(); i++) {
    delete Children[i];
  }
  Children.clear();
  HeaderMap.clear();
}

void SPScope::addMBB(MachineBasicBlock *MBB) {
  if (Blocks.front() != MBB) {
    Blocks.push_back(MBB);
  }
}

bool SPScope::isMember(const MachineBasicBlock *MBB) const {
  for (unsigned i=0; i<Blocks.size(); i++) {
    if (Blocks[i] == MBB) return true;
  }
  return false;
}


bool SPScope::isSubHeader(MachineBasicBlock *MBB) const {
  return HeaderMap.count(MBB) > 0;
}


void SPScope::topoSort(void) {
  std::deque<MachineBasicBlock *> S;
  std::vector<MachineBasicBlock *> succs;
  std::map<MachineBasicBlock *, int> deps;
  // for each block in SPScope excluding header,
  // store the number of predecessors
  for (unsigned i=1; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    deps[MBB] = MBB->pred_size();
    if (HeaderMap.count(MBB)) {
      SPScope *subloop = HeaderMap[MBB];
      deps[MBB] -= subloop->NumBackedges;
    }
  }

  S.push_back(Blocks.front());
  Blocks.clear();
  while (!S.empty()) {
    MachineBasicBlock *n = S.back();
    Blocks.push_back(n); // re-append
    S.pop_back();
    // n is either a subloop header or a simple block of this SPScope
    if (HeaderMap.count(n)) {
      succs.push_back(HeaderMap[n]->getSuccMBB());
    } else {
      // simple block
      succs.insert( succs.end(), n->succ_begin(), n->succ_end() );
    }

    for (unsigned i=0; i<succs.size(); i++) {
      MachineBasicBlock *succ = succs[i];
      // successors for which all preds were visited become available
      if (succ != getHeader()) {
        if (--deps[succ] == 0)
          S.push_back(succ);
      }
    }
    succs.clear();
  }
}

void SPScope::walk(SPScopeWalker &walker) {
  walker.enterSubscope(this);
  for (unsigned i=0; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    if (HeaderMap.count(MBB)) {
      HeaderMap[MBB]->walk(walker);
    } else {
      walker.nextMBB(MBB);
    }
  }
  walker.exitSubscope(this);
}

static void indent(unsigned depth) {
  for(unsigned i=0; i<depth; i++)
    dbgs() << "  ";
}

static void printUDInfo(const SPScope &S, const MachineBasicBlock *MBB) {
  dbgs() << "  u=" << S.getPredUse(MBB);

  const PredDefInfo *DI = S.getDefInfo(MBB);
  if (DI) {
    dbgs() << " dT=";
    printBitVector(dbgs(), DI->getTrue());
    dbgs() << " dF=";
    printBitVector(dbgs(), DI->getFalse());
  }
  dbgs() << "\n";
}

void SPScope::dump() const {
  indent(Depth);
  dbgs() <<  "[BB#" << Blocks.front()->getNumber() << "]";
  if (Parent) {
    dbgs() << " u=" << Parent->getPredUse(Blocks.front());
  }
  if (SuccMBB) {
    dbgs() << " -> BB#" << SuccMBB->getNumber();
  }
  dbgs() << " |P|=" <<  PredCount;
  printUDInfo(*this, Blocks.front());

  for (unsigned i=1; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    if (HeaderMap.count(MBB)) {
      HeaderMap.at(MBB)->dump();
    } else {
      indent(Depth+1);
      dbgs() <<  " BB#" << MBB->getNumber();
      printUDInfo(*this, MBB);
    }
  }
}

int SPScope::getPredUse(const MachineBasicBlock *MBB) const {
  if (PredUse.count(MBB)) {
    return PredUse.at(MBB);
  }
  return -1;
}

const PredDefInfo *SPScope::getDefInfo( const MachineBasicBlock *MBB) const {

  if (PredDefs.count(MBB)) {
    return &PredDefs.at(MBB);
  }
  return NULL;
}





// build the SPScope tree in DFS order, creating new SPScopes preorder
static
void createSPScopeSubtree(MachineLoop *loop, SPScope *parent,
                         std::map<const MachineLoop *, SPScope *> &M) {
  // We need to make some assumptions about the loops we can handle for now...
  // allow only one successor for SPScope
  assert( loop->getExitBlock() != NULL &&
          "Allow only one successor for loops!" );
  assert( loop->getExitingBlock() != NULL &&
          "Allow only exactly one exiting edge for loops!" );
  // for now, also:
  //assert( loop->getHeader() == loop->getExitingBlock() &&
  //        "Allow only loops with Header == Exiting Block!" );

  SPScope *S = new SPScope(parent,
                          loop->getHeader(),
                          loop->getExitBlock(),
                          loop->getNumBackEdges()
                          );

  // update map: Loop -> SPScope
  M[loop] = S;

  for (MachineLoop::iterator I = loop->begin(), E = loop->end();
          I != E; ++I) {
    createSPScopeSubtree(*I, S, M);
  }
}



SPScope *
PatmosSinglePathInfo::createSPScopeTree(MachineFunction &MF) const {
  // Get loop information
  MachineLoopInfo &LI = getAnalysis<MachineLoopInfo>();

  // First, create a SPScope tree
  std::map<const MachineLoop *, SPScope *> M;

  SPScope *Root = new SPScope(NULL, &MF.front(), NULL, 0);

  M[NULL] = Root;

  // iterate over top-level loops
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    createSPScopeSubtree(Loop, Root, M);
  }

  // Then, add MBBs to the corresponding SPScopes
  for (MachineFunction::iterator FI=MF.begin(), FE=MF.end();
          FI!=FE; ++FI) {
    MachineBasicBlock *MBB = FI;
    const MachineLoop *Loop = LI[MBB]; // also accounts for NULL (no loop)
    M[Loop]->addMBB(MBB);
  }

  return Root;
}


