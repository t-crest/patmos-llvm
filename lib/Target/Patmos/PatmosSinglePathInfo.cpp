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
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallVector.h"
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

bool PatmosSinglePathInfo::isConverting(const MachineFunction &MF) {
  const PatmosMachineFunctionInfo *PMFI =
    MF.getInfo<PatmosMachineFunctionInfo>();
  return PMFI->isSinglePath();
}

bool PatmosSinglePathInfo::isEnabled(const MachineFunction &MF) {
  return isRoot(MF) || isReachable(MF) || isMaybe(MF);
}

bool PatmosSinglePathInfo::isRoot(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-root");
}

bool PatmosSinglePathInfo::isReachable(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-reachable");
}

bool PatmosSinglePathInfo::isMaybe(const MachineFunction &MF) {
  return MF.getFunction()->hasFnAttribute("sp-maybe");
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
  // only consider function actually marked for conversion
  std::string curfunc = MF.getFunction()->getName();
  if ( isConverting(MF) ) {
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

  // FIXME
  S.computeCD();

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


SPScope::SPScope(MachineBasicBlock *header, bool isRootTopLevel)
                    : Parent(NULL), RootTopLevel(isRootTopLevel),
                      LoopBound(-1) {
  Depth = 0;
  // add header also to this SPScope's block list
  Blocks.push_back(header);
}


SPScope::SPScope(SPScope *parent, MachineLoop &loop)
  : Parent(parent), RootTopLevel(false), LoopBound(-1) {

    assert(parent);
    MachineBasicBlock *header = loop.getHeader();

    // add to parent's child list
    Parent->HeaderMap[header] = this;
    Parent->Children.push_back(this);
    // add to parent's block list as well
    Parent->addMBB(header);
    Depth = Parent->Depth + 1;

    // add header also to this SPScope's block list
    Blocks.push_back(header);

    // info about loop latches and exit edges
    loop.getLoopLatches(Latches);
    loop.getExitEdges(ExitEdges);

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

bool SPScope::isHeader(const MachineBasicBlock *MBB) const {
  return getHeader() == MBB;
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


const std::vector<const MachineBasicBlock *> SPScope::getSuccMBBs() const {
  std::vector<const MachineBasicBlock *> SuccMBBs;
  for (unsigned i=0; i<ExitEdges.size(); i++) {
    SuccMBBs.push_back(ExitEdges[i].second);
  }
  return SuccMBBs;
}


void SPScope::computeCD(void) {

  // build fcfg
  FCFG fcfg(getHeader());

  std::set<const MachineBasicBlock *> body(++Blocks.begin(), Blocks.end());
  std::vector<const MachineBasicBlock *> succs;

  for (unsigned i=0; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    Node &n = fcfg.getNodeFor(MBB);

    if (HeaderMap.count(MBB)) {
      const std::vector<const MachineBasicBlock *> loop_succs
                                              = HeaderMap[MBB]->getSuccMBBs();
      // successors of the loop
      succs.insert( succs.end(), loop_succs.begin(), loop_succs.end() );
    } else {
      // simple block
      succs.insert( succs.end(), MBB->succ_begin(), MBB->succ_end() );
    }

    for (unsigned i=0; i<succs.size(); i++) {
      const MachineBasicBlock *succ = succs[i];
      // successors for which all preds were visited become available
      if (body.count(succ)) {
        Node &ns = fcfg.getNodeFor(succ);
        n.connect(ns);
      } else {
        fcfg.toexit(n);
      }
    }

    // special case: top-level loop has no exit/backedge
    if (succs.empty()) {
      assert(isTopLevel());
      fcfg.toexit(n);
    }

    succs.clear();

  }

  fcfg.dump();
}


void SPScope::FCFG::_rdfs(Node *n, std::set<Node*> &V,
                          std::vector<Node*> &po) {
  V.insert(n);
  for (Node::child_iterator I = n->preds_begin(), E = n->preds_end();
      I != E; ++I) {
    if (!V.count(*I)) {
      _rdfs(*I, V, po);
    }
  }
  po.push_back(n);
}


SPScope::Node *SPScope::FCFG::_intersect(Node *b1, Node *b2) {
  assert(b2 != NULL);
  if (b2->ipdom == NULL) {
    return b1;
  }
  Node *finger1 = (b1 != NULL) ? b1 : b2;
  Node *finger2 = b2;
  while (finger1->num != finger2->num) {
    while (finger1->num < finger2->num) finger1 = finger1->ipdom;
    while (finger2->num < finger1->num) finger2 = finger2->ipdom;
  }
  return finger1;
}

void SPScope::FCFG::postdominators(void) {
  // adopted from:
  //   Cooper K.D., Harvey T.J. & Kennedy K. (2001).
  //   A simple, fast dominance algorithm
  // As we compute postdominators, we generated a PO numbering of the reversed
  // graph and consider the successors instead of the predecessors.

  // first, we generate a postorder numbering
  std::set<Node*> visited;
  std::vector<Node*> po;
  // as we construct the postdominators, we dfs the reverse graph
  _rdfs(&nexit, visited, po);

  // now compute immediate postdominators
  // initialize "start" (= exit) node
  nexit.ipdom = &nexit;
  nexit.num = po.size() - 1;

  // for all nodes except start node in reverse postorder
  for (int i = po.size() - 2; i >= 0; i--) {
    Node *n = po[i];
    n->num = i; // assign po number on the fly
    // one pass is enough for acyclic graph, no loop required
    Node *new_ipdom = NULL;
    for (Node::child_iterator si = n->succs_begin(), se = n->succs_end();
        si != se; ++si) {
      new_ipdom = _intersect(new_ipdom, *si);
    }
    // assign the intersection
    n->ipdom = new_ipdom;
    // debug output:
    print(*n); print(*n->ipdom); dbgs() << "\n";
  }
}


void SPScope::FCFG::print(Node &n) {
  if (&n == &nentry) {
    dbgs() << "_S(" << n.num << ")";
  } else if (&n == &nexit) {
    dbgs() << "_T(" << n.num << ")";
  } else {
    dbgs() << "BB#" << n.MBB->getNumber() << "(" << n.num << ")";
  }
}



void SPScope::FCFG::dump() {
  dbgs() << "====== FCFG\n";

  postdominators();

  std::vector<Node *> W;
  std::set<Node *> visited;

  W.push_back(&nentry);
  while (!W.empty()) {
    Node *n = W.back();
    W.pop_back();
    visited.insert(n);
    // print edges
    for (Node::child_iterator I = n->succs_begin(), E = n->succs_end();
          I!=E; ++I ) {
      print(*n); dbgs() << " -> "; print(**I); dbgs() << "\n";
      if (!visited.count(*I)) {
        W.push_back(*I);
      }

    }

  }
}


void SPScope::topoSort(void) {
  std::deque<const MachineBasicBlock *> S;
  std::vector<const MachineBasicBlock *> succs;
  std::map<const MachineBasicBlock *, int> deps;
  // for each block in SPScope excluding header,
  // store the number of predecessors
  for (unsigned i=1; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    deps[MBB] = MBB->pred_size();
    if (HeaderMap.count(MBB)) {
      SPScope *subloop = HeaderMap[MBB];
      deps[MBB] -= subloop->Latches.size();
    }
  }

  S.push_back(Blocks.front());
  Blocks.clear();
  while (!S.empty()) {
    MachineBasicBlock *n = const_cast<MachineBasicBlock*>(S.back());
    Blocks.push_back(n); // re-append
    S.pop_back();
    // n is either a subloop header or a simple block of this SPScope
    if (HeaderMap.count(n)) {
      const std::vector<const MachineBasicBlock *> loop_succs
                                              = HeaderMap[n]->getSuccMBBs();
      // successors of the loop
      succs.insert( succs.end(), loop_succs.begin(), loop_succs.end() );
    } else {
      // simple block
      succs.insert( succs.end(), n->succ_begin(), n->succ_end() );
    }

    for (unsigned i=0; i<succs.size(); i++) {
      const MachineBasicBlock *succ = succs[i];
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
  if (!ExitEdges.empty()) {
    dbgs() << " -> { ";
    for (unsigned i=0; i<ExitEdges.size(); i++) {
      dbgs() << "BB#" << ExitEdges[i].second->getNumber() << " ";
    }
    dbgs() << "}";
  }
  if (!Latches.empty()) {
    dbgs() << " L { ";
    for (unsigned i=0; i<Latches.size(); i++) {
      dbgs() << "BB#" << Latches[i]->getNumber() << " ";
    }
    dbgs() << "}";
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

  SPScope *S = new SPScope(parent, *loop);

  // update map: Loop -> SPScope
  M[loop] = S;
  // visit subloops
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

  SPScope *Root = new SPScope(&MF.front(), isRoot(MF));


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


