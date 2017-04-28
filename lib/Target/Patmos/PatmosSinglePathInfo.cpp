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
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include <map>

using namespace llvm;

/// SPRootList - Option to enable single-path code generation and specify entry
///              functions. This option needs to be present even when all
///              roots are specified via attributes.
static cl::list<std::string> SPRootList(
    "mpatmos-singlepath",
    cl::value_desc("list"),
    cl::desc("Entry functions for which single-path code is generated"),
    cl::CommaSeparated);


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
  S.erase("");
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
  AU.addRequired<MachineLoopInfo>();
  AU.addRequired<MachineDominatorTree>();
  AU.setPreservesAll();
  MachineFunctionPass::getAnalysisUsage(AU);
}

bool PatmosSinglePathInfo::runOnMachineFunction(MachineFunction &MF) {
  if (Root) {
    delete Root;
    Root = NULL;
  }
  // clear for sure
  MBBScopeMap.clear();

  // only consider function actually marked for conversion
  std::string curfunc = MF.getFunction()->getName();
  if ( isConverting(MF) ) {
    DEBUG( dbgs() << "[Single-Path] Analyze '" << curfunc << "'\n" );
    analyzeFunction(MF);
  }
  // didn't modify anything
  return false;
}


void PatmosSinglePathInfo::analyzeFunction(MachineFunction &MF) {
  // we cannot handle irreducibility yet
  checkIrreducibility(MF);

  // FIXME Instead of using MachineLoopInfo for creating the Scope-tree,
  // we could use a custom algorithm (e.g. Havlak's algorithm)
  // that also checks irreducibility.
  // build the SPScope tree
  Root = createSPScopeTree(MF);

  // analyze each scope
  // NB: this could be solved more elegantly by analyzing a scope when it is
  // built. But how he tree is created right now, it will not become more
  // elegant anyway.
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(this),
      E = df_end(this); I!=E; ++I) {
    (*I)->computePredInfos();
  }

  DEBUG( print(dbgs()) );

  // XXX for extensive debugging
  //MF.viewCFGOnly();
}


void PatmosSinglePathInfo::checkIrreducibility(MachineFunction &MF) const {
  // Get dominator information
  MachineDominatorTree &DT = getAnalysis<MachineDominatorTree>();

  struct BackedgeChecker {
    MachineDominatorTree &DT;
    std::set<MachineBasicBlock *> visited, finished;
    BackedgeChecker(MachineDominatorTree &dt) : DT(dt) {}
    void dfs(MachineBasicBlock *MBB) {
      visited.insert(MBB);
      for (MachineBasicBlock::const_succ_iterator si = MBB->succ_begin(),
          se = MBB->succ_end(); si != se; ++si) {
        if (!visited.count(*si)) {
          dfs(*si);
        } else if (!finished.count(*si)) {
          // visited but not finished -> this is a backedge
          // we only support natural loops, check domination
          if (!DT.dominates(*si, MBB)) {
            report_fatal_error("Single-path code generation failed due to "
                               "irreducible CFG in '" +
                               MBB->getParent()->getFunction()->getName() +
                               "'!");

          }
        }
      }
      finished.insert(MBB);
    }
  };

  BackedgeChecker c(DT);
  c.dfs(&MF.front());
}


SPScope *
PatmosSinglePathInfo::getScopeFor(const MachineBasicBlock *MBB) const {
  return MBBScopeMap.at(MBB);
}

void PatmosSinglePathInfo::print(raw_ostream &os, const Module *M) const {
  assert(Root);
  os << "========================================\n";
  Root->dump(os);
  os << "========================================\n";
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



///////////////////////////////////////////////////////////////////////////////
// SPScope methods
///////////////////////////////////////////////////////////////////////////////


SPScope::SPScope(MachineBasicBlock *header, bool isRootTopLevel)
                    : Parent(NULL), FCFG(header),
                      RootTopLevel(isRootTopLevel), LoopBound(-1) {
  Depth = 0;
  // add header also to this SPScope's block list
  Blocks.push_back(header);

}


SPScope::SPScope(SPScope *parent, MachineLoop &loop)
  : Parent(parent), FCFG(loop.getHeader()),
    RootTopLevel(false), LoopBound(-1) {

  assert(parent);
  MachineBasicBlock *header = loop.getHeader();

  // add to parent's child list
  Parent->HeaderMap[header] = this;
  Parent->Subscopes.push_back(this);
  // add to parent's block list as well
  Parent->addMBB(header);
  Depth = Parent->Depth + 1;

  // add header also to this SPScope's block list
  Blocks.push_back(header);

  // info about loop latches and exit edges
  loop.getLoopLatches(Latches);
  loop.getExitEdges(ExitEdges);

  // scan the header for loopbound info
  for (MachineBasicBlock::iterator MI = header->begin(), ME = header->end();
      MI != ME; ++MI) {
    if (MI->getOpcode() == Patmos::PSEUDO_LOOPBOUND) {
      // max is the second operand (idx 1)
      LoopBound = MI->getOperand(1).getImm() + 1;
      break;
    }
  }

}


/// destructor - free the child scopes first, cleanup
SPScope::~SPScope() {
  for (unsigned i=0; i<Subscopes.size(); i++) {
    delete Subscopes[i];
  }
  Subscopes.clear();
  HeaderMap.clear();
}


void SPScope::addMBB(MachineBasicBlock *MBB) {
  if (Blocks.front() != MBB) {
    Blocks.push_back(MBB);
  }
}


SPScope::Edge SPScope::getDual(Edge &e) const {
  const MachineBasicBlock *src = e.first;
  assert(src->succ_size() == 2);
  for (MachineBasicBlock::const_succ_iterator si = src->succ_begin(),
      se = src->succ_end(); si != se; ++si) {
    if (*si != e.second) {
      return std::make_pair(src, *si);
    }
  }
  llvm_unreachable("no dual edge found");
  return std::make_pair((const MachineBasicBlock *) NULL,
                        (const MachineBasicBlock *) NULL);
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


void SPScope::computePredInfos(void) {

  buildfcfg();
  toposort();
  FCFG.postdominators();
  DEBUG_TRACE(dumpfcfg()); // uses info about pdom
  ctrldep();
  decompose();
}


///////////////////////////////////////////////////////////////////////////////
//
void SPScope::buildfcfg(void) {
  std::set<const MachineBasicBlock *> body(++Blocks.begin(), Blocks.end());
  std::vector<Edge> outedges;

  for (unsigned i=0; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];

    if (HeaderMap.count(MBB)) {
      const SPScope *subloop = HeaderMap[MBB];
      // successors of the loop
      outedges.insert(outedges.end(),
                      subloop->ExitEdges.begin(),
                      subloop->ExitEdges.end());
    } else {
      // simple block
      for (MachineBasicBlock::succ_iterator si = MBB->succ_begin(),
            se = MBB->succ_end(); si != se; ++si) {
        outedges.push_back(std::make_pair(MBB, *si));
      }
    }

    Node &n = FCFG.getNodeFor(MBB);
    for (unsigned i=0; i<outedges.size(); i++) {
      const MachineBasicBlock *succ = outedges[i].second;
      if (body.count(succ)) {
        Node &ns = FCFG.getNodeFor(succ);
        n.connect(ns, outedges[i]);
      } else {
        if (succ != getHeader()) {
          // record exit edges
          FCFG.toexit(n, outedges[i]);
        } else {
          // we don't need back edges recorded
          FCFG.toexit(n);
        }
      }
    }

    // special case: top-level loop has no exit/backedge
    if (outedges.empty()) {
      assert(isTopLevel());
      FCFG.toexit(n);
    }
    outedges.clear();
  }
}


///////////////////////////////////////////////////////////////////////////////
//
void SPScope::toposort(void) {
  // dfs the FCFG in postorder
  std::vector<MachineBasicBlock *> PO;
  for (po_iterator<SPScope*> I = po_begin(this), E = po_end(this);
      I != E; ++I) {
    MachineBasicBlock *MBB = const_cast<MachineBasicBlock*>((*I)->MBB);
    if (MBB) PO.push_back(MBB);
  }
  // clear the blocks vector and re-insert MBBs in reverse post order
  Blocks.clear();
  Blocks.insert(Blocks.end(), PO.rbegin(), PO.rend());
}

///////////////////////////////////////////////////////////////////////////////


void SPScope::FCFG::_rdfs(Node *n, std::set<Node*> &V,
    std::vector<Node*> &order) {
  V.insert(n);
  n->num = -1;
  for (Node::child_iterator I = n->preds_begin(), E = n->preds_end();
      I != E; ++I) {
    if (!V.count(*I)) {
      _rdfs(*I, V, order);
    }
  }
  n->num = order.size();
  order.push_back(n);
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
  // As we compute _post_dominators, we generate a PO numbering of the
  // reversed graph and consider the successors instead of the predecessors.

  // first, we generate a postorder numbering
  std::set<Node*> visited;
  std::vector<Node*> order;
  // as we construct the postdominators, we dfs the reverse graph
  _rdfs(&nexit, visited, order);

  // initialize "start" (= exit) node
  nexit.ipdom = &nexit;

  // for all nodes except start node in reverse postorder
  for (std::vector<Node *>::reverse_iterator i = ++order.rbegin(),
      e = order.rend(); i != e; ++i) {
    Node *n = *i;
    // one pass is enough for acyclic graph, no loop required
    Node *new_ipdom = NULL;
    for (Node::child_iterator si = n->succs_begin(), se = n->succs_end();
        si != se; ++si) {
      new_ipdom = _intersect(new_ipdom, *si);
    }
    // assign the intersection
    n->ipdom = new_ipdom;
  }
}

///////////////////////////////////////////////////////////////////////////////

void SPScope::_walkpdt(Node *a, Node *b, Edge &e) {
  _walkpdt(a, b, e, a);
}

void SPScope::_walkpdt(Node *a, Node *b, Edge &e, Node *edgesrc) {
  Node *t = b;
  while (t != a->ipdom) {
    // add edge e to control dependence of t
    CD[t->MBB].insert(std::make_pair(edgesrc, e));
    t = t->ipdom;
  }
}


void SPScope::ctrldep(void) {

  for (df_iterator<SPScope*> I = df_begin(this), E = df_end(this);
      I != E; ++I) {
    Node *n = *I;
    if (n->dout() >= 2) {
      for (Node::child_iterator it = n->succs_begin(), et = n->succs_end();
            it != et; ++it) {
        Edge *e = n->edgeto(*it);
        if (e) _walkpdt(n, *it, *e);
      }
    }
  }
  // find exit edges
  for (Node::child_iterator it = FCFG.nexit.preds_begin(),
        et = FCFG.nexit.preds_end(); it != et; ++it) {
    Edge *e = (*it)->edgeto(&FCFG.nexit);
    if (!e) continue;
    // we found an exit edge
    Edge dual = getDual(*e);
    _walkpdt(&FCFG.nentry, &FCFG.getNodeFor(getHeader()), dual, *it);
  }

  DEBUG_TRACE({
    // dump CD
    dbgs() << "Control dependence:\n";
    for (CD_map_t::iterator I=CD.begin(), E=CD.end(); I!=E; ++I) {
      dbgs().indent(4) << "BB#" << I->first->getNumber() << ": { ";
      for (CD_map_entry_t::iterator EI=I->second.begin(), EE=I->second.end();
           EI!=EE; ++EI) {
        Node *n = EI->first;
        Edge e  = EI->second;
        FCFG.printNode(*n) << "(" << ((e.first) ? e.first->getNumber() : -1) << ","
                      << e.second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });
}

///////////////////////////////////////////////////////////////////////////////

void SPScope::decompose(void) {
  R_t R;
  K_t K;
  int p = 0;
  for (unsigned i=0; i<Blocks.size(); i++) {
    const MachineBasicBlock *MBB = Blocks[i];
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
    dbgs().indent(2) << "map R: MBB -> pN\n";
    for (R_t::iterator RI = R.begin(), RE = R.end(); RI != RE; ++RI) {
      dbgs().indent(4) << "R(" << RI->first->getNumber() << ") = p"
                       << RI->second << "\n";
    }
    dbgs().indent(2) << "map K: pN -> t \\in CD\n";
    for (unsigned long i = 0; i < K.size(); i++) {
      dbgs().indent(4) << "K(p" << i << ") -> {";
      for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
            EI!=EE; ++EI) {
        Node *n = EI->first;
        Edge e  = EI->second;
        FCFG.printNode(*n) << "(" << ((e.first) ? e.first->getNumber() : -1)
                           << "," << e.second->getNumber() << "), ";
      }
      dbgs() << "}\n";
    }
  });



  // Properly assign the Uses/Defs
  PredCount = K.size();
  PredUse = R;
  // initialize number of defining edges to 0 for all predicates
  NumPredDefEdges = std::vector<unsigned>( K.size(), 0 );

  // For each predicate, compute defs
  for (unsigned int i=0; i<K.size(); i++) {
    // store number of defining edges
    NumPredDefEdges[i] = K[i].size();
    // for each definition edge
    for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
              EI!=EE; ++EI) {
      Node *n = EI->first;
      Edge e  = EI->second;
      if (n == &FCFG.nentry) {
        // Pseudo edge (from start node)
        //assert(e.first == NULL);
        assert(e.second == getHeader());
        continue;
      }

      // get pred definition info of node
      PredDefInfo &PredDef = getOrCreateDefInfo(n->MBB);
      // insert definition edge for predicate i
      PredDef.define(i, e);
    } // end for each definition edge
  }
}


///////////////////////////////////////////////////////////////////////////////


raw_ostream& SPScope::FCFG::printNode(Node &n) {
  raw_ostream& os = dbgs();
  if (&n == &nentry) {
    os << "_S<" << n.num << ">";
  } else if (&n == &nexit) {
    os << "_T<" << n.num << ">";
  } else {
    os << "BB#" << n.MBB->getNumber() << "<" << n.num << ">";
  }
  return os;
}

void SPScope::dumpfcfg(void) {
  dbgs() << "==========\nFCFG [BB#" << getHeader()->getNumber() << "]\n";

  for (df_iterator<SPScope*> I = df_begin(this), E = df_end(this);
      I != E; ++I) {

    dbgs().indent(2);
    FCFG.printNode(**I) << " ipdom ";
    FCFG.printNode(*(*I)->ipdom) << " -> {";
    // print outgoing edges
    for (Node::child_iterator SI = (*I)->succs_begin(), SE = (*I)->succs_end();
          SI != SE; ++SI ) {
      FCFG.printNode(**SI) << ", ";
    }
    dbgs() << "}\n";
  }
}


///////////////////////////////////////////////////////////////////////////////




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


static void printUDInfo(const SPScope &S, raw_ostream& os,
                        const MachineBasicBlock *MBB) {
  os << "  u=" << S.getPredUse(MBB);
  const SPScope::PredDefInfo *DI = S.getDefInfo(MBB);
  if (DI) {
    os << " d=";
    for (SPScope::PredDefInfo::iterator pi = DI->begin(), pe = DI->end();
        pi != pe; ++pi) {
      os << pi->first << ",";
    }
  }
  os << "\n";
}



void SPScope::dump(raw_ostream& os) const {
  os.indent(2*Depth) <<  "[BB#" << Blocks.front()->getNumber() << "]";
  if (!Parent) {
    os << " (top)";
    assert(ExitEdges.empty());
    assert(Latches.empty());
  }
  if (!ExitEdges.empty()) {
    os << " -> { ";
    for (unsigned i=0; i<ExitEdges.size(); i++) {
      os << "BB#" << ExitEdges[i].second->getNumber() << " ";
    }
    os << "}";
  }
  if (!Latches.empty()) {
    os << " L { ";
    for (unsigned i=0; i<Latches.size(); i++) {
      os << "BB#" << Latches[i]->getNumber() << " ";
    }
    os << "}";
  }
  os << " |P|=" <<  PredCount;
  printUDInfo(*this, os, Blocks.front());

  for (unsigned i=1; i<Blocks.size(); i++) {
    MachineBasicBlock *MBB = Blocks[i];
    os.indent(2*(Depth+1)) << " BB#" << MBB->getNumber();
    printUDInfo(*this, os, MBB);
    if (HeaderMap.count(MBB)) {
      HeaderMap.at(MBB)->dump(os);
    }
  }
}
//
//
///////////////////////////////////////////////////////////////////////////////

int SPScope::getPredUse(const MachineBasicBlock *MBB) const {
  if (PredUse.count(MBB)) {
    return PredUse.at(MBB);
  }
  return -1;
}

const SPScope::PredDefInfo *
SPScope::getDefInfo( const MachineBasicBlock *MBB) const {

  if (PredDefs.count(MBB)) {
    return &PredDefs.at(MBB);
  }
  return NULL;
}

SPScope::PredDefInfo &
SPScope::getOrCreateDefInfo(const MachineBasicBlock *MBB) {

  if (!PredDefs.count(MBB)) {
    // Create new info
    PredDefs.insert(std::make_pair(MBB, PredDefInfo()));
  }

  return PredDefs.at(MBB);
}



///////////////////////////////////////////////////////////////////////////////

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
PatmosSinglePathInfo::createSPScopeTree(MachineFunction &MF) {
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

    MBBScopeMap.insert(std::make_pair(MBB, M[Loop]));
  }

  return Root;
}

