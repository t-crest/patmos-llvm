//==-- SPScope.cpp - Single-Path Scope -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
#define DEBUG_TYPE "patmos-singlepath"

#include "SPScope.h"
#include "Patmos.h"
#include "PatmosSinglePathInfo.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineFunction.h"

using namespace llvm;

/// Node - a node used internally in the scope to construct a forward CFG
/// of the scope MBBs
class Node {
  public:
    typedef std::vector<Node*>::iterator child_iterator;
    Node(const MachineBasicBlock *mbb=NULL)
      : MBB(mbb), num(-1), ipdom(NULL) {}
    const MachineBasicBlock *MBB;
    int num;
    Node *ipdom;
    void connect(Node &n) {
      succs.push_back(&n);
      n.preds.push_back(this);
    }
    void connect(Node &n, SPScope::Edge e) {
      outedges[&n] = e;
      connect(n);
    }
    unsigned long dout() { return succs.size(); }
    SPScope::Edge *edgeto(Node *n) {
      if (outedges.count(n)) {
        return &outedges.at(n);
      }
      return (SPScope::Edge *) NULL;
    }
    child_iterator succs_begin() { return succs.begin(); }
    child_iterator succs_end()   { return succs.end(); }
    child_iterator preds_begin() { return preds.begin(); }
    child_iterator preds_end()   { return preds.end(); }
  private:
    std::vector<Node *> succs;
    std::vector<Node *> preds;
    std::map<Node *, SPScope::Edge> outedges;
};

class FCFG {
  public:
    explicit FCFG(const MachineBasicBlock *header) {
      toexit(nentry);
      nentry.connect(getNodeFor(header),
          std::make_pair((const MachineBasicBlock *)NULL, header));
    }
    Node nentry, nexit;
    Node &getNodeFor(const MachineBasicBlock *MBB) {
      if (!nodes.count(MBB)) {
        nodes.insert(std::make_pair(MBB, Node(MBB)));
      }
      return nodes.at(MBB);
    }
    void toexit(Node &n) { n.connect(nexit); }
    void toexit(Node &n, SPScope::Edge &e) { n.connect(nexit, e); }
    void postdominators(void) {
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
    raw_ostream& printNode(Node &n) {
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
  private:
    std::map<const MachineBasicBlock*, Node> nodes;
    void _rdfs(Node *n, std::set<Node*> &V,
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
    Node *_intersect(Node *b1, Node *b2) {
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
};

namespace llvm{
  // Allow clients to iterate over the Scope FCFG nodes
  template <> struct GraphTraits<FCFG*> {
    typedef Node NodeType;
    typedef Node::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(FCFG *f) { return &f->nentry; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->succs_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->succs_end();
    }
  };

  // For iteration over child scopes
  template <> struct GraphTraits<SPScope*> {
    typedef SPScope NodeType;
    typedef SPScope::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(SPScope *S) { return S; }
    static inline ChildIteratorType child_begin(NodeType *N) {
     return N->child_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
     return N->child_end();
    }
  };
}

// The private implementation of SPScope using the PIMPL pattern.
class SPScope::Impl {
public:

// Typedefs
  typedef std::set<std::pair<Node*, Edge> > CD_map_entry_t;
  typedef std::map<const MachineBasicBlock*, CD_map_entry_t> CD_map_t;
  /**
   *  A map over which predicate is the guard for each basic block.
   */
  typedef std::map<const MachineBasicBlock*, unsigned> MBBPredicates_t;

//Fields

  // A reference to the RAInfo that uses this instance
  // to implement its private members. (I.e. the public part
  // of the implementation)
  SPScope &Pub;

  // parent SPScope
  SPScope *Parent;

  // exit edges
  SmallVector<Edge, 4> ExitEdges;

  // loop bound
  boost::optional<unsigned> LoopBound;

  // Whether this scope is part of a root single-path function
  const bool RootFunc;

  /// The MBBs that are either exclusively contained in this scope,
  /// or are headers of this scope's subscopes.
  /// It is sorted in topological order.
  std::vector<MachineBasicBlock*> Blocks;

  // sub-scopes
  std::vector<SPScope*> Subscopes;

  /// Number of predicates used
  unsigned PredCount;

  /// Map MBBs to predicate they use
  MBBPredicates_t PredUse;

  /// PredDefs - Stores predicate define information for each basic block
  std::map<const MachineBasicBlock*, PredDefInfo> PredDefs;

//Constructors
  Impl(SPScope *pub, SPScope * parent, bool rootFunc, MachineBasicBlock *header):
    Pub(*pub), Parent(parent), RootFunc(rootFunc),
    LoopBound(boost::none), PredCount(0)
  {}

//Functions

  FCFG buildfcfg(void) {
    std::set<const MachineBasicBlock *> body(++Blocks.begin(), Blocks.end());
    std::vector<Edge> outedges;

    FCFG fcfg(Pub.getHeader());

    for (unsigned i=0; i<Blocks.size(); i++) {
      MachineBasicBlock *MBB = Blocks[i];


      if (Pub.isSubHeader(MBB)) {
        const SPScope *subloop = get(Pub.findMBBScope(MBB));
        // successors of the loop
        outedges.insert(outedges.end(),
                        subloop->Priv->ExitEdges.begin(),
                        subloop->Priv->ExitEdges.end());
      } else {
        // simple block
        for (MachineBasicBlock::succ_iterator si = MBB->succ_begin(),
              se = MBB->succ_end(); si != se; ++si) {
          outedges.push_back(std::make_pair(MBB, *si));
        }
      }

      Node &n = fcfg.getNodeFor(MBB);
      for (unsigned i=0; i<outedges.size(); i++) {
        const MachineBasicBlock *succ = outedges[i].second;
        if (body.count(succ)) {
          Node &ns = fcfg.getNodeFor(succ);
          n.connect(ns, outedges[i]);
        } else {
          if (succ != Pub.getHeader()) {
            // record exit edges
            fcfg.toexit(n, outedges[i]);
          } else {
            // we don't need back edges recorded
            fcfg.toexit(n);
          }
        }
      }

      // special case: top-level loop has no exit/backedge
      if (outedges.empty()) {
        assert(Pub.isTopLevel());
        fcfg.toexit(n);
      }
      outedges.clear();
    }
    return fcfg;
  }

  void _walkpdt(Node *a, Node *b, Edge &e, CD_map_t &CD) {
    _walkpdt(a, b, e, a, CD);
  }

  void _walkpdt(Node *a, Node *b, Edge &e, Node *edgesrc,  CD_map_t &CD) {
    Node *t = b;
    while (t != a->ipdom) {
      // add edge e to control dependence of t
      CD[t->MBB].insert(std::make_pair(edgesrc, e));
      t = t->ipdom;
    }
  }

  void decompose(CD_map_t &CD, FCFG &fcfg) {
      MBBPredicates_t mbbPreds;
      std::vector<CD_map_entry_t> K;

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
          mbbPreds[MBB] = q;
        } else {
          // new dependence set:
          K.push_back(t);
          mbbPreds[MBB] = p++;
        }
      } // end for each MBB

      DEBUG_TRACE({
        // dump R, K
        dbgs() << "Decomposed CD:\n";
        dbgs().indent(2) << "map R: MBB -> pN\n";
        for (MBBPredicates_t::iterator RI = mbbPreds.begin(), RE = mbbPreds.end(); RI != RE; ++RI) {
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
            fcfg.printNode(*n) << "(" << ((e.first) ? e.first->getNumber() : -1)
                               << "," << e.second->getNumber() << "), ";
          }
          dbgs() << "}\n";
        }
      });

      // Properly assign the Uses/Defs
      PredCount = K.size();
      PredUse = mbbPreds;

      // For each predicate, compute defs
      for (unsigned int i=0; i<K.size(); i++) {
        // for each definition edge
        for (CD_map_entry_t::iterator EI=K[i].begin(), EE=K[i].end();
                  EI!=EE; ++EI) {
          Node *n = EI->first;
          Edge e  = EI->second;
          if (n == &fcfg.nentry) {
            // Pseudo edge (from start node)
            //assert(e.first == NULL);
            assert(e.second == Pub.getHeader());
            continue;
          }

          // get pred definition info of node
          PredDefInfo &PredDef = getOrCreateDefInfo(n->MBB);
          // insert definition edge for predicate i
          PredDef.push_back(std::make_pair(i, e));
        } // end for each definition edge
      }
    }

  void toposort(FCFG &fcfg) {
    // dfs the fcfg in postorder
    std::vector<MachineBasicBlock *> PO;
    for (po_iterator<FCFG*> I = po_begin(&fcfg), E = po_end(&fcfg);
        I != E; ++I) {
      MachineBasicBlock *MBB = const_cast<MachineBasicBlock*>((*I)->MBB);
      if (MBB) PO.push_back(MBB);
    }
    // clear the blocks vector and re-insert MBBs in reverse post order
    Blocks.clear();
    Blocks.insert(Blocks.end(), PO.rbegin(), PO.rend());
  }

  CD_map_t ctrldep(FCFG &fcfg){
    CD_map_t CD;
    for (df_iterator<FCFG*> I = df_begin(&fcfg), E = df_end(&fcfg);
        I != E; ++I) {
      Node *n = *I;
      if (n->dout() >= 2) {
        for (Node::child_iterator it = n->succs_begin(), et = n->succs_end();
              it != et; ++it) {
          Edge *e = n->edgeto(*it);
          if (e) _walkpdt(n, *it, *e, CD);
        }
      }
    }
    // find exit edges
    for (Node::child_iterator it = fcfg.nexit.preds_begin(),
          et = fcfg.nexit.preds_end(); it != et; ++it) {
      Edge *e = (*it)->edgeto(&fcfg.nexit);
      if (!e) continue;
      // we found an exit edge
      Edge dual = getDual(*e);
      _walkpdt(&fcfg.nentry, &fcfg.getNodeFor(Pub.getHeader()), dual, *it, CD);
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
          fcfg.printNode(*n) << "(" << ((e.first) ? e.first->getNumber() : -1) << ","
                        << e.second->getNumber() << "), ";
        }
        dbgs() << "}\n";
      }
    });
    return CD;
  }

  void dumpfcfg(FCFG &fcfg){
    dbgs() << "==========\nFCFG [BB#" << Pub.getHeader()->getNumber() << "]\n";

    for (df_iterator<FCFG*> I = df_begin(&fcfg), E = df_end(&fcfg);
        I != E; ++I) {

      dbgs().indent(2);
      fcfg.printNode(**I) << " ipdom ";
      fcfg.printNode(*(*I)->ipdom) << " -> {";
      // print outgoing edges
      for (Node::child_iterator SI = (*I)->succs_begin(), SE = (*I)->succs_end();
            SI != SE; ++SI ) {
        fcfg.printNode(**SI) << ", ";
      }
      dbgs() << "}\n";
    }
  }

  /// getOrCreateDefInfo - Returns a predicate definition info
  /// for a given MBB.
  PredDefInfo &
  getOrCreateDefInfo(const MachineBasicBlock *MBB) {

    if (!PredDefs.count(MBB)) {
      // Create new info
      PredDefs.insert(std::make_pair(MBB, PredDefInfo()));
    }

    return PredDefs.at(MBB);
  }

  Edge getDual(Edge &e) const {
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

  bool containsMbb(const MachineBasicBlock *mbb)
  {
    return std::find(Blocks.begin(), Blocks.end(), mbb) != Blocks.end();
  }

  void computePredInfos(void) {

    auto fcfg = buildfcfg();
    toposort(fcfg);
    fcfg.postdominators();
    DEBUG_TRACE(dumpfcfg(fcfg)); // uses info about pdom
    CD_map_t CD = ctrldep(fcfg);
    decompose(CD, fcfg);
  }

  /// addMBB - Add an MBB to the SP scope
  void addMBB(MachineBasicBlock *MBB) {

    if (Blocks.front() != MBB) {
      Blocks.push_back(MBB);
    }
  }

  void addChild(SPScope * child, MachineBasicBlock *childHeader)
  {
    Subscopes.push_back(child);
    addMBB(childHeader);
  }

  /// Returns the number of definitions the given predicate
  /// has in the scope.
  unsigned getNumDefs(unsigned pred)
  {
    // NOTE(Emad): I'm not sure this actually returns the correct result.
    // Through it seems to be correct enough to use in SPScope::hasMultipleDefs
    unsigned count = 0;
    std::for_each(PredUse.begin(), PredUse.end(), [&](auto pair){
      // Look for all MBBs that use that predicate
      if(pair.second == pred){
        auto predDef = PredDefs.find(pair.first);
        if(predDef != PredDefs.end()){
          // sum all their definition edges
          count += predDef->second.size();
        }
      }
    });
    return count;
  }

};

SPScope::SPScope(MachineBasicBlock *header, bool isRootFunc)
  : Priv(spimpl::make_unique_impl<Impl>(this, (SPScope *)NULL, isRootFunc, header))
{
  // add header also to this SPScope's block list
  Priv->Blocks.push_back(header);

}

SPScope::SPScope(SPScope *parent, MachineLoop &loop)
  : Priv(spimpl::make_unique_impl<Impl>(this, parent, parent->Priv->RootFunc, loop.getHeader()))
{

  assert(parent);
  MachineBasicBlock *header = loop.getHeader();

  // add header also to this SPScope's block list
  Priv->Blocks.push_back(header);

  // info about loop exit edges
  loop.getExitEdges(Priv->ExitEdges);

  // scan the header for loopbound info
  for (MachineBasicBlock::iterator MI = header->begin(), ME = header->end();
      MI != ME; ++MI) {
    if (MI->getOpcode() == Patmos::PSEUDO_LOOPBOUND) {
      // max is the second operand (idx 1)
      Priv->LoopBound = boost::make_optional(MI->getOperand(1).getImm() + 1);
      break;
    }
  }

  // add to parent's child list
  parent->Priv->addChild(this, loop.getHeader());
}

/// free the child scopes first, cleanup
SPScope::~SPScope() {
  for (unsigned i=0; i<Priv->Subscopes.size(); i++) {
    delete Priv->Subscopes[i];
  }
}

bool SPScope::isHeader(const MachineBasicBlock *MBB) const {
  return getHeader() == MBB;
}

bool SPScope::isSubHeader(MachineBasicBlock *MBB) const {
  return std::any_of(child_begin(), child_end(), [MBB](auto child){
    return child->isHeader(MBB);
  });
}

const std::vector<const MachineBasicBlock *> SPScope::getSuccMBBs() const {
  std::vector<const MachineBasicBlock *> SuccMBBs;
  for (unsigned i=0; i<Priv->ExitEdges.size(); i++) {
    SuccMBBs.push_back(Priv->ExitEdges[i].second);
  }
  return SuccMBBs;
}

void SPScope::walk(SPScopeWalker &walker) {
  walker.enterSubscope(this);
  for (unsigned i=0; i<Priv->Blocks.size(); i++) {
    MachineBasicBlock *MBB = Priv->Blocks[i];
    if (isSubHeader(MBB)) {
      get(findMBBScope(MBB))->walk(walker);
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
    for (auto pi = DI->begin(), pe = DI->end();
        pi != pe; ++pi) {
      os << pi->first << ",";
    }
  }
  os << "\n";
}

void SPScope::dump(raw_ostream& os) const {
  os.indent(2*getDepth()) <<  "[BB#" << Priv->Blocks.front()->getNumber() << "]";
  if (!Priv->Parent) {
    os << " (top)";
    assert(Priv->ExitEdges.empty());
  }
  if (!Priv->ExitEdges.empty()) {
    os << " -> { ";
    for (unsigned i=0; i<Priv->ExitEdges.size(); i++) {
      os << "BB#" << Priv->ExitEdges[i].second->getNumber() << " ";
    }
    os << "}";
  }

  os << " |P|=" <<  Priv->PredCount;
  printUDInfo(*this, os, Priv->Blocks.front());

  for (unsigned i=1; i<Priv->Blocks.size(); i++) {
    MachineBasicBlock *MBB = Priv->Blocks[i];
    os.indent(2*(getDepth()+1)) << " BB#" << MBB->getNumber();
    printUDInfo(*this, os, MBB);
    if (isSubHeader(MBB)) {
      get(findMBBScope(MBB))->dump(os);
    }
  }
}

unsigned SPScope::getPredUse(const MachineBasicBlock *MBB) const {
  if (Priv->PredUse.count(MBB)) {
    return Priv->PredUse.at(MBB);
  } else {
    errs()  << "Cannot find Predicate for MachineBasicBlock '" << MBB
            << "'(BasicBlock '" << MBB->getBasicBlock()->getName()
            << "') in the scope with header '" << getHeader()
            << "'(BasicBlock '" << getHeader()->getBasicBlock()->getName()
            << "').\n";
    abort();
    return 0; // Unreachable, removed warning about missing return value.
  }
}

const SPScope::PredDefInfo *
SPScope::getDefInfo( const MachineBasicBlock *MBB) const {

  if (Priv->PredDefs.count(MBB)) {
    return &Priv->PredDefs.at(MBB);
  }
  return NULL;
}

bool SPScope::isTopLevel() const { return (NULL == Priv->Parent); }

const SPScope *SPScope::getParent() const { return Priv->Parent; }

bool SPScope::isRootTopLevel() const { return Priv->RootFunc && isTopLevel(); }

bool SPScope::hasLoopBound() const { return Priv->LoopBound.is_initialized(); }

boost::optional<unsigned> SPScope::getLoopBound() const { return Priv->LoopBound; }

SPScope::iterator SPScope::begin() { return Priv->Blocks.begin(); }

SPScope::iterator SPScope::end() { return Priv->Blocks.end(); }

MachineBasicBlock *SPScope::getHeader() const { return Priv->Blocks.front(); }

const std::vector<MachineBasicBlock*> &SPScope::getBlocks() const
{
  return Priv->Blocks;
}

SPScope::child_iterator SPScope::child_begin() const { return Priv->Subscopes.begin(); }

SPScope::child_iterator SPScope::child_end() const { return Priv->Subscopes.end(); }

boost::optional<SPScope*> SPScope::findMBBScope(const MachineBasicBlock *mbb) const
{
  boost::optional<SPScope*> found = boost::none;
  for(auto i = Priv->Subscopes.begin(), end=Priv->Subscopes.end(); i != end; ++i)
  {
    SPScope* child = *i;
    boost::optional<SPScope*> temp = child->findMBBScope(mbb);
    //Ensure two different children don't have the same MBB
    assert(!(temp.is_initialized() && found.is_initialized()));
    if(temp.is_initialized()){
      found = temp;
    }
  }

  if(found.is_initialized()){
    return found;
  } else if(Priv->containsMbb(mbb)){
    return boost::make_optional((SPScope*)this);
  }else{
    return boost::none;
  }
}

unsigned SPScope::getDepth() const { return (Priv->Parent == NULL)? 0 : Priv->Parent->getDepth() + 1; }

unsigned SPScope::getNumPredicates() const { return Priv->PredCount; }

bool SPScope::hasMultDefEdges(unsigned pred) const
{
  return Priv->getNumDefs(pred) > 1;
}

// build the SPScope tree in DFS order, creating new SPScopes preorder
static
void createSPScopeSubtree(MachineLoop *loop, SPScope *parent,
                         std::map<const MachineLoop *, SPScope *> &M) {

  SPScope *childScope = new SPScope(parent, *loop);

  // update map: Loop -> SPScope
  M[loop] = childScope;
  // visit subloops
  for (MachineLoop::iterator I = loop->begin(), E = loop->end();
          I != E; ++I) {
    createSPScopeSubtree(*I, childScope, M);
  }
}

SPScope * SPScope::createSPScopeTree(MachineFunction &MF, MachineLoopInfo &LI) {

  // First, create a SPScope tree
  std::map<const MachineLoop *, SPScope *> M;

  SPScope *Root = new SPScope(&MF.front(), PatmosSinglePathInfo::isRoot(MF));


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
    M[Loop]->Priv->addMBB(MBB);
  }

  // analyze each scope
  // NB: this could be solved more elegantly by analyzing a scope when it is
  // built. But how he tree is created right now, it will not become more
  // elegant anyway.
  for(df_iterator<SPScope*> I = df_begin(Root), E = df_end(Root); I != E; ++I)
  {
    (*I)->Priv->computePredInfos();
  }

  return Root;
}

