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
#include "PredicatedBlock.h"

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

  // loop bound
  boost::optional<unsigned> LoopBound;

  // Whether this scope is part of a root single-path function
  const bool RootFunc;

  /// The MBBs that are exclusively contained in this scope.
  /// The header of the scope is always the first element.
  std::vector<PredicatedBlock> Blocks;

  // Tracks subheaders using PredicatedBlocks.
  // The PredicatedBlock of a subscope's header is different in the parent
  // than it is in the subscope itself.
  // E.g. for block 'b' which is a header of 'S',
  // 'b' in 'S.getParent().SubHeaderPredicate' might have a different
  // predicate that 'b' in 'S.Blocks'.
  std::vector<PredicatedBlock> SubHeaderPredicates;

  // sub-scopes
  std::vector<SPScope*> Subscopes;

  /// Number of predicates used
  unsigned PredCount;

//Constructors
  Impl(SPScope *pub, SPScope * parent, bool rootFunc, MachineLoop *loop, MachineBasicBlock *header, MachineFunction &MF, MachineLoopInfo &LI):
    Pub(*pub), Parent(parent), LoopBound(boost::none), RootFunc(rootFunc),
    PredCount(0)
  {
    Blocks.push_back(PredicatedBlock(header,0));

    // add the rest of the MBBs to the scope
    for (MachineFunction::iterator FI=MF.begin(), FE=MF.end();
            FI!=FE; ++FI) {
      MachineBasicBlock *MBB = FI;
      if(LI[MBB] == loop){
        addMBB(MBB);
      }
    }
  }

//Functions

  FCFG buildfcfg(void) {

    auto fcfgBlocks = Pub.getScopeBlocks();
    std::set<const MachineBasicBlock *> succBlocks(++fcfgBlocks.begin(), fcfgBlocks.end());
    std::for_each(Pub.child_begin(), Pub.child_end(), [&](auto child){
      fcfgBlocks.push_back(child->getHeader());
      succBlocks.insert(child->getHeader());
    });

    FCFG fcfg(Pub.getHeader());

    std::for_each(fcfgBlocks.begin(), fcfgBlocks.end(), [&](auto MBB){
//      std::vector<Edge> outedges;
      std::set<Edge> outedges;
      if (Pub.isSubHeader(MBB)) {
        const SPScope *subloop = get(Pub.findMBBScope(MBB));
        outedges = subloop->Priv->getOutEdges();
      } else {
        // simple block
        for (auto si = MBB->succ_begin(),
              se = MBB->succ_end(); si != se; ++si) {
          outedges.insert(std::make_pair(MBB, *si));
        }
      }

      Node &n = fcfg.getNodeFor(MBB);
      std::for_each(outedges.begin(), outedges.end(), [&](auto edge){
        auto succ = edge.second;
        // if succ is one of the fcfg blocks except the scope header.
        if (succBlocks.count(succ)) {
          Node &ns = fcfg.getNodeFor(succ);
          n.connect(ns, edge);
        } else {
          if (succ != Pub.getHeader()) {
            // record exit edges
            fcfg.toexit(n, edge);
          } else {
            // we don't need back edges recorded
            fcfg.toexit(n);
          }
        }
      });

      // special case: top-level loop has no exit/backedge
      if (outedges.empty()) {
        //assert(Pub.isTopLevel());
        fcfg.toexit(n);
      }
    });
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
      auto blocks = Pub.getBlocksTopoOrd();
      std::for_each(blocks.begin(), blocks.end(), [&](auto MBB){
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
      }); // end for each MBB

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
      // Assign each block's predicate
      for(auto iter = Blocks.begin(), end = Blocks.end(); iter != end; ++iter)
      {
        auto b = &*iter;
        b->setPredicate(mbbPreds[b->getMBB()]);
      }

      // Assign subHeader's predicates
      for(auto subscope_iter = Pub.child_begin(), end = Pub.child_end(); subscope_iter != end; ++subscope_iter)
      {
        auto subscope = *subscope_iter;
        SubHeaderPredicates.push_back(PredicatedBlock(subscope->getHeader(), mbbPreds[subscope->getHeader()]));
      }

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

          // insert definition edge for predicate i
          auto pBlock = getPredicatedFcfg(n->MBB);
          assert(pBlock.is_initialized());
          get(pBlock)->addDefinition(i, e.second);
        } // end for each definition edge
      }
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

  bool fcfgContainsMbb(const MachineBasicBlock *mbb)
  {
    return std::any_of(Blocks.begin(), Blocks.end(), [&](auto MBB){return MBB.getMBB() == mbb;}) ||
        std::any_of(Pub.child_begin(), Pub.child_end(), [&](auto child){ return child->getHeader() == mbb;});
  }

  void computePredInfos(void) {

    auto fcfg = buildfcfg();
    //toposort(fcfg);
    fcfg.postdominators();
    DEBUG_TRACE(dumpfcfg(fcfg)); // uses info about pdom
    CD_map_t CD = ctrldep(fcfg);
    decompose(CD, fcfg);
  }

  /// addMBB - Add an MBB to the SP scope
  void addMBB(MachineBasicBlock *MBB) {

    if (Blocks.front().getMBB() != MBB) {
      Blocks.push_back(PredicatedBlock(MBB,0));
    }
  }

  /// Returns the number of definitions the given predicate
  /// has in the scope.
  unsigned getNumDefs(unsigned pred)
  {
    // NOTE(Emad): I'm not sure this actually returns the correct result.
    // Through it seems to be correct enough to use in SPScope::hasMultipleDefs
    unsigned count = 0;
    std::vector<PredicatedBlock*> blocksUsingPred;

    auto addAllUsingPred = [&](auto container){
      for( auto iter = container->begin(), end = container->end(); iter != end; ++iter)
      {
        auto blockPredicates = (*iter).getBlockPredicates();
        if (blockPredicates.find(pred) != blockPredicates.end()) {
          blocksUsingPred.push_back(&*iter);
        }
      }
    };

    // Look for all MBBs that use that predicate
    addAllUsingPred(&Blocks);
    addAllUsingPred(&SubHeaderPredicates);

    // Sum the number of definition edges of all the blocks using pred
    std::for_each(blocksUsingPred.begin(), blocksUsingPred.end(), [&](auto b){
      count +=b->getDefinitions().size();
    });
    return count;
  }

  static boost::optional<PredicatedBlock*>
  getPredicatedFrom(const MachineBasicBlock *mbb, std::vector<PredicatedBlock>* container)
  {
    auto found = std::find_if(container->begin(), container->end(), [&](auto block){
      return block.getMBB() == mbb;
    });

    return found != container->end()?
      boost::make_optional(&*found) :
      boost::none;
  }

  /// Returns the PredicatedBlock of the given MBB, if it exists exclusively
  /// in this scope.
  boost::optional<PredicatedBlock*> getPredicated(const MachineBasicBlock *mbb)
  {
    return getPredicatedFrom(mbb, &Blocks);
  }

  /// Returns the PredicatedBlock of the given MBB, if it is either exclusively
  /// in this scope, or is a subheader of the scope.
  boost::optional<PredicatedBlock*> getPredicatedFcfg(const MachineBasicBlock *mbb)
  {
    auto pBlocks = getPredicated(mbb);
    if(!pBlocks.is_initialized())
    {
      return getPredicatedFrom(mbb, &SubHeaderPredicates);
    }
    return pBlocks;
  }

  /// Returns all edges in the control flow who's source is inside the loop and
  /// target is outside the loop.
  /// The source may therefore be a MBB that resides in a subscope of this scope.
  /// The target will always be a MBB that is not in this scope or any subscope.
  const std::set<SPScope::Edge> getOutEdges() const
  {
    std::set<SPScope::Edge> outs;

    for(auto b: Blocks)
    {
      auto exits = b.getExitTargets();
      for(auto t: exits)
      {
        outs.insert(std::make_pair(b.getMBB(),t));
      }
    }

    for(auto subscope: Subscopes){
      auto subexits = subscope->Priv->getOutEdges();
      for(auto edge: subexits)
      {
        if(std::none_of(Blocks.begin(), Blocks.end(), [&](auto pb){ return pb.getMBB() == edge.second;}))
        {
          outs.insert(edge);
        }
      }
    }
    return outs;
  }
};

SPScope::SPScope(bool isRootFunc, MachineFunction &MF, MachineLoopInfo &LI)
  : Priv(spimpl::make_unique_impl<Impl>(this, (SPScope *)NULL, isRootFunc, (MachineLoop*)NULL, &MF.front(), MF, LI))
{}

SPScope::SPScope(SPScope *parent, MachineLoop &loop, MachineFunction &MF, MachineLoopInfo &LI)
  : Priv(spimpl::make_unique_impl<Impl>(this, parent, parent->Priv->RootFunc, &loop, loop.getHeader(), MF, LI))
{

  assert(parent);
  MachineBasicBlock *header = loop.getHeader();

  // info about loop exit edges
  SmallVector<Edge, 4> ExitEdges;
  loop.getExitEdges(ExitEdges);

  std::for_each(ExitEdges.begin(), ExitEdges.end(), [&](auto edge){
    for(auto iter = Priv->Blocks.begin(), end = Priv->Blocks.end(); iter != end; iter++){
      if(iter->getMBB() == edge.first){
        iter->addExitTarget(edge.second);
        return;
      }
    }
  });

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
  parent->Priv->Subscopes.push_back(this);
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

bool SPScope::isSubHeader(const MachineBasicBlock *MBB) const {
  return std::any_of(child_begin(), child_end(), [MBB](auto child){
    return child->isHeader(MBB);
  });
}

const std::set<const MachineBasicBlock *> SPScope::getSuccMBBs() const {
  std::set<const MachineBasicBlock *> succMBBs;
  auto outEdges = Priv->getOutEdges();
  for(auto edge: outEdges)
  {
    succMBBs.insert(edge.second);
  }
  return succMBBs;
}

void SPScope::walk(SPScopeWalker &walker) {
  walker.enterSubscope(this);
  auto blocks = getBlocksTopoOrd();
  std::for_each(blocks.begin(), blocks.end(), [&](auto MBB){
    if (isSubHeader(MBB)) {
      get(findMBBScope(MBB))->walk(walker);
    } else {
      walker.nextMBB(MBB);
    }
  });
  walker.exitSubscope(this);
}

static void printUDInfo(const SPScope &S, raw_ostream& os,
                        const MachineBasicBlock *MBB) {
  os << "  u=" << S.getPredUse(MBB);
  auto opDI = S.getDefInfo(MBB);
  if (opDI.is_initialized()) {
    auto DI = get(opDI);
    os << " d=";
    for (auto pi = DI.begin(), pe = DI.end();
        pi != pe; ++pi) {
      os << pi->first << ",";
    }
  }
  os << "\n";
}

void SPScope::dump(raw_ostream& os) const {
  os.indent(2*getDepth()) <<  "[BB#" << Priv->Blocks.front().getMBB()->getNumber() << ":" << Priv->Blocks.front().getMBB() <<"]";
  auto succs = getSuccMBBs();
  if (!Priv->Parent) {
    os << " (top)";
    assert(succs.empty());
  }
  if (!succs.empty()) {
    os << " -> { ";
    for (auto target: succs) {
      os << "BB#" << target->getNumber() << ":" << target << ", ";
    }
    os << "}";
  }

  os << " |P|=" <<  Priv->PredCount;
  printUDInfo(*this, os, Priv->Blocks.front().getMBB());

  os.indent(2*getDepth()) << "Blocks: {\n";
  for(auto b: Priv->Blocks){
    b.dump(os, 2*getDepth() + 2);
  }
  os.indent(2*getDepth()) << "}\n";
  os.indent(2*getDepth()) << "children:\n";
  for(auto sub: Priv->Subscopes){
    sub->dump(os);
  }
}

unsigned SPScope::getPredUse(const MachineBasicBlock *MBB) const {

  // First look for the MBB in the scope blocks
  auto scopeBlock = Priv->getPredicatedFcfg(MBB);
  if( scopeBlock.is_initialized() )
  {
    return *get(scopeBlock)->getBlockPredicates().begin();
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

boost::optional<SPScope::PredDefInfo>
SPScope::getDefInfo( const MachineBasicBlock *MBB) const {
  auto pb = Priv->getPredicatedFcfg(MBB);
  if(pb.is_initialized()){
    auto defs = get(pb)->getDefinitions();

    if(defs.size() > 0)
    {
      PredDefInfo result;
      std::for_each(defs.begin(), defs.end(), [&](auto pair){
        result.push_back(std::make_pair(pair.first, std::make_pair(MBB, pair.second)));
      });

      return result;
    }
  }
  return boost::none;
}

bool SPScope::isTopLevel() const { return (NULL == Priv->Parent); }

const SPScope *SPScope::getParent() const { return Priv->Parent; }

bool SPScope::isRootTopLevel() const { return Priv->RootFunc && isTopLevel(); }

bool SPScope::hasLoopBound() const { return Priv->LoopBound.is_initialized(); }

boost::optional<unsigned> SPScope::getLoopBound() const { return Priv->LoopBound; }

MachineBasicBlock *SPScope::getHeader() const { return Priv->Blocks.front().getMBB(); }

std::vector<MachineBasicBlock*> SPScope::getScopeBlocks() const
{
  std::vector<MachineBasicBlock*> result;
  for(auto block: Priv->Blocks)
  {
    result.push_back(block.getMBB());
  }

  return result;
}

std::vector<MachineBasicBlock*> SPScope::getBlocksTopoOrd() const
{
  auto fcfg = Priv->buildfcfg();
  // dfs the fcfg in postorder
  std::vector<MachineBasicBlock *> PO;
  for (po_iterator<FCFG*> I = po_begin(&fcfg), E = po_end(&fcfg);
      I != E; ++I) {
    MachineBasicBlock *MBB = const_cast<MachineBasicBlock*>((*I)->MBB);
    if (MBB) PO.push_back(MBB);
  }
  std::reverse(PO.begin(), PO.end());
  return PO;
}

unsigned SPScope::getNumberOfFcfgBlocks() const
{
  return getScopeBlocks().size()
      // Each subscope has 1 header, so just count supscopes
      + Priv->Subscopes.size();
}

std::vector<MachineBasicBlock*> SPScope::getFcfgBlocks() const
{
  auto result = getScopeBlocks();
  std::for_each(child_begin(), child_end(), [&](auto child){
    result.push_back(child->getHeader());
  });
  return result;
}

SPScope::child_iterator SPScope::child_begin() const { return Priv->Subscopes.begin(); }

SPScope::child_iterator SPScope::child_end() const { return Priv->Subscopes.end(); }

boost::optional<SPScope*> SPScope::findMBBScope(const MachineBasicBlock *mbb) const
{
  if(std::any_of(Priv->Blocks.begin(), Priv->Blocks.end(),
      [&](auto PB){return PB.getMBB() == mbb;})
  ){
    return boost::make_optional((SPScope*)this);
  }else{
    for(auto subscope = Priv->Subscopes.begin(), end = Priv->Subscopes.end();
          subscope != end; ++subscope){
      auto inSubscope = (*subscope)->findMBBScope(mbb);
      if(inSubscope.is_initialized()){
        return inSubscope;
      }
    }
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
void createSPScopeSubtree(MachineLoop *loop, SPScope *parent, MachineFunction &MF, MachineLoopInfo &LI) {

  SPScope *subScope = new SPScope(parent, *loop, MF, LI);

  // visit subloops
  std::for_each(loop->begin(), loop->end(), [&](auto subLoop){
    createSPScopeSubtree(subLoop, subScope, MF, LI);
  });
}

SPScope * SPScope::createSPScopeTree(MachineFunction &MF, MachineLoopInfo &LI) {

  SPScope *Root = new SPScope(PatmosSinglePathInfo::isRoot(MF), MF, LI);

  // iterate over top-level loops
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    createSPScopeSubtree(Loop, Root, MF, LI);
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

