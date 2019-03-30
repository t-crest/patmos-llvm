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
#include "PatmosSPBundling.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/CodeGen/MachineFunction.h"

using namespace llvm;

/// Node - a node used internally in the scope to construct a forward CFG
/// of the scope blocks
class Node {
  public:
    typedef std::vector<Node*>::iterator child_iterator;
    Node(const PredicatedBlock *b=NULL)
      : Block(b), num(-1), ipdom(NULL) {}
    const PredicatedBlock *Block;
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

/// represents the Forward Control-Flow Graph (FCFG) of a SPScope.
class FCFG {
  public:
    explicit FCFG(const PredicatedBlock *header) {
      toexit(nentry);
      nentry.connect(getNodeFor(header),
          std::make_pair((const PredicatedBlock *)NULL, header));
    }
    Node nentry, nexit;
    Node &getNodeFor(const PredicatedBlock *block) {
      if (!nodes.count(block)) {
        nodes.insert(std::make_pair(block, Node(block)));
      }
      return nodes.at(block);
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
        os << "BB#" << n.Block->getMBB()->getNumber() << "<" << n.num << ">";
      }
      return os;
    }
  private:
    std::map<const PredicatedBlock*, Node> nodes;
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

}

// The private implementation of SPScope using the PIMPL pattern.
class SPScope::Impl {
public:

// Typedefs
  typedef std::set<std::pair<Node*, Edge> > CD_map_entry_t;
  typedef std::map<const PredicatedBlock*, CD_map_entry_t> CD_map_t;
  /**
   *  A map over which predicate is the guard for each basic block.
   */
  typedef std::map<const PredicatedBlock*, unsigned> BlockPredicates;

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

  /// The blocks that are exclusively contained in this scope.
  /// The header of the scope is always the first element.
  std::vector<PredicatedBlock*> Blocks;

  // sub-scopes
  std::vector<SPScope*> Subscopes;

  /// Number of predicates used
  unsigned PredCount;

//Constructors
  Impl(SPScope *pub, SPScope * parent, bool rootFunc, MachineLoop *loop,
      MachineBasicBlock *header, MachineFunction &MF, MachineLoopInfo &LI):
    Pub(*pub), Parent(parent), LoopBound(boost::none), RootFunc(rootFunc),
    PredCount(0)
  {
    Blocks.push_back(new PredicatedBlock(header));

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

  /// Returns the FCFG of this scope.
  FCFG buildfcfg(void) {

    auto fcfgBlocks = Pub.getScopeBlocks();
    std::set<const PredicatedBlock *> succBlocks(++fcfgBlocks.begin(), fcfgBlocks.end());
    for(auto subscope: Subscopes){
      fcfgBlocks.push_back(subscope->getHeader());
      succBlocks.insert(subscope->getHeader());
    }

    FCFG fcfg(Pub.getHeader());

    for(auto block: fcfgBlocks){
      std::set<Edge> outedges;
      if (Pub.isSubheader(block)) {
        auto subscopeOp = Pub.findScopeOf(block);
        assert(subscopeOp.is_initialized());
        const SPScope *subscope = get(subscopeOp);
        outedges = subscope->Priv->getOutEdges();
      } else {
        // simple block
        auto succs = block->getSuccessors();
        for (auto si = succs.begin(),
              se = succs.end(); si != se; ++si) {
          outedges.insert(std::make_pair(block, si->first));
        }
      }

      Node &n = fcfg.getNodeFor(block);

      std::set<std::pair<const MachineBasicBlock*, const MachineBasicBlock*>> outedgesMbb;
      for(auto edge: outedges){
        outedgesMbb.insert(std::make_pair(edge.first->getMBB(), edge.second->getMBB()));
      }

      // For some reason, RAInfo is dependent on 'outedges' running this for loop
      // in the order dictated by the being comprised of MachineBasicBlock (i.e. outedgesMbb).
      // Changing 'outedgesMbb' might cause predicate register allocation to fail for unknown reaons.
      for(auto mbbEdge : outedgesMbb){
        auto foundEdge = std::find_if(outedges.begin(), outedges.end(), [&](auto e){
          return e.first->getMBB() == mbbEdge.first && e.second->getMBB() == mbbEdge.second;
        });
        assert(foundEdge != outedges.end());
        auto edge = *foundEdge;
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
      }

      // special case: top-level loop has no exit/backedge
      if (outedges.empty()) {
        assert(Pub.isTopLevel());
        fcfg.toexit(n);
      }
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
      CD[t->Block].insert(std::make_pair(edgesrc, e));
      t = t->ipdom;
    }
  }

  void decompose(CD_map_t &CD, FCFG &fcfg, const PatmosInstrInfo* instrInfo) {
    BlockPredicates blockPreds;
    std::map<unsigned, CD_map_entry_t> K;

    int p;
    if(!Pub.isTopLevel()){
      auto parentPreds = Parent->getAllPredicates();
      p = 1 + *max_element(std::begin(parentPreds), std::end(parentPreds));
    }else{
      p = 0;
    }

    auto blocks = Pub.getBlocksTopoOrd();
    for(auto block: blocks){
      CD_map_entry_t t = CD.at(block);
      int q=-1;
      // try to lookup the control dependence
      for (auto pair: K) {
        if ( t == pair.second ) {
          q = pair.first;
          break;
        }
      }

      if (q != -1) {
        // we already have handled this dependence
        blockPreds[block] = q;
      } else {
        // new dependence set:
        if(!Pub.isTopLevel() && block == Pub.getHeader()){
          K.insert(make_pair(*Pub.getHeader()->getBlockPredicates().begin(), t));
          blockPreds[block] = *Pub.getHeader()->getBlockPredicates().begin();
        }else{
          K.insert(make_pair(p, t));
          blockPreds[block] = p++;
        }
      }
    }

    DEBUG_TRACE({
      // dump R, K
      dbgs() << "Decomposed CD:\n";
      dbgs().indent(2) << "map R: MBB -> pN\n";
      for (BlockPredicates::iterator RI = blockPreds.begin(), RE = blockPreds.end(); RI != RE; ++RI) {
        dbgs().indent(4) << "R(" << RI->first->getMBB()->getNumber() << ") = p"
                         << RI->second << "\n";
      }
      dbgs().indent(2) << "map K: pN -> t \\in CD\n";
      for (auto pair: K) {
        dbgs().indent(4) << "K(p" << pair.first << ") -> {";
        for (CD_map_entry_t::iterator EI=pair.second.begin(), EE=pair.second.end();
              EI!=EE; ++EI) {
          Node *n = EI->first;
          Edge e  = EI->second;
          fcfg.printNode(*n) << "(" << ((e.first) ? e.first->getMBB()->getNumber() : -1)
                             << "," << e.second->getMBB()->getNumber() << "), ";
        }
        dbgs() << "}\n";
      }
    });

    // Properly assign the Uses/Defs
    PredCount = K.size();
    // Assign predicates for each block in the fcfg of this scope
    auto fcfgBlocks = Pub.getFcfgBlocks();
    std::for_each(fcfgBlocks.begin(), fcfgBlocks.end(),
            [&](auto b){ b->setPredicate(blockPreds[b]); });

    // For each predicate, compute defs
    for (auto pair: K) {
      auto map_entry = pair.second;
      // for each definition edge
      for (CD_map_entry_t::iterator EI=map_entry.begin(), EE=map_entry.end();
                EI!=EE; ++EI) {
        Node *n = EI->first;
        Edge e  = EI->second;
        if (n == &fcfg.nentry) {
          // Pseudo edge (from start node)
          assert(e.second == Pub.getHeader());
          continue;
        }

        // insert definition edge for predicate i
        auto block = (PredicatedBlock*)n->Block;
        assert(!block->getBlockPredicates().empty());

        auto condition = getCondition(block, e.second, instrInfo);

        block->addDefinition(
            pair.first, *block->getBlockPredicates().begin(),
            e.second,
            std::get<0>(condition), std::get<1>(condition));
      } // end for each definition edge
    }
  }

  /// Gets the condition predicate and flag for the transition between the given blocks
  /// E.g. if the transition is '( !P1) br targetBlock', then the first element is predicate
  /// register P1, and the second element is the false value (!).
  std::tuple<MachineOperand, MachineOperand> getCondition(
      const PredicatedBlock* sourceBlock,
      const PredicatedBlock* targetBlock, const PatmosInstrInfo* instrInfo)
  {
    MachineBasicBlock *SrcMBB = sourceBlock->getMBB(),
                      *DstMBB = targetBlock->getMBB();

    // get the branch condition
    MachineBasicBlock *TBB = NULL, *FBB = NULL;
    SmallVector<MachineOperand, 2> Cond;
    if (instrInfo->AnalyzeBranch(*SrcMBB, TBB, FBB, Cond)) {
      DEBUG(dbgs() << *SrcMBB);
      report_fatal_error("AnalyzeBranch for SP-Transformation failed; "
          "could not determine branch condition");
    }
    // following is a fix: if it appears to be an unconditional branch though
    // (disabled/missed optimization), we set the condition to P0 (true)
    if (Cond.empty()) {
        Cond.push_back(MachineOperand::CreateReg(Patmos::P0, false)); // reg
        Cond.push_back(MachineOperand::CreateImm(0)); // flag
    }
    if (TBB != DstMBB) {
      instrInfo->ReverseBranchCondition(Cond);
    }
    return std::make_tuple(Cond[0], Cond[1]);
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
        dbgs().indent(4) << "BB#" << I->first->getMBB()->getNumber() << ": { ";
        for (CD_map_entry_t::iterator EI=I->second.begin(), EE=I->second.end();
             EI!=EE; ++EI) {
          Node *n = EI->first;
          Edge e  = EI->second;
          fcfg.printNode(*n) << "(" << ((e.first) ? e.first->getMBB()->getNumber() : -1) << ","
                        << e.second->getMBB()->getNumber() << "), ";
        }
        dbgs() << "}\n";
      }
    });
    return CD;
  }

  void dumpfcfg(FCFG &fcfg){
    dbgs() << "==========\nFCFG [BB#" << Pub.getHeader()->getMBB()->getNumber() << "]\n";

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
    auto src = e.first;
    auto succs = src->getSuccessors();
    assert(succs.size() == 2);
    for (auto si = succs.begin(),
        se = succs.end(); si != se; ++si) {
      if (si->first != e.second) {
        return std::make_pair(src, si->first);
      }
    }
    llvm_unreachable("no dual edge found");
    return std::make_pair((const PredicatedBlock *) NULL,
                          (const PredicatedBlock *) NULL);
  }

  void computePredInfos(const PatmosInstrInfo* instrInfo) {

    auto fcfg = buildfcfg();
    //toposort(fcfg);
    fcfg.postdominators();
    DEBUG_TRACE(dumpfcfg(fcfg)); // uses info about pdom
    CD_map_t CD = ctrldep(fcfg);
    decompose(CD, fcfg, instrInfo);
  }

  /// addMBB - Add an MBB to the SP scope
  void addMBB(MachineBasicBlock *MBB) {

    if (Blocks.front()->getMBB() != MBB) {
      Blocks.push_back(new PredicatedBlock(MBB));
    }
  }

  /// Returns the number of definitions the given predicate
  /// has in the scope.
  unsigned getNumDefs(unsigned pred)
  {
    // NOTE(Emad): I'm not sure this actually returns the correct result.
    // Through it seems to be correct enough to use in SPScope::hasMultipleDefs
    unsigned count = 0;
    // Sum the number of blocks that define the predicate.
    // Don't check the subheaders since they can't define predicates
    // for this scope.
    for(auto b: Blocks){
      auto defs = b->getDefinitions();
      if(std::find_if(defs.begin(), defs.end(), [&](auto def){
        return def.predicate == pred;
      }) != defs.end()){
        count++;
      }
    }
    return count;
  }

  /// Returns the PredicatedBlock of the given MBB, if it exists exclusively
  /// in this scope.
  boost::optional<PredicatedBlock*> getPredicated(const MachineBasicBlock *mbb)
  {
    auto found = std::find_if(Blocks.begin(), Blocks.end(), [&](auto block){
      return block->getMBB() == mbb;
    });

    return found != Blocks.end()?
      boost::make_optional(*found) :
      boost::none;
  }

  /// Returns the PredicatedBlock of the given MBB, if it is either exclusively
  /// in this scope, or is a subheader of the scope.
  boost::optional<PredicatedBlock*> getPredicatedFcfg(const MachineBasicBlock *mbb)
  {
    auto pBlocks = getPredicated(mbb);
    if(!pBlocks.is_initialized())
    {
      for(auto subheader: getSubheaders()){
        if(subheader->getMBB() == mbb){
          return boost::make_optional(subheader);
        }
      }
    }
    return pBlocks;
  }

  /// Returns all edges in the control flow who's source is inside the loop and
  /// target is outside the loop.
  /// The source may therefore be a block that resides in a subscope of this scope.
  /// The target will always be a block that is not in this scope or any subscope.
  const std::set<SPScope::Edge> getOutEdges() const
  {
    std::set<std::pair<const PredicatedBlock*, const PredicatedBlock*>> outs;

    for(auto block: Blocks){
      auto exits = block->getExitTargets();
      for(auto t: exits){
        outs.insert(std::make_pair(block,t));
      }
    }

    return outs;
  }

  /// Searches for the given block's PredicatedBlock recursively in the parent of
  /// this scope.
  /// Returns none of no block was found in any ancestor.
  boost::optional<PredicatedBlock*> getPredicatedParent(const MachineBasicBlock *mbb)
  {
    if(Parent){
      auto pb = Parent->Priv->getPredicatedFcfg(mbb);
      return pb.is_initialized() ?
          pb :
          Parent->Priv->getPredicatedParent(mbb);
    }
    return boost::none;
  }

  std::vector<PredicatedBlock*> getSubheaders()
  {
    std::vector<PredicatedBlock*> result;
    for(auto subscope: Subscopes){
      result.push_back(subscope->getHeader());
    }
    return result;
  }

  void replaceUseOfBlockWith(PredicatedBlock* oldBlock, PredicatedBlock* newBlock){
    for(auto block: Pub.getScopeBlocks()){
      block->replaceUseOfBlockWith(oldBlock, newBlock);
    }
    for(auto subscope: Subscopes){
      subscope->Priv->replaceUseOfBlockWith(oldBlock, newBlock);
    }
  }

  void assignSuccessors(){
    // Add the successors to all blocks
    auto fcfgBlocks = Pub.getFcfgBlocks();
    if(!Pub.isTopLevel()){
      auto parentFcfgBlocks = Parent->getFcfgBlocks();
      fcfgBlocks.insert(fcfgBlocks.end(), parentFcfgBlocks.begin(), parentFcfgBlocks.end());
    }
    for(auto block: Blocks){
      auto mbb = block->getMBB();
      std::for_each(mbb->succ_begin(), mbb->succ_end(), [&](MachineBasicBlock* succMbb){
        auto found = std::find_if(fcfgBlocks.begin(), fcfgBlocks.end(),
            [&](auto b){ return b->getMBB() == succMbb;});
        assert(found != fcfgBlocks.end());
        block->addSuccessor(*found, 0);
      });
    }

    for(auto subscope: Subscopes){
      subscope->Priv->assignSuccessors();
    }
  }
};

SPScope::SPScope(bool isRootFunc, MachineFunction &MF, MachineLoopInfo &LI)
  : Priv(spimpl::make_unique_impl<Impl>(this, (SPScope *)NULL, isRootFunc, (MachineLoop*)NULL, &MF.front(), MF, LI))
{}

SPScope::SPScope(SPScope *parent, MachineLoop &loop, MachineFunction &MF, MachineLoopInfo &LI)
  : Priv(spimpl::make_unique_impl<Impl>(this, parent, parent->Priv->RootFunc, &loop, loop.getHeader(), MF, LI))
{
  parent->Priv->Subscopes.push_back(this);
  assert(parent);
  MachineBasicBlock *header = loop.getHeader();

  // info about loop exit edges
  SmallVector<std::pair<const MachineBasicBlock*,const MachineBasicBlock*>, 4> ExitEdges;
  loop.getExitEdges(ExitEdges);

  for(auto edge: ExitEdges){
    auto found = std::find_if(Priv->Blocks.begin(), Priv->Blocks.end(),
        [&](auto block){ return block->getMBB() == edge.first; });
    assert(found != Priv->Blocks.end());
    (*found)->addExitTarget(get(Priv->getPredicatedParent(edge.second)));
  }

  // scan the header for loopbound info
  for (MachineBasicBlock::iterator MI = header->begin(), ME = header->end();
      MI != ME; ++MI) {
    if (MI->getOpcode() == Patmos::PSEUDO_LOOPBOUND) {
      // max is the second operand (idx 1)
      Priv->LoopBound = boost::make_optional(MI->getOperand(1).getImm() + 1);
      break;
    }
  }
  assert(Priv->LoopBound.is_initialized());
}

/// free the child scopes first, cleanup
SPScope::~SPScope() {
  for(auto block: Priv->Blocks){
    delete block;
  }
  for (unsigned i=0; i<Priv->Subscopes.size(); i++) {
    delete Priv->Subscopes[i];
  }
}

bool SPScope::isHeader(const PredicatedBlock *block) const {
  return getHeader() == block;
}

bool SPScope::isSubheader(const PredicatedBlock *block) const {
  return std::any_of(Priv->Subscopes.begin(), Priv->Subscopes.end(),
      [&](auto subscope){return subscope->isHeader(block);});
}

const std::set<const PredicatedBlock *> SPScope::getSucceedingBlocks() const {
  std::set<const PredicatedBlock *> succblocks;
  auto outEdges = Priv->getOutEdges();
  for(auto edge: outEdges)
  {
    succblocks.insert(edge.second);
  }
  return succblocks;
}

void SPScope::walk(SPScopeWalker &walker) {
  walker.enterSubscope(this);
  auto blocks = getBlocksTopoOrd();
  for(auto block: blocks){
    auto MBB = block->getMBB();
    if (isSubheader(block)) {
      auto opt = findScopeOf(block);
      assert(opt.is_initialized());
      get(opt)->walk(walker);
    } else {
      walker.nextMBB(MBB);
    }
  }
  walker.exitSubscope(this);
}

static void printUDInfo(raw_ostream& os, const PredicatedBlock *block) {
  os << "  u={";
  for(auto pred: block->getBlockPredicates()) os << pred << ", ";
  os << "}";
  auto defs = block->getDefinitions();
  if (!defs.empty()) {
    os << " d=";
    for (auto def: defs) {
      os << def.predicate << ",";
    }
  }
  os << "\n";
}

void SPScope::dump(raw_ostream& os) const {
  os.indent(2*getDepth()) <<  "[BB#" << Priv->Blocks.front()->getMBB()->getNumber()
      << ":" << Priv->Blocks.front()->getMBB() <<"]";
  auto succs = getSucceedingBlocks();
  if (!Priv->Parent) {
    os << " (top)";
    assert(succs.empty());
  }
  if (!succs.empty()) {
    os << " -> { ";
    for (auto target: succs) {
      os << "BB#" << target->getMBB()->getNumber() << ":" << target << ", ";
    }
    os << "}";
  }

  os << " |P|=" <<  Priv->PredCount;
  printUDInfo(os, Priv->Blocks.front());
  os << " loopBound=";
  if(Priv->LoopBound.is_initialized()){
    os << get(Priv->LoopBound);
  }else{
    os << "none";
  }
  os << "\n";

  os.indent(2*getDepth()) << "Blocks: {\n";
  for(auto b: Priv->Blocks){
    b->dump(os, 2*getDepth() + 2);
  }
  os.indent(2*getDepth()) << "}\n";
  os.indent(2*getDepth()) << "children:\n";
  for(auto sub: Priv->Subscopes){
    sub->dump(os);
  }
}

bool SPScope::isTopLevel() const { return (NULL == Priv->Parent); }

const SPScope *SPScope::getParent() const { return Priv->Parent; }

bool SPScope::isRootTopLevel() const { return Priv->RootFunc && isTopLevel(); }

bool SPScope::hasLoopBound() const { return Priv->LoopBound.is_initialized(); }

boost::optional<unsigned> SPScope::getLoopBound() const { return Priv->LoopBound; }

PredicatedBlock *SPScope::getHeader() const { return Priv->Blocks.front(); }

std::vector<PredicatedBlock*> SPScope::getScopeBlocks() const
{
  return std::vector<PredicatedBlock*>(Priv->Blocks);
}

std::vector<PredicatedBlock*> SPScope::getBlocksTopoOrd() const
{
  auto fcfg = Priv->buildfcfg();
  // dfs the fcfg in postorder
  std::vector<PredicatedBlock *> PO;
  for (po_iterator<FCFG*> I = po_begin(&fcfg), E = po_end(&fcfg);
      I != E; ++I) {
    auto block = const_cast<PredicatedBlock*>((*I)->Block);
    if (block) {
      PO.push_back(block);
    }
  }
  std::reverse(PO.begin(), PO.end());
  return PO;
}

unsigned SPScope::getNumberOfFcfgBlocks() const
{
  return Priv->Blocks.size()
      // Each subscope has 1 header, so just count subscopes
      + Priv->Subscopes.size();
}

std::vector<PredicatedBlock*> SPScope::getFcfgBlocks() const
{
  auto result = getScopeBlocks();
  auto subheaders = Priv->getSubheaders();
  result.insert(result.end(), subheaders.begin(), subheaders.end());
  return result;
}

SPScope::child_iterator SPScope::child_begin() const { return Priv->Subscopes.begin(); }

SPScope::child_iterator SPScope::child_end() const { return Priv->Subscopes.end(); }

boost::optional<SPScope*> SPScope::findScopeOf(const PredicatedBlock *block) const
{
  // Look in this scope's blocks
  if(std::find(Priv->Blocks.begin(), Priv->Blocks.end(), block) != Priv->Blocks.end()){
    return boost::make_optional((SPScope*)this);
  }

  // Otherwise, look through the subscopes
  for(auto subscope: Priv->Subscopes){
    auto inSubscope = subscope->findScopeOf(block);
    if(inSubscope.is_initialized()){
      return inSubscope;
    }
  }

  // Not found
  return boost::none;
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

SPScope * SPScope::createSPScopeTree(MachineFunction &MF, MachineLoopInfo &LI, const PatmosInstrInfo* instrInfo) {

  SPScope *Root = new SPScope(PatmosSinglePathInfo::isRoot(MF), MF, LI);

  // iterate over top-level loops
  for (MachineLoopInfo::iterator I=LI.begin(), E=LI.end(); I!=E; ++I) {
    MachineLoop *Loop = *I;
    createSPScopeSubtree(Loop, Root, MF, LI);
  }

  Root->Priv->assignSuccessors();

  // analyze each scope
  // NB: this could be solved more elegantly by analyzing a scope when it is
  // built. But how he tree is created right now, it will not become more
  // elegant anyway.
  for(df_iterator<SPScope*> I = df_begin(Root), E = df_end(Root); I != E; ++I)
  {
    (*I)->Priv->computePredInfos(instrInfo);
  }

  return Root;
}

std::set<unsigned> SPScope::getAllPredicates() const
{
  std::set<unsigned> result;

  for(auto block: getFcfgBlocks()){
    auto preds = block->getBlockPredicates();
    result.insert(preds.begin(), preds.end());
  }

  return result;
}

PredicatedBlock* SPScope::bundle(PredicatedBlock* b1, PredicatedBlock* b2){

  b1->bundleWith(b2);

  auto mbb1 = b1->getMBB(), mbb2 = b2->getMBB();
  auto func = mbb2->getParent();

  for(auto iter = func->begin(), end = func->end(); iter != end; iter++){
    if(iter->isSuccessor(mbb2)){
      iter->ReplaceUsesOfBlockWith(mbb2, mbb1);
    }
  }
  while(mbb2->succ_begin() != mbb2->succ_end()){
    mbb2->removeSuccessor(mbb2->succ_begin());
  }

  func->erase(b2->getMBB());
//  func->remove(b2->getMBB());
//  func->DeleteMachineBasicBlock(b2->getMBB());

  Priv->replaceUseOfBlockWith(b2, b1);

//  for(auto iter = Priv->Blocks.begin(), end = Priv->Blocks.end(); iter != end; iter++){
//    if((*iter)->getMBB() == b2->getMBB()){
//      Priv->Blocks.erase(iter);
//      break;
//    }
//  }
//
//  for(auto iter = Priv->Blocks.begin(), end = Priv->Blocks.end(); iter != end; iter++){
//    if((*iter)->getMBB() == mbb1){
//      auto newb1 = *iter;
//      Priv->replaceUseOfBlockWith(b1, newb1);
//      return newb1;
//    }
//  }

  Priv->Blocks.erase(std::find(Priv->Blocks.begin(), Priv->Blocks.end(), b2));
  return b1;

  assert("Unreachable");
  return (PredicatedBlock*)NULL;
}

