//===- PatmosiCFG.cpp - Interprocedural Control-flow graph. ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Compute the amount of code executed on loops, i.e., this can be used for
// persistence analysis and/or the function splitter.
//
//===----------------------------------------------------------------------===//

#include "PatmosiCFG.h"
#include "PatmosDFA.h"
#include "PatmosLPGenerator.h"

#include "llvm/CodeGen/MachineInstr.h"

namespace llvm {
  class iCFGDomain;
  class iCFGAnalysisContext;

  typedef class PatmosDFA<iCFGDomain, iCFGAnalysisContext> iCFGDFA;

  class iCFGCostWrapper {
  public:
    const iCFG &G;
    PatmosLP &LP;

    iCFGCostWrapper(const iCFG &g, PatmosLP &lp) : G(g), LP(lp) {
    }
  };

  void iCFGNode::dump() const {
    dbgs() << "iCFGN(" << FirstMI << ", " << LastMI << ")";
  }

  void iCFGEdge::dump() const {
    Src->dump();
    dbgs() << "->";
    Dest->dump();
  }

  void iCFG::view() const {
    ViewGraph(*this, "iCFG");
  }

  void iCFG::view(PatmosLP &LP) const {
    iCFGCostWrapper W(*this, LP);
    ViewGraph(W, "iwCFG");
  }

  void iCFG::write() const {
    std::string tmp;
    raw_fd_ostream of("icfg.dot", tmp);
    WriteGraph(of, *this);
  }

  // An interprocedural CFG.
  class iCFGAnalysisContext {
  private:
    typedef std::map<PatmosDFALocation, iCFGNode*> Map;
    iCFG &G;
    Map NodeMap;

  public:
    iCFGAnalysisContext(iCFG &g) : G(g) {
    }

    void makeEdge(const iCFGNode &src, const iCFGNode &dest) {
      const MachineBasicBlock *MBBs(src.FirstMI ?
                                    src.FirstMI->getParent() : NULL);
      const MachineFunction *MFs(MBBs ? MBBs->getParent() : NULL);
      const MachineFunction *MFd(dest.FirstMI ?
                                 dest.FirstMI->getParent()->getParent() : NULL);

      // check if we switch context
      unsigned int flags = 0;
      if (MFs != MFd) {
        // check if this is a return?
        if (MBBs && &MBBs->back() == src.LastMI && MBBs->succ_empty())
          flags = iCFGEdge::RETURN;
        else
          flags = iCFGEdge::CALL;
      }

      G.Edges.insert(iCFGEdge(src, dest, flags));
    }

    void makeImmEdge(const iCFGNode &src, const iCFGNode &dest) {
      G.Edges.insert(iCFGEdge(src, dest, iCFGEdge::BBLINK));
    }

    iCFGNode &makeNode(const PatmosDFALocation &L) {
      bool isRoot = NodeMap.empty();

      iCFGNode *&N(NodeMap[L]);
      if (!N) {
        N = &*G.Nodes.insert(G.Nodes.end(), iCFGNode(G, L.second));
      }

      if (isRoot)
        G.RootNode = N;

      return *N;
    }
  };

  // Domain to compute an interprocedural CFG
  class iCFGDomain {
  private:
    iCFGNode &Node;

    bool Visited;
  public:
    iCFGDomain(const PatmosDFALocation &L, iCFGAnalysisContext &AC) :
        Node(AC.makeNode(L)), Visited(false) {
    }

    void merge(const iCFGDomain &o, const PatmosDFALocation &oL,
               const PatmosDFALocation &L, iCFGAnalysisContext &AC) {
      AC.makeEdge(o.Node, Node);
    }

    void mergeImm(const iCFGDomain &o, const PatmosDFALocation &oL,
               const PatmosDFALocation &L, iCFGAnalysisContext &AC) {
      AC.makeImmEdge(o.Node, Node);
    }

    void transform(const PatmosDFALocation &L, iCFGAnalysisContext &AC) {
      assert(Node.FirstMI);
      Node.LastMI = L.second;
    }

    bool operator!=(const iCFGDomain &o) const {
      return Visited != o.Visited;
    }

    bool operator<(const iCFGDomain &o) const {
      return Visited < o.Visited;
    }

    void dump() const {
      Node.dump();
    }
  };

  iCFG *computeiCFG(PatmosCallGraphBuilder &pcgb) {
    iCFG *G = new iCFG();
    iCFGAnalysisContext analysisContext(*G);
    iCFGDFA iCFGBuilder(analysisContext, 0, &pcgb);
    iCFGBuilder.solve(pcgb.getEntryNode()->getMF());
    return G;
  }

  template<class ChildBaseIterator, class NodeType>
  class ChildIteratorBaseType
  {
    ChildBaseIterator I;
    ChildBaseIterator E;

  public:
    typedef typename ChildBaseIterator::iterator_category iterator_category;
    typedef typename ChildBaseIterator::difference_type difference_type;
    typedef typename ChildBaseIterator::pointer pointer;
    typedef typename ChildBaseIterator::reference reference;
    typedef NodeType value_type;

    static ChildBaseIterator find(ChildBaseIterator i, ChildBaseIterator e,
                                  NodeType &src) {
      while(i != e && i->getSrc() != &src)
        i++;

      return i;
    }

    ChildIteratorBaseType(const ChildIteratorBaseType &o) : I(o.I), E(o.E) {
    }

    ChildIteratorBaseType(ChildBaseIterator i, ChildBaseIterator e,
                          NodeType &src) : I(find(i, e, src)), E(e) {
    }

    bool operator!=(ChildIteratorBaseType a) {
      return I != a.I;
    }

    ChildIteratorBaseType operator++() {
      ChildIteratorBaseType tmp(*this);
      I = find(++I, E, *tmp.I->getSrc());
      return tmp;
    }

    NodeType *operator*() {
      return getDest();
    }

    const NodeType *getSrc() {
      return I->getSrc();
    }

    const NodeType *getDest() {
      return I->getDest();
    }

    const iCFGEdge *getEdge() {
      return &*I;
    }
  };

  template <> struct GraphTraits<iCFG> {
    typedef iCFGNode const NodeType;
    typedef iCFGNode::List::const_iterator nodes_iterator;

    typedef ChildIteratorBaseType<iCFGEdge::Set::const_iterator,
                                  NodeType> ChildIteratorType;

    static inline ChildIteratorType child_begin(NodeType *N) {
      return ChildIteratorType(N->G.getEdges().begin(),
                               N->G.getEdges().end(), *N);
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return ChildIteratorType(N->G.getEdges().end(),
                               N->G.getEdges().end(), *N);
    }

    static NodeType *getEntryNode(const iCFG &G) {
      return &G.getRootNode();
    }

    static nodes_iterator nodes_begin(const iCFG &G) {
      return G.getNodes().begin();
    }
    static nodes_iterator nodes_end (const iCFG &G) {
      return G.getNodes().end();
    }
    static unsigned size (const iCFG &G) {
      return G.getNodes().size();
    }
  };

  template<>
  struct DOTGraphTraits<iCFG> : public DefaultDOTGraphTraits {
    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const iCFG &G) {
      return "iCFG";
    }

    static bool isNodeHidden(const iCFGNode *N, const iCFG &G) {
      return false;
    }

    static std::string getEdgeAttributes(const void *,
                                         GraphTraits<iCFG>::ChildIteratorType e,
                                         const iCFG &G) {
      // check if we switch context
      if (e.getEdge()->isCall())
        return "color=\"red\"";
      else if (e.getEdge()->isReturn())
        return "color=\"blue\"";
      else if (e.getEdge()->isBBLink())
        return "color=\"gray\"";
      else
        return "";
    }

    std::string getNodeLabel(const iCFGNode *N, const iCFG &G) {
      std::string tmp;
      raw_string_ostream s(tmp);

      const MachineInstr *MI = N->FirstMI;
      const MachineBasicBlock *MBB = MI->getParent();
      const MachineFunction *MF = MBB->getParent();

      s << "MF" << MF->getFunctionNumber() << "::"
        << "MBB" << MBB->getNumber() << "::"
        << "MI" << std::distance(MBB->instr_begin(),
                                 MachineBasicBlock::const_instr_iterator(MI));

      return s.str();
    }

    static std::string getNodeAttributes(const iCFGNode *N, const iCFG &G) {
      if (N == &G.getRootNode() || N->FirstMI == NULL)
        return "style=filled, fillcolor=\"red\"";
      else
        return "";
    }
  };

  template <> struct GraphTraits<iCFGCostWrapper> {
    typedef iCFGNode const NodeType;
    typedef iCFGNode::List::const_iterator nodes_iterator;

    typedef ChildIteratorBaseType<iCFGEdge::Set::const_iterator,
                                  NodeType> ChildIteratorType;

    static inline ChildIteratorType child_begin(NodeType *N) {
      return ChildIteratorType(N->G.getEdges().begin(),
                               N->G.getEdges().end(), *N);
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return ChildIteratorType(N->G.getEdges().end(),
                               N->G.getEdges().end(), *N);
    }

    static NodeType *getEntryNode(const iCFGCostWrapper &G) {
      return &G.G.getRootNode();
    }

    static nodes_iterator nodes_begin(const iCFGCostWrapper &G) {
      return G.G.getNodes().begin();
    }
    static nodes_iterator nodes_end (const iCFGCostWrapper &G) {
      return G.G.getNodes().end();
    }
    static unsigned size (const iCFGCostWrapper &G) {
      return G.G.getNodes().size();
    }
  };

  template<>
  struct DOTGraphTraits<iCFGCostWrapper> : public DefaultDOTGraphTraits {
    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const iCFGCostWrapper &G) {
      return "iwCFG";
    }

    static bool isNodeHidden(const iCFGNode *N, const iCFGCostWrapper &G) {
      return false;
    }

    static std::string getEdgeAttributes(const void *,
                                         GraphTraits<iCFG>::ChildIteratorType e,
                                         const iCFGCostWrapper &G) {
      std::string tmp;
      raw_string_ostream s(tmp);

      // check if we switch context
      if (e.getEdge()->isCall())
        s << "color=\"red\"";
      else if (e.getEdge()->isReturn())
        s << "color=\"blue\"";
      else if (e.getEdge()->isBBLink())
        s << "color=\"gray\"";

      // get weights associated with edge
      PatmosLP::LPSolution S(G.LP.getSolution(e.getEdge()));
      if (!S.empty()) {
        if (!tmp.empty())
          s << ", ";
        s << "label = \"";
        for(PatmosLP::LPSolution::const_iterator i(S.begin()), ie(S.end());
            i != ie; i++) {
          s << i->getVar().getPrefix() << "="
            << (unsigned int)i->getWeight() << "\\n";
        }
        s << "\"";
      }

      return s.str();
    }

    std::string getNodeLabel(const iCFGNode *N, const iCFGCostWrapper &G) {
      std::string tmp;
      raw_string_ostream s(tmp);

      const MachineInstr *MI = N->FirstMI;
      const MachineBasicBlock *MBB = MI->getParent();
      const MachineFunction *MF = MBB->getParent();

      s << "MF" << MF->getFunctionNumber() << "::"
        << "MBB" << MBB->getNumber() << "::"
        << "MI" << std::distance(MBB->instr_begin(),
                                 MachineBasicBlock::const_instr_iterator(MI));


      // get weights associated with edge
      PatmosLP::LPSolution S(G.LP.getSolution(N));
      if (!S.empty()) {
        s << "\n";
        for(PatmosLP::LPSolution::const_iterator i(S.begin()), ie(S.end());
            i != ie; i++) {
          s << i->getVar().getPrefix() << "="
            << (unsigned int)i->getWeight() << "\n";
        }
      }

      return s.str();
    }

    static std::string getNodeAttributes(const iCFGNode *N,
                                         const iCFGCostWrapper &G) {
      if (N == &G.G.getRootNode() || N->FirstMI == NULL)
        return "style=filled, fillcolor=\"red\"";
      else
        return "";
    }

    static void addCustomGraphFeatures(const iCFGCostWrapper &G,
                                       GraphWriter<iCFGCostWrapper> &W) {
      raw_ostream &OS = W.getOStream();

      unsigned int lastCluster = std::numeric_limits<unsigned int>::max();

      for(iCFGNode::List::const_iterator i(G.G.getNodes().begin()),
          ie(G.G.getNodes().end()); i != ie; i++) {
        const iCFGNode *N = &*i;
        const MachineFunction *MF = N->FirstMI ?
                                    N->FirstMI->getParent()->getParent() : NULL;

        if (MF && MF->getFunctionNumber() != lastCluster) {
          // close last cluster
          if (lastCluster != std::numeric_limits<unsigned int>::max()) {
            OS.indent(7) << "}\n";
          }

          lastCluster = MF->getFunctionNumber();

          OS.indent(7) << "subgraph cluster_" << lastCluster << "{\n";
          OS.indent(14) << "clusterrank=\"local\";\n";
        }

        if (MF) {
          OS.indent(14) << "Node" << static_cast<const void*>(N) << ";\n";
        }
      }

      // close last cluster
      if (lastCluster != std::numeric_limits<unsigned int>::max()) {
        OS.indent(7) << "}\n";
      }
    }
  };
}
