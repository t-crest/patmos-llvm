//===-- PatmosCallGraphBuilder.h - Codegen-based call graph construction. -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Build a module-level call graph at the machine-level.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Pass.h"
#include "llvm/Type.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/GraphWriter.h"

#include <vector>
#include <map>

using namespace llvm;

namespace llvm {
  // forward declarations
  class MCGSite;
  class MCGNode;
  class MCallGraph;
  class MCallSubGraph;

  /// A vector of call sites.
  typedef std::vector<MCGSite*> MCGSites;

  /// A vector of call graph nodes.
  typedef std::vector<MCGNode*> MCGNodes;

  /// A node in the machine-level call graph.
  class MCGNode
  {
    friend class MCallGraph;
    friend struct GraphTraits<MCallGraph>;
    friend struct GraphTraits<MCallSubGraph>;
  private:
    /// The MachineFunction represented by this call graph node, or NULL.
    MachineFunction *MF;

    /// Type of the MachineFunction represented by this call graph node, if MF
    /// is NULL.
    Type *T;

    /// The node's call sites.
    MCGSites Sites;

    /// The node's call sites calling this node.
    MCGSites CallingSites;

    /// Flag indicating whether the node's function is dead code.
    bool IsDead;
  public:
    /// Construct a new call graph node.
    explicit MCGNode(MachineFunction *mf) : MF(mf), T(NULL), IsDead(true) {}

    /// Construct a new call graph node.
    explicit MCGNode(Type *t) : MF(NULL), T(t), IsDead(true) {}

    /// getMF - Return the node's MachineFunction.
    MachineFunction *getMF() const
    {
      return MF;
    }

    /// getType - Return the node's Function type.
    Type *getType() const
    {
      return T;
    }

    /// getSites - Get the call sites of the call node.
    const MCGSites &getSites() const
    {
      return Sites;
    }

    /// getCallingSites - Get the call sites calling this node.
    const MCGSites &getCallingSites() const
    {
      return CallingSites;
    }

    /// markLive - Mark the node's function as live, i.e., not dead.
    void markLive()
    {
      IsDead = false;
    }

    /// isDead - Check whether the node's function is marked as dead code.
    bool isDead() const
    {
      return IsDead;
    }

    /// isLeaf - Check whether the node's function is a leaf function, i.e.,
    /// does not contain any call sites.
    bool isLeaf() const
    {
      return Sites.empty();
    }

    /// isUnknown - Check whether the node represents an UNKNOWN node.
    bool isUnknown() const
    {
      return MF == NULL;
    }

    /// findSite - Find the call site of the given MachineInstr.
    MCGSite *findSite(const MachineInstr *MI) const;

    /// getLabel - get a string representation of the call graph node.
    std::string getLabel() const;

    /// dump - print the call graph node to the debug stream.
    void dump() const;
  };

  /// A machine-level call site
  class MCGSite
  {
  private:
    /// The parent call graph node of this call site.
    MCGNode *Caller;

    /// The call instruction of the call site.
    MachineInstr *MI;

    /// The call graph node referenced by this call site.
    MCGNode *Callee;

  public:
    /// Construct a new call site.
    MCGSite(MCGNode *caller, MachineInstr *mi, MCGNode *callee) :
        Caller(caller), MI(mi), Callee(callee)
    {
    }

    /// getCaller - Return the parent call graph node of the call site.
    MCGNode *getCaller() const
    {
      return Caller;
    }

    /// getMI - Return the call site's call instruction.
    MachineInstr *getMI() const
    {
      return MI;
    }

    /// getCallee - Return the call site's call graph node.
    MCGNode *getCallee() const
    {
      return Callee;
    }

    /// dump - print the call site to the debug stream.
    void dump(bool short_format = true) const;
  };

  /// A machine-level call graph.
  class MCallGraph
  {
    friend struct GraphTraits<MCallGraph>;
    friend struct DOTGraphTraits<MCallGraph>;
    friend class PatmosCallGraphBuilder;
  private:
    /// The graph's nodes.
    MCGNodes Nodes;

    /// The graph's call sites.
    MCGSites Sites;

    typedef std::map<std::pair<Type *, Type *>, int> equivalent_types_t;
    equivalent_types_t EQ;

    /// areTypesIsomorphic - check whether two types are isomorphic.
    /// This is taken from LinkModules.cpp.
    int areTypesIsomorphic(Type *DstTy, Type *SrcTy);
  public:
    /// getNodes - Return the graph's nodes.
    const MCGNodes &getNodes() const
    {
      return Nodes;
    }

    /// getNodes - Return the graph's nodes.
    MCGNodes &getNodes()
    {
      return Nodes;
    }

    /// getSites - Return the graph's call sites.
    const MCGSites &getSites() const
    {
      return Sites;
    }

    /// getSites - Return the graph's call sites.
    MCGSites &getSites()
    {
      return Sites;
    }

    /// makeMCGNode - Return a call graph node for the MachineFunction. The node
    /// is either newly constructed, or, if one exists, a node from the nodes
    /// set associated with the MachineFunction is returned.
    MCGNode *makeMCGNode(MachineFunction *MF);

    /// getUnknownNode - Get a pseudo call graph node for unknown call targets
    /// of the given type.
    MCGNode *getUnknownNode(Type *t);

    /// makeMCGSite - Return a call site.
    MCGSite *makeMCGSite(MCGNode *Caller, MachineInstr *MI, MCGNode *Callee);

    /// dump - print all call sites of the call graph to the debug stream.
    void dump() const;

    /// view - show a DOT dump of the call graph.
    void view() const;

    /// Free the call graph and all its nodes and call sites.
    virtual ~MCallGraph();
  };

  /// A sub-graph of a call graph for DOT dumps only.
  class MCallSubGraph
  {
    friend struct GraphTraits<MCallSubGraph>;
    friend struct DOTGraphTraits<MCallSubGraph>;
  private:
    const MCallGraph &G;
    const MCGNodes Nodes;

  public:
    MCallSubGraph(const MCallGraph &g, MCGNodes &nodes) : G(g), Nodes(nodes) { }

    /// isNodeHidden - Callback from DOTGraphTraits, check if node is in 
    /// sub-graph.
    bool isNodeHidden(const MCGNode *N) const {
      return std::find(Nodes.begin(), Nodes.end(), N) == Nodes.end();
    }

    /// view - show a DOT dump of the call graph.
    void view();
  };

  /// Pass to construct the call graph at the machine-level of the current
  /// module.
  class PatmosCallGraphBuilder: public MachineModulePass {
  private:
    /// A call graph.
    MCallGraph MCG;

  public:
    /// Pass ID
    static char ID;

    PatmosCallGraphBuilder() : MachineModulePass(ID)
    {
    }

    /// getAnalysisUsage - Inform the pass manager that nothing is modified
    /// here.
    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.setPreservesAll();
      AU.addRequired<MachineModuleInfo>();

      ModulePass::getAnalysisUsage(AU);
    }

    MCallGraph *getCallGraph()
    {
      return &MCG;
    }

    /// getNodes - Return the graph's nodes.
    const MCGNodes &getNodes() const
    {
      return MCG.getNodes();
    }

    /// getNodes - Return the graph's nodes.
    MCGNodes &getNodes()
    {
      return MCG.getNodes();
    }

    /// getSites - Return the graph's call sites.
    const MCGSites &getSites() const
    {
      return MCG.getSites();
    }

    /// getSites - Return the graph's call sites.
    MCGSites &getSites()
    {
      return MCG.getSites();
    }

    /// visitCallSites - Visit all call-sites of the MachineFunction and append
    /// them to a simple machine-level call graph.
    void visitCallSites(const Module &M, MachineFunction *MF);

    /// getMCGNode - Return the call graph node of the function with the given
    /// name.
    MCGNode *getMCGNode(const Module &M, const char *name);

    /// markLive_ - Mark the node and all its callees as live.
    void markLive_(MCGNode *N);

    /// markLive - Mark the node and all its callees as live.
    void markLive(MCGNode *N);

    /// runOnModule - Construct a simple machine-level call graph from the given
    /// module.
    virtual bool runOnMachineModule(const Module &M);

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Call Graph Builder";
    }
  };
}

namespace llvm {
  template <> struct GraphTraits<MCallGraph> {
    typedef MCGNode NodeType;
    class ChildIteratorType
    {
      MCGSites::iterator I;

    public:
      typedef MCGSites::iterator::iterator_category iterator_category;
      typedef MCGSites::iterator::difference_type difference_type;
      typedef MCGSites::iterator::pointer pointer;
      typedef MCGSites::iterator::reference reference;
      typedef NodeType value_type;

      ChildIteratorType(MCGSites::iterator i) : I(i) {
      }

      bool operator!=(ChildIteratorType a) {
        return I != a.I;
      }

      bool operator==(ChildIteratorType a) {
        return I == a.I;
      }

      ChildIteratorType operator++() {
        I++;
        return *this;
      }

      ChildIteratorType operator++(int) {
        ChildIteratorType tmp(I);
        I++;
        return tmp;
      }

      difference_type operator-(ChildIteratorType &a) {
        return I - a.I;
      }

      NodeType *operator*() {
        return (*I)->getCallee();
      }
    };

    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Sites.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Sites.end();
    }

    static NodeType *getEntryNode(const MCallGraph &G) {
      for(MCGNodes::const_iterator i(G.Nodes.begin()), ie(G.Nodes.end());
          i != ie; i++) {
        if ((*i)->getMF() && (*i)->getMF()->getFunction()->getName() == "main")
          return *i;
      }

      return G.Nodes.front();
    }

    // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
    typedef MCGNodes::const_iterator nodes_iterator;
    static nodes_iterator nodes_begin(const MCallGraph &G) {
      return G.Nodes.begin();
    }
    static nodes_iterator nodes_end  (const MCallGraph &G) {
      return G.Nodes.end();
    }
    static unsigned       size       (const MCallGraph &G)  {
      return G.Nodes.size();
    }
  };

  template<>
  struct DOTGraphTraits<MCallGraph> : public DefaultDOTGraphTraits {
    typedef MCGSites::const_iterator EdgeIteratorType;

    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const MCallGraph &G) {
      return "xxx";
    }

    template<typename T>
    static bool isNodeHidden(const T, const MCallGraph &G) {
      return false;
    }

    std::string getNodeLabel(const MCGNode *N, const MCallGraph &G) {
      if (N->isUnknown()) {
        std::string tmp;
        raw_string_ostream s(tmp);
        s << "<UNKNOWN-" << *N->getType() << ">";
        return s.str();
      }
      else
        return N->getMF()->getFunction()->getName();
    }

    static std::string getNodeAttributes(const MCGNode *N,
                                         const MCallGraph &G) {
      return N->isDead() ? "color=\"red\"" : "";
    }
  };


  template <> struct GraphTraits<MCallSubGraph> {
    typedef MCGNode NodeType;
    class ChildIteratorType
    {
      MCGSites::iterator I;

    public:
      typedef MCGSites::iterator::iterator_category iterator_category;
      typedef MCGSites::iterator::difference_type difference_type;
      typedef MCGSites::iterator::pointer pointer;
      typedef MCGSites::iterator::reference reference;
      typedef NodeType value_type;

      ChildIteratorType(MCGSites::iterator i) : I(i) {
      }

      bool operator!=(ChildIteratorType a) {
        return I != a.I;
      }

      bool operator==(ChildIteratorType a) {
        return I == a.I;
      }

      ChildIteratorType operator++() {
        I++;
        return *this;
      }

      ChildIteratorType operator++(int) {
        ChildIteratorType tmp(I);
        I++;
        return tmp;
      }

      difference_type operator-(ChildIteratorType &a) {
        return I - a.I;
      }

      NodeType *operator*() {
        return (*I)->getCallee();
      }
    };

    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->Sites.begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->Sites.end();
    }

    static NodeType *getEntryNode(const MCallSubGraph &G) {
      for(MCGNodes::const_iterator i(G.Nodes.begin()), ie(G.Nodes.end());
          i != ie; i++) {
        if ((*i)->getMF() && (*i)->getMF()->getFunction()->getName() == "main")
          return *i;
      }

      return G.Nodes.front();
    }

    // nodes_iterator/begin/end - Allow iteration over all nodes in the graph
    typedef MCGNodes::const_iterator nodes_iterator;
    static nodes_iterator nodes_begin(const MCallSubGraph &G) {
      return G.Nodes.begin();
    }
    static nodes_iterator nodes_end  (const MCallSubGraph &G) {
      return G.Nodes.end();
    }
    static unsigned       size       (const MCallSubGraph &G)  {
      return G.Nodes.size();
    }
  };

  template<>
  struct DOTGraphTraits<MCallSubGraph> : public DefaultDOTGraphTraits {
    typedef MCGSites::const_iterator EdgeIteratorType;

    DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

    static std::string getGraphName(const MCallSubGraph &G) {
      return "xxx";
    }

    template<typename T>
    static bool isNodeHidden(const T N, const MCallSubGraph &G) {
      return G.isNodeHidden(N);
    }

    std::string getNodeLabel(const MCGNode *N, const MCallSubGraph &G) {
      if (N->isUnknown()) {
        std::string tmp;
        raw_string_ostream s(tmp);
        s << "<UNKNOWN-" << *N->getType() << ">";
        return s.str();
      }
      else
        return N->getMF()->getFunction()->getName();
    }

    static std::string getNodeAttributes(const MCGNode *N,
                                         const MCallSubGraph &G) {
      return N->isDead() ? "color=\"red\"" : "";
    }
  };
}
