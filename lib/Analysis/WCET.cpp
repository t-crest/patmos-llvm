#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/DOTGraphTraitsPass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Regex.h"
#include "llvm/Module.h"

#include <list>
#include <set>
#include <map>
#include <sstream>

using namespace llvm;

static cl::opt<bool>
DumpCDG("dot-domleaves", cl::init(false), cl::Hidden,
  cl::desc("Dump combined dominator graph."));

namespace {

  class graph_t;

  struct DomLeaves : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid

    DomLeaves() : FunctionPass(ID) {
      initializeDomLeavesPass(*PassRegistry::getPassRegistry());
    }

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<DominatorTree>();
      AU.addRequired<PostDominatorTree>();
      AU.setPreservesAll();
    }

    virtual bool runOnFunction(Function &F);

    void dumpGraph(graph_t *G, const std::string &name) const;
  };
}

char DomLeaves::ID = 0;
INITIALIZE_PASS(DomLeaves, "domleaves",
                "prototype pass", false, false)

// Public interface to the pass
FunctionPass *llvm::createDomLeavesPass() {
  return new DomLeaves();
}


//
// Combined dominators graph
//

namespace {
struct node_t {
  graph_t *G;
  int Idx;
  node_t(graph_t *g, int n) : G(g), Idx(n) {}
};

typedef std::vector<node_t*> adj_t;

struct graph_t {
  std::vector<BasicBlock*> Blocks;
  std::vector<node_t*> Nodes;
  std::vector<adj_t> Succs;

  std::map<BasicBlock*, int> map;

  ~graph_t() {
    // we own the nodes
    for (std::vector<node_t*>::iterator I = Nodes.begin(), E = Nodes.end();
         I != E; ++I)
      delete *I;
  }
};
}

namespace llvm {
template <> struct GraphTraits<node_t*> {
  typedef node_t NodeType;
  typedef std::vector<node_t*>::iterator ChildIteratorType;

  static NodeType *getEntryNode(node_t *N) { return N; }

  static inline ChildIteratorType child_begin(node_t *N) {
    return N->G->Succs[N->Idx].begin();
  }
  static inline ChildIteratorType child_end(node_t *N) {
    return N->G->Succs[N->Idx].end();
  }
};

//
// graph traits for SCC algorithm

template <> struct GraphTraits<graph_t*> : public GraphTraits<node_t*> {
  static NodeType *getEntryNode(graph_t *G) { return G->Nodes[0]; }

  typedef std::vector<node_t*>::iterator nodes_iterator;
  static nodes_iterator nodes_begin(graph_t *G) { return G->Nodes.begin(); }
  static nodes_iterator nodes_end  (graph_t *G) { return G->Nodes.end(); }
};
}

template<class T_tree, class T_node>
void copyEdges(T_tree *t, graph_t &g) {
  typedef GraphTraits<T_tree*> GDT;
  typedef GraphTraits<T_node*> GDN;
  for (typename GDT::nodes_iterator I = GDT::nodes_begin(t),
       E = GDT::nodes_end(t); I != E; ++I) {
    for (typename GDN::ChildIteratorType II = GDN::child_begin(*I),
         EE = GDN::child_end(*I); II != EE; ++II) {
      int srcidx = g.map[I->getBlock()];
      node_t *dst = g.Nodes[g.map[(*II)->getBlock()]];
      g.Succs[srcidx].push_back(dst);
    }
  }
}

BasicBlock *escapesLeaf(std::vector<node_t*> &SCC) {
  for (std::vector<node_t*>::iterator I = SCC.begin(),
       E = SCC.end(); I != E; ++I) {
    graph_t *g = (*I)->G;
    for (adj_t::iterator SI = g->Succs[(*I)->Idx].begin(),
         SE = g->Succs[(*I)->Idx].end(); SI != SE; ++SI) {
      if (std::find(SCC.begin(), SCC.end(), *SI) == SCC.end())
        return g->Blocks[(*I)->Idx];
    }
  }

  return NULL;
}

//
// Pass implementation
//

bool DomLeaves::runOnFunction(Function &F) {

  DominatorTree &DT = getAnalysis<DominatorTree>();
  PostDominatorTree &PDT = getAnalysis<PostDominatorTree>();

  graph_t G;

  for (Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
    node_t *N = new node_t(&G, G.Nodes.size());
    G.Nodes.push_back(N);
    G.Blocks.push_back(I);
    G.Succs.push_back(adj_t());
    G.map[I] = N->Idx;
  }


  copyEdges<DominatorTree, DomTreeNode>(&DT, G);
  copyEdges<PostDominatorTree, DomTreeNode>(&PDT, G);

  unsigned sccNum = 0;
  unsigned sccNonTriv = 0;
  unsigned leaves = 0;
  DEBUG(dbgs() << "SCCs for Function " << F.getName() << " in PostOrder:");
  for (scc_iterator<graph_t*> SCCI = scc_begin(&G),
         E = scc_end(&G); SCCI != E; ++SCCI) {
    std::vector<node_t*> &nextSCC = *SCCI;
    DEBUG(dbgs() << "\nSCC #" << sccNum << " : ");
    for (std::vector<node_t*>::iterator I = nextSCC.begin(),
           E = nextSCC.end(); I != E; ++I) {
      //SCCMap[*I] = sccNum;
      BasicBlock *BB = G.Blocks[(*I)->Idx];
      DEBUG(dbgs() << BB->getName() << ", ");
    }
    BasicBlock *esc = escapesLeaf(nextSCC);
    if (esc)
      DEBUG(dbgs() << "  (" << esc->getName() << " escapes SCC)");
    else {
      DEBUG(dbgs() << "  (leaf)");
      leaves++;
    }
    if (nextSCC.size() > 1)
      sccNonTriv++;
    sccNum++;
  }
  DEBUG(dbgs() << "\n");


  DEBUG(DT.dump());
  DEBUG(PDT.dump());

  //ViewGraph(&DT, "dom");
  //ViewGraph(&PDT, "post-dom");
  //ViewGraph(&G, "combined dominators");


  std::string err, sep = "\t";
  SmallVector<StringRef, 4> matches;
  Regex rx("([a-z0-9]*).ll");
  assert(rx.isValid(err));
  const std::string &modid = F.getParent()->getModuleIdentifier();
  rx.match(modid, &matches);
  assert(matches.size() > 1 && "unexpected module name");

  std::stringstream ss;
  ss << matches[1].str() << "::" <<  F.getName().str();
  std::string name = ss.str();

  if (DumpCDG) {
    std::string filename = name;
    std::replace(filename.begin(), filename.end(), ':', '_');
    dumpGraph(&G, filename);
  }

  DEBUG(errs() << "name\t#BBs\t#SCCs\t#leaves\n");
  errs() << name
    << sep << F.size() << sep << sccNonTriv
    << sep << leaves << "\n";
  return false;
}

void DomLeaves::dumpGraph(graph_t *G, const std::string &name) const {
  std::string Filename = "cdg." + name + ".dot";
  dbgs() << "Writing '" << Filename << "'...";

  std::string ErrorInfo;
  raw_fd_ostream File(Filename.c_str(), ErrorInfo);

  if (ErrorInfo.empty())
    WriteGraph(File, G);
  else
    dbgs() << "  error opening file for writing!";
  dbgs() << "\n";
}

//
// DOT graph drawing
//
namespace llvm {
template<>
struct DOTGraphTraits<node_t*> : public DefaultDOTGraphTraits {

  DOTGraphTraits (bool isSimple=false)
    : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(node_t *node, node_t *graph) {
    BasicBlock *BB = node->G->Blocks[node->Idx];

    assert(BB);

    return DOTGraphTraits<const Function*>
      ::getSimpleNodeLabel(BB, BB->getParent());
  }
};

template<>
struct DOTGraphTraits<graph_t*> : public DOTGraphTraits<node_t*> {

  DOTGraphTraits (bool isSimple=false)
    : DOTGraphTraits<node_t*>(isSimple) {}

  static std::string getGraphName(graph_t *G) {
    return "Foo graph";
  }

  std::string getNodeLabel(node_t *Node, graph_t *G) {
    return DOTGraphTraits<node_t*>::getNodeLabel(Node, NULL);
  }
};
}
