#include "llvm/ADT/SCCIterator.h"
#include "llvm/Analysis/DOTGraphTraitsPass.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Regex.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"

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
  class annotations_t;

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

  annotations_t *Annotations;

  graph_t() : Annotations(0) {}
  ~graph_t();
};

struct annotations_t {
  std::map<BasicBlock*, int> SCCMap;
  std::vector<bool> leafSCCs;
  std::vector<int> nonDomSCCs;
  std::vector<BasicBlock*> nonDomBBs;
  std::vector<bool> calcSCCs;
  std::set<BasicBlock*> derivBBs;
};

graph_t::~graph_t() {
  // we own the nodes
  for (std::vector<node_t*>::iterator I = Nodes.begin(), E = Nodes.end();
       I != E; ++I)
    delete *I;
  // and the annotations object
  delete Annotations;
}
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

// analyze an SCC to find out if it is a leaf, or an edge escapes to another
// vertex. return that vertex or NULL if it is a leaf
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

template<class T>
struct SizeGreaterOne {
  bool operator()(const T &t) { return t.size() > 1; }
  bool operator()(const T *t) { return t->size() > 1; }
};

template<class T_IterCFG, class T_IterSCC, class T_Dom>
bool canDerive(BasicBlock *BB, T_IterCFG succ_begin, T_IterCFG succ_end,
               T_IterSCC scc_begin, T_IterSCC scc_end, const T_Dom &DT) {
  for (T_IterCFG SI = succ_begin, SE = succ_end; SI != SE; ++SI) {
    assert(*SI != BB && "interesting self-loop");
    if (std::binary_search(scc_begin, scc_end, *SI))
      return false;
    if (!DT.dominates(BB, *SI))
      return false;
  }
  return true;
}

//
// Pass implementation
//

bool DomLeaves::runOnFunction(Function &F) {

  using namespace std;

  DominatorTree &DT = getAnalysis<DominatorTree>();
  PostDominatorTree &PDT = getAnalysis<PostDominatorTree>();

  // our combined dominator graph (CDG)
  graph_t G;

  // SCC data structures
  typedef std::vector<node_t*> SCC_t;
  std::vector<SCC_t> SCCs;
  std::vector<const SCC_t*> leaves;
  std::vector<bool> leafSCCs;
  std::map<BasicBlock*, int> SCCMap;

  // blocks with an outgoing critical edge do not dominate WCET properties
  std::vector<BasicBlock*> nonDomBBs;
  std::vector<int> nonDomSCCs; // SCCs containing the above blocks

  // blocks on the border of SCCs that can be derived from neighboring blocks
  // outside their SCC
  std::set<BasicBlock*> derivBBs;

  // all SCCs that we need to calculate (leaves and non-derivable ones)
  std::vector<bool> calcSCCs;

  for (Function::iterator I = F.begin(), E = F.end(); I != E; ++I) {
    // populate the new graph with nodes
    node_t *N = new node_t(&G, G.Nodes.size());
    G.Nodes.push_back(N);
    G.Blocks.push_back(I);
    G.Succs.push_back(adj_t());
    G.map[I] = N->Idx;

    // critical edge analysis
    TerminatorInst *TI = I->getTerminator();
    assert(!isa<IndirectBrInst>(TI) && "fixme: what to do here?");
    for (unsigned i = 0, e = TI->getNumSuccessors(); i != e; ++i)
      if (isCriticalEdge(TI, i, false)) {
        DEBUG(dbgs() << "critical edge found: " << I->getName() << " -- "
                     << TI->getSuccessor(i)->getName() << "\n");
        nonDomBBs.push_back(I);
      }
  }

  // combine dom and post-dom edges in new graph
  copyEdges<DominatorTree, DomTreeNode>(&DT, G);
  copyEdges<PostDominatorTree, DomTreeNode>(&PDT, G);


  // prevent any further reallocation (b/c we take pointers)
  SCCs.reserve(G.Nodes.size());

  DEBUG(dbgs() << "SCCs for Function " << F.getName() << " in PostOrder:");
  for (scc_iterator<graph_t*> SCCI = scc_begin(&G),
         E = scc_end(&G); SCCI != E; ++SCCI) {
    std::vector<node_t*> &nextSCC = *SCCI;
    unsigned sccNum = SCCs.size();
    SCCs.push_back(nextSCC);
    DEBUG(dbgs() << "\nSCC #" << sccNum << " : ");
    for (std::vector<node_t*>::iterator I = nextSCC.begin(),
           E = nextSCC.end(); I != E; ++I) {
      BasicBlock *BB = G.Blocks[(*I)->Idx];
      SCCMap[BB] = sccNum;
      DEBUG(dbgs() << BB->getName() << ", ");
    }
    BasicBlock *esc = escapesLeaf(nextSCC);
    if (!esc) {
      DEBUG(dbgs() << "  (leaf)");
      leaves.push_back(&SCCs.back());
    } else
      DEBUG(dbgs() << "  (" << esc->getName() << " escapes SCC)");

    leafSCCs.push_back(esc ? false : true);

  }
  DEBUG(dbgs() << "\n");

  DEBUG(DT.dump());
  DEBUG(PDT.dump());

  // start with the leaves that we need to calculate for sure
  calcSCCs = leafSCCs;

  for (std::vector<BasicBlock*>::iterator I = nonDomBBs.begin(),
       E = nonDomBBs.end(); I != E; ++I) {
    assert(SCCMap.count(*I));
    int sccidx = SCCMap[*I];
    DEBUG(dbgs() << "non-dominating-BB " << (*I)->getName()
          << " in SCC #" << sccidx << "\n");
    nonDomSCCs.push_back(sccidx);
    calcSCCs[sccidx] = true;
  }
  std::sort(nonDomSCCs.begin(), nonDomSCCs.end());
  nonDomSCCs.resize(std::unique(nonDomSCCs.begin(), nonDomSCCs.end())
                    - nonDomSCCs.begin());

  // figure out name for statistics output
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


  // analyse nonDomBBs and impact on their containing SCCs
  for (int i = 0, e = nonDomSCCs.size(); i < e; ++i) {
    SCC_t &SCC = SCCs[nonDomSCCs[i]];
    std::vector<BasicBlock*> cand;
    std::vector<BasicBlock*> SCCBBs; // XXX ugly overhead for searching
    errs() << ";nonDomSCC-Info(" << name << ") #" << nonDomSCCs[i];
    if (leafSCCs[nonDomSCCs[i]]) errs() << " [isLeaf]";
    errs() << ": ";
    for (SCC_t::iterator I = SCC.begin(), E = SCC.end(); I != E; ++I) {
      BasicBlock *BB = G.Blocks[(*I)->Idx];
      SCCBBs.push_back(BB);
      errs() << BB->getName();
      if (std::find(nonDomBBs.begin(), nonDomBBs.end(), BB) != nonDomBBs.end())
        errs() << "*";
      else
        cand.push_back(BB);
      errs() << ", ";
    }
    errs() << "\n";
    errs() << "; ^--has " << cand.size() << " candidate(s)\n";
    std::sort(SCCBBs.begin(), SCCBBs.end());
    for (std::vector<BasicBlock*>::iterator I = cand.begin(), E = cand.end();
         I != E; ++I) {
      BasicBlock *BB = *I;
      // check if any of the candidates can be derived
      if (canDerive(BB, succ_begin(BB), succ_end(BB), SCCBBs.begin(),
                     SCCBBs.end(), DT) ||
          canDerive(BB, pred_begin(BB), pred_end(BB), SCCBBs.begin(),
                    SCCBBs.end(), PDT)) {
        // at this point all BB's successors are dominated by BB and outside
        // its SCC.
        errs() << ";        ^--"<< BB->getName() << " saves the day\n";
        derivBBs.insert(BB);
        calcSCCs[nonDomSCCs[i]] = false;
      }
    }
  }

  //ViewGraph(&DT, "dom");
  //ViewGraph(&PDT, "post-dom");
  //ViewGraph(&G, "combined dominators");

  if (DumpCDG) {
    G.Annotations = new annotations_t();
    G.Annotations->SCCMap = SCCMap;
    G.Annotations->leafSCCs = leafSCCs;
    G.Annotations->nonDomSCCs = nonDomSCCs;
    G.Annotations->nonDomBBs = nonDomBBs;
    G.Annotations->calcSCCs = calcSCCs;
    G.Annotations->derivBBs = derivBBs;
    dumpGraph(&G, name);
  }

  assert((int) leaves.size() == count(leafSCCs.begin(), leafSCCs.end(), true));

  DEBUG(errs() << "name\t#BBs\t#SCCs\t#leaves\t#(non-triv)"
                  "\tnon-dom-BBs"
                  "\tnon-dom-SCCs"
                  "\tcalcSCCs\n");
  errs() << name
    << sep << F.size()
    << sep << count_if(SCCs.begin(), SCCs.end(), SizeGreaterOne<SCC_t>())
    << sep << count(leafSCCs.begin(), leafSCCs.end(), true)
    << sep << count_if(leaves.begin(), leaves.end(), SizeGreaterOne<SCC_t>())
    << sep << nonDomBBs.size()
    << sep << nonDomSCCs.size()
    << sep << count(calcSCCs.begin(), calcSCCs.end(), true)
    << "\n";

  return false;
}

void DomLeaves::dumpGraph(graph_t *G, const std::string &name) const {
  std::string safename = name;
  std::replace(safename.begin(), safename.end(), ':', '_');
  std::string Filename = "cdg." + safename + ".dot";
  dbgs() << "Writing '" << Filename << "'...";

  std::string ErrorInfo;
  raw_fd_ostream File(Filename.c_str(), ErrorInfo);

  if (ErrorInfo.empty())
    WriteGraph(File, G, false, "Combined dominator graph for " + name);
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

  std::string getNodeLabel(node_t *node, node_t*) {
    BasicBlock *BB = node->G->Blocks[node->Idx];

    assert(BB);

    std::string str;
    raw_string_ostream OS(str);

    WriteAsOperand(OS, BB, false);
    annotations_t *anno = node->G->Annotations;
    if (anno) {
      OS << " (#" << anno->SCCMap[BB] << ")";
    }
    return OS.str();
  }

  std::string getNodeAttributes(const node_t *N,
                                       graph_t *G) {
    annotations_t *anno = G->Annotations;
    std::stringstream attr;
    BasicBlock *BB = G->Blocks[N->Idx];
    if (anno) {
      if (anno->leafSCCs[getSCCIdx(N, anno)])
        attr << "color=forestgreen";
      else
        attr << "color=black";
      if (std::find(anno->nonDomBBs.begin(), anno->nonDomBBs.end(),
                    BB) != anno->nonDomBBs.end())
        attr << ",fontcolor=firebrick1";
      else if (anno->derivBBs.count(BB))
        attr << ",fontcolor=dodgerblue1";
      if (anno->calcSCCs[getSCCIdx(N, anno)])
        attr << ",style=filled,fillcolor=gray89";

    }
    return attr.str();
  }

  int getSCCIdx(const node_t *N, annotations_t *A) {
    assert(A);
    return A->SCCMap[N->G->Blocks[N->Idx]];
  }
};

template<>
struct DOTGraphTraits<graph_t*> : public DOTGraphTraits<node_t*> {

  DOTGraphTraits (bool isSimple=false)
    : DOTGraphTraits<node_t*>(isSimple) {}

  std::string getNodeLabel(node_t *Node, graph_t *G) {
    return DOTGraphTraits<node_t*>::getNodeLabel(Node, NULL);
  }
};
}
