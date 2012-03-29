//===- Ipet.cpp - Generate an ILP problem for WCET Analysis ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements a pass to generate an ILP problem.
//
// TODO support enforcing the WCET path over some nodes (take care of nodes in loops!)
//
// TODO construct an overlay graph, containing edges and split nodes for call sites, support indirect calls
// TODO construct a supergraph for callgraph regions, support SCCs of size > 1, construct global problem?
// TODO read loop bounds and costs of external calls from file, read flow-facts from file (as separate pass)
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "ipet"

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/BasicBlock.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Instructions.h"
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Config/config.h"

#include "CostProvider.h"
#include "FlowFactProvider.h"
#include "Ipet.h"

#ifdef HAVE_LPLIB_H
#include <lp_lib.h>
#else
#include <lpsolve/lp_lib.h>
#include "llvm/ADT/SmallSet.h"
#endif

using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");

namespace wcet {

bool IpetPass::doInitialization(CallGraph &CG) {

  destroy();

  this->CG = &CG;

  ipetResult = new IpetResult();

  return false;
}

void IpetPass::destroy() {
  if (ipetResult) delete ipetResult;
}

bool IpetPass::runOnSCC(CallGraphSCC & SCC) {

  if (!SCC.isSingular()) {
    // TODO call IPET solver for whole SCC

    DEBUG(errs() << "Not a singular SCC; size: " << SCC.size() << "\n");

  } else {
    Function *F = (*(SCC.begin()))->getFunction();
    if (!F) {
      // anything we should do here?
      return false;
    }

    if (F->isDeclaration()) {
      DEBUG(errs() << "Skipping declaration " << F->getName() << "\n");
      return false;
    }

    DEBUG(errs() << "Analyzing function " << F->getName() << "\n");

    //TODO find a way to select between cost providers
    SimpleCostProvider CP;

    IpetConfig ipetConfig(*CG, CP, getAnalysis<SCEVFlowFactProvider>());

    Ipet ipet(ipetConfig, *ipetResult);

    ipet.analyze(*F);
  }

  return false;
}

void IpetPass::print(raw_ostream &O, const Module *M) const {
  if (ipetResult) {
    ipetResult->print(O);
  }
}




Ipet::Ipet(IpetConfig &config, IpetResult &result)
 : config(config), result(result), CP(config.getCostProvider()), FFP(config.getFlowFactProvider())
{
}

uint64_t Ipet::getCost(BasicBlock &BB)
{
  int costs = CP.getLocalCost(BB);

  // add costs of callees for all call sites in the basic block
  for (BasicBlock::iterator II = BB.begin(), IE = BB.end(); II != IE; ++II) {
    CallSite CS(cast<Value>(II));
    if (!CS) continue;

    if (!isa<IntrinsicInst>(II)) {
      Function *Callee = getCallee(CS);

      // TODO in case we have more than one callee, get max over all callees

      if (Callee) {
        // this is a (direct) call
        costs += getNonlocalCost(CS, *Callee);
      } else {
        // This is a call to an unknown function, need to ask the cost provider
        costs += CP.getNonlocalCost(CS);
      }

    } else {
      // we have an intrinsic call .. this should already be included in the local costs (?)
    }
  }

  return costs;
}

uint64_t Ipet::getNonlocalCost(CallSite &CS, Function &Callee) {

  if (Callee.isDeclaration() || result.isUnbounded(Callee)) {
     return CP.getNonlocalCost(CS, Callee);
  }

  // we have a definition at this point, try to get costs from recursive analysis

  if (!result.hasWCET(Callee)) {

    Ipet ipet(config, result);

    if (!ipet.analyze(Callee)) {
      errs() << "Failed to get analysis results for call site " << CS <<
                " calling function " << Callee.getName() << "\n";
      // TODO some error handling
      return CP.getNonlocalCost(CS, Callee);
    }

  }

  return result.getWCET(Callee);
}

bool Ipet::analyze(Function &F) {
  // TODO if F is part of a SCC in the call-graph or has any non-local flow-facts, call analyze for whole SCC

  // poor-mans recursion handling: abort if we are within recursive calls
  if (result.inProgress(F)) {
    errs() << "Recursive call found for function " << F.getName() << ", not implemented!\n";
    return false;
  }
  result.setInProgress(F);

  // initialize (clear all maps, collect edges, construct edge list)
  loadStructure(F);

  // initialize lp-solve (create lp-solve instance, add variables)
  lprec *lp = initSolver(F);

  // construct objective function (max sum( edge-freq * edge-cost ))
  setObjective(lp, F);

  // construct structural constraints
  setStructConstraints(lp, F);

  // construct flow constraints
  setFlowConstraints(lp, F);

  // close entry mode
  finishEntry(lp, F);

  // dump lp-solve file
  // TODO make this optional
  dumpProblem(lp, F);

  // run lp-solve, handle errors (did not terminate, no solution,..)
  if (!runSolver(lp, F)) {
    cleanup(lp, F, false);
    return false;
  }

  // map results back to edges and basic blocks
  readResults(lp, F);

  // TODO dump/print results? -> used by IpetPrintPass or something

  cleanup(lp, F, true);
  return true;
}

Function* Ipet::getCallee(const CallSite &CS) {
  // TODO handle function pointer calls, invokes, .. etc, which may call more than one function!
  return CS.getCalledFunction();
}

void Ipet::loadStructure(Function & F)
{
  // find all edges in the CFG

  // TODO to support interprocedural analysis, context-sensitive analysis, cache analysis or flowfacts that
  // restrict the number of calls of indirect calls, we need to construct a custom graph here:
  // - Nodes can be Entry, Exit, Basic-block, Call, Return, split, merge, Cache-costs; nodes only have costs
  //   and references to the BasicBlock or Call Site if appropriate.
  // - Edges can be between (mostly) any of the nodes
  // The graph should contain an Entry node, an Exit node and nodes for all 'simple' basic blocks (no calls),
  // with the appropriate edges. For every basic block that contains a call, the following subgraph should
  // be created for that basic block
  // - A node representing the (full) basic block local costs; no splitting of the basic block needed, we
  //   assume composability of pipeline effects for call sites
  // - For every call site in the BB, add the following graph in series
  //   - A split node
  //   - A call node for every possible callee candidate
  //   - A cache miss cost node for every callee candidate that may be a miss
  //   - Either an edge to the entry of the callee, or a copy of the callee graph, depending on context-strings
  //   - A return node, with an edge from the exit node of the callee
  //   - A return cache miss cost node
  //   - A merge node

  // .. for now, we just construct a simple edge graph containing basic blocks, and null pointer for entry/exit
  // Note: all edges with the same source node are stored sequentially in the vector!

  edges.clear();

  // add an entry edge
  edges.push_back(Edge(NULL, &(F.getEntryBlock())));

  // add all edges
  for (Function::iterator it = F.begin(), end = F.end(); it != end; ++it) {
    BasicBlock *BB = it;

    bbIndexMap.insert(std::make_pair(BB, edges.size()));

    for (succ_iterator SI = succ_begin(BB), E = succ_end(BB); SI != E; ++SI) {
      edges.push_back(Edge(BB, *SI));
    }

    // TODO hackish check for exit edges, clean up. What about resume, unwind, unreachableinstr?
    if (dyn_cast_or_null<ReturnInst>(BB->getTerminator())) {
      edges.push_back(Edge(BB, NULL));
    }
  }
}

lprec *Ipet::initSolver(Function & F)
{
  lprec *lp = make_lp(0, edges.size());

  set_add_rowmode(lp, TRUE);

  for (size_t i = 0; i < edges.size(); i++) {
    // build edge name
    const BasicBlock *source = edges[i].first;
    const BasicBlock *target = edges[i].second;

    std::string edge_name = std::string("e_");
    edge_name.append(source ? result.getBlockLabel(*source, F) : "entry");
    edge_name.append("__");
    edge_name.append(target ? result.getBlockLabel(*target, F) : "exit");

    set_col_name(lp, i+1, (char*)edge_name.c_str());
    set_int(lp, i+1, TRUE);
  }

  return lp;
}

void Ipet::setObjective(lprec *lp, Function & F)
{
  set_maxim(lp);

  // For every edge, get costs of BB at source of edge, add as costs to edge, build objective function
  // We use the costs of the source BB so that we can support different costs for the terminator instruction
  // depending on the target)
  REAL *row = new REAL[edges.size()];
  int *colno = new int[edges.size()];
  int cnt = 0;

  // cost of the entry edge is zero (no need to set zero value explicitly, skip it)

  for (size_t i = 1; i < edges.size(); i++) {
    BasicBlock *source = edges[i].first;

    // TODO we could use different costs for different outgoing edges here (e.g. to have different
    //  costs for branch-taken and branch-not-taken)
    uint64_t cost = getCost(*source);
    if (!cost) continue;

    row[cnt] = cost;
    colno[cnt] = i+1;
    cnt++;
  }

  set_obj_fnex(lp, cnt, row, colno);

  delete[] row;
  delete[] colno;
}

void Ipet::setStructConstraints(lprec *lp, Function & F)
{
  std::vector<REAL> row;
  std::vector<int> colno;

  // add constraint for entry
  row.push_back(1);
  colno.push_back(1);
  add_constraintex(lp, 1, &row[0], &colno[0], EQ, 1);
  set_row_name(lp, 1, (char*)"c_entry");

  std::vector<int> exits;

  // add struct constaints for all edges
  for (Function::iterator it = F.begin(), end = F.end(); it != end; ++it) {
    BasicBlock *BB = it;
    size_t idx = bbIndexMap.lookup(BB);

    // sum over all ingoing edges - sum over all outgoing edges = 0
    // => row=1 for all ingoing edges, row=-1 for all outgoing edges, row=0 for self-loops)
    row.clear();
    colno.clear();

    // get outgoing edges, excluding self-loops
    size_t i = idx;
    while (i < edges.size() && edges[i].first == BB) {
      const BasicBlock *target = edges[i].second;

      // skip self-loops
      if (target != BB) {
        row.push_back(-1);
        colno.push_back(i+1);
      }
      // collect all exit edges
      if (target == NULL) {
        exits.push_back(i+1);
      }
      i++;
    }

    // get ingoing edges, excluding self-loops
    for (pred_iterator pi = pred_begin(BB), end = pred_end(BB); pi != end; ++pi) {
      if (*pi == BB) continue;
      int edge = findEdge(*pi, BB);

      row.push_back(1);
      colno.push_back(edge+1);
    }

    // for entry block, add entry edge as ingoing edge
    if (BB == &F.getEntryBlock()) {
      row.push_back(1);
      colno.push_back(1);
    }

    add_constraintex(lp, colno.size(), &row[0], &colno[0], EQ, 0);

    std::string block_name = std::string("b_");
    block_name.append(result.getBlockLabel(*BB, F));

    set_row_name(lp, get_Nrows(lp)-1, (char*)block_name.c_str());
  }

  // add constraint that exactly one exit edge will be taken
  row.clear();
  for (size_t i = 0; i < exits.size(); i++) {
    row.push_back(1);
  }

  add_constraintex(lp, exits.size(), &row[0], &exits[0], EQ, 1);
  set_row_name(lp, get_Nrows(lp)-1, (char*)"c_exit");
}

void Ipet::setFlowConstraints(lprec *lp, Function &F)
{
  std::vector<REAL> row;
  std::vector<int> colno;

  // add all block constraints
  for (FlowFactProvider::BlockConstraints::const_iterator bc = FFP.getBlockConstraints().begin(),
       end = FFP.getBlockConstraints().end(); bc != end; ++bc)
  {
    const BasicBlock *bb = bc->Block;
    const BasicBlock *ref = bc->Ref;
    int cmp = getConstrType(bc->Cmp);
    int N = bc->N;

    // build constraint of form Block - N * Ref <cmp> 0, or Block <cmp> N if Ref == NULL
    row.clear();
    colno.clear();

    // collect outgoing edges of Block
    size_t i = bbIndexMap.lookup(bb);
    while (i < edges.size() && edges[i].first == bb) {
      row.push_back(1);
      colno.push_back(i+1);
      i++;
    }

    // collect outgoing edges of Ref
    if (ref != NULL) {
      i = bbIndexMap.lookup(ref);
      while (i < edges.size() && edges[i].first == ref) {
        row.push_back(-N);
        colno.push_back(i+1);
        i++;
      }
    }

    add_constraintex(lp, colno.size(), &row[0], &colno[0], cmp, ref != NULL ? 0 : N);
    //set_row_name(lp, get_Nrows(lp)-1, (char*)("ff_"+result.getBlockLabel(*bb, F)));
  }

  // add edge constraints

  // do we have any edges in the edge list that end in Ref?
  SmallSet<int,4> skipEdges;

  for (FlowFactProvider::EdgeConstraints::const_iterator ec = FFP.getEdgeConstraints().begin(),
       end = FFP.getEdgeConstraints().end(); ec != end; ++ec)
  {
    const BasicBlock *ref = ec->Ref;
    int cmp = getConstrType(ec->Cmp);
    int N = ec->N;

    // build constraint of form: sum(edges) - N * (Ingoing(Ref)\Edges) <cmp> 0, or sum(edges) <cmp> N if Ref == NULL
    row.clear();
    colno.clear();
    skipEdges.clear();

    // add all edges
    for (FlowFactProvider::EdgeList::const_iterator edge = ec->Edges.begin(),
         eend = ec->Edges.end(); edge != eend; ++edge)
    {
      int idx = findEdge(edge->first, edge->second);
      if (idx < 0) {
        errs() << "Did not find edge!\n";
        continue;
      }

      row.push_back(1);
      colno.push_back(idx+1);

      // ingoing edge in ref is skipped
      if (edge->second == ref) skipEdges.insert(idx);
    }

    // add ingoing(ref)\edges
    if (ref != NULL) {
      for (const_pred_iterator pi = pred_begin(ref), end = pred_end(ref); pi != end; ++pi) {
        int idx = findEdge(*pi, ref);
        if (skipEdges.count(idx)) continue;

        row.push_back(-N);
        colno.push_back(idx+1);
      }
    }

    add_constraintex(lp, colno.size(), &row[0], &colno[0], cmp, ref != NULL ? 0 : N);

    // TODO add name ff_something
  }

}

void Ipet::finishEntry(lprec *lp, Function &F)
{
  set_add_rowmode(lp, FALSE);
}

bool Ipet::runSolver(lprec *lp, Function &F)
{
  set_verbose(lp, IMPORTANT);
  set_timeout(lp, 600);
  //set_presolve(lp, PRESOLVE_ROWS, 2);

  int ret = solve(lp);
  if (ret == OPTIMAL || ret == PRESOLVED) {
    errs() << "Found optimal result for " << F.getName() << "\n";
    return true;
  }
  if (ret == SUBOPTIMAL) {
    errs() << "Found suboptimal result for " << F.getName() << "\n";
    return true;
  }

  errs() << "Failed to calculate solution for function " << F.getName() << ", retcode: " << ret << "\n";
  return false;
}



void Ipet::readResults(lprec *lp, Function & F)
{
  // kill old wcet and ef results
  result.clearResults(F);

  // store new WCET
  result.setWCET(F, get_objective(lp));

  // read ef for all edges, sum up all outgoing edges to get wcef for basic blocks
  REAL *vars;
  get_ptr_variables(lp, &vars);

  const BasicBlock *curr = NULL;
  uint64_t ef = 0;

  // skip the entry edge, should be 1
  for (size_t i = 1; i < edges.size(); i++) {
    const BasicBlock *source = edges[i].first;

    // next segment in edge array for next source basic block found
    if (source != curr) {
      // skip entry edge
      if (curr != NULL) {
        result.setExecFrequency(*curr, ef);
      }
      ef = 0;
      curr = source;
    }

    ef += vars[i];
  }
  // update last block
  if (curr != NULL) {
    result.setExecFrequency(*curr, ef);
  }
}

void Ipet::dumpProblem(lprec *lp, Function & F)
{
  std::string fname = std::string(F.getName().str());
  fname.append(".lp");
  write_lp(lp, (char*)fname.c_str());
}

void Ipet::cleanup(lprec *lp, Function &F, bool success) {
  if (lp) delete_lp(lp);

  // cleanup temp structures
  edges.clear();
  bbIndexMap.clear();

  result.setFinished(F, !success);
}

int Ipet::findEdge(const BasicBlock *source, const BasicBlock *target)
{
  if (!source) return 0;

  size_t pos = bbIndexMap.lookup(source);
  while (pos < edges.size()) {
    Edge edge = edges[pos];
    if (edge.first != source) {
      // iterated over the end of the list of edges for this source, not found
      return -1;
    }
    if (edge.second == target) {
      return pos;
    }
    pos++;
  }

  return -1;
}

int Ipet::getConstrType(FlowFactProvider::ConstraintType type)
{
  switch (type) {
    case FlowFactProvider::CT_LE: return LE;
    case FlowFactProvider::CT_GE: return GE;
    case FlowFactProvider::CT_EQ: return EQ;
    default:
      errs() << "You naughty boy!\n";
      return 0;
  }
}


void IpetResult::reset() {
  // clear all results
  costWCET.clear();
  execFreq.clear();
  status.clear();
}

uint64_t IpetResult::getWCExecFrequency(const BasicBlock &BB) const {
  if (!execFreq.count(&BB)) return 0;
  return execFreq.lookup(&BB);
}

uint64_t IpetResult::getWCET(const Function &F) const {
  return costWCET.lookup(&F);
}

bool IpetResult::hasWCET(const Function &F) const {
  return costWCET.count(&F);
}

bool IpetResult::inProgress(const Function &F) const {
  if (!status.count(&F)) return false;
  return status.lookup(&F) == IPET_RUNNING;
}

bool IpetResult::isUnbounded(const Function &F) const {
  if (!status.count(&F)) return false;
  return status.lookup(&F) == IPET_UNBOUNDED;
}

void IpetResult::setInProgress(const Function &F) {
  status.erase(&F);
  status.insert(std::make_pair(&F, IPET_RUNNING));
}

void IpetResult::setFinished(const Function &F, bool unbounded) {
  status.erase(&F);
  if (unbounded) {
    costWCET.erase(&F);
    status.insert(std::make_pair(&F, IPET_UNBOUNDED));
  }
}

void IpetResult::setWCET(Function &F, uint64_t wcet)
{
  costWCET.erase(&F);
  costWCET.insert(std::make_pair(&F, wcet));
}

void IpetResult::setExecFrequency(const BasicBlock &BB, uint64_t freq)
{
  // we assume that clearResults(F) has been called!
  execFreq.insert(std::make_pair(&BB, freq));
}

void IpetResult::clearResults(Function &F)
{
  costWCET.erase(&F);

  for (Function::iterator bb = F.begin(), end = F.end(); bb != end; ++bb) {
    execFreq.erase(bb);
  }
}

void IpetResult::print(raw_ostream &O) const
{
  //O << "Dumping IPET results:\n";

  for (FunctionMap::const_iterator it = costWCET.begin(), end = costWCET.end(); it != end; ++it) {
    const Function *F = it->first;
    uint64_t wcet = it->second;

    O << " Result for " << F->getName() << ": WCET " << wcet << "\n";

    for (Function::const_iterator bit = F->begin(), bend = F->end(); bit != bend; ++bit) {
      const BasicBlock *bb = bit;
      if (execFreq.count(bb)) {
        O << "   Block " << getBlockLabel(*bb, *F) << ": ef " << execFreq.lookup(bb) << "\n";
      }
    }
  }
}

std::string IpetResult::getBlockLabel(const BasicBlock &BB, const Function &F) const
{
  return DOTGraphTraits<const Function*>::getSimpleNodeLabel(&BB, &F);
}



} // namespace ipet

char wcet::IpetPass::ID = 0;
static RegisterPass<wcet::IpetPass> X("ipet", "IPET Pass");
