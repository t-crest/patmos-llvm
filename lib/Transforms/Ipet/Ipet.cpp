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

#include <lpsolve/lp_lib.h>

#include "llvm/Pass.h"
#include "llvm/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CallGraphSCCPass.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/BasicBlock.h"
#include "llvm/IntrinsicInst.h"
#include "llvm/Instructions.h"
#include "llvm/Support/CFG.h"
#include "llvm/Support/Casting.h"

#include "CostProvider.h"
#include "Ipet.h"


using namespace llvm;

//STATISTIC(SomeCounter, "Counts something");

namespace ipet {

bool IpetPass::doInitialization(CallGraph &CG) {

  destroy();

  //TODO find a way to select between cost providers
  CP  = new SimpleCostProvider();
  // CP = new BasicCostProvider();

  FFP = new SimpleFlowFactProvider();

  IPET = new Ipet(CG, *CP, *FFP);

  return false;
}

void IpetPass::destroy() {
  if (IPET) delete IPET;
  if (CP)   delete CP;
  if (FFP)  delete FFP;
}

bool IpetPass::runOnSCC(CallGraphSCC & SCC) {

  if (!SCC.isSingular()) {
    // TODO call IPET solver for whole SCC

    errs() << "Not a singular SCC; size: " << SCC.size() << "\n";

  } else {
    Function *F = (*(SCC.begin()))->getFunction();
    if (!F) {
      // anything we should do here?
      return false;
    }

    IPET->analyze(*F);
  }

  return false;
}



void Ipet::reset() {
  // clear all results
  costWCET.clear();
  execFreq.clear();
  edges.clear();
  bbIndexMap.clear();
}

uint64_t Ipet::getWCExecFrequency(const BasicBlock &BB) const {
  if (!execFreq.count(&BB)) return 0;
  return execFreq.lookup(&BB);
}

uint64_t Ipet::getWCET(const Function &F) const {
  return costWCET.lookup(&F);
}

bool Ipet::hasWCET(const Function &F) const {
  return costWCET.count(&F);
}

bool Ipet::inProgress(const Function &F) const {
  if (!hasWCET(F)) return false;
  return getWCET(F) == UINT64_MAX;
}

void Ipet::setInProgress(const Function &F) {
  costWCET.erase(&F);
  costWCET.insert(std::make_pair(&F, UINT64_MAX));
}

void Ipet::clearInProgress(const Function &F, bool success) {
  if (!success) {
    costWCET.erase(&F);
  }
}

void Ipet::setWCET(Function &F, uint64_t wcet)
{
  costWCET.erase(&F);
  costWCET.insert(std::make_pair(&F, wcet));
}

void Ipet::clearResults(Function &F)
{
  costWCET.erase(&F);

  for (Function::iterator bb = F.begin(), end = F.end(); bb != end; ++bb) {
    execFreq.erase(bb);
  }
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

  if (Callee.isDeclaration()) {
     return CP.getNonlocalCost(CS, Callee);
  }

  // we have a definition at this point, try to get costs from recursive analysis

  if (!hasWCET(Callee)) {
    if (!analyze(Callee)) {
      errs() << "Failed to get analysis results for call site " << CS << " calling function " << Callee << "\n";
      // TODO some error handling
      return 0;
    }
  }

  return getWCET(Callee);
}

bool Ipet::analyze(Function &F) {
  // TODO if F is part of a SCC in the call-graph or has any non-local flow-facts, call analyze for whole SCC

  // poor-mans recursion handling: abort if we are within recursive calls
  if (inProgress(F)) {
    errs() << "Recursive call found for function " << F << ", not implemented!\n";
    return false;
  }
  setInProgress(F);

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

  // TODO add additional constraints to force WCEP

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
    std::string edge_name = std::string("e_");
    edge_name.append(edges[i].first->getName());
    edge_name.append("__");
    edge_name.append(edges[i].second->getName());

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

    add_constraintex(lp, exits.size(), &row[0], &colno[0], EQ, 1);

    std::string block_name = std::string("b_");
    block_name.append(BB->getName());
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
  // TODO find all loops, use FlowFactProvider to get loop bounds, add other flow facts?

}

bool Ipet::runSolver(lprec *lp, Function &F)
{
  set_verbose(lp, IMPORTANT);
  set_timeout(lp, 600);
  //set_presolve(lp, PRESOLVE_ROWS, 2);

  int ret = solve(lp);
  if (ret == OPTIMAL || ret == PRESOLVED) {
    errs() << "Found optimal result for " << F << "\n";
    return true;
  }
  if (ret == SUBOPTIMAL) {
    errs() << "Found suboptimal result for " << F << "\n";
    return true;
  }

  errs() << "Failed to calculate solution for function " << F << ", retcode: " << ret << "\n";
  return false;
}



void Ipet::readResults(lprec *lp, Function & F)
{
  // kill old wcet and ef results
  clearResults(F);

  // store new WCET
  setWCET(F, get_objective(lp));

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
        execFreq.insert(std::make_pair(curr, ef));
      }
      ef = 0;
      curr = source;
    }

    ef += vars[i];
  }
  // update last block
  if (curr != NULL) {
    execFreq.insert(std::make_pair(curr, ef));
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

  clearInProgress(F, success);
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

}

char ipet::IpetPass::ID = 0;
static RegisterPass<ipet::IpetPass> X("ipet", "IPET Pass");
