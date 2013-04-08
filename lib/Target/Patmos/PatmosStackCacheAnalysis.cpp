//===-- PatmosStackCacheAnalysis.cpp - Analysis of the stack-cache usage. -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Analyze the usage of the stack cache based on an machine-level call graph.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-stack-cache-analysis"
#undef PATMOS_TRACE_REMOVE
#undef PATMOS_TRACE_LOCAL_USAGE
#undef PATMOS_PRINT_USER_BOUNDS
#undef PATMOS_TRACE_GLOBAL_VISITS
#undef PATMOS_TRACE_CALLFREES
#undef PATMOS_TRACE_GLOBAL_USAGE
#undef PATMOS_TRACE_ILP

#include "Patmos.h"
#include "PatmosCallGraphBuilder.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineModulePass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"

#include <map>
#include <set>
#include <fstream>

using namespace llvm;

/// This is expected to be a program that takes one argument specifying the name
/// of a LP file to solve and generate a file with the same name and an appended
/// suffix .sol containing the numeric value of the integer solution of the LP.
static cl::opt<std::string> Solve_ilp(
  "mpatmos-ilp-solver",
  cl::init("solve_ilp"),
  cl::desc("Path to an ILP solver."),
  cl::Hidden);

/// Option to specify a file containing user-supplied bounds when solving ILP
/// problems (for regions of the call graph with recursion).
static cl::opt<std::string> BoundsFile(
  "mpatmos-stack-cache-analysis-bounds",
  cl::desc("File containing bounds for the stack cache analysis."),
  cl::Hidden);

namespace llvm {
  STATISTIC(RemovedSENS, "Useless SENS instructions removed.");
  STATISTIC(RemainingSENS, "SENS instructions remaining.");

  /// Information concerning a specific SCC.
  struct SCCInfo {
    /// Text fragment to be inserted right after the objective function.
    std::string ObjectiveFunction;

    /// Text fragment to be inserted right after the constraints.
    std::string Constraints;

    /// Text fragment to be inserted right after the variable definition.
    std::string Variables;
  };

  typedef std::map<std::string, SCCInfo> SCCInfos;

  /// Class to parse bounds specifications constraining paths through SCCs in
  /// the call graph during ILP solving.
  class BoundsInformation {
    /// Information specific to specific SCCs.
    SCCInfos Infos;

    /// appendDefaultConstraitns - append some default constraints, e.g., for 
    /// the vfprintf function.
    void appendDefaultConstraitns()
    {
      /// the vfprintf function calls itself, but only once.
      SCCInfo vfprintf = {"",
                          "usr:     + X_vfprintf_r <= 2\n",
                          ""};
      Infos["_vfprintf_r"] = vfprintf;
    }

    /// parseBoundsFile - read user defined bounds for the ILP solving from a
    /// file.
    /// The expected file format is based on lines, each line should look 
    /// something like this:
    /// [name]: {[text]}{[text]}{[text]} \n
    void parseBoundsFile(const std::string &boundsfile) {
      // nothing to read?
      if (boundsfile.empty())
        return;

      // open file
      std::ifstream IS(boundsfile.c_str());

      // worked?
      if (!IS.good()) {
        errs() << "Error: Failed to read from file '" << boundsfile << "'.\n";
      }

      // read one line at a time
      while(IS.good()) {
        std::string line;
        std::getline(IS, line, ';');

        if (IS.eof())
          continue;

        // the format of a line is:
        // [name]: {[text]} {[text]} {[text]} \n
        std::size_t name_start = line.find_first_not_of(" \t\n");
        std::size_t name_end = line.find_first_of(':', name_start);

        std::size_t objective_start = line.find_first_of('{', name_end);
        std::size_t objective_end = line.find_first_of('}', objective_start);

        std::size_t constraint_start = line.find_first_of('{', objective_end);
        std::size_t constraint_end = line.find_first_of('}', constraint_start);

        std::size_t variables_start = line.find_first_of('{', constraint_end);
        std::size_t variables_end = line.find_first_of('}', variables_start);

        if (name_start == std::string::npos || name_end == std::string::npos ||
            objective_start == std::string::npos ||
            objective_end == std::string::npos ||
            constraint_start == std::string::npos ||
            constraint_end == std::string::npos ||
            variables_start == std::string::npos ||
            variables_end == std::string::npos) {
          errs() << "Error: Invalid line in ILP bounds file: " << line << ".\n";
        }
        else {
          // extract bounds information
          std::string name(line.substr(name_start, name_end - name_start));
          std::string objective(line.substr(objective_start + 1,
                                          objective_end - objective_start - 1));
          std::string constraint(line.substr(constraint_start + 1,
                                        constraint_end - constraint_start - 1));
          std::string variables(line.substr(variables_start + 1,
                                          variables_end - variables_start - 1));

          // debug output
#ifdef PATMOS_PRINT_USER_BOUNDS
          dbgs() << "bounds: " << name << ": [" << objective << "]["
                                                << constraint << "]["
                                                << variables <<"]\n";
#endif // PATMOS_PRINT_USER_BOUNDS

          // add bounds information
          SCCInfo tmp = {objective, constraint, variables};
          Infos[name] = tmp;
        }
      }
    }
  public:
    /// Parse the bounds specification from a file.
    BoundsInformation(const std::string &boundsfile)
    {
      parseBoundsFile(boundsfile);
      appendDefaultConstraitns();
    }

    /// getInfo - Retrieve information for a specific SCC.
    const SCCInfo &getInfo(const MCGNodes &SCC) const {
      for(MCGNodes::const_iterator i(SCC.begin()), ie(SCC.end()); i != ie;
          i++) {
        // TODO: maybe add support for unknown functions.
        if (!(*i)->isUnknown() &&
            hasInfo((*i)->getMF()->getFunction()->getName()))
          return getInfo((*i)->getMF()->getFunction()->getName());
      }

      dbgs() << "Error: Missing bounds for SCC: ";
      (*SCC.begin())->dump();
      dbgs() << "\n";
      for (SCCInfos::const_iterator i(Infos.begin()), ie(Infos.end()); i != ie;
           i++) {
        dbgs() << "'" << i->first << "' ";
      }
      dbgs() << "\n";
      assert(false && "Missing bounds for SCC during stack cache analysis.");
      abort();
    }

    /// getInfo - Retrieve information for a specific SCC represented by a
    /// function.
    const SCCInfo &getInfo(const std::string &function) const {
      SCCInfos::const_iterator tmp(Infos.find(function));
      assert(tmp != Infos.end());
      return tmp->second;
    }

    /// hasInfo - Check if information for a specific SCC represented by a
    /// function is available.
    bool hasInfo(const std::string &function) const {
      SCCInfos::const_iterator tmp(Infos.find(function));
      return tmp != Infos.end();
    }
  };

  /// Pass to analyze the usage of Patmos' stack cache.
  class PatmosStackCacheAnalysis : public MachineModulePass {
  private:
    /// Work list of basic blocks.
    typedef std::set<MachineBasicBlock*> MBBs;

    /// Set of call graph nodes.
    typedef std::set<MCGNode*> MCGNodeSet;

    /// List of call graph SCCs and a flag indicating whether the SCC actually 
    /// contains loops.
    typedef std::vector<std::pair<MCGNodes, bool> > MCGNSCCs;

    /// Map call graph nodes to booleans.
    typedef std::map<MCGNode*, bool> MCGNodeBool;

    /// Map basic blocks to local stack access usage information.
    typedef std::map<MachineBasicBlock*, unsigned int> MBBUInt;

    /// Map basic blocks to a boolean.
    typedef std::map<MachineBasicBlock*, bool> MBBBool;

    /// Map call graph nodes to their (potentially trivial SCC).
    typedef std::map<MCGNode*, std::pair<MCGNodes, bool>*> MCGNodeSCC;

    /// Map call graph nodes to stack usage information.
    typedef std::map<MCGNode*, unsigned int> MCGNodeUInt;

    /// List of ensures to be removed/not removed.
    typedef std::map<MachineInstr*, bool> ENSUREs;

    /// List of ensures and their respective sizes.
    typedef std::map<MachineInstr*, unsigned int> SIZEs;

    /// Track for each call graph node the max. stack usage.
    MCGNodeUInt MaxStackUse;

    /// Track functions with call-free paths in them.
    MCGNodeBool IsCallFree;

    /// Subtarget information (stack cache block size)
    const PatmosSubtarget &STC;

    /// Instruction information
    const TargetInstrInfo &TII;

    /// Bounds to solve ILPs during stack cache analysis.
    const BoundsInformation BI;
  public:
    /// Pass ID
    static char ID;

    PatmosStackCacheAnalysis(const PatmosTargetMachine &tm) :
        MachineModulePass(ID), STC(tm.getSubtarget<PatmosSubtarget>())
        TII(*tm.getInstrInfo()), BI(BoundsFile)
    {
      initializePatmosCallGraphBuilderPass(*PassRegistry::getPassRegistry());
    }

    /// getAnalysisUsage - Inform the pass manager that nothing is modified
    /// here.
    virtual void getAnalysisUsage(AnalysisUsage &AU) const
    {
      AU.setPreservesAll();
      AU.addRequired<PatmosCallGraphBuilder>();

      ModulePass::getAnalysisUsage(AU);
    }

    /// getMaxStackUsage - Find the computed maximal stack usage for the node,
    /// including all its children in the call graph.
    /// \see computeMaxUsage 
    /// \see getStackUse
    unsigned int getMaxStackUsage(MCGNode *Node) const {
      MCGNodeUInt::const_iterator nodeUse(MaxStackUse.find(Node));

      // handle those nodes in an SCC of the call graph, i.e., recursion
      if (Node->isDead())
        return 0;
      else if (nodeUse == MaxStackUse.end())
        return STC.getStackCacheSize();
      else
        return nodeUse->second;
    }

    /// getStackUse - Determine the stack cache usage of the call graph node. 
    /// Returns 0 for the unknown node.
    unsigned int getStackUse(MCGNode *Node)
    {
      if (Node->isUnknown())
        return 0;
      else {
        MachineFunction *MF = Node->getMF();
        PatmosMachineFunctionInfo *PMFI =
                                       MF->getInfo<PatmosMachineFunctionInfo>();

        assert(PMFI->getStackCacheReservedBytes() %
               STC.getStackCacheBlockSize() == 0);

	// TODO a function might contain inline asm code that might use SRES/SFREE,
	// we should check for that.

        return PMFI->getStackCacheReservedBytes();
      }
    }

    /// computeMaxUsage - Visit all children in the call graph and find the 
    /// maximal amount of  space allocated by any of them (and their children).
    void computeMaxUsage(MCGNodeSCC &SCCMap, MCGNode *Node,
                         MCGNodeUInt &succCount, MCGNodes &WL)
    {
      // get the stack usage of the call graph node and all its children
      unsigned int maxChildUse = 0;
      unsigned int nodeUse = getStackUse(Node);

#ifdef PATMOS_TRACE_GLOBAL_VISITS
      DEBUG(Node->dump(); dbgs() << "\n");
#endif // PATMOS_TRACE_GLOBAL_VISITS

      // ok, dead nodes don't do anything
      if (Node->isDead()) {
        maxChildUse = 0;
        return;
      }
      else if (SCCMap[Node]->second) {
        // the node is in an SCC! -> make an ILP
        // note: we know here that all successors of the entire SCC have been
        // handled
        maxChildUse = computeMaxUsageILP(SCCMap[Node]->first, Node);
        assert(maxChildUse > nodeUse);
      }
      else {
        // check all called functions
        const MCGSites &callSites(Node->getSites());
        for(MCGSites::const_iterator i(callSites.begin()), ie(callSites.end());
            i != ie; i++) {
          // get the child's stack usage
          maxChildUse = std::max(maxChildUse,
                                 getMaxStackUsage((*i)->getCallee()));
        }

        // include the current function's stack space:
        maxChildUse += nodeUse;
      }

      // compute the call graph node's stack use
      MaxStackUse[Node] = std::min(STC.getStackCacheSize(), maxChildUse);

      // update the work list
      MCGNodeSet preds;
      const MCGSites &callingSites(Node->getCallingSites());
      for(MCGSites::const_iterator i(callingSites.begin()),
          ie(callingSites.end()); i != ie; i++) {
        MCGNode *caller = (*i)->getCaller();
        // do not consider dead functions and functions in the same SCC her
        if (!caller->isDead() && SCCMap[caller] != SCCMap[Node]) {
          const MCGNodes &predSCC(SCCMap[caller]->first);
          preds.insert(predSCC.begin(), predSCC.end());
        }
      }

      for(MCGNodeSet::const_iterator i(preds.begin()), ie(preds.end()); i != ie;
          i++) {
        assert(succCount[*i] > 0);
        if (--succCount[*i] == 0)
          WL.push_back(*i);

#ifdef PATMOS_TRACE_GLOBAL_VISITS
        (*i)->dump(); dbgs() << "(" << succCount[*i] << ") ";
#endif // PATMOS_TRACE_GLOBAL_VISITS
      }
#ifdef PATMOS_TRACE_GLOBAL_VISITS
      dbgs() << "\n";
#endif // PATMOS_TRACE_GLOBAL_VISITS
    }

    /// computeMaxUsage - Visit all children in the call graph and find the
    /// maximal amount of  space allocated by any of them (and their children).
    void computeMaxUsage(const MCallGraph &G)
    {
      /// List of SCCs in the call graph
      MCGNSCCs SCCs;
      MCGNodeSCC SCCMap;
      const MCGNodes &nodes(G.getNodes());

      // make sure we have enough space for all SCCs without re-allocation
      SCCs.reserve(nodes.size());

      // get SCCs
      typedef scc_iterator<MCallGraph> PCGSCC_iterator;
      for(PCGSCC_iterator s(scc_begin(G)); !s.isAtEnd(); s++) {

        // keep track of call graph nodes and their SCCs.
        SCCs.push_back(std::make_pair(*s, s.hasLoop()));
        for(MCGNodes::iterator n((*s).begin()), ne((*s).end()); n != ne; n++) {
          SCCMap[*n] = &SCCs.back();
        }

      }

      // initialize the work list
      MCGNodeUInt succCount;
      MCGNodes WL;
      for(MCGNSCCs::const_iterator i(SCCs.begin()), ie(SCCs.end()); i != ie;
          i++) {
        // get info of the current SCC
        const MCGNodes *SCC = &i->first;

        // get the number of successors of the SCC that are not in that SCC.
        MCGNodeSet succs;
        for(MCGNodes::const_iterator j(SCC->begin()), je(SCC->end()); j != je;
            j++) {
          for(MCGSites::const_iterator k((*j)->getSites().begin()),
              ke((*j)->getSites().end()); k != ke; k++) {
            // check if the two are in the same SCC
            if (&SCCMap[(*k)->getCallee()]->first != SCC) {
              succs.insert((*k)->getCallee());
            }
          }
        }

        // be sure to keep all nodes within an SCC off the WL for now
        for(MCGNodes::const_iterator j(SCC->begin()), je(SCC->end()); j != je;
            j++) {
          if (succs.size() == 0)
            WL.push_back(*j);
          else
            succCount[*j] = succs.size();
        }
      }

      // process nodes in topological order
#ifdef PATMOS_TRACE_GLOBAL_VISITS
      DEBUG(dbgs() << "====================================\n";);
#endif // PATMOS_TRACE_GLOBAL_VISITS

      while(!WL.empty()) {
        // pop some node from the work list
        MCGNode *tmp = WL.back();
        WL.pop_back();

        // compute its stack usage
        computeMaxUsage(SCCMap, tmp, succCount, WL);
      }

#ifdef PATMOS_TRACE_GLOBAL_USAGE
      DEBUG(
        for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
            i++) {
          if (!(*i)->isDead()) {
            (*i)->dump();
            dbgs() << ": " << getMaxStackUsage(*i)
                   << " (" << getStackUse(*i) << ")\n";
          }
        }
      );
#endif // PATMOS_TRACE_GLOBAL_USAGE
    }

    /// getStackUsage - Bound the address accessed by the instruction wrt. the 
    /// stack cache.
    unsigned int getStackUsage(MachineInstr *MI)
    {
      unsigned int scale = 1;
      switch(MI->getOpcode())
      {
        case Patmos::SWS:
          scale = 2;
        case Patmos::SHS:
          scale <<= 1;
        case Patmos::SBS:
        {
          unsigned B = MI->getOperand(2).getReg();
          if (MI->getOperand(3).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(3).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        case Patmos::LWS:
          scale = 2;
        case Patmos::LHS:
        case Patmos::LHUS:
          scale <<= 1;
        case Patmos::LBS:
        case Patmos::LBUS:
        {
          unsigned B = MI->getOperand(3).getReg();
          if (MI->getOperand(4).isImm() && B == Patmos::R0) {
            return scale * (MI->getOperand(4).getImm() + 1);
          }
          else return STC.getStackCacheSize();
        }
        default:
          return 0;
      }
    }

    /// propagateStackUse - Propagate information on the use of the stack cache
    /// e.g., by loads and stores, upwards trough the CFG to ensure 
    /// instructions. This information can be used to downsize or remove 
    /// ensures.
    void propagateStackUse(MBBs &WL, MBBUInt &INs, SIZEs &ENSs,
                           MachineBasicBlock *MBB)
    {
      // get max. stack usage from CFG successors.
      unsigned int stackUsage = INs[MBB];

      // propagate within the basic block
      for(MachineBasicBlock::reverse_instr_iterator i(MBB->instr_rbegin()),
          ie(MBB->instr_rend()); i != ie; i++) {

        // check for ensures
        if (i->getOpcode() == Patmos::SENS) {
          assert(i->getOperand(2).isImm());

          // compute actual space to ensure here
          unsigned int ensure = 
             std::ceil((float)stackUsage / (float)STC.getStackCacheBlockSize());

          // update the ensure to reserve only the space actually used.
          ENSs[&*i] = std::min(ensure, (unsigned int)i->getOperand(2).getImm());

          // If we encounter an ensure, we can reset the stackUsage counter to 0
          // since all following accesses will be served by this ensure. This
          // also applies when this ensure is eliminated.
          stackUsage = 0;
        }
        else {
          // get the instruction's local stack usage
          unsigned int instructionUsage = getStackUsage(&*i);

          stackUsage = std::max(stackUsage, instructionUsage);
        }
      }

      // propagate to CFG predecessors
      for(MachineBasicBlock::pred_iterator i(MBB->pred_begin()),
          ie(MBB->pred_end()); i != ie; i++) {
        // check if the new stack usage is larger than what was known previously
        if (INs[*i] < stackUsage) {
          // update the predecessor's stack usage and put it on the work list
          INs[*i] = stackUsage;
          WL.insert(*i);
        }
      }
    }

    /// propagateStackUse - Propagate information on the use of the stack cache
    /// e.g., by loads and stores, upwards trough the CFG to ensure
    /// instructions. This information can be used to downsize or remove
    /// ensures.
    void propagateStackUse(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown()) {
          MBBs WL;
          SIZEs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse-reverse post order would be 
          // optimal, but this works too).
          for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
              i++) {
            WL.insert(i);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its predecessors on the work list.
            propagateStackUse(WL, INs, ENSs, MBB);
          }

          // actually update the sizes of the ensure instructions.
          for(SIZEs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
              i++) {
            i->first->getOperand(2).setImm(i->second);
          }

#ifdef PATMOS_TRACE_LOCAL_USAGE
          DEBUG(
            dbgs() << "*************************** "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
#endif // PATMOS_TRACE_LOCAL_USAGE
        }
      }
    }

    /// removeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding call plus the current frame on the stack cache fit into
    /// the stack cache.
    // TODO: take care of predication, i.e., predicated SENS/CALL instructions
    // might be mangled and they might no match one to one.
    void removeEnsures(MBBs &WL, MBBUInt &INs, ENSUREs &ENSs, MCGNode *Node,
                       MachineBasicBlock *MBB)
    {
      // track maximum stack usage of children in the call graph -- initialize
      // from predecessors in the CFG.
      unsigned int childUse = INs[MBB];

      // propagate maximum stack usage of call graph children within the basic
      // block
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie; i++) {
        if (i->isCall()) {
          // find call site
          MCGSite *site = Node->findSite(i);
          assert(site);

          childUse = std::max(childUse, getMaxStackUsage(site->getCallee()));
        }
        else if (i->getOpcode() == Patmos::SENS) {
          unsigned int ensure = i->getOperand(2).getImm() *
                                STC.getStackCacheBlockSize();

          // does the content of the ensure and all the children in the call
          // graph fit into the stack cache?
          bool remove = (ensure + childUse) <= STC.getStackCacheSize();

          if (ensure == 0) assert(remove);

          // if all fits, the SENS can be removed.
          ENSs[i] = remove;

          if (!remove && (i->getOperand(0).getReg() == Patmos::NoRegister ||
                          i->getOperand(0).getReg() == Patmos::P0)) {
            // update the current usage of the stack cache.
            if (childUse >= ensure)
              childUse -= ensure;
            else
              childUse = 0;
          }
        }
      }

      // propagate to CFG successors
      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie; i++) {
        // check if the new stack usage is larger than what was known previously
        if (INs[*i] < childUse) {
          // update the successors's stack usage and put it on the work list
          INs[*i] = childUse;
          WL.insert(*i);
        }
      }
    }

    /// removeEnsures - Does what it says. SENS instructions can be removed if
    /// the preceding call plus the current frame on the stack cache fit into
    /// the stack cache.
    void removeEnsures(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {
        if (!(*i)->isUnknown()) {
          MBBs WL;
          ENSUREs ENSs;
          MBBUInt INs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list (yeah, reverse post order would be optimal, 
          // but this works too).
          for(MachineFunction::iterator j(MF->begin()), je(MF->end()); j != je;
              j++) {
            WL.insert(j);
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its successors on the work list.
            removeEnsures(WL, INs, ENSs, *i, MBB);
          }

          // actually remove ensure instructions
          for(ENSUREs::const_iterator i(ENSs.begin()), ie(ENSs.end()); i != ie;
              i++) {
            if (i->second) {
              i->first->getParent()->erase(i->first);
              RemovedSENS++;
            }
            else {
              RemainingSENS++;
            }
          }

#ifdef PATMOS_TRACE_REMOVE
          DEBUG(
            dbgs() << "########################### "
                   << MF->getFunction()->getName() << "\n";
            for(MBBUInt::const_iterator i(INs.begin()), ie(INs.end()); i != ie;
                i++) {
              dbgs() << "  " << i->first->getName()
                    << "(" << i->first->getNumber() << ")"
                    << ": " << i->second << "\n";
            }
          );
#endif // PATMOS_TRACE_REMOVE
        }
      }
    }

    /// ilp_name - make a name for a node suitable for the LP file.
    static std::string ilp_name(const MCGNode *N)
    {
      std::string tmps;
      raw_string_ostream tmp(tmps);
      if (N->isUnknown()) {
        tmp << "U" << (void*)N;
      }
      else
        tmp << "X" << N->getMF()->getFunction()->getName();
      return tmp.str();
    }

    /// ilp_name - make a name for a call site suitable for the LP file.
    static std::string ilp_name(const MCGSite *S)
    {
      std::string tmps;
      raw_string_ostream tmp(tmps);
      tmp << "S" << (void*)S;
      return tmp.str();
    }

    /// solve_ilp - solve the ILP problem.
    static unsigned int solve_ilp(const char *LPname)
    {
      unsigned int result = std::numeric_limits<unsigned int>::max();

      std::vector<const char*> args;
      args.push_back(Solve_ilp.c_str());
      args.push_back(LPname);
      args.push_back(0);

      std::string ErrMsg;
      if (sys::Program::ExecuteAndWait(sys::Path(Solve_ilp),
                                       &args[0],0,0,0,0,&ErrMsg)) {
        errs() << "Error: " << ErrMsg << "\n";
      }
      else {
        // read solution
        // construct name of solution
        sys::Path SOLname(LPname);
        SOLname.appendSuffix("sol");

        std::ifstream IS(SOLname.c_str());
        if (!IS.good()) {
          errs() << "Error: Failed to read ILP solution.\n";
        }
        else {
          double tmp;
          IS >> tmp;
          result = tmp;
        }

        SOLname.eraseFromDisk();
      }

      return result;
    }

    /// computeMaxUsageILP - construct an ILP problem, write it to an LP file, 
    /// and solve it.
    unsigned int computeMaxUsageILP(const MCGNodes &SCC, const MCGNode *N)
    {
      assert(std::find(SCC.begin(), SCC.end(), N) != SCC.end());

      // get bounds to solve the ILP.
      const SCCInfo &BInfo(BI.getInfo(SCC));

      // open LP file.
      std::string ErrMsg;
      sys::Path LPname(sys::Path::GetTemporaryDirectory(&ErrMsg));
      if (LPname.isEmpty()) {
        errs() << "Error: " << ErrMsg << "\n";
        return STC.getStackCacheSize();
      }

      LPname.appendComponent("scc.lp");
      raw_fd_ostream OS(LPname.c_str(), ErrMsg);
      if (!ErrMsg.empty()) {
        errs() << "Error: Failed to open file '" << LPname.str()
               << "' for writing!\n";
        return STC.getStackCacheSize();
      }

      // find entry and exit call sites
      typedef std::set<MCGSite*> MCGSiteSet;
      MCGSiteSet entries, exits;
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        // look for call sites exiting the SCC
        for(MCGSites::const_iterator cs((*n)->getSites().begin()),
          cse((*n)->getSites().end()); cs != cse; cs++) {
          MCGNode *d = (*cs)->getCallee();

          if (std::find(SCC.begin(), SCC.end(), d) == SCC.end())
            exits.insert(*cs);
        }

        // look for call sites entering the SCC
        for(MCGSites::const_iterator cs((*n)->getCallingSites().begin()),
          cse((*n)->getCallingSites().end()); cs != cse; cs++) {
          MCGNode *s = (*cs)->getCaller();

          if (std::find(SCC.begin(), SCC.end(), s) == SCC.end())
            entries.insert(*cs);
        }
      }

      //************************************************************************
      // objective function

      OS << "Maximize";

      // nodes in the SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << "\n + " << getStackUse(*n) << " " << ilp_name(*n);
      }

      // exit sites
      for(MCGSiteSet::iterator n(exits.begin()), ne(exits.end()); n != ne;
          n++) {
        OS << "\n + " << getMaxStackUsage((*n)->getCallee()) << "\t"
           << ilp_name(*n);

        if (!(*n)->getCallee()->isUnknown())
          OS << "\t\\ " << (*n)->getCallee()->getMF()->getFunction()->getName();
      }

      // entries do not matter here

      // add user defined parts of objective function
      OS << BInfo.ObjectiveFunction;

      //************************************************************************
      // constraints
      OS << "\nSubject To\n";
      unsigned int cnt = 0;

      // force path over N
      OS << "path:\t" << ilp_name(N) << " >= 1\n";

      // constraints on in-flow of nodes in SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << "if" << cnt << ":\t";

        const MCGSites &CS((*n)->getCallingSites());
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          OS << " + " << ilp_name(*cs);
        }

        OS << " - " << ilp_name(*n) << " = 0\n";
        cnt++;
      }

      // constraints on out-flow of nodes in SCC
      cnt = 0;
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << "of" << cnt << ":\t";

        const MCGSites &CS((*n)->getSites());
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          OS << " + " << ilp_name(*cs);
        }

        // handle function containing at least one call-free path
        if (IsCallFree[*n]) {
          OS << " + OF" << ilp_name(*n);
        }

        OS << " - " << ilp_name(*n) << " = 0\n";
        cnt++;
      }

      // constraint on out-flow of entry nodes
      OS << "en:\t";
      for(MCGSiteSet::const_iterator cs(entries.begin()), cse(entries.end());
          cs != cse; cs++) {
        OS << " + " << ilp_name(*cs);;
      }
      OS << " = 1\n";

      // constraint on in-flow of exit nodes
      OS << "ex:\t";
      bool ex_printed = false;
      for(MCGSiteSet::const_iterator cs(exits.begin()), cse(exits.end());
          cs != cse; cs++) {
        OS << " + " << ilp_name(*cs);
        ex_printed = true;
      }

      // handle exits through function containing at least one call-free path
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        if (IsCallFree[*n]) {
          OS << " + OF" << ilp_name(*n);
          ex_printed = true;
        }
      }

      assert(ex_printed);
      OS << " = 1\n";

      // add user defined constraints
      OS << BInfo.Constraints;

      //************************************************************************
      // variable domains
      OS << "Generals\n";

      // nodes and call sites in SCC
      for(MCGNodes::const_iterator n(SCC.begin()), ne(SCC.end()); n != ne;
          n++) {
        OS << ilp_name(*n) << "\n";

        // handle functions with at least one call-free path
        if (IsCallFree[*n]) {
          OS << "OF" << ilp_name(*n) << "\n";
        }

        const MCGSites &CS((*n)->getCallingSites());
        for(MCGSites::const_iterator cs(CS.begin()), cse(CS.end()); cs != cse;
            cs++) {
          // skip entry sites
          if(entries.find(*cs) == entries.end())
            OS << ilp_name(*cs) << "\n";
        }
      }

      // entries
      for(MCGSiteSet::const_iterator cs(entries.begin()), cse(entries.end());
          cs != cse; cs++) {
        OS << ilp_name(*cs) << "\n";
      }

      // exits
      for(MCGSiteSet::const_iterator cs(exits.begin()), cse(exits.end()); 
          cs != cse; cs++) {
        OS << ilp_name(*cs) << "\n";
      }

      // add user defined variable definitions
      OS << BInfo.Variables;

      OS << "End\n";

      OS.close();

      // solve the ILP
      unsigned int result = solve_ilp(LPname.c_str());

#ifdef PATMOS_TRACE_ILP
      dbgs() << "ILP: ";
      N->dump();
      dbgs() << ": " << result << "\n";
#endif // PATMOS_TRACE_ILP

      // remove LP file
      LPname.eraseFromDisk();

      return std::min(STC.getStackCacheSize(), result);
    }

    /// checkCallFreePaths - Check whether functions have call free paths.
    /// see below.
    void checkCallFreePaths(MBBs &WL, MBBBool &OUTs, MachineBasicBlock *MBB)
    {
      // see if a successor contains a call-free path to a sink
      bool is_call_free = OUTs[MBB];

      for(MachineBasicBlock::succ_iterator i(MBB->succ_begin()),
          ie(MBB->succ_end()); i != ie && is_call_free; i++) {
        is_call_free |= OUTs[*i];
      }

      // see if the block contains a call
      for(MachineBasicBlock::instr_iterator i(MBB->instr_begin()),
          ie(MBB->instr_end()); i != ie && is_call_free; i++) {
        if (i->isCall() && !TII.isPredicated(i))
          is_call_free = false;
      }

      // simply put all predecessors on the work list -- this may happen only
      // once.
      if (is_call_free != OUTs[MBB]) {
        WL.insert(MBB->pred_begin(), MBB->pred_end());
      }

      OUTs[MBB] = is_call_free;
    }

    /// checkCallFreePaths - Check whether functions have call free paths. In 
    /// the data flow problem, we propagate whether a basic block and some path 
    /// from it to a sink do not contain a call instruction.
    void checkCallFreePaths(const MCallGraph &G)
    {
      const MCGNodes &nodes(G.getNodes());

#ifdef PATMOS_TRACE_CALLFREES
      dbgs() << "///////////////////////////\n";
#endif // PATMOS_TRACE_CALLFREES

      // visit all functions
      for(MCGNodes::const_iterator i(nodes.begin()), ie(nodes.end()); i != ie;
          i++) {

        bool is_call_free = false;

        // ignore unknown functions here
        if (!(*i)->isUnknown()) {
          MBBs WL;
          MBBBool OUTs;
          MachineFunction *MF = (*i)->getMF();

          // initialize work list -- initially we assume
          for(MachineFunction::iterator i(MF->begin()), ie(MF->end()); i != ie;
              i++) {
            WL.insert(i);

            // initially assume the basic block does not contain a call and a
            // path to a sink exists without any calls.
            OUTs[i] = true;
          }

          // process until the work list becomes empty
          while (!WL.empty()) {
            // get some basic block
            MachineBasicBlock *MBB = *WL.begin();
            WL.erase(WL.begin());

            // update the basic block's information, potentially putting any of
            // its predecessors on the work list.
            checkCallFreePaths(WL, OUTs, MBB);
          }

          // see if the entry node contains a call-free path
          is_call_free = OUTs[MF->begin()];
        }

        IsCallFree[*i] = is_call_free;

#ifdef PATMOS_TRACE_CALLFREES
        (*i)->dump();
        dbgs() << ": " << is_call_free << "\n";
#endif // PATMOS_TRACE_CALLFREES
      }
    }

    /// runOnModule - determine the state of the stack cache for each call site.
    virtual bool runOnMachineModule(const Module &M)
    {
      PatmosCallGraphBuilder &PCGB(getAnalysis<PatmosCallGraphBuilder>());
      const MCallGraph &G(*PCGB.getCallGraph());

      // find out whether a call free path exists in each function
      checkCallFreePaths(G);

      // find for each ensure instruction the amount of stack content actually 
      // used after its execution.
      propagateStackUse(G);

      // compute call-graph-level information on maximal stack cache usage
      computeMaxUsage(G);

      // remove useless SENS instructions
      removeEnsures(G);

      return false;
    }

    /// getPassName - Return the pass' name.
    virtual const char *getPassName() const {
      return "Patmos Stack Cache Analysis";
    }
  };

  char PatmosStackCacheAnalysis::ID = 0;
}

/// createPatmosStackCacheAnalysis - Returns a new PatmosStackCacheAnalysis.
ModulePass *llvm::createPatmosStackCacheAnalysis(const PatmosTargetMachine &tm){
  return new PatmosStackCacheAnalysis(tm);
}
