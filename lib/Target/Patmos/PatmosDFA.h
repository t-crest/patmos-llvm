//===-- PatmosDFA.h - Infrastructure for basic data-flow analyses. --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Infrastructure to solve data-flow analysis problems on the machine-code
// DFAlevel.
//
// TODO: the whole framework assumes that basic blocks are never empty.
// TODO: better way to initialize domain values
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOSDFA_H_
#define _LLVM_TARGET_PATMOSDFA_H_

// #define TRACE_MERGE
// #define TRACE_SUCCS
// #define TRACE_WORKLIST

#include "PatmosCallGraphBuilder.h"

#include "llvm/Support/Debug.h"

#include <map>
#include <set>
#include <vector>

using namespace llvm;

namespace llvm {
  /// Control direction of data-flow analysis.
  enum PatmosDFADirection
  {
    DFA_FORWARD,
    DFA_BACKWARD
  };

  /// Control the level of the data-flow analysis information retained.
  enum PatmosDFALevel
  {
    /// Retain information for each instruction.
    DFA_INSTR,
    /// Retain information for each basic block.
    DFA_BLOCK
  };

  /// A context is a sequence of call sites from the machine-level call graph
  typedef std::vector<const MCGSite*> PatmosDFAContext;

  /// A vector of contexts.
  typedef std::vector<const PatmosDFAContext*> PatmosDFAContexts;

  /// A set of contexts.
  typedef std::set<PatmosDFAContext> PatmosDFAContextSet;

  /// Print a context to the debug stream.
  void dump(const PatmosDFAContext *C);

  /// Class to manage contexts for inter-procedural data-flow analysis.
  class PatmosDFAContextProvider
  {
  private:
    /// A mapping of contexts to their calling contexts.
    typedef std::map<const PatmosDFAContext*,
                     PatmosDFAContexts> CallingContextMap;

    /// Maximum depth of the call string.
    const unsigned int MaxDepth;

    /// A set of known contexts.
    PatmosDFAContextSet KnownContexts;

    /// Keep track of
    CallingContextMap CallingContexts;

  public:
    explicit PatmosDFAContextProvider(unsigned int depth);

    /// Create a default context.
    const PatmosDFAContext *getDefaultContext() {
      PatmosDFAContext tmp(MaxDepth);
      return &*KnownContexts.insert(tmp).first;
    }

    const PatmosDFAContext *getDefaultContext() const {
      PatmosDFAContext tmp(MaxDepth);
      PatmosDFAContextSet::const_iterator i(KnownContexts.find(tmp));
      assert(i != KnownContexts.end());
      return &*i;
    }

    /// append - Append a new call site to a context.
    const PatmosDFAContext *append(const PatmosDFAContext *C,
                                   const MCGSite *S) {
      // reserve space for a new context
      PatmosDFAContext tmp;

      if (MaxDepth == 0) {
        // just return the default context when context-depth is 0
        if (CallingContexts[C].empty())
          CallingContexts[C].push_back(C);
        return getDefaultContext();
      }


      if (C->front()) {
        // maximum length reached?
        tmp.insert(tmp.begin(), C->begin(), --C->end());
      }
      else {
        // still space left?
        tmp.insert(tmp.begin(), ++C->begin(), C->end());
      }

      tmp.push_back(S);
      assert(tmp.size() == MaxDepth);

      // make a unique context reference
      const PatmosDFAContext &result(*KnownContexts.insert(tmp).first);

      // remember calling contexts
      if (std::find(CallingContexts[&result].begin(),
                    CallingContexts[&result].end(),
                    C) == CallingContexts[&result].end())
        CallingContexts[&result].push_back(C);

      // create the respective context from it.
      return &result;
    }

    /// Check if the context is the empty default context.
    bool IsDefaultContext(const PatmosDFAContext *C) const {
      for(PatmosDFAContext::const_iterator i(C->begin()), ie(C->end());
          i != ie; i++) {
        if (*i != NULL)
          return false;
      }

      return true;
    }

    /// Get contexts potentially leading to the current context. That is, find
    /// all contexts whose tail matches of length MaxDepth-1 matches the prefix
    /// of the current head of length MaxDepth-1
    const PatmosDFAContexts &getCallingContexts(const PatmosDFAContext *C) {
      return CallingContexts[C];
    }

    /// Return the known contexts.
    const PatmosDFAContextSet &getContexts() const {
      return KnownContexts;
    }
  };

  /// Location in the program associated with DFA information.
  typedef std::pair<const PatmosDFAContext*,
                    const MachineInstr *> PatmosDFALocation;

  /// print a location potentially visited during data-flow analysis.
  void dump(const PatmosDFALocation &L);

  /// Return whether the instruction of the location matches the given IDs.
  bool isLocation(const PatmosDFALocation &L, unsigned int MFID,
                  unsigned int MBBID, unsigned int MIID);

  /// Base class for defining data-flow analyses.
  /// Argument DOM:
  ///   - Domain of the analysis
  ///   - Objects of DOM represent abstract values
  ///   - DOM provides static transfer functions for machine instructions
  template<class DOM, class DOMCTX, PatmosDFADirection DIR = DFA_FORWARD,
           PatmosDFALevel LEVEL = DFA_BLOCK>
  class PatmosDFA
  {
  public:
    /// An abstract states.
    typedef std::map<PatmosDFALocation, DOM> DFAStates;
  private:
    /// Set of locations.
    typedef std::set<PatmosDFALocation> DFALocations;

    /// List of machine instructions.
    typedef std::vector<const MachineInstr*> MIs;

    // The target machine of the Patmos processor.
    DOMCTX &AnalysisContext;

    /// The underlying machine-level call graph
    PatmosCallGraphBuilder *PCGB;

    /// Keep track where we are.
    DFALocations WL;

    /// Associate a location with the information after/before
    /// (FORWARD/BACKWORD) a given location.
    DFAStates OUTs;

    /// Manage contexts for context-sensitive analyses.
    PatmosDFAContextProvider Contexts;

    /// Check if the analysis is interprocedural or not.
    bool isInterprocedural() const {
      return PCGB != NULL;
    }

    static const MachineInstr *getNext(const MachineInstr *MI) {
      MachineBasicBlock::const_instr_iterator I(MI);
      if (++I == MI->getParent()->instr_end())
        return NULL;

      return I;
    }

    static const MachineInstr *getPrev(const MachineInstr *MI) {
      MachineBasicBlock::const_instr_iterator I(MI);
      if (I == MI->getParent()->instr_begin())
        return NULL;

      return --I;
    }

    static const MachineInstr *getFirst(const MachineFunction *MF) {
      assert(!MF->empty() && !MF->front().empty());
      return &MF->front().front();
    }

    static const MachineInstr *getFirst(const MachineBasicBlock *MBB) {
      assert(!MBB->empty());
      return &MBB->front();
    }

    static const MachineInstr *getLast(const MachineBasicBlock *MBB) {
      assert(!MBB->empty());
      return &MBB->back();
    }

    /// Find immediate successors of contexts representing UNKNOWN call graph
    /// nodes.
    void addUnknownSuccessorLocations(const PatmosDFAContext *C, MCGNode *N,
                                      DFALocations &LOCs) {
      assert(N->isUnknown());

      for(MCGSites::const_iterator i(N->getSites().begin()), 
          ie(N->getSites().end()); i != ie; i++) {

        // an UNKNOWN node should never call another unknown node.
        MCGNode *callee = (*i)->getCallee();
        assert(!callee->isUnknown());

        // make a new context for the callee
        const PatmosDFAContext *P = Contexts.append(C, *i);

        // get first instruction of the called machine function and create a new
        // location
        LOCs.insert(PatmosDFALocation(P, getFirst(callee->getMF())));
      }
    }

    /// Find the locations succeeding a return when no context information is
    /// available.
    void addReturnSuccessorLocations(const MCGNode *N,
                                     const PatmosDFAContext *C,
                                     DFALocations &LOCs) {
      for(MCGSites::const_iterator i(N->getCallingSites().begin()),
          ie(N->getCallingSites().end()); i != ie; i++) {
        MCGNode *caller = (*i)->getCaller();

        if (caller->isUnknown()) {
          const PatmosDFAContexts &PCTXs(Contexts.getCallingContexts(C));
          for(PatmosDFAContexts::const_iterator j(PCTXs.begin()),
              je(PCTXs.end()); j != je; j++) {
            addReturnSuccessorLocations(caller, *j, LOCs);
          }
        }
        else {
          // create a location
          PatmosDFALocation tmp(C, (*i)->getMI());
          assert((*i)->getMI());

          // see if analysis information is available for the location. if not,
          // the call site has not been visited yet -- and might never be. so,
          // just skip it for now.
          if (OUTs.find(tmp) != OUTs.end()) {
            addImmSuccessorLocations(tmp, LOCs);
          }
        }
      }
    }

    /// Find the immediate successors of a location, not following call edges,
    /// but maybe following return edges on the call graph.
    void addImmSuccessorLocations(const PatmosDFALocation &LOC,
                                  DFALocations &LOCs) {
      const PatmosDFAContext *C(LOC.first);
      const MachineInstr *MI = LOC.second;
      assert(MI);
      const MachineBasicBlock *MBB = MI->getParent();
      const MachineFunction *MF = MBB->getParent();

      const MachineInstr *NMI = getNext(MI);

      // find the immediate successor of the location.
      if (NMI) {
        // an instruction in the middle of the basic block.
        LOCs.insert(PatmosDFALocation(C, NMI));
      }
      else {
        if (!MBB->succ_empty()) {
          // an instruction at the end of a basic block that has successors.
          for(MachineBasicBlock::const_succ_iterator i(MBB->succ_begin()),
              ie(MBB->succ_end()); i != ie; i++) {
            LOCs.insert(PatmosDFALocation(C, getFirst(*i)));
          }
        }
        // TODO: this might not hold when we have predicated returns.
        else if (isInterprocedural()) {
          // an instruction at the end of a basic block that has no successor.
          const PatmosDFAContexts &PCTXs(Contexts.getCallingContexts(C));
          for(PatmosDFAContexts::const_iterator i(PCTXs.begin()),
              ie(PCTXs.end()); i != ie; i++) {
            if (C->empty() || !C->back()->getMI()) {
              // ok, no context information available. so, the call site cannot
              // be extracted from the context.
              const MCGNode *N = PCGB->getNode(MF);
              assert(N);

              addReturnSuccessorLocations(N, *i, LOCs);
            }
            else {
              // only one call site possible
              addImmSuccessorLocations(PatmosDFALocation(*i,
                                                         C->back()->getMI()),
                                       LOCs);
            }
          }
        }
      }
    }

    /// Find the locations preceding a function entry when no context
    /// information is available.
    void addEntryPredecessorLocations(const MCGNode *N,
                                      const PatmosDFAContext *C,
                                      DFALocations &LOCs) {
      for(MCGSites::const_iterator i(N->getCallingSites().begin()),
          ie(N->getCallingSites().end()); i != ie; i++) {

        MCGNode *caller = (*i)->getCaller();

        if (caller->isUnknown()) {
          // ok, the caller is an UNKNOWN node, i.e., a virtual node in the call
          // graph. No analysis information can be associated with it. so,
          // recurs to find a caller that is an actual function.
          const PatmosDFAContexts &PCTXs(Contexts.getCallingContexts(C));

          for(PatmosDFAContexts::const_iterator j(PCTXs.begin()),
              je(PCTXs.end()); j != je; j++) {
            addEntryPredecessorLocations(caller, *j, LOCs);
          }
        }
        else {
          // create a location
          PatmosDFALocation tmp(C, (*i)->getMI());
          assert((*i)->getMI());

          // see if analysis information is available for the location. if not,
          // the call site has not been visited yet -- and might never be. so,
          // just skip it for now.
          if (OUTs.find(tmp) != OUTs.end())
            LOCs.insert(tmp);
        }
      }
    }

    /// Find locations representing returns from a given function in a given
    /// context.
    static void addReturnLocations(const MachineFunction *MF,
                                   const PatmosDFAContext *C,
                                   DFALocations &LOCs) {
      for(MachineFunction::const_iterator i(MF->begin()), ie(MF->end());
          i != ie; i++) {
        if (i->succ_empty()) {
          assert(!i->empty());
          LOCs.insert(PatmosDFALocation(C, &i->back()));
        }
      }
    }

    /// Find the predecessors of a location representing a call, following
    /// return edges of the call graph.
    void addReturnPredecessorLocations(const PatmosDFAContext *C,
                                       const MachineInstr *MI,
                                       DFALocations &preds,
                                       DFALocations &immPreds) {
      // not a call? only add the location itself
      if (!MI->isCall() || !isInterprocedural())
        preds.insert(PatmosDFALocation(C, MI));
      else {
        immPreds.insert(PatmosDFALocation(C, MI));

        // get call sites associated with the instruction
        MCGSites CSs(PCGB->getSites(MI));
        assert(!CSs.empty());

        // create new contexts for each call site
        for(MCGSites::const_iterator i(CSs.begin()), ie(CSs.end()); i != ie;
            i++) {
          MCGNode *callee = (*i)->getCallee();

          // make a new context
          const PatmosDFAContext *P(Contexts.append(C, *i));

          // special handling for UNKNWON functions, basically skip them and
          // continue with the called functions below them.
          if (callee->isUnknown()) {
            for(MCGSites::const_iterator j(callee->getSites().begin()),
                je(callee->getSites().end()); j != je; j++) {
              addReturnLocations((*j)->getCallee()->getMF(),
                                 Contexts.append(P, *j), preds);
            }
          }
          else
            addReturnLocations(callee->getMF(), P, preds);
        }
      }
    }

    /// Find the locations (immediately) preceding a location (depending on the
    /// direction of the data-flow analysis).
    void getPredecessors(const PatmosDFALocation &LOC, DFALocations &preds,
                         DFALocations &immPreds) {
      // clear results
      preds.clear();
      immPreds.clear();

      switch(DIR) {
        case DFA_FORWARD:
          getFWPredecessors(LOC, preds, immPreds);
          break;
        case DFA_BACKWARD:
          getBWPredecessors(LOC, preds, immPreds);
          break;
      }
    }

    /// Find the locations (immediately) preceding a location for forward
    /// analyses.
    void getFWPredecessors(const PatmosDFALocation &LOC, DFALocations &preds,
                           DFALocations &immPreds) {
      const MachineInstr *MI = LOC.second;
      assert(MI);

      const MachineBasicBlock *MBB = MI->getParent();
      const MachineFunction *MF = MBB->getParent();

      const MachineInstr *PMI = getPrev(MI);

      // find the immediate predecessors of the location.
      if (PMI) {
        // an instruction in the middle of the basic block.
        addReturnPredecessorLocations(LOC.first, PMI, preds, immPreds);
      }
      else {
        if (!MBB->pred_empty()) {
          // an instruction at the beginning of a basic block that has
          // predecessors.
          for(MachineBasicBlock::const_pred_iterator i(MBB->pred_begin()),
              ie(MBB->pred_end()); i != ie; i++) {
            addReturnPredecessorLocations(LOC.first, getLast(*i), preds,
                                          immPreds);
          }
        }

        if (MBB == MF->begin() && isInterprocedural()) {
          // an instruction at the beginning of a basic block that happens to be
          // the entry of a function (might have other predecessors too).
          const PatmosDFAContext *C(LOC.first);
          const PatmosDFAContexts &PCTXs(Contexts.getCallingContexts(C));
          for(PatmosDFAContexts::const_iterator i(PCTXs.begin()),
              ie(PCTXs.end()); i != ie; i++) {
            if (C->empty() || !C->back() || !C->back()->getMI()) {
              // ok, context depth is 0 or the calling context has no machine
              // instruction (call sites from UNKNOWN nodes). We cannot extract
              // the call site from the context. Instead, check all call sites
              // leading to the current function.
              const MCGNode *N = PCGB->getNode(MF);
              assert(N);

              addEntryPredecessorLocations(N, *i, preds);
            }
            else {
              // just one call site per calling context.
              preds.insert(PatmosDFALocation(*i, C->back()->getMI()));
            }
          }
        }
      }
    }

    /// Find the locations (immediately) preceding a location for backward
    /// analyses.
    void getBWPredecessors(const PatmosDFALocation &LOC, DFALocations &preds,
                           DFALocations &immPreds) {
      // TODO: implement
      llvm_unreachable("Implement getBWPredecessors");
    }

    /// Find the locations (immediately) succeeding a location (depending on the
    /// direction of the data-flow analysis).
    void getSuccessors(const PatmosDFALocation &LOC, DFALocations &succs,
                       DFALocations &immSuccs) {
      // clear return values
      succs.clear();
      immSuccs.clear();

      switch(DIR) {
        case DFA_FORWARD:
          getFWSuccessors(LOC, succs, immSuccs);
          break;
        case DFA_BACKWARD:
          getBWSuccessors(LOC, succs, immSuccs);
          break;
      }
    }

    /// Find the locations (immediately) succeeding a location for forward
    /// analyses.
    void getFWSuccessors(const PatmosDFALocation &LOC, DFALocations &succs,
                         DFALocations &immSuccs) {
      const MachineInstr *MI = LOC.second;
      assert(MI);

      // find the immediate successor of the location.
      if (MI->isCall() && isInterprocedural()) {
        // get call sites associated with the instruction
        MCGSites CSs(PCGB->getSites(MI));
        assert(!CSs.empty());

        // create new contexts for each call site
        for(MCGSites::const_iterator i(CSs.begin()), ie(CSs.end()); i != ie;
            i++) {
          MCGNode *callee = (*i)->getCallee();
          if (callee->isUnknown()) {
            // The callee is an UNKNOWN node, i.e., a virtual node of the call
            // graph. No analysis information can be associated with it, so just
            // skip it and move on to its children.
            addUnknownSuccessorLocations(Contexts.append(LOC.first, *i), callee,
                                         succs);
          }
          else {
            // make new context
            const PatmosDFAContext *C(Contexts.append(LOC.first, *i));

            // find first instruction in the called function and add location
            succs.insert(PatmosDFALocation(C, getFirst(callee->getMF())));
          }
        }

        // when no context information is available, we compute the successors
        // of returns on demand -- only considering those calling contexts where
        // the call was analyzed already.
        //
        // This might lead to a situation where instructions immediately
        // following calls are not analyzed. for instance, if two call sites
        // lead to a function A. The first time A is analyzed it will only
        // propagate its output to the respective calling site (but not the
        // other). Now when the second call site is analyzed and the analysis
        // information within A does not change, the successors of the return
        // are not revisited. Leading to the problem.
        //
        // Thus simply add the immediate successors of the call here.
        addImmSuccessorLocations(LOC, immSuccs);
      }
      else
        addImmSuccessorLocations(LOC, succs);
    }

    /// Find the locations (immediately) succeeding a location for backward
    /// analyses.
    void getBWSuccessors(const PatmosDFALocation &LOC, DFALocations &succs,
                         DFALocations &immSuccs) {
      // TODO: implement
      llvm_unreachable("Implement getBWSuccessors");
    }

    // Check if the current location can be skipped, i.e., whether its analysis
    // information should be retained (depending on analysis direction).
    //
    // If the location can be skipped, return the immediate successor location.
    bool canSkipLocation(const MachineInstr *&MI) {
      switch(LEVEL) {
        case DFA_INSTR:
          // all information should be retained, so never skip
          return NULL; 
        case DFA_BLOCK:
          switch(DIR) {
            case DFA_FORWARD:
              return canSkipLocationFW(MI);
            case DFA_BACKWARD:
              return canSkipLocationBW(MI);
          }
      }

      llvm_unreachable("");
    }

    // Check if the current location can be skipped, i.e., whether its analysis
    // information should be retained, for forward analyses.
    //
    // If the location can be skipped, return the immediate successor location.
    bool canSkipLocationFW(const MachineInstr *&MI) {
      const MachineInstr *NMI = getNext(MI);

      // do not skip call sites for interprocedural analyses.
      if (MI->isCall() && isInterprocedural())
        return false;
      else if (NMI) {
        // otherwise skip machine instructions until the end of the basic block
        MI = NMI;
        return true;
      }

      return false;
    }

    // Check if the current location can be skipped, i.e., whether its analysis
    // information should be retained, for backward analyses.
    //
    // If the location can be skipped, return the immediate successor location.
    bool canSkipLocationBW(const MachineInstr *&MI) {
      // skip instructions until the beginning of the basic block is reached,
      // exception ...
      const MachineInstr *PMI = getPrev(MI);

      // ... never skip calls for interprocedural analyses.
      if (PMI->isCall() && isInterprocedural()) {
        return false;
      }
      else if (PMI) {
        MI = PMI;
        return true;
      }

      return false;
    }

    DOM getFWResultBefore(const PatmosDFALocation &LOC) {
      // default value of domain
      DOM D;

      // find preceding locations.
      DFALocations preds;
      DFALocations immPreds;
      getPredecessors(LOC, preds, immPreds);

      // merge OUT information of predecessors.
      for(typename DFALocations::const_iterator i(preds.begin()),
          ie(preds.end()); i != ie; i++) {

        typename DFAStates::const_iterator j(OUTs.find(*i));

        // is the predecessor's information available?
        if (j == OUTs.end()) {
          // if not, we are in the middle of a basic block, back-trace in order
          // to find a preceding call site or the basic block's beginning.
          assert(preds.size() == 1 &&
                 i->second->getParent() == LOC.second->getParent());

          DOM P(getFWResultBefore(*i));
          P.transform(*i, AnalysisContext);

          D.merge(P, *i, LOC, AnalysisContext);
        }
        else {
          // merge the values of predecessors
          D.merge(j->second, *i, LOC, AnalysisContext);
        }
      }

      return D;
    }

    DOM getBWResultBefore(const PatmosDFALocation &LOC) {
      llvm_unreachable("implement getBWResultBefore");
    }

    // Update data-flow equations for a given location (update OUTs and WL).
    void update(const PatmosDFALocation &LOC) {
      // Defaul-value of the domain
      DOM D(LOC, AnalysisContext);

      // find preceding locations.
      DFALocations preds;
      DFALocations immPreds;
      getPredecessors(LOC, preds, immPreds);

      // merge OUT information of predecessors.
      for(typename DFALocations::const_iterator i(preds.begin()),
          ie(preds.end()); i != ie; i++) {

        if (OUTs.find(*i) != OUTs.end())
          D.merge(OUTs.at(*i), *i, LOC, AnalysisContext);

#ifdef TRACE_MERGE
        dbgs() << "  merge:";
        dump(*i);
        dbgs() << ": ";
        OUTs[*i].dump();
        dbgs() << " => ";
        D.dump();
        dbgs() << "\n";
#endif // TRACE_MERGE
      }

      // merge OUT information of predecessors.
      for(typename DFALocations::const_iterator i(immPreds.begin()),
          ie(immPreds.end()); i != ie; i++) {

        if (OUTs.find(*i) != OUTs.end())
          D.mergeImm(OUTs.at(*i), *i, LOC, AnalysisContext);

#ifdef TRACE_MERGE
        dbgs() << "  mergeImm:";
        dump(*i);
        dbgs() << ": ";
        OUTs[*i].dump();
        dbgs() << " => ";
        D.dump();
        dbgs() << "\n";
#endif // TRACE_MERGE
      }

      // apply transfer function
      D.transform(LOC, AnalysisContext);

#ifdef TRACE_MERGE
      dbgs() << "  transfer:";
      dump(LOC);
      dbgs() << " => ";
      D.dump();
      dbgs() << "\n";
#endif // TRACE_MERGE

      const MachineInstr *MI = LOC.second;
      assert(MI);

      // apply transfer functions of immediate successors until a location is
      // hit whose analysis information needs to be retained.
      while ((canSkipLocation(MI))) {
        PatmosDFALocation tmp(LOC.first, MI);
        D.transform(tmp, AnalysisContext);

#ifdef TRACE_MERGE
        dbgs() << "  transfer:";
        dump(tmp);
        dbgs() << " => ";
        D.dump();
        dbgs() << "\n";
#endif // TRACE_MERGE

      }

      // possibly new location, whose analysis information needs to be retained.
      PatmosDFALocation newLOC(LOC.first, MI);
      assert(MI);

      // see if the analysis information of the location changed:

      if (OUTs.find(newLOC) == OUTs.end() || D != OUTs.at(newLOC)) {
        // update the analysis information of the location
        OUTs.insert(std::make_pair(newLOC, D));

        // update the work list
        DFALocations succs;
        DFALocations immSuccs;
        getSuccessors(newLOC, succs, immSuccs);
        WL.insert(succs.begin(), succs.end());
        WL.insert(immSuccs.begin(), immSuccs.end());

#ifdef TRACE_SUCCS
        for(typename DFALocations::const_iterator i(succs.begin()),
            ie(succs.end()); i != ie; i++) {
          dbgs() << "  succ:";
          dump(*i);
          dbgs() << "\n";
        }
#endif // TRACE_SUCCS

#ifdef TRACE_WORKLIST
        dbgs() << "  OUT:";
        llvm::dump(newLOC);
        dbgs() << " => ";
        D.dump();
        dbgs() << "\n";
#endif // TRACE_WORKLIST
      }
    }

    /// Initialize the data-flow analysis solver (work list, contexts, ...)
    void initialize(const MachineFunction *MF) {
      // create initial context
      const PatmosDFAContext *C = Contexts.getDefaultContext();

      // Initialize the work list
      switch (DIR) {
        case DFA_BACKWARD:
        {
          // find all instructions returning from the machine function
          for(MachineFunction::const_iterator i(MF->begin()), ie(MF->end());
              i != ie; i++) {
            if (i->succ_empty())
              WL.insert(PatmosDFALocation(C, getLast(i)));
          }
        }
        case DFA_FORWARD:
          // find first instruction of the function
          WL.insert(PatmosDFALocation(C, getFirst(MF)));
      }
    }
  public:
    PatmosDFA(DOMCTX &analysis_context, unsigned int context_depth = 0,
              PatmosCallGraphBuilder *pcgb = NULL) :
        AnalysisContext(analysis_context), PCGB(pcgb), Contexts(context_depth) {
    }

    /// Return the states computed by the analysis results.
    const DFAStates &getResults() const {
      return OUTs;
    }

    /// Retrieve the analysis result right before the instruction at the given
    /// location.
    DOM getResultBefore(const PatmosDFALocation &LOC) {
      switch(DIR) {
        case DFA_FORWARD:
          return getFWResultBefore(LOC);
        case DFA_BACKWARD:
          return getBWResultBefore(LOC);
      }
    }

    /// Return the contexts provider used by the analysis.
    const PatmosDFAContextProvider &getContexts() const {
      return Contexts;
    }

    /// Solve a data-flow analysis problem using iterative fixed-point search.
    void solve(const MachineFunction *MF) {
      // initialize data´flow analysis solver.
      initialize(MF);

      // process until the work list becomes empty
      while (!WL.empty()) {
        // get some location.
        PatmosDFALocation LOC = *WL.begin();
        WL.erase(WL.begin());

        assert(LOC.first && LOC.second);

#ifdef TRACE_WORKLIST
        dump(LOC);
        dbgs() << ": ";
        OUTs[LOC].dump();
        dbgs() << "\n";
#endif // TRACE_WORKLIST

        // update the analysis information for that location, potentially adding
        // locations to the work list.
        update(LOC);
      }
    }
  };
}

#endif // _LLVM_TARGET_PATMOSDFA_H_