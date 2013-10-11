//==-- PatmosSinglePathInfo.h - Analysis Pass for SP CodeGen -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// This file defines a pass to compute imformation for single-path converting
// seleced functions.
//
//===---------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
#define _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_

#include <Patmos.h>
#include <PatmosTargetMachine.h>
#include <llvm/IR/Module.h>
#include <llvm/ADT/BitVector.h>
#include <llvm/CodeGen/MachineFunctionPass.h>
#include "llvm/CodeGen/MachinePostDominators.h"

#include <vector>
#include <set>
#include <map>


// define for more detailed debugging output
#define PATMOS_SINGLEPATH_TRACE

#ifdef PATMOS_SINGLEPATH_TRACE
#define DEBUG_TRACE(x) DEBUG(x)
#else
#define DEBUG_TRACE(x) /*empty*/
#endif


namespace llvm {

///////////////////////////////////////////////////////////////////////////////

  // forward decl
  class SPScope;
  class SPScopeWalker;
  class PredDefInfo;

  /// PatmosSinglePathInfo - Single-Path analysis pass
  class PatmosSinglePathInfo : public MachineFunctionPass {
    private:
      /// Typedefs for control dependence:
      /// CD: MBB -> set of edges
      /// CD describes, which edges an MBB is control-dependent on.
      typedef std::set<std::pair<const MachineBasicBlock*,
              const MachineBasicBlock*> > CD_map_entry_t;
      typedef std::map<const MachineBasicBlock*, CD_map_entry_t> CD_map_t;

      /// Typedefs for R and K (decomposed CD)
      typedef std::vector<CD_map_entry_t>                  K_t;
      typedef std::map<const MachineBasicBlock*, unsigned> R_t;

      const PatmosTargetMachine &TM;
      const PatmosSubtarget &STC;
      const PatmosInstrInfo *TII;

      /// Set of functions yet to be analyzed
      std::set<std::string> FuncsRemain;

      /// Root SPScope
      SPScope *Root;


      /// Analyze a given MachineFunction
      void analyzeFunction(MachineFunction &MF);

      /// createSPScopeTree - Create an SPScope tree, return the root scope.
      /// The tree needs to be destroyed by the client,
      /// by deleting the root scope.
      SPScope *createSPScopeTree(MachineFunction &MF) const;

      /// computeControlDependence - Compute CD for a given SPScope
      void computeControlDependence(SPScope &S,
                                    MachinePostDominatorTree &PDT,
                                    CD_map_t &CD) const;

      /// decomposeControlDependence - Decompose CD into K and R.
      void decomposeControlDependence(SPScope &S, const CD_map_t &CD,
                                      K_t &K, R_t &R) const;

      /// assignPredInfo - Set the predicate information of an SPScope
      void assignPredInfo(SPScope &S, const K_t &K, const R_t &R) const;

      /// getOrCreateDefInfo - Returns a predicate definition info
      /// for a given MBB.
      PredDefInfo &getOrCreateDefInfo(SPScope &, const MachineBasicBlock *)
                                      const;

    public:
      /// Pass ID
      static char ID;

      /// isEnabled - Return true if there are functions specified to
      /// to be converted to single-path code.
      static bool isEnabled();

      /// isEnabled - Return true if a particular function is specified to
      /// to be converted to single-path code.
      static bool isEnabled(const MachineFunction &MF);

      static bool isConverting(const MachineFunction &MF);

      static bool isRoot(const MachineFunction &MF);

      static bool isReachable(const MachineFunction &MF);

      static bool isMaybe(const MachineFunction &MF);

      /// getRootNames - Fill a set with the names of
      /// single-path root functions
      static void getRootNames(std::set<std::string> &S);

      /// PatmosSinglePathInfo - Constructor
      PatmosSinglePathInfo(const PatmosTargetMachine &tm);

      /// doInitialization - Initialize SinglePathInfo pass
      virtual bool doInitialization(Module &M);

      /// doFinalization - Finalize SinglePathInfo pass
      virtual bool doFinalization(Module &M);

      /// getAnalysisUsage - Specify which passes this pass depends on
      virtual void getAnalysisUsage(AnalysisUsage &AU) const;

      /// runOnMachineFunction - Run the SP converter on the given function.
      virtual bool runOnMachineFunction(MachineFunction &MF);

      /// getPassName - Return the pass' name.
      virtual const char *getPassName() const {
        return "Patmos Single-Path Info";
      }

      /// print - Convert to human readable form
      virtual void print(raw_ostream &OS, const Module* = 0) const;

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
      /// dump - Dump the state of this pass
      virtual void dump() const;
#endif

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

      /// getRootScope - Return the Root SPScope for this function
      SPScope *getRootScope() const { return Root; }

      /// walkRoot - Walk the top-level SPScope
      void walkRoot(SPScopeWalker &walker) const;
  };

///////////////////////////////////////////////////////////////////////////////

/// PredDefInfo - Class containing predicate definition information
/// of one MachineBasicBlock.
/// Instances for MBBs are stored in the PredDefs map.
  class PredDefInfo {
    private:
      BitVector TrueEdge;
      BitVector FalseEdge;
      const MachineBasicBlock *TBB;
      const SmallVector<MachineOperand, 2> Cond;
    public:
      PredDefInfo(unsigned num_preds, const MachineBasicBlock *tbb,
          const SmallVector<MachineOperand, 2> &cond)
        : TBB(tbb), Cond(cond) {
          TrueEdge  = BitVector(num_preds);
          FalseEdge = BitVector(num_preds);
        }
      bool isTrueDest(const MachineBasicBlock *MBBDst) const {
        return MBBDst == TBB;
      }
      void define(unsigned pred, const MachineBasicBlock *MBBDst) {
        BitVector &bv = (MBBDst == TBB) ? TrueEdge : FalseEdge;
        bv.set(pred);
      }
      const BitVector &getTrue() const { return TrueEdge; }
      const BitVector &getFalse() const { return FalseEdge; }
      const BitVector getBoth() const {
        BitVector both(TrueEdge);
        both |= FalseEdge;
        return both;
      }
      const SmallVector<MachineOperand, 2> &getCond() const { return Cond; }
  };

///////////////////////////////////////////////////////////////////////////////

  class SPScope {
    friend class PatmosSinglePathInfo;
    public:
      /// constructor - Create an SPScope with specified parent SP scope or
      /// NULL if top level;
      /// the header/entry MBB; the succ MBB; number of backedges
      /// isRootTopLevel is true when this scope is a top level scope of
      /// a single-path root function.
      explicit SPScope(SPScope *parent, MachineBasicBlock *header,
                      MachineBasicBlock *succ, unsigned numbe,
                      bool isRootTopLevel=false);

      /// destructor - free the child scopes first, cleanup
      ~SPScope();

      /// getParent
      const SPScope *getParent() const { return Parent; }

      /// getHeader
      MachineBasicBlock *getHeader() const { return Blocks.front(); }

      /// getSuccMBB - Get the single successor MBB
      MachineBasicBlock *getSuccMBB() const { return SuccMBB; }

      /// getDepth - Get the nesting depth of the SPScope
      unsigned int getDepth() const { return Depth; }

      /// isTopLevel - Returs true if the SPScope is the top-level SPScope
      /// (not a loop)
      bool isTopLevel() const { return (NULL == Parent); }
      //
      /// isRootTopLevel - Returs true if the SPScope is the top-level SPScope
      /// of a single-path root function
      bool isRootTopLevel() const { return RootTopLevel; }

      /// isHeader - Returns true if the specified MBB is a member of this
      /// SPScope, (non-recursively)
      bool isHeader(const MachineBasicBlock *MBB) const;

      /// isMember - Returns true if the specified MBB is a member of this
      /// SPScope, (non-recursively)
      bool isMember(const MachineBasicBlock *MBB) const;

      /// isSubHeader - Returns true if the specified MBB is header of a
      /// subscope of this scope
      bool isSubHeader(MachineBasicBlock *MBB) const;

      /// hasLoopBound - Returs true if the SPScope is a loop and has a bound
      /// to be accounted for
      bool hasLoopBound() const { return (-1 != LoopBound); }

      /// getLoopBound - Return the loop bound for this SPScope
      int getLoopBound() const { return LoopBound; }

      /// walk - Walk this SPScope recursively
      void walk(SPScopeWalker &walker);

      /// getNumPredicates - Returns the number of predicates required for
      /// this function
      unsigned getNumPredicates() const { return PredCount; }

      /// getPredUse - Returns the guarding predicate for an MBB
      int getPredUse(const MachineBasicBlock *) const;

      /// getDefInfo - Returns a pointer to a predicate definition info for
      /// a given MBB, or NULL if no pred info exists for the MBB.
      const PredDefInfo *getDefInfo( const MachineBasicBlock *) const;

      /// getNumDefEdges - Returns the number of definition edges for a given
      /// predicate.
      unsigned getNumDefEdges(unsigned pred) const {
        return NumPredDefEdges.at(pred);
      }

      /// getBlocks - Returns the list of basic blocks in this SPScope,
      /// in topological order.
      const std::vector<MachineBasicBlock*> &getBlocks() const {
        return Blocks;
      }

      // dump() - Dump state of this SP scope and the subtree
      void dump() const;

      /// iterator - Type for iterator through MBBs
      typedef std::vector<MachineBasicBlock*>::iterator iterator;

      /// child_iterator - Type for child iterator
      typedef std::vector<SPScope*>::iterator child_iterator;

    private:
      // parent SPScope
      SPScope *Parent;

      // successor MBB
      MachineBasicBlock *SuccMBB;

      // number of backedges
      const unsigned NumBackedges;

      // flag that only is set to true if the scope is a top-level scope
      // of a single-path root function
      const bool RootTopLevel;

      // loop bound
      int LoopBound;

      // children as map: header MBB -> SPScope
      std::map<MachineBasicBlock*, SPScope*> HeaderMap;

      // MBBs contained
      std::vector<MachineBasicBlock*> Blocks;

      // sub-scopes
      std::vector<SPScope*> Children;

      // nesting depth
      unsigned int Depth;

      /// Number of predicates used
      unsigned PredCount;

      /// Map MBBs to predicate they use
      std::map<const MachineBasicBlock*, unsigned> PredUse;

      /// PredDefs - Stores predicate define information for each basic block
      std::map<const MachineBasicBlock*, PredDefInfo> PredDefs;

      // number of defining edges for each predicate
      std::vector<unsigned> NumPredDefEdges;

      /// addMBB - Add an MBB to the SP scope
      void addMBB(MachineBasicBlock *MBB);

      /// topoSort - sort blocks of this SPScope topologically
      void topoSort(void);

    public:
      /// begin - Iterator begin for MBBs
      iterator begin() { return Blocks.begin(); }

      /// child_end - Iterator end for MBBs
      iterator end() { return Blocks.end(); }

      /// child_begin - Iterator begin for subloops
      child_iterator child_begin() { return Children.begin(); }

      /// child_end - Iterator end for subloops
      child_iterator child_end() { return Children.end(); }
  };

///////////////////////////////////////////////////////////////////////////////

  class SPScopeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubscope(SPScope *) = 0;
      virtual void exitSubscope(SPScope *) = 0;
      virtual ~SPScopeWalker() {};
  };

///////////////////////////////////////////////////////////////////////////////

  // Allow clients to walk the list of nested SPScopes
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

} // end of namespace llvm

#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
