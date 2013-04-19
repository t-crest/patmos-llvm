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
#include <llvm/Module.h>
#include <llvm/ADT/BitVector.h>
#include <llvm/CodeGen/MachineFunctionPass.h>

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
  class SPNode;
  class SPNodeWalker;

  /// PatmosSinglePathInfo - Single-Path analysis pass
  class PatmosSinglePathInfo : public MachineFunctionPass {
    public:
      // forward decl
      class PredDefInfo;
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


      ////////// state of the pass

      /// Number of predicates used
      unsigned PredCount;

      /// Map MBBs to predicate they use
      std::map<const MachineBasicBlock*, unsigned> PredUse;

      /// PredDefs - Stores predicate define information for each basic block
      std::map<const MachineBasicBlock*, PredDefInfo> PredDefs;

      /// Specifies, which predicates are true at the entry edge
      BitVector PredEntryEdge;

      /// Root SPNode
      SPNode *Root;



      /// Analyze a given MachineFunction
      void analyzeFunction(MachineFunction &MF);

      /// createSPNodeTree - Create an SPNode tree, return the root node.
      /// The tree needs to be destroyed by the client,
      /// by deleting the root node.
      SPNode *createSPNodeTree(MachineFunction &MF) const;

      /// computeControlDependence - Compute CD for a given function.
      void computeControlDependence(MachineFunction &MF, CD_map_t &CD) const;

      /// decomposeControlDependence - Decompose CD into K and R.
      void decomposeControlDependence(MachineFunction &MF, const CD_map_t &CD,
                                      K_t &K, R_t &R) const;

      /// collectPredDefs - Create predicate definition information for
      /// affected MBBs
      void collectPredDefs(MachineFunction &MF, const K_t &K);

      /// getOrCreateDefInfo - Returns a predicate definition info
      /// for a given MBB.
      PredDefInfo &getOrCreateDefInfo(const MachineBasicBlock *);

    public:
      /// Pass ID
      static char ID;

      /// isEnabled - Return true if there are functions specified to
      /// to be converted to single-path code.
      static bool isEnabled();

      /// isEnabled - Return true if a particular function is specified to
      /// to be converted to single-path code.
      static bool isEnabled(MachineFunction &MF);

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

      ////////////////////////////////////
      // accessing main analysis results:
      ////////////////////////////////////

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

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

      /// getNumPredicates - Returns the number of predicates required for
      /// this function
      unsigned getNumPredicates() const { return PredCount; }

      /// getNumPredicates - Returns the number of predicates required for
      /// a particular SPNode
      unsigned getNumPredicates(const SPNode *N) const;

      /// getPredUse - Returns the guarding predicate for an MBB
      int getPredUse(const MachineBasicBlock *) const;

      /// getDefInfo - Returns a pointer to a predicate definition info for
      /// a given MBB, or NULL if no pred info exists for the MBB.
      const PredDefInfo *getDefInfo(const MachineBasicBlock *) const;

      /// getPredEntryEdge - Returns a bitvector for preds true on entry edge
      BitVector getPredEntryEdge() const { return PredEntryEdge; }

      /// getInitializees - Return the predicates that need to be initialized
      /// for a given SPNode
      BitVector getInitializees(const SPNode *N) const;

      /// getRootNode - Return the Root SPNode for this function
      SPNode *getRootNode() const { return Root; }

      /// walkRoot - Walk the top-level SPNode
      void walkRoot(SPNodeWalker &walker) const;
  };

///////////////////////////////////////////////////////////////////////////////

  class SPNode {
    friend class PatmosSinglePathInfo;
    public:
      /// constructor - Create an SPNode with specified parent SP node or NULL
      /// if top level; the header/entry MBB; the succ MBB; number of backedges
      explicit SPNode(SPNode *parent, MachineBasicBlock *header,
                      MachineBasicBlock *succ, unsigned numbe);

      /// destructor - free the child nodes first, cleanup
      ~SPNode();

      /// addMBB - Add an MBB to the SP node
      void addMBB(MachineBasicBlock *MBB);

      /// getParent
      const SPNode *getParent() const { return Parent; }

      /// getHeader
      MachineBasicBlock *getHeader() const { return Blocks.front(); }

      /// getSuccMBB - Get the single successor MBB
      MachineBasicBlock *getSuccMBB() const { return SuccMBB; }

      /// getDepth - Get the nesting depth of the SPNode
      unsigned int getDepth() const { return Depth; }

      /// isTopLevel - Returs true if the SPNode is the top-level SPNode
      /// (not a loop)
      bool isTopLevel() const { return (NULL == Parent); }

      /// hasLoopBound - Returs true if the SPNode is a loop and has a bound
      /// to be accounted for
      bool hasLoopBound() const { return (-1 != LoopBound); }

      /// getLoopBound - Return the loop bound for this SPNode
      int getLoopBound() const { return LoopBound; }

      /// walk - Walk this SPNode recursively
      void walk(SPNodeWalker &walker);

      // dump() - Dump state of this SP node and the subtree
      void dump() const;

      /// child_iterator - Type for child iterator
      typedef std::vector<SPNode*>::iterator child_iterator;

    private:
      // parent SPNode
      SPNode *Parent;

      // successor MBB
      MachineBasicBlock *SuccMBB;

      // number of backedges
      const unsigned NumBackedges;

      // loop bound
      int LoopBound;

      // children as map: header MBB -> SPNode
      std::map<MachineBasicBlock*, SPNode*> HeaderMap;

      // MBBs contained
      std::vector<MachineBasicBlock*> Blocks;

      // sub-nodes
      std::vector<SPNode*> Children;

      // nesting depth
      unsigned int Depth;

    public:
      /// child_begin - Iterator begin for subloops
      child_iterator child_begin() { return Children.begin(); }

      /// child_end - Iterator end for subloops
      child_iterator child_end() { return Children.end(); }
  };

///////////////////////////////////////////////////////////////////////////////

  class SPNodeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubnode(SPNode *) = 0;
      virtual void exitSubnode(SPNode *) = 0;
  };

///////////////////////////////////////////////////////////////////////////////

  // Allow clients to walk the list of nested SPNodes
  template <> struct GraphTraits<SPNode*> {
    typedef SPNode NodeType;
    typedef SPNode::child_iterator ChildIteratorType;

    static NodeType *getEntryNode(SPNode *N) { return N; }
    static inline ChildIteratorType child_begin(NodeType *N) {
      return N->child_begin();
    }
    static inline ChildIteratorType child_end(NodeType *N) {
      return N->child_end();
    }
  };

} // end of namespace llvm

#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
