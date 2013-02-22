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

      /// Typedef for MBB -> BitVector map
      typedef std::map<const MachineBasicBlock*, BitVector> MBB_BV_map_t;

      const PatmosTargetMachine &TM;
      const PatmosSubtarget &STC;
      const PatmosInstrInfo *TII;

      /// Set of functions to be converted
      const std::set<std::string> Funcs;


      /// Set of functions yet to be analyzed
      std::set<std::string> FuncsRemain;


      ////////// state of the pass

      /// Number of predicates used
      unsigned PredCount;

      /// Map MBBs to predicate they use
      std::map<const MachineBasicBlock*, unsigned> PredUse;

      /// Map MBBs to predicates they set to their true edge
      MBB_BV_map_t PredDefsT;

      /// Map MBBs to predicates they set to their false edge
      MBB_BV_map_t PredDefsF;

      /// Root SPNode
      SPNode *Root;



      /// Analyze a given MachineFunction
      void analyzeFunction(MachineFunction &MF);

      /// createSPNodeTree - Create an SPNode tree, return the root node.
      /// The tree needs to be destroyed by the client,
      /// by deleting the root node.
      SPNode *createSPNodeTree(const MachineFunction &MF) const;

      /// computeControlDependence - Compute CD for a given function.
      void computeControlDependence(MachineFunction &MF, CD_map_t &CD) const;

      /// decomposeControlDependence - Decompose CD into K and R
      void decomposeControlDependence(MachineFunction &MF, const CD_map_t &CD,
                                      K_t &K, R_t &R) const;

      /// getInitializees - Return the predicates that need to be initialized
      /// for a given SPNode
      BitVector getInitializees(const SPNode *N) const;

      /// collectPredDefs - Collect definitions in MBBs, in PredDefsT and
      /// PredDefsF
      void collectPredDefs(MachineFunction &MF, const K_t &K);


    public:
      /// Pass ID
      static char ID;

      /// isEnabled - Return true if there are functions specified to
      /// to be converted to single-path code.
      static bool isEnabled();

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

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

      /// getNumPredicates - Returns the number of predicates required for
      /// this function
      unsigned getNumPredicates() const { return PredCount; }

      /// getGuardPredicate - Returns the guarding predicate for an MBB
      BitVector getPredUse(const MachineBasicBlock *);

      /// getPredDefsT - Returs a bitvector for preds set on the true edge
      BitVector getPredDefsT(const MachineBasicBlock *);

      /// getPredDefsF - Returs a bitvector for preds set on the false edge
      BitVector getPredDefsF(const MachineBasicBlock *);

      /// getRootNode - Return the Root SPNode for this function
      SPNode *getRootNode() const { return Root; }
  };

///////////////////////////////////////////////////////////////////////////////

  class MachineBasicBlock;

  class SPNode {
    friend class PatmosSinglePathInfo;
    public:
      /// constructor - Create an SPNode with specified parent SP node or NULL
      /// if top level; the header/entry MBB; the succ MBB; number of backedges
      explicit SPNode(SPNode *parent, const MachineBasicBlock *header,
                      const MachineBasicBlock *succ, unsigned numbe);

      /// destructor - free the child nodes first, cleanup
      ~SPNode();

      /// addMBB - Add an MBB to the SP node
      void addMBB(const MachineBasicBlock *MBB);

      /// getParent
      const SPNode *getParent() const { return Parent; }

      /// getHeader
      const MachineBasicBlock *getHeader() const { return Blocks.front(); }

      /// getSuccMBB - Get the single successor MBB
      const MachineBasicBlock *getSuccMBB() const { return SuccMBB; }

      /// getLevel - Get the nesting level of the SPNode
      unsigned int getLevel() const { return Level; }

      /// isTopLevel - Returs true if the SPNode is the top-level SPNode
      /// (not a loop)
      bool isTopLevel() const { return (NULL == Parent); }

      /// getOrder - Get a list of MBBs for the final layout
      void getOrder(std::vector<const MachineBasicBlock *> &list);

      // dump() - Dump state of this SP node and the subtree
      void dump() const;

      /// child_iterator - Type for child iterator
      typedef std::map<const MachineBasicBlock*,
                       SPNode*>::iterator child_iterator;

    private:
      // parent SPNode
      SPNode *Parent;

      // successor MBB
      const MachineBasicBlock *SuccMBB;

      // number of backedges
      const unsigned NumBackedges;

      // children as map: header MBB -> SPNode
      std::map<const MachineBasicBlock*, SPNode*> Children;

      // MBBs contained
      std::vector<const MachineBasicBlock*> Blocks;

      // nesting level
      unsigned int Level;
  };

///////////////////////////////////////////////////////////////////////////////

} // end of namespace llvm

#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
