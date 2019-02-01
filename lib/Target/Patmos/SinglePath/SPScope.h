//==-- SPScope.h -  -------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
//
//
//===---------------------------------------------------------------------===//

#ifndef TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_
#define TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_

#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/Debug.h"
#include "boost/optional.hpp"
#include "spimpl.h"


// define for more detailed debugging output
#define PATMOS_SINGLEPATH_TRACE

#ifdef PATMOS_SINGLEPATH_TRACE
#define DEBUG_TRACE(x) DEBUG(x)
#else
#define DEBUG_TRACE(x) /*empty*/
#endif

namespace llvm {

  class SPScopeWalker;

  class SPScope {

    public:
      /// iterator - Type for iterator through MBBs
      typedef std::vector<MachineBasicBlock*>::iterator iterator;

      /// child_iterator - Type for child iterator
      typedef std::vector<SPScope*>::iterator child_iterator;

      // Edge type
      typedef std::pair<const MachineBasicBlock *,
                        const MachineBasicBlock *> Edge;

      /// PredDefInfo - Class containing predicate definition information
      /// of one MachineBasicBlock.
      /// Instances for MBBs are stored in the PredDefs map.
      class PredDefInfo {
        private:

          /// TODO:(Emad) What does the unsigned means?
          typedef std::vector<std::pair<unsigned, Edge> > PredEdgeList;
          PredEdgeList Defs;
        public:
          typedef PredEdgeList::const_iterator iterator;
          void define(unsigned pred, const Edge e) {
            Defs.push_back(std::make_pair(pred, e));
          }
          iterator begin() const { return Defs.begin(); }
          iterator end() const { return Defs.end(); }
      };

      /// constructor - Create a top-level SPScope
      /// @param entry            The entry MBB;
      /// @param isRootTopLevel   True when this scope is a top level scope of
      ///                         a single-path root function.
      explicit SPScope(MachineBasicBlock *entry, bool isRootFunc);

      /// constructor - Create a loop SPScope
      explicit SPScope(SPScope *parent, MachineLoop &loop);

      /// Deletes the scope and all its children.
      ~SPScope();

      /// getParent
      const SPScope *getParent() const;

      /// getHeader
      MachineBasicBlock *getHeader() const;

      /// getSuccMBB - Get the successors
      const std::vector<const MachineBasicBlock *> getSuccMBBs() const;

      /// getDepth - Get the nesting depth of the SPScope
      unsigned int getDepth() const;

      /// isTopLevel - Returns true if the SPScope is the top-level SPScope
      /// (not a loop)
      bool isTopLevel() const;

      /// isRootTopLevel - Returns true if the SPScope is the top-level SPScope
      /// of a single-path root function
      bool isRootTopLevel() const ;

      /// isHeader - Returns true if the specified MBB is the header of this
      /// SPScope
      bool isHeader(const MachineBasicBlock *MBB) const;

      /// Returns whether the specified MBB is a member of this SPScope.
      /// Does not check for whether the MBB is part of a child or parent scope.
      bool isMember(const MachineBasicBlock *MBB) const;

      /// Returns whether the specified MBB is the header of an
      /// immediate subscope of this scope.
      /// I.e. it only checks one-level down the subscopes.
      bool isSubHeader(MachineBasicBlock *MBB) const;

      /// hasLoopBound - Returs true if the SPScope is a loop and has a bound
      /// to be accounted for
      bool hasLoopBound() const ;

      /// getLoopBound - Return the loop bound for this SPScope
      int getLoopBound() const ;

      /// walk - Walk this SPScope recursively
      void walk(SPScopeWalker &walker);

      /// getNumPredicates - Returns the number of predicates required for
      /// this function
      unsigned getNumPredicates() const;

      /// getPredUse - Returns the guarding predicate for an MBB
      const std::vector<unsigned> * getPredUse(const MachineBasicBlock *) const;

      /// getDefInfo - Returns a pointer to a predicate definition info for
      /// a given MBB, or NULL if no pred info exists for the MBB.
      const PredDefInfo *getDefInfo( const MachineBasicBlock *) const;

      /// getNumDefEdges - Returns the number of definition edges for a given
      /// predicate.
      unsigned getNumDefEdges(unsigned pred) const;

      /// getBlocks - Returns the list of basic blocks in this SPScope,
      /// in topological order.
      const std::vector<MachineBasicBlock*> &getBlocks() const;

      // dump() - Dump state of this SP scope and the subtree
      void dump(raw_ostream&) const;

      /// addMBB - Add an MBB to the SP scope
      void addMBB(MachineBasicBlock *MBB);

      void addChild(SPScope * child, MachineBasicBlock *childHeader);

      /// begin - Iterator begin for MBBs
      iterator begin();

      /// child_end - Iterator end for MBBs
      iterator end();

      /// child_begin - Iterator begin for subloops
      child_iterator child_begin() const;

      /// child_end - Iterator end for subloops
      child_iterator child_end() const;

      /// Returns the innermost scope containing the given basic block.
      /// If the is not part of any scope, none is returned.
      boost::optional<SPScope*> findMBBScope(const MachineBasicBlock *mbb) const;

      /// Create an SPScope tree, return the root scope.
      /// The tree needs to be destroyed by the client,
      /// by deleting the root scope.
      static SPScope * createSPScopeTree(MachineFunction &MF, MachineLoopInfo &LI);

    private:
      class Impl;
      /// We use the PIMPL pattern to implement the private
      /// members of this instance.
      spimpl::unique_impl_ptr<Impl> Priv;
  };

///////////////////////////////////////////////////////////////////////////////

  class SPScopeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubscope(SPScope *) = 0;
      virtual void exitSubscope(SPScope *) = 0;
      virtual ~SPScopeWalker() {};
  };

}
#endif /* TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_ */
