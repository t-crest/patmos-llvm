//==-- SPScope.h - Single-Path Scope -------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===---------------------------------------------------------------------===//
//
// SPScope contains all information on the single-path scopes in a function.
// It models the scopes as a tree, where the root is the scope of the function,
// called the top-level scope, and each child is a loop inside that function,
// called subscopes. Subscopes of subscopes are nested loops.
//
// The static function SPScope::createSPScopeTree can be used to construct
// the scope tree from a MachineFunction.
//
// Also contains SPScopeWalker, which can be extended to walk the tree
// of scopes.
//
//===---------------------------------------------------------------------===//

#ifndef TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_
#define TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_

#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/Support/Debug.h"
#include "boost/optional.hpp"
#include "spimpl.h"
#include "PredicatedBlock.h"

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
      /// Type for iteration through the basic blocks that are part of the scope.
      typedef std::vector<MachineBasicBlock*>::iterator iterator;

      /// Type for iteration through the subscopes of this scope.
      typedef std::vector<SPScope*>::iterator child_iterator;

      /// Type representing control flow from one MachineBasicBlock to another.
      typedef std::pair<const MachineBasicBlock *,
                        const MachineBasicBlock *> Edge;

      /// TODO:(Emad) What does the unsigned means?
      typedef std::vector<std::pair<unsigned, std::pair<const PredicatedBlock*, const PredicatedBlock*>> > PredDefInfo;

      /// Create a top-level SPScope. I.e. the SPScope representing the function.
      ///
      /// @param isRootFunc
      /// Whether the function represented by this SPSCope is a root SP function,
      /// I.e. it is given as a command argument to the compiler.
      ///
      /// @param MF
      /// The MachineFunction that the loop represented by this scope resides in.
      ///
      /// @param LI
      /// used with MF to get all the needed information about the loop.
      explicit SPScope(bool isRootFunc, MachineFunction &MF, MachineLoopInfo &LI);

      /// Create a subscope of the given parent scope that represents the given loop in the function.
      /// The given loop must be nested inside the loop represented by the parent.
      explicit SPScope(SPScope *parent, MachineLoop &loop, MachineFunction &MF, MachineLoopInfo &LI);

      /// Deletes the scope and all its subscopes.
      ~SPScope();

      /// Returns the parent scope of this scope.
      /// NULL is returned if this scope has no parent.
      const SPScope *getParent() const;

      /// Returns the header MBB of this scope.
      PredicatedBlock *getHeader() const;

      /// Returns all the MBBs that succeed the loop represented by this scope.
      /// I.e. all the MBBs that control may branch after exiting the loop.
      const std::set<const PredicatedBlock *> getSuccMBBs() const;

      /// Returns the nesting depth of the SPScope.
      /// The top-level scope has depth 0.
      unsigned getDepth() const;

      /// Returns whether the scope represents the functions itself and
      /// not a loop in the function.
      /// The top-level scope always returns true, while all subscopes return false.
      bool isTopLevel() const;

      /// Returns whether the scope is the Top-Level scope of a root SP function.
      bool isRootTopLevel() const ;

      /// isHeader - Returns true if the specified MBB is the header of this
      /// SPScope
      bool isHeader(const MachineBasicBlock *MBB) const;

      /// Returns whether the specified MBB is the header of an
      /// immediate subscope of this scope.
      /// I.e. it only checks one-level down the subscopes.
      bool isSubHeader(const MachineBasicBlock *MBB) const;

      /// Returns whether the loop represented by the scope has a loop bound.
      /// The top-level scope never has a loop bound, since it only represents the
      /// function.
      bool hasLoopBound() const ;

      /// Returns the loop bound for the scope. If the scope doesn't
      /// have a loop bound, none is returned.
      boost::optional<unsigned> getLoopBound() const;

      /// Walk this SPScope recursively
      void walk(SPScopeWalker &walker);

      /// Returns the number of predicates required for this scope.
      unsigned getNumPredicates() const;

      /// Returns the guarding predicate for an MBB that is part of the scope or is a subheader.
      /// If it is not found, will abort.
      unsigned getPredUse(const MachineBasicBlock *) const;

      /// Returns the predicates that are defined in the block, paired
      /// with the block that makes use of the predicate.
      /// If the given block is not part of the scope, or it does not define
      /// any predicates, then none is returned.
      boost::optional<SPScope::PredDefInfo> getDefInfo( const MachineBasicBlock *) const;

      /// Returns whether the the predicate has multiple definitions
      bool hasMultDefEdges(unsigned pred) const;

      /// Returns the MBBs that are either exclusively contained in this scope,
      /// or are headers of this scope's subscopes.
      /// It is sorted in topological order.
      std::vector<PredicatedBlock*> getBlocksTopoOrd() const;

      /// Returns the number of MBBs that are part of this scope,
      /// including the headers of the scope's subscopes.
      unsigned getNumberOfFcfgBlocks() const;

      /// Returns the MBBs that are exclusively contained in this scope.
      std::vector<PredicatedBlock*> getScopeBlocks() const;

      /// Returns the MBBs that are either exclusively contained in this scope,
      /// or are header of this scope's subscopes.
      /// Unlike getBlocksTopoOrd(), this is not sorted.
      std::vector<PredicatedBlock*> getFcfgBlocks() const;

      /// Dump state of this scope and its subscopes recursively
      void dump(raw_ostream&) const;

      /// Beginning iterator over the subscopes of this scope.
      child_iterator child_begin() const;

      /// The end of the iterator over the subscopes of this scope.
      child_iterator child_end() const;

      /// Returns the deepest scope, starting from this scope,
      /// containing the given MBB.
      /// If the MBB is not part of any scope, none is returned.
      boost::optional<SPScope*> findMBBScope(const MachineBasicBlock *mbb) const;

      /// Create an SPScope tree, return the top-level scope.
      /// The tree needs to be destroyed by the client,
      /// by deleting the top-level scope.
      static SPScope * createSPScopeTree(MachineFunction &MF, MachineLoopInfo &LI);

    private:
      class Impl;
      /// We use the PIMPL pattern to implement the private
      /// members of this instance.
      spimpl::unique_impl_ptr<Impl> Priv;
  };

  class SPScopeWalker {
    public:
      virtual void nextMBB(MachineBasicBlock *) = 0;
      virtual void enterSubscope(SPScope *) = 0;
      virtual void exitSubscope(SPScope *) = 0;
      virtual ~SPScopeWalker() {};
  };

}
#endif /* TARGET_PATMOS_SINGLEPATH_SPSCOPE_H_ */
