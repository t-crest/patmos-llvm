//==-- PatmosSinglePathInfo.h - Class to hold information for SP CodeGen ---==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a class to hold information and configuration of
// Single-Path Code Generation.
//
//===----------------------------------------------------------------------===//


#ifndef _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
#define _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_

#include <Patmos.h>
#include <PatmosTargetMachine.h>
#include <llvm/ADT/GraphTraits.h>

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

// forward decl
class SPNode;

  /// PatmosSinglePathInfo - Class to hold info about Single-path code generation
  class PatmosSinglePathInfo {
    private:

      const PatmosTargetMachine &TM;

      /// Set of functions to be converted
      std::set<std::string> SPConvFuncs;

    public:

      /// PatmosSinglePathInfo
      explicit PatmosSinglePathInfo(const PatmosTargetMachine &tm);

      bool enabled() {
        return !SPConvFuncs.empty();
      }

      /// isToConvert - Return true if the function should be if-converted
      bool isToConvert(MachineFunction &MF) const;

  };


  class MachineBasicBlock;

  class SPNode {
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

} // end of namespace llvm


#endif // _LLVM_TARGET_PATMOS_SINGLEPATHINFO_H_
