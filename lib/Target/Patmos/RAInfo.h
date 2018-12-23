//===-- RAInfo.cpp - Patmos LLVM single-path predicate register allocator -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the RAInfo class that handles
// predicate register allocation for single-path code.
//
//===----------------------------------------------------------------------===//


#ifndef TARGET_PATMOS_SINGLEPATH_RAINFO_H_
#define TARGET_PATMOS_SINGLEPATH_RAINFO_H_

#define DEBUG_TYPE "patmos-singlepath"

#define USE_BCOPY
#define NOSPILL_OPTIMIZATION

#include "boost/optional.hpp"
#include "PatmosSinglePathInfo.h"
#include "spimpl.h"

using namespace std;
using namespace boost;

namespace llvm {

///////////////////////////////////////////////////////////////////////////////

  /// RAInfo - Class to hold register allocation information for a SPScope
  class RAInfo {
    public:

      enum LocType{Register, Stack};

      // the SPScope this RAInfo belongs to
      const SPScope *Scope;

      RAInfo(SPScope *S, unsigned availRegs);

      // needsScopeSpill - Returns true if S0 must be spilled/restored
      // upon entry/exit of this SPScope.
      bool needsScopeSpill(void) const ;

      // isFirstDef - Returns true if the given MBB contains the first
      // definition of the given predicate.
      // This has to be false for a header predicate, as the first
      // definition is before the loop is entered
      bool isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const;
      // hasSpillLoad - Returns true if the use location of the given MBB
      // requires the predicate register to be first spilled and/or loaded
      // to/from a spill location.
      bool hasSpillLoad(const MachineBasicBlock *MBB) const;

      /// getUseLoc - Get the use location, which is a register location,
      /// for the given MBB. If the MBB is not mapped to a location the empty
      /// option is returned.
      optional<tuple<LocType, unsigned>> getUseLoc(const MachineBasicBlock *MBB) const;

      /// getLoadLoc - Get the load location, which is a spill location,
      /// for the given MBB. Returns the empty option if the predicate does not need to be
      /// loaded from a spill slot.
      optional<unsigned> getLoadLoc(const MachineBasicBlock *MBB) const;

      /// getSpillLoc - Get the spill location, i.e. the location where the use
      /// register has to be spilled first, for the given MBB.
      /// Returns -1 if it does not need to be spilled.
      int getSpillLoc(const MachineBasicBlock *MBB) const;

      /// Returns the definition location for the given predicate (as the first element)
      /// and whether that location is a physical register (in the second element).
      ///
      tuple<unsigned, bool> getDefLoc(unsigned pred) const;

      // Dump this RAInfo to dbgs().
      void dump(void) const;

      // Unifies with parent, such that this RAInfo knows which registers it can use
      // and where is spill slots are
      void unifyWithParent(const RAInfo &parent, int parentSpillLocCnt, bool topLevel);

      // Unifies with the child, such that this RAInfo knows which registers it can use
      // and where is spill slots are
      void unifyWithChild(const RAInfo &child);

      // How many spill slots this RAInfo needs.
      unsigned neededSpillLocs();

    private:
      class Impl;
      // We use the PIMPL pattern to implement the private
      // members of this instance.
      spimpl::unique_impl_ptr<Impl> priv;

  };


}

#endif /* TARGET_PATMOS_SINGLEPATH_RAINFO_H_ */
