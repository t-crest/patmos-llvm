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

namespace llvm {

/// RAInfo - Class to hold register allocation information for a SPScope
class RAInfo {
public:

  /// Specifies a location type.
  enum LocType{Register, Stack};

  /// the SPScope this RAInfo belongs to
  const SPScope *Scope;

  RAInfo(SPScope *S, unsigned availRegs);

  /// Returns true if the predicate register file must be spilled before
  /// entry into this scope associated with this instance
  /// and restored upon exit.
  bool needsScopeSpill(void) const ;

  /// Returns true if the given MBB contains the first
  /// definition of the given predicate.
  /// This has to be false for a header predicate, as the first
  /// definition is before the loop is entered
  bool isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const;

  /// Returns true if the use location of the given MBB
  /// requires the predicate register to be first spilled and/or loaded
  /// to/from a spill location.
  bool hasSpillLoad(const MachineBasicBlock *MBB) const;

  /// Get the use location, which is a register location,
  /// for the given MBB. If the MBB is not mapped to a register location the empty
  /// option is returned. Pred -> Location
  std::map<unsigned, unsigned> getUseLocs(const MachineBasicBlock *MBB) const;

  /// Get the load location, which is a spill location,
  /// for the given MBB. Returns the empty option if the predicate does not need to be
  /// loaded from a spill slot. Pred -> Location
  std::map<unsigned, unsigned> getLoadLocs(const MachineBasicBlock *MBB) const;

  /// Get the spill location, i.e. the location where the use
  /// register has to be spilled first, for the given MBB.
  /// Returns the empty option if it does not need to be spilled.
  std::map<unsigned, unsigned> getSpillLocs(const MachineBasicBlock *MBB) const;

  /// Returns the definition location for the given predicate.
  /// The first element is the type of the location (Register/Stack).
  /// The second element is the index of the location within the type.
  std::tuple<LocType, unsigned> getDefLoc(unsigned pred) const;

  /// Dump this RAInfo to dbgs().
  void dump(void) const;

  /// How many spill slots this RAInfo needs.
  unsigned neededSpillLocs();

  /// Performs register allocation for the function associate with 'PSPI' assuming
  /// it has 'availPredRegs' predicate registers it can use.
  /// returns a mapping of the register allocation information computed for
  /// each scope in the function.
  static std::map<const SPScope*, RAInfo> computeRegAlloc(PatmosSinglePathInfo *PSPI, unsigned availPredRegs);

private:
  class Impl;
  /// We use the PIMPL pattern to implement the private
  /// members of this instance.
  spimpl::unique_impl_ptr<Impl> Priv;

};
}

#endif /* TARGET_PATMOS_SINGLEPATH_RAINFO_H_ */
