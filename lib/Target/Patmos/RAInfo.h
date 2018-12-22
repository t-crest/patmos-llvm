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
#include <sstream>

using namespace boost;

namespace llvm {
///////////////////////////////////////////////////////////////////////////////

  /// LiveRange - Class to hold live range information for a predicate in
  /// an RAInfo object.
  /// A live range is a set of position, each of which is associated with a
  /// basic block in the scope being described. The first position in the
  /// range matches the first block in the scope. There is one more position
  /// that there are blocks, so the last position is not associated with any block.
  /// At any location, the predicate can be used and/or defined.
  /// TODO:(Emad) what does it mean that a predicate is 'defined' at a position?
  /// TODO:(Emad) can it be 'defined' in more than one position ? i think so, see PatmosSinglePathInfo.h::PredDefInfo
  class LiveRange {
  friend class RAInfo;
  private:

    // Where each predicate is used.
    // The position is the index of the block in the scope
    // except for the last one which doesn't have an associated block
    BitVector uses;

    // Where each predicate is defined.
    // The position is the index of the block in the scope
    // except for the last one which doesn't have an associated block
    BitVector defs;

    // Add a use of the predicate associated with this range
    // at the position given.
    void addUse(long pos) { uses.set(pos); }

    // Add a use of the predicate associated with this range
    // at the position given.
    void addDef(long pos) { defs.set(pos); }
  public:
    /// Constructs a new live range for a scope.
    /// the number of points in the range is 1 more than the number of blocks.
    LiveRange(SPScope *S){
      int range = S->getBlocks().size()+1;
      uses = BitVector(range);
      defs = BitVector(range);
    }
    bool isUse(long pos) const { return uses.test(pos); }
    bool isDef(long pos) const { return defs.test(pos); }
    bool lastUse(long pos) const {
      // test whether shifting out up to this use will result in an empty
      // bitvector
      // return (uses >> (pos+1LL)) == 0;
      for (unsigned i = pos+1; i < uses.size(); i++) {
        if (uses.test(i)) return false;
      }
      return true;
    }
    bool hasDefBefore(long pos) const {
      // 00000100000 pos
      // 00000011111 before
      // -> any common?
      //return (defs & ((1LL << pos)-1LL)) != 0;
      unsigned i = pos;
      while (i-- > 0) {
        if (defs.test(i)) return true;
      }
      return false;
    }

    // check if there is any use before (and including) pos
    bool anyUseBefore(long pos) const {
      //return (uses & ((1LL << (pos+1LL))-1LL)) != 0;
      for (unsigned i = 0; i <= pos; i++) {
        if (uses.test(i)) return true;
      }
      return false;
    }
    bool hasNextUseBefore(long pos, const LiveRange &other) const {
      assert(uses.size() == other.uses.size());
      // this   ....10000|...
      // other ......1000|...   -> no
      //                ^pos
      for (unsigned i = pos; i < uses.size(); i++) {
        if (other.uses.test(i)) break;
        if (uses.test(i)) return true;
      }
      return false;
    }
    std::string str(void) const {
      std::stringbuf buf;
      char kind[] = { '-', 'u', 'd', 'x' };
      for (unsigned long i = 0; i < uses.size(); i++) {
        int x = 0;
        if (uses.test(i)) x += 1;
        if (defs.test(i)) x += 2;
        buf.sputc(kind[x]);
      }
      return buf.str();
    }
  };

///////////////////////////////////////////////////////////////////////////////

  // A class defining a predicate location in memory.
  // A location is either a register or a stack spill slot, i.e. the 'type'.
  // The 'idx' field specifie the index of the register or stack spill slot
  // used by this location.
  // E.g. Location{Register, 1} specifies that this location is the second
  // register, while Location{Stack, 3} specifies this location is the
  // fourth stack spill slot.
  class Location {

    public:
      enum Type{Register, Stack};
      friend bool operator<(const Location &, const Location &);

      Location(const Location &o): type(o.type), loc(o.loc){}
      Location(Type type, unsigned loc): type(type), loc(loc){}

      const Type &getType() const { return type;}
      const unsigned &getLoc() const { return loc;}

    private:
      Location::Type type;
      unsigned loc;


  };

  /// RAInfo - Class to hold register allocation information for a SPScope
  class RAInfo {
    public:
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
      optional<Location> getUseLoc(const MachineBasicBlock *MBB) const;

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
      std::tuple<unsigned, bool> getDefLoc(unsigned pred) const;

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
