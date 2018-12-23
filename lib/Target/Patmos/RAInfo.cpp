//===-- RAInfo.cpp - Patmos LLVM single-path predicate register allocator -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The RAInfo class handles predicate register allocation for single-path code.
//
//===----------------------------------------------------------------------===//
#include "RAInfo.h"

#include "llvm/Support/Debug.h"
#include "llvm/ADT/Statistic.h"

using namespace llvm;
using namespace boost;

STATISTIC( SPNumPredicates, "Number of predicates for single-path code");

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
  public:
    // Add a use of the predicate associated with this range
    // at the position given.
    void addUse(long pos) { uses.set(pos); }

    // Add a use of the predicate associated with this range
    // at the position given.
    void addDef(long pos) { defs.set(pos); }

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

bool llvm::operator<(const Location&l, const Location &r){
  if( l.getType() == r.getType()){
    return l.getLoc() < r.getLoc();
  } else {
    return false;
  }
}

// The private implementation of RAInfo using the PIMPL pattern.
class RAInfo::Impl {
public:
  // A reference to the RAInfo that uses this instance
  // to implement its private members. (I.e. the public part
  // of the implementation)
  RAInfo &Pub;

  Impl(RAInfo *pub, SPScope *S, unsigned availRegs):
    Pub(*pub), AvailRegs(availRegs), NeedsScopeSpill(true),
    LRs(S->getNumPredicates(), LiveRange(S)),
    DefLocs(S->getNumPredicates(),-1), NumLocs(0),
    ChildrenMaxCumLocs(0), Offset(0), SpillOffset(0)
  {
    createLiveRanges();
    assignLocations();
  }

  // Number of physically available registers for use by the Scope.
  const unsigned AvailRegs;

  // The live ranges of predicates.
  // Given a predicate x, then its live range is LRs[x]
  std::vector<LiveRange> LRs;

  // The definition location of each predicate.
  // Given the predicate x, its definition is pimpl->DefLocs[x].
  // TODO:(Emad) what does the int mean? block index in scope? what about when its -1?
  std::vector<int> DefLocs;

  // The total number of predicate locations used by this instance.
  unsigned NumLocs;

  // The maximum number of location used by any child
  unsigned ChildrenMaxCumLocs,
           Offset,  // If S0 does not need to be spilled around this scope,
                    //   this is the offset to the available registers
           SpillOffset; // Starting offset for this scope's spill locations

  // Comparator for predicates, furthest next use;
  // used in assignLocations()
  struct FurthestNextUseComparator {
    RAInfo::Impl &RI;
    int pos;
    bool operator()(int a, int b) {
      return RI.LRs[a].hasNextUseBefore(pos, RI.LRs[b]);
    }
    FurthestNextUseComparator(RAInfo::Impl &ri, int p) : RI(ri), pos(p) {}
  };

  // UseLoc - Record to hold predicate use information for a MBB
  // - loc:   which location to use (a register)
  // - spill: where to spill loc first (spill location)
  // - load:  where to load loc before using it (spill location)
  struct UseLoc {
    int loc, load, spill;
    UseLoc(void) : loc(-1), load(-1), spill(-1) {}
  };
  // Map of MBB -> UseLoc, for an SPScope
  std::map<const MachineBasicBlock*, UseLoc> UseLocs;

  bool NeedsScopeSpill;

  // Returns the first available location in the given set, removing it from the set.
  // If the set is empty, a new Location is created and returned.
  Location getAvailLoc(std::set<Location> &FreeLocs) {
    if (!FreeLocs.empty()) {
      std::set<Location>::iterator it = FreeLocs.begin();
      FreeLocs.erase(it);
      return *it;
    }

    // create a new location
    return Location(
        NumLocs < (AvailRegs) ? Location::Register : Location::Stack,
        NumLocs++
      );
  }

  // Returns whether either there is a free register location available
  // in the given set, or one can be created.
  // if true, the next call to getAvailLoc is guaranteed to produce a Register
  // location (assuming the given set or the fields don't change).
  bool hasFreePhys(std::set<Location> &FreeLocs) {
    return (!FreeLocs.empty() && (FreeLocs.begin()->getType() == Location::Register))
        || (NumLocs < (AvailRegs));
  }

  // Returns whether the given register location is a physical
  // register location.
  bool isPhysRegLoc(int loc) const {
    return loc < (int)(AvailRegs);
  }

  // getCumLocs - Get the maximum number of locations
  // used by this scope and any of its children
  unsigned getCumLocs(void) const { return NumLocs + ChildrenMaxCumLocs; }

  // assignSpillOffset
  void assignSpillOffset(unsigned spillOffset) {
    // assign the spill offset, increment
    if (NumLocs > (AvailRegs)) {
      this->SpillOffset = spillOffset;
    }
  }

  void createLiveRanges(void) {
    // create live range infomation for each predicate
    DEBUG(dbgs() << " Create live-ranges for [MBB#"
                 << Pub.Scope->getHeader()->getNumber() << "]\n");


    for (unsigned i=0, e=Pub.Scope->getBlocks().size(); i<e; i++) {
      MachineBasicBlock *MBB = Pub.Scope->getBlocks()[i];
      // insert use
      const std::vector<unsigned> *predUses = Pub.Scope->getPredUse(MBB);
      std::for_each(predUses->begin(), predUses->end(), [&](unsigned p){
        LRs[p].addUse(i);
      });
      // insert defs
      const SPScope::PredDefInfo *DI = Pub.Scope->getDefInfo(MBB);
      if (DI) {
        for (SPScope::PredDefInfo::iterator pi = DI->begin(), pe = DI->end();
            pi != pe; ++pi) {
          LRs[pi->first].addDef(i); // TODO:(Emad) why don't we check that the edge hits the block?
        }
      }
    }
    // add a use for header predicate
    // TODO:(Emad) is that because its a loop and P0 is used when we jump back to the start
    // TODO:(Emad) of the loop, therefore we say that the last block also uses P0? i.e. connecting
    // TODO:(Emad) the loop end with the start?
    if (!Pub.Scope->isTopLevel()) {
      LRs[0].addUse(Pub.Scope->getBlocks().size());
    }
  }

  void assignLocations(void) {
    DEBUG(dbgs() << " Assign locations for [MBB#"
                 << Pub.Scope->getHeader()->getNumber() << "]\n");
    SPNumPredicates += Pub.Scope->getNumPredicates(); // STATISTIC

    std::set<Location> FreeLocs;

    // map to keep track of locations of predicates during the scan
    std::map<unsigned, Location> curLocs;

    for (unsigned i=0, e=Pub.Scope->getBlocks().size(); i<e; i++) {
      MachineBasicBlock *MBB = Pub.Scope->getBlocks()[i];

      DEBUG( dbgs() << "  MBB#" << MBB->getNumber() << ": " );

      // (1) handle use
      unsigned usePred = (*Pub.Scope->getPredUse(MBB))[0];
      // TODO:(Emad) handle multiple predicates.

      // for the top-level entry of a single-path root,
      // we don't need to assign a location, as we will use p0
      if (!(usePred==0 && Pub.Scope->isRootTopLevel())) {
        UseLoc UL;

        std::map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(usePred);

        assert(MBB == Pub.Scope->getHeader() || i>0);

        if (MBB != Pub.Scope->getHeader()) {
          // each use must be preceded by a location assignment
          if (findCurUseLoc == curLocs.end()){
            errs() << __FILE__ <<":" << __LINE__ << "curLocs does not map predicate: " << usePred;
            abort();
          }
          Location &curUseLoc = findCurUseLoc->second;
          // if previous location was not a register, we have to allocate
          // a register and/or possibly spill
          if ( curUseLoc.getType() != Location::Register ) {
            if (hasFreePhys(FreeLocs)) {
              UL.load = curUseLoc.getLoc();

              // reassign, but
              // DO NOT free stack locations again, i.e. not freeLoc(curUseLoc);
              curUseLoc = getAvailLoc(FreeLocs);
              UL.loc = curUseLoc.getLoc(); // gets a register
              if((UL.load < (int)(AvailRegs)) || (UL.loc > (int)(AvailRegs))){
                errs() << __FILE__ <<":" << __LINE__ << " UL has wrong values: load(" << UL.load << "), loc(" << UL.loc << "), AvailRegs(" << (AvailRegs) << ")\n";
                abort();
              }
            } else {
              // spill and reassign
              // order predicates wrt furthest next use
              std::vector<unsigned> order;
              for(unsigned j=0; j<LRs.size(); j++) {
                // consider all physical registers in use
                std::map<unsigned, Location>::iterator cj = curLocs.find(j);
                if (cj != curLocs.end() && cj->second.getType() == Location::Register) {
                  order.push_back(j);
                }
              }
              std::sort(order.begin(), order.end(),
                  FurthestNextUseComparator(*this,i));
              unsigned furthestPred = order.back();
              Location stackLoc = getAvailLoc(FreeLocs); // guaranteed to be a stack location, since there are no physicals free
              assert( stackLoc.getType() == Location::Stack );
              if(stackLoc.getLoc() < (AvailRegs)){
                errs() << __FILE__ << ":" << __LINE__ << ": stack location is not smaller than AvailRegs: " << stackLoc.getLoc();
                abort();
              }

              UL.load  = curUseLoc.getLoc();

              std::map<unsigned, Location>::iterator findFurthest = curLocs.find(furthestPred);
              if(findFurthest == curLocs.end()){
                errs() << __FILE__ << ":" << __LINE__ << " Furthest predicate not mapped: " << furthestPred;
                abort();
              }
              UL.loc   = findFurthest->second.getLoc();

              // differentiate between already used and not yet used
              if (LRs[furthestPred].anyUseBefore(i)) {
                UL.spill = stackLoc.getLoc();
              } else {
                // if it has not been used, we change the initial
                // definition location
                DefLocs[furthestPred] = stackLoc.getLoc();
              }
              findCurUseLoc = findFurthest;
              findFurthest->second = stackLoc;
            }
          } else {
            // everything stays as is
            UL.loc = curUseLoc.getLoc();
          }
        } else {
          assert(usePred == 0);
          // we get a loc for the header predicate
          Location loc = getAvailLoc(FreeLocs);
          DefLocs[0] = UL.loc = loc.getLoc();
          std::map<unsigned, Location>::iterator curLoc0 = curLocs.find(0);
          if(curLoc0 == curLocs.begin()){
            curLocs.insert(std::make_pair(0, loc));
          }else{
            curLoc0->second = loc;
          }
          assert(UL.loc == 0);
        }

        //DEBUG( dbgs() << "new " << curUseLoc << ". ");

        // (2) retire locations
        if (LRs[usePred].lastUse(i)) {
          DEBUG(dbgs() << "retire. ");
          if (findCurUseLoc == curLocs.end()){
            errs() << __FILE__ <<":" << __LINE__ << "curLocs does not map predicate: " << usePred << "\n";
            abort();
          }
          Location &curUseLoc = findCurUseLoc->second;

          // free location, also removing it from the current one is use
          assert(!FreeLocs.count(curUseLoc));
          FreeLocs.insert(curUseLoc);
          curLocs.erase(findCurUseLoc);
        }

        // store info
        UseLocs[MBB] = UL;
      }

      // (3) handle definitions in this basic block.
      //     if we need to get new locations for predicates (loc==-1),
      //     assign new ones in nearest-next-use order
      const SPScope::PredDefInfo *DI = Pub.Scope->getDefInfo(MBB);
      if (DI) {
        std::vector<unsigned> order;
        for (SPScope::PredDefInfo::iterator pi = DI->begin(), pe = DI->end();
            pi != pe; ++pi) {
          int r = pi->first;
          if (curLocs.find(r) == curLocs.end()) {
            // need to get a new loc for predicate r
            order.push_back(r);
          }
        }
        std::sort(order.begin(), order.end(),
            FurthestNextUseComparator(*this,i));
        // nearest use is in front
        for (unsigned j=0; j<order.size(); j++) {
          unsigned pred = order[j];
          Location l = getAvailLoc(FreeLocs);
          std::map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(pred);
          if(findCurUseLoc == curLocs.end()){
            curLocs.insert(std::make_pair(pred, l));
          }else{
            findCurUseLoc->second = l;
          }
          DefLocs[pred] = l.getLoc();

          if(curLocs.find(pred)->second.getLoc() != DefLocs[pred]){
            errs() << __FILE__ <<":" << __LINE__ << "\nLocation not equal to definition: " << curLocs.find(pred)->second.getLoc() << " != "<< DefLocs[pred] << "\n";
            abort();
          }

          DEBUG( dbgs() << "def " << pred << " in loc "
                        << DefLocs[pred] << ", ");
        }
      }

      DEBUG(dbgs() << "\n");
    } // end of forall MBB

    // What is the location of the header predicate after handling all blocks?
    // We store this location, as it is where the next iteration has to get it
    // from (if different from its use location)
    // Code for loading the predicate is placed before the back-branch,
    // generated in LinearizeWalker::exitSubscope().
    if (!Pub.Scope->isTopLevel()) {
      UseLoc &ul = UseLocs[Pub.Scope->getHeader()];
      std::map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(0);
      if(findCurUseLoc == curLocs.end()){
        errs() << __FILE__ <<":" << __LINE__ << "curLocs does not map predicate: 0";
        abort();
      }
      if (ul.loc != (int)(findCurUseLoc->second.getLoc())) {
        ul.load = findCurUseLoc->second.getLoc();
      }
    }
  }

};

///////////////////////////////////////////////////////////////////////////////
//  RAInfo methods
///////////////////////////////////////////////////////////////////////////////

RAInfo::RAInfo(SPScope *S, unsigned availRegs) :
  Scope(S), priv(spimpl::make_unique_impl<Impl>(this, S, availRegs))
  {}

bool RAInfo::needsScopeSpill(void) const {
  return priv->NeedsScopeSpill;
}

bool RAInfo::isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const {
  // header predicate
  if (pred == 0) return false;

  for(unsigned i=0; i< Scope->getBlocks().size(); i++) {
    if (Scope->getBlocks()[i] == MBB) {
      return !priv->LRs[pred].hasDefBefore(i);
    }
  }
  return false;
}

bool RAInfo::hasSpillLoad(const MachineBasicBlock *MBB) const {
  if (priv->UseLocs.count(MBB)) {
    const Impl::UseLoc &ul = priv->UseLocs.at(MBB);
    return (ul.spill!=-1) || (ul.load!=-1);
  }
  return false;
}

optional<Location> RAInfo::getUseLoc(const MachineBasicBlock *MBB) const {
  if (priv->UseLocs.count(MBB)) {
    int loc = priv->UseLocs.at(MBB).loc + priv->Offset;
    assert( loc < (int)(priv->AvailRegs) );
    return optional<Location>{Location(Location::Register, loc)};
  }
  return optional<Location>{};
}

optional<unsigned> RAInfo::getLoadLoc(const MachineBasicBlock *MBB) const {
  if (priv->UseLocs.count(MBB)) {
    int loc = priv->UseLocs.at(MBB).load;
    if (loc != -1) {
      assert( loc >= (int)(priv->AvailRegs) );
      return optional<unsigned>{(loc - (priv->AvailRegs)) + priv->SpillOffset};
    }
  }
  return optional<unsigned>{};
}

int RAInfo::getSpillLoc(const MachineBasicBlock *MBB) const {
  if (priv->UseLocs.count(MBB)) {
    int loc = priv->UseLocs.at(MBB).spill;
    if (loc != -1) {
      assert( loc >= (int)(priv->AvailRegs) );
      return (loc - (priv->AvailRegs)) + priv->SpillOffset;
    }
  }
  return -1;
}

std::tuple<unsigned, bool> RAInfo::getDefLoc(unsigned pred) const {
  int dloc = priv->DefLocs[pred];
  assert(dloc != -1);
  bool isreg = priv->isPhysRegLoc(dloc);
  unsigned loc;
  if (isreg) {
    loc = dloc + priv->Offset;
  } else {
    loc = (dloc - (priv->AvailRegs)) + priv->SpillOffset;
  }
  return std::make_tuple(loc, isreg);
}

void RAInfo::unifyWithParent(const RAInfo &parent, int parentSpillLocCnt, bool topLevel){

  // If we want to try to minimize the number of spills
  #ifdef NOSPILL_OPTIMIZATION
    // check if we must spill the PRegs
    // Parent.num + S.cum <= size  --> no spill!
    if ( !topLevel && parent.priv->NumLocs + priv->getCumLocs() <= (priv->AvailRegs) ) {

      // compute offset, i.e. the first register not used by the parent.
      priv->Offset = parent.priv->NumLocs + parent.priv->Offset;

      // If the total number of locations the parent, myself, and my children need
      // are less than/equal to the number of available registers
      // we do not have to spill any predicates.
      priv->NeedsScopeSpill = false;
    }
  #endif
  priv->assignSpillOffset(parentSpillLocCnt);
}

void RAInfo::unifyWithChild(const RAInfo &child){
  priv->ChildrenMaxCumLocs = std::max(child.priv->getCumLocs(), priv->ChildrenMaxCumLocs);
}

unsigned RAInfo::neededSpillLocs(){
  if(priv->NumLocs > (priv->AvailRegs)) {
    return 0;
  }else{
    return priv->NumLocs - (priv->AvailRegs);
  }
}

void RAInfo::dump() const {
  dbgs() << "[MBB#"     << Scope->getHeader()->getNumber()
         <<  "] depth=" << Scope->getDepth() << "\n";

  for(unsigned i=0; i<priv->LRs.size(); i++) {
    const LiveRange &LR = priv->LRs[i];
    dbgs() << "  LR(p" << i << ") = [" << LR.str() << "]\n";
  }

  for (unsigned i=0, e=Scope->getBlocks().size(); i<e; i++) {
    MachineBasicBlock *MBB = Scope->getBlocks()[i];

    dbgs() << "  " << i << "| MBB#" << MBB->getNumber();
    if (priv->UseLocs.count(MBB)) {
      const Impl::UseLoc &UL = priv->UseLocs.at(MBB);
      dbgs() << "  loc=" << UL.loc << " load=" << UL.load
          << " spill=" << UL.spill;
    }
    dbgs() << "\n";
  }

  dbgs() << "  pimpl->DefLocs:     ";
  for (unsigned j=0; j<priv->DefLocs.size(); j++) {
    dbgs() << " p" << j << "=" << priv->DefLocs[j];
  }
  dbgs() << "\n";

  dbgs() << "  NumLocs:      " << priv->NumLocs << "\n"
            "  CumLocs:      " << priv->getCumLocs() << "\n"
            "  Offset:       " << priv->Offset  << "\n"
            "  SpillOffset:  " << priv->SpillOffset  << "\n";
}
