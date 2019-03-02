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
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/DepthFirstIterator.h"

#include <sstream>

using namespace llvm;
using namespace boost;
using namespace std;

STATISTIC( SPNumPredicates, "Number of predicates for single-path code");
STATISTIC( PredSpillLocs, "Number of required spill bits for predicates");
STATISTIC( NoSpillScopes,
                  "Number of SPScopes (loops) where S0 spill can be omitted");

///////////////////////////////////////////////////////////////////////////////

/// LiveRange - Class to hold live range information for a predicate in
/// an RAInfo object.
/// A live range is a set of position, each of which is associated with a
/// basic block in the scope being described. The first position in the
/// range matches the header block in the scope. The rest of the blocks
/// are indexed in topological ordering.
/// There is one more position that there are blocks, so the last position
/// is not associated with any block.
/// At any location, the predicate can be used and/or defined.
/// TODO:(Emad) what does it mean that a predicate is 'defined' at a position?
/// TODO:(Emad) can it be 'defined' in more than one position ?
///             i think so, see SPScope::PredDefInfo
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
    int range = S->getNumberOfFcfgBlocks()+1;
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
  string str(void) const {
    stringbuf buf;
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
// The 'idx' field specifies the index of the register or stack spill slot
// used by this location.
// E.g. Location{Register, 1} specifies that this location is the second
// register, while Location{Stack, 3} specifies this location is the
// fourth stack spill slot.
// Location indices start at 0 for both registers and stack spill slots.
class Location {

  public:
    friend bool operator<(const Location &, const Location &);
    friend llvm::raw_ostream &operator<<(llvm::raw_ostream&, const Location &);

    Location(const Location &o): type(o.type), loc(o.loc){}
    Location(RAInfo::LocType type, unsigned loc): type(type), loc(loc){}

    const RAInfo::LocType &getType() const { return type;}
    const unsigned &getLoc() const { return loc;}

  private:
    RAInfo::LocType type;
    unsigned loc;

};

bool operator<(const Location&l, const Location &r){
  if( l.getType() == r.getType()){
    return l.getLoc() < r.getLoc();
  } else {
    return false;
  }
}

llvm::raw_ostream &operator<<(llvm::raw_ostream& os, const Location &m) {
    os << "Location{" << (m.getType() == RAInfo::Register? "Register":"Stack") <<", " << m.getLoc() <<"}";
    return os;
}

// The private implementation of RAInfo using the PIMPL pattern.
class RAInfo::Impl {
public:
  // A reference to the RAInfo that uses this instance
  // to implement its private members. (I.e. the public part
  // of the implementation)
  RAInfo &Pub;

  /// Number of available registers for use by the function.
  /// Not necessarily all of these registers are usable by the
  /// scope associated with this instance, since the parent scope
  /// may be using some of them.
  /// See 'firstUsableReg'.
  const unsigned MaxRegs;

  // The live ranges of predicates.
  // Given a predicate x, then its live range is LRs[x]
  vector<LiveRange> LRs;

  // The definition location of each predicate.
  // Given the predicate x, its definition is DefLocs[x].
  vector<optional<Location>> DefLocs;

  // The total number of predicate locations used by this instance.
  unsigned NumLocs;

  // The maximum number of location used by any child.
  unsigned ChildrenMaxCumLocs;

  /// The index of the first register this instance can use.
  /// The registers below the index are used by a parent scope.
  unsigned FirstUsableReg;

  /// The index of the first stack spill slot this instance can use.
  /// The slots below the index are used by a parent scope.
  unsigned FirstUsableStackSlot;

  /// Record to hold predicate use information for a MBB.
  struct UseLoc {
    /// Which register location to use as the predicate
    /// to an MBB
    unsigned loc;

    /// From which spill location to load the predicate
    /// before using it (load it into 'loc').
    /// If 'none' does not need to load before use.
    optional<unsigned> load;

    /// To which spill location to spill the predicate (from 'loc')
    /// after the MBB is done.
    /// If 'none' does not need to spill after use.
    optional<unsigned> spill;
    UseLoc(unsigned loc) : loc(loc), load(none), spill(none) {}
  };

  // Map of MBB -> UseLoc, for an SPScope
  map<const MachineBasicBlock*, UseLoc> UseLocs;

  bool NeedsScopeSpill;

  Impl(RAInfo *pub, SPScope *S, unsigned availRegs):
    Pub(*pub), MaxRegs(availRegs), LRs(S->getNumPredicates(), LiveRange(S)),
    DefLocs(S->getNumPredicates(),none), NumLocs(0), ChildrenMaxCumLocs(0),
    FirstUsableReg(0), FirstUsableStackSlot(0),NeedsScopeSpill(true)
  {
    createLiveRanges();
    assignLocations();
  }

  // Returns the first available location in the given set, removing it from the set.
  // If the set is empty, a new Location is created and returned.
  Location getAvailLoc(set<Location> &FreeLocs) {
    if (!FreeLocs.empty()) {
      set<Location>::iterator it = FreeLocs.begin();
      FreeLocs.erase(it);
      return *it;
    }
    // Create a new location
    unsigned oldNumLocs = NumLocs++;

    return (oldNumLocs < MaxRegs)?
      Location(Register, oldNumLocs)
      : Location(Stack, oldNumLocs - MaxRegs)
    ;
  }

  // Returns whether either there is a free register location available
  // in the given set, or one can be created.
  // if true, the next call to getAvailLoc is guaranteed to produce a Register
  // location (assuming the given set or the fields don't change).
  bool hasFreeRegister(set<Location> &FreeLocs) {
    return (!FreeLocs.empty() && (FreeLocs.begin()->getType() == Register))
        || (NumLocs < MaxRegs);
  }

  // getCumLocs - Get the maximum number of locations
  // used by this scope and any of its children
  unsigned getCumLocs(void) const { return NumLocs + ChildrenMaxCumLocs; }

  void createLiveRanges(void) {
    // create live range infomation for each predicate
    DEBUG(dbgs() << " Create live-ranges for [MBB#"
                 << Pub.Scope->getHeader()->getMBB()->getNumber() << "]\n");

    auto blocks = Pub.Scope->getBlocksTopoOrd();
    for (unsigned i = 0, e = blocks.size(); i < e; i++) {
      MachineBasicBlock *MBB = blocks[i];
      // insert use
      LRs[Pub.Scope->getPredUse(MBB)].addUse(i);

      // insert defs
      auto opDI = Pub.Scope->getDefInfo(MBB);
      if (opDI.is_initialized()) {
        auto DI = get(opDI);
        for (auto pi = DI.begin(), pe = DI.end();
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
      LRs[0].addUse(blocks.size());
    }
  }

  void assignLocations(void) {
    DEBUG(dbgs() << " Assign locations for [MBB#"
                 << Pub.Scope->getHeader()->getMBB()->getNumber() << "]\n");
    SPNumPredicates += Pub.Scope->getNumPredicates(); // STATISTIC

    set<Location> FreeLocs;

    // map to keep track of locations of predicates during the scan
    map<unsigned, Location> curLocs;

    auto blocks = Pub.Scope->getBlocksTopoOrd();
    for (unsigned i = 0, e = blocks.size(); i < e; i++) {
      MachineBasicBlock *MBB = blocks[i];

      DEBUG( dbgs() << "  MBB#" << MBB->getNumber() << ": " );

      // (1) handle use
      handlePredUse(i, MBB, curLocs, FreeLocs);

      // (3) handle definitions in this basic block.
      //     if we need to get new locations for predicates (loc==-1),
      //     assign new ones in nearest-next-use order
      auto opDI = Pub.Scope->getDefInfo(MBB);
      if (opDI.is_initialized()) {
        auto DI = get(opDI);
        vector<unsigned> order;
        for (auto pi = DI.begin(), pe = DI.end();
            pi != pe; ++pi) {
          int r = pi->first;
          if (curLocs.find(r) == curLocs.end()) {
            // need to get a new loc for predicate r
            order.push_back(r);
          }
        }
        sortFurthestNextUse(i, order);
        // nearest use is in front
        for (unsigned j=0; j<order.size(); j++) {
          unsigned pred = order[j];
          Location l = getAvailLoc(FreeLocs);
          map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(pred);
          if(findCurUseLoc == curLocs.end()){
            curLocs.insert(make_pair(pred, l));
          }else{
            findCurUseLoc->second = l;
          }
          DefLocs[pred] = make_optional(l);
          assert(curLocs.find(pred)->second.getLoc() == get(DefLocs[pred]).getLoc());
          DEBUG( dbgs() << "def " << pred << " in loc "
                        << get(DefLocs[pred]).getLoc() << ", ");
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
      UseLoc &ul = UseLocs.at(Pub.Scope->getHeader()->getMBB());
      map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(0);
      assert(findCurUseLoc != curLocs.end());
      if (ul.loc != findCurUseLoc->second.getLoc()) {
        ul.load = make_optional(findCurUseLoc->second.getLoc());
      }
    }
  }

  /// Converts a register index into a global index that takes parent
  /// into account.
  unsigned unifyRegister(unsigned idx){
    // We don't have to check whether the result is larger that the number
    // of available registers, because we know the parent will spill
    // if that is the case.
    return idx + FirstUsableReg;
  }

  /// Converts a Stack spill slot index into a global index that takes parent
  /// into account.
  unsigned unifyStack(unsigned idx){
    return idx + FirstUsableStackSlot;
  }

  /// Unifies with parent, such that this RAInfo knows which registers it can use
  /// and where its spill slots are.
  void unifyWithParent(const RAInfo::Impl &parent, int parentSpillLocCnt, bool topLevel){

      // We can avoid a spill if the total number of locations
      // used by the parent, this instance, and any child is less
      // than/equal to the number of registers available to the function.
      if ( !topLevel && parent.NumLocs + getCumLocs() <= MaxRegs ) {

        // Compute the first register not used by an ancestor.
        FirstUsableReg = parent.FirstUsableReg + parent.NumLocs;

        // If the total number of locations the parent, myself, and my children need
        // are less than/equal to the number of available registers
        // we do not have to spill any predicates.
        NeedsScopeSpill = false;
      }

    if (NumLocs > MaxRegs) {
      FirstUsableStackSlot = parentSpillLocCnt;
    }
  }

  /// Unifies with child, such that this RAInfo knows how many locations will
  /// be used by the given child.
  void unifyWithChild(const RAInfo::Impl &child){
    ChildrenMaxCumLocs = max(child.getCumLocs(), ChildrenMaxCumLocs);
  }
private:
  UseLoc calculateNotHeaderUseLoc(unsigned blockIndex, unsigned usePred,
      map<unsigned, Location>& curLocs, set<Location>& FreeLocs)
  {
    map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(usePred);
    assert(findCurUseLoc != curLocs.end());
    // each use must be preceded by a location assignment
    Location& curUseLoc = findCurUseLoc->second;
    // if previous location was not a register, we have to allocate
    // a register and/or possibly spill
    if (curUseLoc.getType() != Register) {
      return handleIfNotInRegister(blockIndex, FreeLocs, curLocs,
          findCurUseLoc);
    } else {
      // everything stays as is
      return UseLoc(curUseLoc.getLoc());
    }
  }

  UseLoc calculateHeaderUseLoc(set<Location>& FreeLocs, map<unsigned, Location>& curLocs) {

    // we get a loc for the header predicate
    Location loc = getAvailLoc(FreeLocs);
    UseLoc UL(loc.getLoc());
    DefLocs[0] = make_optional(loc);
    map<unsigned, Location>::iterator curLoc0 = curLocs.find(0);
    if(curLoc0 == curLocs.end()){
      curLocs.insert(make_pair(0, loc));
    }else{
      curLoc0->second = loc;
    }
    assert(UL.loc == 0);
    return UL;
  }

  void handlePredUse(unsigned i, MachineBasicBlock* MBB,
      map<unsigned, Location>& curLocs, set<Location>& FreeLocs) {

    unsigned usePred = Pub.Scope->getPredUse(MBB);

    // TODO:(Emad) handle multiple predicates.
    // for the top-level entry of a single-path root,
    // we don't need to assign a location, as we will use p0
    if (!(usePred == 0 && Pub.Scope->isRootTopLevel())) {
      assert(MBB == Pub.Scope->getHeader()->getMBB() || i > 0);

      assert(!UseLocs.count(MBB));
      UseLocs.insert(make_pair(MBB,
        (Pub.Scope->isHeader(MBB)) ?
          calculateHeaderUseLoc(FreeLocs, curLocs)
          : calculateNotHeaderUseLoc(i, usePred, curLocs, FreeLocs)
      ));

      //DEBUG( dbgs() << "new " << curUseLoc << ". ");
      // (2) retire locations
      if (LRs[usePred].lastUse(i)) {
        DEBUG(dbgs() << "retire. ");
    	  map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(usePred);
        assert(findCurUseLoc != curLocs.end());
        Location& curUseLoc = findCurUseLoc->second;

        // free location, also removing it from the current one is use
        assert(!FreeLocs.count(curUseLoc));
        FreeLocs.insert(curUseLoc);
        curLocs.erase(findCurUseLoc);
      }
    }
  }

  UseLoc handleIfNotInRegister(unsigned blockIndex, set<Location>& FreeLocs,
      map<unsigned, Location>& curLocs,
      map<unsigned, Location>::iterator& findCurUseLoc)
  {

    assert(findCurUseLoc != curLocs.end());
    Location& curUseLoc = findCurUseLoc->second;

    if (hasFreeRegister(FreeLocs)) {
      Location newLoc = getAvailLoc(FreeLocs);
      UseLoc UL(newLoc.getLoc());
      UL.load = make_optional(curUseLoc.getLoc());
      curUseLoc = newLoc;
      assert(UL.loc <= MaxRegs);
      return UL;
    } else {

      // spill and reassign
      // order predicates wrt furthest next use
      vector<unsigned> order;
      for (unsigned j = 0; j < LRs.size(); j++) {
        // consider all physical registers in use
        map<unsigned, Location>::iterator cj = curLocs.find(j);
        if (cj != curLocs.end() && cj->second.getType() == Register) {
          order.push_back(j);
        }
      }
      sortFurthestNextUse(blockIndex, order);
      unsigned furthestPred = order.back();

      Location stackLoc = getAvailLoc(FreeLocs); // guaranteed to be a stack location, since there are no physicals free

      assert(stackLoc.getType() == Stack);
      map<unsigned, Location>::iterator findFurthest = curLocs.find(
          furthestPred);
      assert(findFurthest != curLocs.end());

      UseLoc UL(findFurthest->second.getLoc());
      UL.load = make_optional(curUseLoc.getLoc());

      // differentiate between already used and not yet used
      if (LRs[furthestPred].anyUseBefore(blockIndex)) {
        UL.spill = make_optional(stackLoc.getLoc());
      } else {
        // if it has not been used, we change the initial
        // definition location
        DefLocs[furthestPred] = make_optional(stackLoc);
      }
      curUseLoc = findFurthest->second;
      findFurthest->second = stackLoc;
      return UL;
    }
  }

  /// Sorts the given vector of predicates according to the
  /// furthest next use from the given MBB position.
  void sortFurthestNextUse(unsigned pos, vector<unsigned>& order) {
    sort(order.begin(), order.end(), [this, pos](int a, int b){
      return LRs[a].hasNextUseBefore(pos, LRs[b]);
    });
  }
};

///////////////////////////////////////////////////////////////////////////////
//  RAInfo methods
///////////////////////////////////////////////////////////////////////////////

RAInfo::RAInfo(SPScope *S, unsigned availRegs) :
  Scope(S), Priv(spimpl::make_unique_impl<Impl>(this, S, availRegs))
  {}

bool RAInfo::needsScopeSpill(void) const {
  return Priv->NeedsScopeSpill;
}

bool RAInfo::isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const {
  // header predicate
  if (pred == 0) return false;

  auto blocks = Scope->getBlocksTopoOrd();
  for(unsigned i=0; i< blocks.size(); i++) {
    if (blocks[i] == MBB) {
      return !Priv->LRs[pred].hasDefBefore(i);
    }
  }
  return false;
}

bool RAInfo::hasSpillLoad(const MachineBasicBlock *MBB) const {
  if (Priv->UseLocs.count(MBB)) {
    const Impl::UseLoc &ul = Priv->UseLocs.at(MBB);
    return (ul.spill.is_initialized()) || (ul.load.is_initialized());
  }
  return false;
}

optional<unsigned> RAInfo::getUseLoc(const MachineBasicBlock *MBB) const {
  if (Priv->UseLocs.count(MBB)) {
    unsigned loc = Priv->unifyRegister(Priv->UseLocs.at(MBB).loc);
    assert( loc < Priv->MaxRegs );
    return make_optional(loc);
  }
  return none;
}

optional<unsigned> RAInfo::getLoadLoc(const MachineBasicBlock *MBB) const {
  if (Priv->UseLocs.count(MBB)) {
    optional<unsigned> loc = Priv->UseLocs.at(MBB).load;
    if (loc.is_initialized()) {
      return make_optional(Priv->unifyStack(get(loc)));
    }
  }
  return none;
}

optional<unsigned> RAInfo::getSpillLoc(const MachineBasicBlock *MBB) const {
  if (Priv->UseLocs.count(MBB)) {
    optional<unsigned> loc = Priv->UseLocs.at(MBB).spill;
    if (loc.is_initialized()) {
      return make_optional(Priv->unifyStack(get(loc)));
    }
  }
  return none;
}

tuple<RAInfo::LocType, unsigned> RAInfo::getDefLoc(unsigned pred) const {
  optional<Location> locOpt = Priv->DefLocs[pred];
  assert(locOpt.is_initialized());
  Location loc = get(locOpt);
  if( loc.getType() == RAInfo::Register){
    return make_tuple(loc.getType(), Priv->unifyRegister(loc.getLoc()));
  }else{
    return make_tuple(loc.getType(), Priv->unifyStack(loc.getLoc()));
  }
}

unsigned RAInfo::neededSpillLocs(){
  if(Priv->NumLocs < Priv->MaxRegs) {
    return 0;
  }else{
    return Priv->NumLocs - Priv->MaxRegs;
  }
}

void RAInfo::dump() const {
  dbgs() << "[MBB#"     << Scope->getHeader()->getMBB()->getNumber()
         <<  "] depth=" << Scope->getDepth() << "\n";

  for(unsigned i=0; i<Priv->LRs.size(); i++) {
    const LiveRange &LR = Priv->LRs[i];
    dbgs() << "  LR(p" << i << ") = [" << LR.str() << "]\n";
  }

  auto blocks = Scope->getBlocksTopoOrd();
  for (unsigned i=0, e=blocks.size(); i<e; i++) {
    MachineBasicBlock *MBB = blocks[i];
    dbgs() << "  " << i << "| MBB#" << MBB->getNumber();
    if (Priv->UseLocs.count(MBB)) {
      const Impl::UseLoc &UL = Priv->UseLocs.at(MBB);
      dbgs() << "  loc=" << UL.loc << " load=";
      if (UL.load.is_initialized()) {
        dbgs() << get(UL.load);
      }else{
        dbgs() << "none";
      }
      dbgs() << " spill=";
      if (UL.spill.is_initialized()) {
        dbgs() << get(UL.spill);
      }else{
        dbgs() << "none";
      }
    }
    dbgs() << "\n";
  }

  dbgs() << "  DefLocs:     ";
  for (unsigned j=0; j<Priv->DefLocs.size(); j++) {
    dbgs() << " p" << j << "=";
    if( Priv->DefLocs[j].is_initialized() ) {
      dbgs() << "optional(" << get(Priv->DefLocs[j]) << ")";
    }else{
      dbgs() << "none";
    }
  }
  dbgs() << "\n";

  dbgs() << "  NumLocs:      " << Priv->NumLocs << "\n"
            "  CumLocs:      " << Priv->getCumLocs() << "\n"
            "  Offset:       " << Priv->FirstUsableReg  << "\n"
            "  SpillOffset:  " << Priv->FirstUsableStackSlot  << "\n";
}

std::map<const SPScope*, RAInfo> RAInfo::computeRegAlloc(PatmosSinglePathInfo *PSPI, unsigned AvailPredRegs){

  std::map<const SPScope*, RAInfo> RAInfos;
  // perform reg-allocation in post-order to compute cumulative location
  // numbers in one go
  for (po_iterator<PatmosSinglePathInfo*> I = po_begin(PSPI), E = po_end(PSPI);
      I!=E; ++I) {
    SPScope *S = *I;
    // create RAInfo for SPScope
    RAInfos.insert(std::make_pair(S, RAInfo(S,  AvailPredRegs)));
    RAInfo &RI = RAInfos.at(S);

    // Because this is a post-order traversal, we have already visited
    // all children of the current scope (S). Synthesize the cumulative number of locations
    for(SPScope::child_iterator CI = S->child_begin(), CE = S->child_end();
        CI != CE; ++CI) {
      SPScope *CN = *CI;
      RI.Priv->unifyWithChild(*(RAInfos.at(CN).Priv));
    }
  } // end of PO traversal for RegAlloc


  // Visit all scopes in depth-first order to compute offsets:
  // - Offset is inherited during traversal
  // - SpillOffset is assigned increased depth-first, from left to right
  unsigned spillLocCnt = 0;
  for (df_iterator<PatmosSinglePathInfo*> I = df_begin(PSPI), E = df_end(PSPI);
        I!=E; ++I) {
    SPScope *S = *I;
    RAInfo &RI = RAInfos.at(S);

    if (!S->isTopLevel()) {
       RI.Priv->unifyWithParent(*(RAInfos.at(S->getParent()).Priv), spillLocCnt, S->isTopLevel());
      if (!RI.needsScopeSpill()) NoSpillScopes++; // STATISTIC
    }
    spillLocCnt += RI.neededSpillLocs();
    DEBUG( RI.dump() );
  } // end df

  PredSpillLocs += spillLocCnt; // STATISTIC
  return RAInfos;
}

