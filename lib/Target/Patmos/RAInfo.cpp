

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosSubtarget.h"
#include "PatmosTargetMachine.h"
#include "llvm/IR/Function.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineDominators.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/MachinePostDominators.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/Format.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/raw_ostream.h"

#include "PatmosSinglePathInfo.h"

#include "boost/optional.hpp"

#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <sstream>
#include <iostream>

#include "RAInfo.h"

using namespace llvm;
using namespace boost;


bool llvm::operator<(const Location&l, const Location &r){
  if( l.getType() == r.getType()){
    return l.getLoc() < r.getLoc();
  } else {
    return false;
  }
}

///////////////////////////////////////////////////////////////////////////////
//  RAInfo methods
///////////////////////////////////////////////////////////////////////////////

// Returns the first available location in the given set, removing it from the set.
// If the set is empty, a new Location is created and returned.
Location RAInfo::getAvailLoc(std::set<Location> &FreeLocs) {
  if (!FreeLocs.empty()) {
    std::set<Location>::iterator it = FreeLocs.begin();
    FreeLocs.erase(it);
    return *it;
  }

  // create a new location
  return Location(
      NumLocs < AvailRegs ? Location::Register : Location::Stack,
      NumLocs++
    );
}


// Returns whether either there is a free register location available
// in the given set, or one can be created.
// if true, the next call to getAvailLoc is guaranteed to produce a Register
// location (assuming the given set or the fields don't change).
bool RAInfo::hasFreePhys(std::set<Location> &FreeLocs) {
  return (!FreeLocs.empty() && (FreeLocs.begin()->getType() == Location::Register))
      || (NumLocs < AvailRegs);
}

// Returns whether the given register location is a physical
// register location.
bool RAInfo::isPhysRegLoc(int loc) const {
  return loc < (int)AvailRegs;
}

// getCumLocs - Get the maximum number of locations
// used by this scope and any of its children
unsigned RAInfo::getCumLocs(void) const { return NumLocs + ChildrenMaxCumLocs; }

// assignSpillOffset
void RAInfo::assignSpillOffset(unsigned spillOffset) {
  // assign the spill offset, increment
  if (NumLocs > AvailRegs) {
    this->SpillOffset = spillOffset;
  }
}

RAInfo::RAInfo(SPScope *S, unsigned availRegs) :
  Scope(S), AvailRegs(availRegs),
  LRs(S->getNumPredicates(), LiveRange(S)),
  DefLocs(S->getNumPredicates(),-1),
  NumLocs(0), ChildrenMaxCumLocs(0), Offset(0), SpillOffset(0),
  NeedsScopeSpill(true) {
    createLiveRanges();
    assignLocations();
  }

// needsScopeSpill - Returns true if S0 must be spilled/restored
// upon entry/exit of this SPScope.
bool RAInfo::needsScopeSpill(void) const {
  return NeedsScopeSpill;
}

// isFirstDef - Returns true if the given MBB contains the first
// definition of the given predicate.
// This has to be false for a header predicate, as the first
// definition is before the loop is entered
bool RAInfo::isFirstDef(const MachineBasicBlock *MBB, unsigned pred) const {
  // header predicate
  if (pred == 0) return false;

  for(unsigned i=0; i< Scope->getBlocks().size(); i++) {
    if (Scope->getBlocks()[i] == MBB) {
      return !LRs[pred].hasDefBefore(i);
    }
  }
  return false;
}

// hasSpillLoad - Returns true if the use location of the given MBB
// requires the predicate register to be first spilled and/or loaded
// to/from a spill location.
bool RAInfo::hasSpillLoad(const MachineBasicBlock *MBB) const {
  if (UseLocs.count(MBB)) {
    const UseLoc &ul = UseLocs.at(MBB);
    return (ul.spill!=-1) || (ul.load!=-1);
  }
  return false;
}

/// getUseLoc - Get the use location, which is a register location,
/// for the given MBB. If the MBB is not mapped to a location the empty
/// option is returned.
optional<Location> RAInfo::getUseLoc(const MachineBasicBlock *MBB) const {
  if (UseLocs.count(MBB)) {
    int loc = UseLocs.at(MBB).loc + Offset;
    assert( loc < (int)AvailRegs );
    return optional<Location>{Location(Location::Register, loc)};
  }
  return optional<Location>{};
}

/// getLoadLoc - Get the load location, which is a spill location,
/// for the given MBB. Returns the empty option if the predicate does not need to be
/// loaded from a spill slot.
optional<unsigned> RAInfo::getLoadLoc(const MachineBasicBlock *MBB) const {
  if (UseLocs.count(MBB)) {
    int loc = UseLocs.at(MBB).load;
    if (loc != -1) {
      assert( loc >= (int)AvailRegs );
      return optional<unsigned>{(loc - AvailRegs) + SpillOffset};
    }
  }
  return optional<unsigned>{};
}

/// getSpillLoc - Get the spill location, i.e. the location where the use
/// register has to be spilled first, for the given MBB.
/// Returns -1 if it does not need to be spilled.
int RAInfo::getSpillLoc(const MachineBasicBlock *MBB) const {
  if (UseLocs.count(MBB)) {
    int loc = UseLocs.at(MBB).spill;
    if (loc != -1) {
      assert( loc >= (int)AvailRegs );
      return (loc - AvailRegs) + SpillOffset;
    }
  }
  return -1;
}

/// Returns the definition location for the given predicate (as the first element)
/// and whether that location is a physical register (in the second element).
///
std::tuple<unsigned, bool> RAInfo::getDefLoc(unsigned pred) const {
  int dloc = DefLocs[pred];
  assert(dloc != -1);
  bool isreg = isPhysRegLoc(dloc);
  unsigned loc;
  if (isreg) {
    loc = dloc + Offset;
  } else {
    loc = (dloc - AvailRegs) + SpillOffset;
  }
  return std::make_tuple(loc, isreg);
}

// Unifies with parent, such that this RAInfo knows which registers it can use
// and where is spill slots are
void RAInfo::unifyWithParent(const RAInfo &parent, int parentSpillLocCnt, bool topLevel){

  // If we want to try to minimize the number of spills
  #ifdef NOSPILL_OPTIMIZATION
    // check if we must spill the PRegs
    // Parent.num + S.cum <= size  --> no spill!
    if ( !topLevel && parent.NumLocs + getCumLocs() <= AvailRegs ) {

      // compute offset, i.e. the first register not used by the parent.
      Offset = parent.NumLocs + parent.Offset;

      // If the total number of locations the parent, myself, and my children need
      // are less than/equal to the number of available registers
      // we do not have to spill any predicates.
      NeedsScopeSpill = false;
    }
  #endif
  assignSpillOffset(parentSpillLocCnt);
}

void RAInfo::unifyWithChild(const RAInfo &child){
  ChildrenMaxCumLocs = std::max(child.getCumLocs(), ChildrenMaxCumLocs);
}

// How many spill slots this RAInfo needs.
unsigned RAInfo::neededSpillLocs(){
  if(NumLocs > AvailRegs) {
    return 0;
  }else{
    return NumLocs - AvailRegs;
  }
}


void RAInfo::createLiveRanges(void) {
  // create live range infomation for each predicate
  DEBUG(dbgs() << " Create live-ranges for [MBB#"
               << Scope->getHeader()->getNumber() << "]\n");


  for (unsigned i=0, e=Scope->getBlocks().size(); i<e; i++) {
    MachineBasicBlock *MBB = Scope->getBlocks()[i];
    // insert use
    const std::vector<unsigned> *predUses = Scope->getPredUse(MBB);
    std::for_each(predUses->begin(), predUses->end(), [&](unsigned p){
      LRs[p].addUse(i);
    });
    // insert defs
    const SPScope::PredDefInfo *DI = Scope->getDefInfo(MBB);
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
  if (!Scope->isTopLevel()) {
    LRs[0].addUse(Scope->getBlocks().size());
  }
}


void RAInfo::assignLocations(void) {
  DEBUG(dbgs() << " Assign locations for [MBB#"
               << Scope->getHeader()->getNumber() << "]\n");
  //SPNumPredicates += Scope->getNumPredicates(); // STATISTIC

  std::set<Location> FreeLocs;

  // map to keep track of locations of predicates during the scan
  std::map<unsigned, Location> curLocs;

  for (unsigned i=0, e=Scope->getBlocks().size(); i<e; i++) {
    MachineBasicBlock *MBB = Scope->getBlocks()[i];

    DEBUG( dbgs() << "  MBB#" << MBB->getNumber() << ": " );

    // (1) handle use
    unsigned usePred = (*Scope->getPredUse(MBB))[0];
    // TODO:(Emad) handle multiple predicates.

    // for the top-level entry of a single-path root,
    // we don't need to assign a location, as we will use p0
    if (!(usePred==0 && Scope->isRootTopLevel())) {
      UseLoc UL;

      std::map<unsigned, Location>::iterator findCurUseLoc = curLocs.find(usePred);

      assert(MBB == Scope->getHeader() || i>0);

      if (MBB != Scope->getHeader()) {
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
            if((UL.load < (int)AvailRegs) || (UL.loc > (int)AvailRegs)){
              errs() << __FILE__ <<":" << __LINE__ << " UL has wrong values: load(" << UL.load << "), loc(" << UL.loc << "), AvailRegs(" << AvailRegs << ")\n";
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
            if(stackLoc.getLoc() < AvailRegs){
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
    const SPScope::PredDefInfo *DI = Scope->getDefInfo(MBB);
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
  if (!Scope->isTopLevel()) {
    UseLoc &ul = UseLocs[Scope->getHeader()];
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


void RAInfo::dump() const {
  dbgs() << "[MBB#"     << Scope->getHeader()->getNumber()
         <<  "] depth=" << Scope->getDepth() << "\n";

  for(unsigned i=0; i<LRs.size(); i++) {
    const LiveRange &LR = LRs[i];
    dbgs() << "  LR(p" << i << ") = [" << LR.str() << "]\n";
  }

  for (unsigned i=0, e=Scope->getBlocks().size(); i<e; i++) {
    MachineBasicBlock *MBB = Scope->getBlocks()[i];

    dbgs() << "  " << i << "| MBB#" << MBB->getNumber();
    if (UseLocs.count(MBB)) {
      const UseLoc &UL = UseLocs.at(MBB);
      dbgs() << "  loc=" << UL.loc << " load=" << UL.load
          << " spill=" << UL.spill;
    }
    dbgs() << "\n";
  }

  dbgs() << "  DefLocs:     ";
  for (unsigned j=0; j<DefLocs.size(); j++) {
    dbgs() << " p" << j << "=" << DefLocs[j];
  }
  dbgs() << "\n";

  dbgs() << "  NumLocs:      " << NumLocs << "\n"
            "  CumLocs:      " << getCumLocs() << "\n"
            "  Offset:       " << Offset  << "\n"
            "  SpillOffset:  " << SpillOffset  << "\n";
}



