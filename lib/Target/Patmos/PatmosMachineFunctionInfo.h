//===- PatmosMachineFuctionInfo.h - Patmos machine function info -*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares Patmos-specific per-machine-function information.
//
//===----------------------------------------------------------------------===//

#ifndef _PATMOS_MACHINEFUNCTIONINFO_H_
#define _PATMOS_MACHINEFUNCTIONINFO_H_

#include "llvm/ADT/BitVector.h"
#include "llvm/CodeGen/MachineFunction.h"

#include <limits>
#include <set>
#include <vector>
#include <map>

namespace llvm {

/// PatmosAnalysisInfo - Store information from different analyses and from
/// (WCET) profiling.
/// TODO There might be one instance per context, provide functions to manage
///      context-sensitive results (merge, get-per-context,..).
/// TODO Does it make sense to have all analyses share one class? Probably yes,
///      to make the code simpler, but analyses might use different contexts.
class PatmosAnalysisInfo {
private:
  typedef std::map<const MachineBasicBlock*, double> CritMap;
  typedef std::map<const MachineBasicBlock*, uint64_t> FreqMap;

  CritMap BlockCriticalitites;

  FreqMap BlockFrequencies;

public:

  double getCriticality(const MachineBasicBlock *MBB, double Default = -1.0) {
    CritMap::iterator it = BlockCriticalitites.find(MBB);
    if (it != BlockCriticalitites.end()) {
      return it->second;
    }
    return Default;
  }

  void setCriticality(const MachineBasicBlock *MBB, double Crit) {
    BlockCriticalitites.insert(std::make_pair(MBB, Crit));
  }

  int64_t getFrequency(const MachineBasicBlock *MBB, int64_t Default = -1) {
    FreqMap::iterator it = BlockFrequencies.find(MBB);
    if (it != BlockFrequencies.end()) {
      return it->second;
    }
    return Default;
  }

  void setFrequency(const MachineBasicBlock *MBB, uint64_t Freq) {
    BlockFrequencies.insert(std::make_pair(MBB, Freq));
  }

};

/// PatmosMachineFunctionInfo - This class is derived from MachineFunction and
/// contains private Patmos target-specific information for each
/// MachineFunction.
class PatmosMachineFunctionInfo : public MachineFunctionInfo {
  /// StackCacheReservedSize - Size in bytes that need to be reserved on the
  /// stack cache.
  unsigned StackCacheReservedBytes;

  /// StackReservedSize - Size in bytes that need to be reserved on the shadow 
  /// stack.
  unsigned StackReservedBytes;

  /// StackCacheFIs - Set of FIs assigned to the stack cache.
  BitVector StackCacheFIs;

  /// VarArgsFI - FrameIndex to access parameters of variadic functions.
  int VarArgsFI;

  /// RegScavengingFI - FrameIndex for an emergency spill slot.
  int RegScavengingFI;

  /// Register used to spill s0 to instead of the stack cache.
  unsigned S0SpillReg;

  /// True if this function is to be single-path converted
  bool SinglePathConvert;

  /// FIs created for SinglePath conversion
  /// | LoopCnts | S0Spills | ExcessSpills |
  std::vector<int> SinglePathFIs;

  // Index to the SinglePathFIs where the S0 spill slots start
  unsigned SPS0SpillOffset;

  // Index to the SinglePathFIs where the excess spill slots start
  unsigned SPExcessSpillOffset;

  // Index to the SinglePathFIs where the call spill slots start (R9)
  unsigned SPCallSpillOffset;

  /// Set of entry blocks to code regions that are potentially cached by the
  /// method cache.
  std::set<const MachineBasicBlock*> MethodCacheRegionEntries;

  /// Store analysis results per function.
  PatmosAnalysisInfo AnalysisInfo;

  // do not provide any default constructor.
  PatmosMachineFunctionInfo();
public:
  explicit PatmosMachineFunctionInfo(MachineFunction &MF) :
    StackCacheReservedBytes(0), StackReservedBytes(0), VarArgsFI(0),
    RegScavengingFI(0), S0SpillReg(0),
    SinglePathConvert(false), SPS0SpillOffset(0), SPExcessSpillOffset(0),
    SPCallSpillOffset(0)
    {}

  /// getStackCacheReservedBytes - Get the number of bytes reserved on the
  /// stack cache.
  unsigned getStackCacheReservedBytes() const {
    return StackCacheReservedBytes;
  }

  /// setStackCacheReservedBytes - Set the number of bytes reserved on the stack
  /// cache.
  void setStackCacheReservedBytes(unsigned newSize) {
    StackCacheReservedBytes = newSize;
  }

  /// getStackReservedBytes - Get the number of bytes reserved on the shadow 
  /// stack.
  unsigned getStackReservedBytes() const {
    return StackReservedBytes;
  }

  /// setStackReservedBytes - Set the number of bytes reserved on the shadow 
  /// stack.
  void setStackReservedBytes(unsigned newSize) {
    StackReservedBytes = newSize;
  }

  /// getStackCacheFIs - Return the set of FIs assigned to the stack cache.
  const BitVector &getStackCacheFIs() const {
    return StackCacheFIs;
  }

  /// setStackCacheFIs - Set the set of FIs assigned to the stack cache.
  void setStackCacheFIs(const BitVector &fis) {
    StackCacheFIs = fis;
  }

  /// getVarArgsFI - Get the FI used to access parameters of variadic functions.
  unsigned getVarArgsFI() const {
    return VarArgsFI;
  }

  /// setVarArgsFI - Set the FI used to access parameters of variadic functions.
  void setVarArgsFI(int newFI) {
    VarArgsFI = newFI;
  }

  /// getRegScavengingFI - Get the FI used to access the emergency spill slot.
  unsigned getRegScavengingFI() const {
    return RegScavengingFI;
  }

  /// setRegScavengingFI - Set the FI used to access the emergency spill slot.
  void setRegScavengingFI(int newFI) {
    RegScavengingFI = newFI;
  }

  unsigned getS0SpillReg() const {
    return S0SpillReg;
  }

  void setS0SpillReg(unsigned Reg) {
    S0SpillReg = Reg;
  }

  /// addMethodCacheRegionEntry - Add the block to the set of method cache
  /// region entry blocks.
  /// \see MethodCacheRegionEntries
  void addMethodCacheRegionEntry(const MachineBasicBlock *MBB) {
    MethodCacheRegionEntries.insert(MBB);
  }

  /// isMethodCacheRegionEntry - Return whether the block is a method cache
  /// region entry.
  /// \see MethodCacheRegionEntries
  bool isMethodCacheRegionEntry(const MachineBasicBlock *MBB) const {
    return MethodCacheRegionEntries.find(MBB) != MethodCacheRegionEntries.end();
  }

  void setSinglePath(bool convert=true) {
    SinglePathConvert = convert;
  }

  bool isSinglePath(void) const {
    return SinglePathConvert;
  }

  void addSinglePathFI(int fi) {
    SinglePathFIs.push_back(fi);
  }

  void startSinglePathS0Spill(void) {
    SPS0SpillOffset = SinglePathFIs.size();
  }

  void startSinglePathExcessSpill(void) {
    SPExcessSpillOffset = SinglePathFIs.size();
  }

  void startSinglePathCallSpill(void) {
    SPCallSpillOffset = SinglePathFIs.size();
  }

  int getSinglePathLoopCntFI(unsigned num) const {
    return SinglePathFIs[0 + num];
  }

  int getSinglePathS0SpillFI(unsigned num) const {
    return SinglePathFIs[SPS0SpillOffset + num];
  }

  int getSinglePathExcessSpillFI(unsigned num) const {
    return SinglePathFIs[SPExcessSpillOffset + num];
  }

  int getSinglePathCallSpillFI(void) const {
    return SinglePathFIs[SPCallSpillOffset + 0];
  }

  const std::vector<int>& getSinglePathFIs(void) const {
    return SinglePathFIs;
  }

  unsigned getSinglePathFICnt(void) const {
    return SinglePathFIs.size();
  }

  PatmosAnalysisInfo &getAnalysisInfo() { return AnalysisInfo; }

  const PatmosAnalysisInfo &getAnalysisInfo() const { return AnalysisInfo; }

};

} // End llvm namespace

#endif // _PATMOS_MACHINEFUNCTIONINFO_H_
