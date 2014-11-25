#ifndef _LLVM_TARGET_PATMOS_SWSC_HELPER_H
#define _LLVM_TARGET_PATMOS_SWSC_HELPER_H



namespace llvm {

  class MachineFunction;

  void ReserveRegsSWSC(const MachineFunction &MF, BitVector &Reserved);
}

#endif
