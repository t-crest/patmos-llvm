//==-- PatmosISelLowering.h - Patmos DAG Lowering Interface ------*- C++ -*-==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that Patmos uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#ifndef _LLVM_TARGET_PATMOS_ISELLOWERING_H_
#define _LLVM_TARGET_PATMOS_ISELLOWERING_H_

#include "Patmos.h"
#include "llvm/CodeGen/MachineJumpTableInfo.h"
#include "llvm/CodeGen/SelectionDAG.h"
#include "llvm/Target/TargetLowering.h"

namespace llvm {
  namespace PatmosISD {
    enum {
      FIRST_NUMBER = ISD::BUILTIN_OP_END,

      /// Return with a flag operand. Operand 0 is the chain operand.
      RET_FLAG,

      /// multiplication
      MUL, MULU,

      /// CALL - These operations represent an abstract call
      /// instruction, which includes a bunch of information.
      CALL = ISD::FIRST_TARGET_MEMORY_OPCODE
    };
  } // end namespace PatmosISD

  class PatmosSubtarget;
  class PatmosTargetMachine;

  class PatmosTargetLowering : public TargetLowering {
  public:
    explicit PatmosTargetLowering(PatmosTargetMachine &TM);

    /// LowerOperation - Provide custom lowering hooks for some operations.
    virtual SDValue LowerOperation(SDValue Op, SelectionDAG &DAG) const;

    /// getTargetNodeName - This method returns the name of a target specific
    /// DAG node.
    virtual const char *getTargetNodeName(unsigned Opcode) const;

    virtual EVT getSetCCResultType(EVT VT) const;

    /******************************************************************
     * Jump Tables
     ******************************************************************/

    /// getJumpTableEncoding - Return the entry encoding for a jump table in the
    /// current function.  The returned value is a member of the
    /// MachineJumpTableInfo::JTEntryKind enum.
    virtual unsigned getJumpTableEncoding() const {
      return MachineJumpTableInfo::EK_Custom32;
    }

    virtual const MCExpr *
    LowerCustomJumpTableEntry(const MachineJumpTableInfo * MJTI,
                              const MachineBasicBlock * MBB, unsigned uid,
                              MCContext &OutContext) const;

    /******************************************************************
     * Addressing and Offsets
     ******************************************************************/

    /* TODO needed? should return correct values for Patmos here
    /// isOffsetFoldingLegal - Return true if folding a constant offset
    /// with the given GlobalAddress is legal.  It is frequently not legal in
    /// PIC relocation models.
    virtual bool isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const;

    /// getMaximalGlobalOffset - Returns the maximal possible offset which can be
    /// used for loads / stores from the global.
    virtual unsigned getMaximalGlobalOffset() const {
      return 0;
    }
    */

    /*
    virtual bool getPostIndexedAddressParts(SDNode *N, SDNode *Op,
                                            SDValue &Base,
                                            SDValue &Offset,
                                            ISD::MemIndexedMode &AM,
                                            SelectionDAG &DAG) const;
    */

    /******************************************************************
     * Inline asm support
     ******************************************************************/

    ConstraintType getConstraintType(const std::string &Constraint) const;

    virtual std::pair<unsigned, const TargetRegisterClass*>
      getRegForInlineAsmConstraint(const std::string &Constraint,
                                   EVT VT) const;

  private:
    const PatmosSubtarget &Subtarget;
    const DataLayout *TD;

    SDValue LowerCCCCallTo(CallLoweringInfo &CLI,
                           SmallVectorImpl<SDValue> &InVals) const;

    SDValue LowerCCCArguments(SDValue Chain,
                              CallingConv::ID CallConv,
                              bool isVarArg,
                              const SmallVectorImpl<ISD::InputArg> &Ins,
                              DebugLoc dl,
                              SelectionDAG &DAG,
                              SmallVectorImpl<SDValue> &InVals) const;

    SDValue LowerCallResult(SDValue Chain, SDValue InFlag,
                            CallingConv::ID CallConv, bool isVarArg,
                            const SmallVectorImpl<ISD::InputArg> &Ins,
                            DebugLoc dl, SelectionDAG &DAG,
                            SmallVectorImpl<SDValue> &InVals) const;
  public:
    virtual SDValue LowerCall(CallLoweringInfo &CLI,
                      SmallVectorImpl<SDValue> &InVals) const;

    virtual SDValue LowerFormalArguments(SDValue Chain,
                                 CallingConv::ID CallConv, bool isVarArg,
                                 const SmallVectorImpl<ISD::InputArg> &Ins,
                                 DebugLoc dl, SelectionDAG &DAG,
                                 SmallVectorImpl<SDValue> &InVals) const;

    virtual SDValue LowerReturn(SDValue Chain,
                        CallingConv::ID CallConv, bool isVarArg,
                        const SmallVectorImpl<ISD::OutputArg> &Outs,
                        const SmallVectorImpl<SDValue> &OutVals,
                        DebugLoc dl, SelectionDAG &DAG) const;

    /// LowerVASTART - Lower the va_start intrinsic to access parameters of
    /// variadic functions.
    SDValue LowerVASTART(SDValue Op, SelectionDAG &DAG) const;

    /// LowerFRAMEADDR - Lower the llvm.frameaddress intrinsic.
    SDValue LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const;

    /// LowerMUL_LOHI - Lower Lo/Hi multiplications.
    SDValue LowerMUL_LOHI(SDValue Op, SelectionDAG &DAG) const;

    /// LowerSTORE - Promote i1 store operations to i8.
    SDValue LowerSTORE(SDValue Op, SelectionDAG &DAG) const;

    /// LowerLOAD - Promote i1 load operations to i8.
    SDValue LowerLOAD(SDValue Op, SelectionDAG &DAG) const;
  };




} // namespace llvm

#endif // _LLVM_TARGET_PATMOS_ISELLOWERING_H_







