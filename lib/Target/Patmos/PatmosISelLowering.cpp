//===-- PatmosISelLowering.cpp - Patmos DAG Lowering Implementation  ------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the PatmosTargetLowering class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "patmos-lower"

#include "PatmosISelLowering.h"
#include "Patmos.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "PatmosSubtarget.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Intrinsics.h"
#include "llvm/CallingConv.h"
#include "llvm/GlobalVariable.h"
#include "llvm/GlobalAlias.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/TargetLoweringObjectFileImpl.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;


PatmosTargetLowering::PatmosTargetLowering(PatmosTargetMachine &tm) :
  TargetLowering(tm, new TargetLoweringObjectFileELF()),
  Subtarget(*tm.getSubtargetImpl()), TM(tm) {

  TD = getTargetData();

  // Set up the register classes.
  // SRegs are not used for computations.
  addRegisterClass(MVT::i32, Patmos::RRegsRegisterClass);
  addRegisterClass(MVT::i1,  Patmos::PRegsRegisterClass);

  // Compute derived properties from the register classes
  computeRegisterProperties();

  // Provide all sorts of operation actions

  // Division is expensive
  setIntDivIsCheap(false);
  // Select is not
  setSelectIsExpensive(false);

  setStackPointerRegisterToSaveRestore(Patmos::RSP);
  setBooleanContents(ZeroOrOneBooleanContent);

  //setSchedulingPreference(Sched::Latency);

  setMinFunctionAlignment(4);
  setPrefFunctionAlignment(4);

  setLoadExtAction(ISD::EXTLOAD, MVT::i1, Promote);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);

  //TODO there are a bunch of operations which are not legal for i1 -> promote all
  setOperationAction(ISD::SIGN_EXTEND, MVT::i1, Promote);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i1, Promote);

  // Expand to S/UMUL_LOHI
  setOperationAction(ISD::MUL,   MVT::i32, Expand);
  setOperationAction(ISD::MULHS, MVT::i32, Expand);
  setOperationAction(ISD::MULHU, MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Custom);
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Custom);
  // Patmos has no DIV, REM or DIVREM operations.
  setOperationAction(ISD::SDIV, MVT::i32, Expand);
  setOperationAction(ISD::UDIV, MVT::i32, Expand);
  setOperationAction(ISD::SREM, MVT::i32, Expand);
  setOperationAction(ISD::UREM, MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  // we don't have carry setting add/sub instructions.
  // TODO custom lowering with predicates?
  setOperationAction(ISD::CARRY_FALSE, MVT::i32, Expand);
  setOperationAction(ISD::ADDC, MVT::i32, Expand);
  setOperationAction(ISD::SUBC, MVT::i32, Expand);
  setOperationAction(ISD::ADDE, MVT::i32, Expand);
  setOperationAction(ISD::SUBE, MVT::i32, Expand);
  // add/sub/mul with overflow
  setOperationAction(ISD::SADDO, MVT::i32, Expand);
  setOperationAction(ISD::UADDO, MVT::i32, Expand);
  setOperationAction(ISD::SSUBO, MVT::i32, Expand);
  setOperationAction(ISD::USUBO, MVT::i32, Expand);
  setOperationAction(ISD::SMULO, MVT::i32, Expand);
  setOperationAction(ISD::UMULO, MVT::i32, Expand);

  // no bit-fiddling
  setOperationAction(ISD::BSWAP, MVT::i32, Expand);
  setOperationAction(ISD::CTTZ , MVT::i32, Expand);
  setOperationAction(ISD::CTLZ , MVT::i32, Expand);
  setOperationAction(ISD::CTPOP, MVT::i32, Expand);

  setOperationAction(ISD::SELECT_CC, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC,     MVT::Other, Expand);
  // TODO at some point we want to support jumptables
  setOperationAction(ISD::BR_JT,     MVT::Other, Expand);

  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32  , Custom);

  // handling of variadic parameters
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAARG  , MVT::Other, Expand);
  setOperationAction(ISD::VACOPY , MVT::Other, Expand);
  setOperationAction(ISD::VAEND  , MVT::Other, Expand);
}


SDValue PatmosTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {

  switch (Op.getOpcode()) {
    case ISD::SMUL_LOHI:
    case ISD::UMUL_LOHI:          return LowerMUL_LOHI(Op, DAG);
    // alloca
    case ISD::DYNAMIC_STACKALLOC: return LowerDYNAMIC_STACKALLOC(Op, DAG);
    case ISD::VASTART:            return LowerVASTART(Op, DAG);
    default:
      llvm_unreachable("unimplemented operation");
  }
}

EVT PatmosTargetLowering::getSetCCResultType(EVT VT) const {
  // All our compare results should be i1
  return MVT::i1;
}


//===----------------------------------------------------------------------===//
//                      Custom Lower Operation
//===----------------------------------------------------------------------===//




SDValue PatmosTargetLowering::LowerMUL_LOHI(SDValue Op,
                                            SelectionDAG &DAG) const {
  unsigned MultOpc;
  EVT Ty = Op.getValueType();
  DebugLoc dl = Op.getDebugLoc();

  assert(Ty == MVT::i32 && "Unexpected type for MUL");

  MultOpc = (Op.getOpcode()==ISD::UMUL_LOHI)? PatmosISD::MULU
                                            : PatmosISD::MUL;


  SDValue Mul = DAG.getNode(MultOpc, dl, MVT::Glue,
                            Op.getOperand(0), Op.getOperand(1));
  SDValue InChain = DAG.getEntryNode();
  SDValue InGlue = Mul;

  if (!Op.getValue(0).use_empty()) {
    SDValue CopyFromLo = DAG.getCopyFromReg(InChain, dl,
        Patmos::SL, Ty, InGlue);
    DAG.ReplaceAllUsesOfValueWith(Op.getValue(0), CopyFromLo);
    InChain = CopyFromLo.getValue(1);
    InGlue = CopyFromLo.getValue(2);
  }
  if (!Op.getValue(1).use_empty()) {
    SDValue CopyFromHi = DAG.getCopyFromReg(InChain, dl,
        Patmos::SH, Ty, InGlue);
    DAG.ReplaceAllUsesOfValueWith(Op.getValue(1), CopyFromHi);
  }

  return Mul;
}




//===----------------------------------------------------------------------===//
//                      Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "PatmosGenCallingConv.inc"

SDValue
PatmosTargetLowering::LowerFormalArguments(SDValue Chain,
                                           CallingConv::ID CallConv,
                                           bool isVarArg,
                                           const SmallVectorImpl<ISD::InputArg>
                                             &Ins,
                                           DebugLoc dl,
                                           SelectionDAG &DAG,
                                           SmallVectorImpl<SDValue> &InVals)
                                             const {

  switch (CallConv) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::C:
  case CallingConv::Fast:
    return LowerCCCArguments(Chain, CallConv, isVarArg, Ins, dl, DAG, InVals);
  }
}




SDValue
PatmosTargetLowering::LowerCall(SDValue Chain, SDValue Callee,
                                CallingConv::ID CallConv, bool isVarArg,
                                bool doesNotRet, bool &isTailCall,
                                const SmallVectorImpl<ISD::OutputArg> &Outs,
                                const SmallVectorImpl<SDValue> &OutVals,
                                const SmallVectorImpl<ISD::InputArg> &Ins,
                                DebugLoc dl, SelectionDAG &DAG,
                                SmallVectorImpl<SDValue> &InVals) const {
  // Patmos target does not yet support tail call optimization.
  isTailCall = false;

  switch (CallConv) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::Fast:
  case CallingConv::C:
    return LowerCCCCallTo(Chain, Callee, CallConv, isVarArg, isTailCall,
                          Outs, OutVals, Ins, dl, DAG, InVals);
  }
}





/// LowerCCCArguments - transform physical registers into virtual registers and
/// generate load operations for arguments places on the stack.
// FIXME: struct return stuff
// FIXME: varargs
SDValue
PatmosTargetLowering::LowerCCCArguments(SDValue Chain,
                                        CallingConv::ID CallConv,
                                        bool isVarArg,
                                        const SmallVectorImpl<ISD::InputArg>
                                          &Ins,
                                        DebugLoc dl,
                                        SelectionDAG &DAG,
                                        SmallVectorImpl<SDValue> &InVals)
                                          const {

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  MachineRegisterInfo &RegInfo = MF.getRegInfo();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
		 getTargetMachine(), ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeFormalArguments(Ins, CC_Patmos);

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    if (VA.isRegLoc()) {
      // Arguments passed in registers
      EVT RegVT = VA.getLocVT();
      switch (RegVT.getSimpleVT().SimpleTy) {
      default:
        {
#ifndef NDEBUG
          errs() << "LowerFormalArguments Unhandled argument type: "
               << RegVT.getSimpleVT().SimpleTy << "\n";
#endif
          llvm_unreachable(0);
        }
      case MVT::i32:
        unsigned VReg =
          RegInfo.createVirtualRegister(Patmos::RRegsRegisterClass);
        RegInfo.addLiveIn(VA.getLocReg(), VReg);
        SDValue ArgValue = DAG.getCopyFromReg(Chain, dl, VReg, RegVT);

        // If this is an 8/16-bit value, it is really passed promoted to 32
        // bits. Insert an assert[sz]ext to capture this, then truncate to the
        // right size.
        if (VA.getLocInfo() == CCValAssign::SExt)
          ArgValue = DAG.getNode(ISD::AssertSext, dl, RegVT, ArgValue,
                                 DAG.getValueType(VA.getValVT()));
        else if (VA.getLocInfo() == CCValAssign::ZExt)
          ArgValue = DAG.getNode(ISD::AssertZext, dl, RegVT, ArgValue,
                                 DAG.getValueType(VA.getValVT()));

        if (VA.getLocInfo() != CCValAssign::Full)
          ArgValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), ArgValue);

        InVals.push_back(ArgValue);
      }
    } else {
      // Sanity check
      assert(VA.isMemLoc());
      // Load the argument to a virtual register
      unsigned ObjSize = VA.getLocVT().getSizeInBits()/8;
      if (ObjSize > 4) {
        errs() << "LowerFormalArguments Unhandled argument type: "
             << EVT(VA.getLocVT()).getEVTString()
             << "\n";
      }
      // Create the frame index object for this incoming parameter...
      int FI = MFI->CreateFixedObject(ObjSize, VA.getLocMemOffset(), true);

      // Create the SelectionDAG nodes corresponding to a load
      //from this parameter
      SDValue FIN = DAG.getFrameIndex(FI, MVT::i32);
      InVals.push_back(DAG.getLoad(VA.getLocVT(), dl, Chain, FIN,
                                   MachinePointerInfo::getFixedStack(FI),
                                   false, false, 0, 0));
    }
  }

  // handle parameters of variadic functions.
  if (isVarArg) {
    // create a fixed FI to reference the variadic parameters passed on the 
    // stack and store it with the patmos machine function info.
    PMFI.setVarArgsFI(MFI->CreateFixedObject(4, CCInfo.getNextStackOffset(),
                                             true));
  }

  return Chain;
}




SDValue
PatmosTargetLowering::LowerReturn(SDValue Chain,
                                  CallingConv::ID CallConv, bool isVarArg,
                                  const SmallVectorImpl<ISD::OutputArg> &Outs,
                                  const SmallVectorImpl<SDValue> &OutVals,
                                  DebugLoc dl, SelectionDAG &DAG) const {

  // CCValAssign - represent the assignment of the return value to a location
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), RVLocs, *DAG.getContext());

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Patmos);

  // If this is the first return lowered for this function, add the regs to the
  // liveout set for the function.
  if (DAG.getMachineFunction().getRegInfo().liveout_empty()) {
    for (unsigned i = 0; i != RVLocs.size(); ++i)
      if (RVLocs[i].isRegLoc())
        DAG.getMachineFunction().getRegInfo().addLiveOut(RVLocs[i].getLocReg());
  }

  SDValue Flag;

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(),
                             OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together,
    // avoiding something bad.
    Flag = Chain.getValue(1);
  }

  unsigned Opc = PatmosISD::RET_FLAG;

  if (Flag.getNode())
    return DAG.getNode(Opc, dl, MVT::Other, Chain, Flag);

  // Return Void
  return DAG.getNode(Opc, dl, MVT::Other, Chain);
}




/// LowerCCCCallTo - functions arguments are copied from virtual regs to
/// (physical regs)/(stack frame), CALLSEQ_START and CALLSEQ_END are emitted.
/// TODO: sret.
SDValue
PatmosTargetLowering::LowerCCCCallTo(SDValue Chain, SDValue Callee,
                                     CallingConv::ID CallConv, bool isVarArg,
                                     bool isTailCall,
                                     const SmallVectorImpl<ISD::OutputArg>
                                       &Outs,
                                     const SmallVectorImpl<SDValue> &OutVals,
                                     const SmallVectorImpl<ISD::InputArg> &Ins,
                                     DebugLoc dl, SelectionDAG &DAG,
                                     SmallVectorImpl<SDValue> &InVals) const {
  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), ArgLocs, *DAG.getContext());

  CCInfo.AnalyzeCallOperands(Outs, CC_Patmos);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  Chain = DAG.getCALLSEQ_START(Chain, DAG.getConstant(NumBytes,
                                                      getPointerTy(), true));

  SmallVector<std::pair<unsigned, SDValue>, 4> RegsToPass;
  SmallVector<SDValue, 12> MemOpChains;
  SDValue StackPtr;

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];

    SDValue Arg = OutVals[i];

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
      default: llvm_unreachable("Unknown loc info!");
      case CCValAssign::Full: break;
      case CCValAssign::SExt:
        Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, VA.getLocVT(), Arg);
        break;
      case CCValAssign::ZExt:
        Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, VA.getLocVT(), Arg);
        break;
      case CCValAssign::AExt:
        Arg = DAG.getNode(ISD::ANY_EXTEND, dl, VA.getLocVT(), Arg);
        break;
    }

    // Arguments that can be passed on register must be kept at RegsToPass
    // vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      assert(VA.isMemLoc());

      if (StackPtr.getNode() == 0)
        StackPtr = DAG.getCopyFromReg(Chain, dl, Patmos::RSP, getPointerTy());

      SDValue PtrOff = DAG.getNode(ISD::ADD, dl, getPointerTy(),
                                   StackPtr,
                                   DAG.getIntPtrConstant(VA.getLocMemOffset()));


      MemOpChains.push_back(DAG.getStore(Chain, dl, Arg, PtrOff,
                                         MachinePointerInfo(),false, false, 0));
    }
  }

  // Transform all store nodes into one single node because all store nodes are
  // independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                        &MemOpChains[0], MemOpChains.size());

  // Build a sequence of copy-to-reg nodes chained together with token chain and
  // flag operands which copy the outgoing args into registers.  The InFlag in
  // necessary since all emitted instructions must be stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Chain = DAG.getCopyToReg(Chain, dl, RegsToPass[i].first,
                             RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress node (quite common, every direct call is)
  // turn it into a TargetGlobalAddress node so that legalize doesn't hack it.
  // Likewise ExternalSymbol -> TargetExternalSymbol.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl, MVT::i32);
  else if (ExternalSymbolSDNode *E = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(E->getSymbol(), MVT::i32);

  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i)
    Ops.push_back(DAG.getRegister(RegsToPass[i].first,
                                  RegsToPass[i].second.getValueType()));

  if (InFlag.getNode())
    Ops.push_back(InFlag);

  Chain = DAG.getNode(PatmosISD::CALL, dl, NodeTys, &Ops[0], Ops.size());
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.
  Chain = DAG.getCALLSEQ_END(Chain,
                             DAG.getConstant(NumBytes, getPointerTy(), true),
                             DAG.getConstant(0, getPointerTy(), true),
                             InFlag);
  InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, isVarArg, Ins, dl,
                         DAG, InVals);
}





/// LowerCallResult - Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
///
SDValue
PatmosTargetLowering::LowerCallResult(SDValue Chain, SDValue InFlag,
                                      CallingConv::ID CallConv, bool isVarArg,
                                      const SmallVectorImpl<ISD::InputArg> &Ins,
                                      DebugLoc dl, SelectionDAG &DAG,
                                      SmallVectorImpl<SDValue> &InVals) const {

  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), RVLocs, *DAG.getContext());

  CCInfo.AnalyzeCallResult(Ins, RetCC_Patmos);

  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    Chain = DAG.getCopyFromReg(Chain, dl, RVLocs[i].getLocReg(),
                               RVLocs[i].getValVT(), InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

PatmosTargetLowering::ConstraintType PatmosTargetLowering::
getConstraintType(const std::string &Constraint) const
{
  // Patmos specific constrainy
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
      default : break;
      case 'R':
      case 'S':
      case 'P':
        return C_RegisterClass;
    }
  }
  return TargetLowering::getConstraintType(Constraint);
}

std::pair<unsigned, const TargetRegisterClass*> PatmosTargetLowering::
getRegForInlineAsmConstraint(const std::string &Constraint,
                             EVT VT) const
{
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'R':  // r0-r31
    case 'r':  // general purpose registers
      if (VT == MVT::i32) {
        return std::make_pair(0U, Patmos::RRegsRegisterClass);
      }
      assert("Unexpected register type");
      return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));
    case 'S':
      if (VT == MVT::i32) {
        return std::make_pair(0U, Patmos::SRegsRegisterClass);
      }
      assert("Unexpected register type");
      return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));
    case 'P':
      if (VT == MVT::i1) {
        return std::make_pair(0U, Patmos::PRegsRegisterClass);
      }
      assert("Unexpected register type");
      return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));

    // TODO support for i,n,m,o,X,f,.. (immediates, floats?, memory, labels,..) ??

    }
  }

  // Handle '{}'
  return TargetLowering::getRegForInlineAsmConstraint(Constraint, VT);
}


SDValue
PatmosTargetLowering::LowerDYNAMIC_STACKALLOC(SDValue Op,
                                              SelectionDAG &DAG) const {
  SDValue chain = Op.getOperand(0);
  SDValue size = Op.getOperand(1);
  DebugLoc dl = Op.getDebugLoc();

  // get result pointer, which needs to be adjusted according to the maximal 
  // call framer size (this is lowered when the epilog is emitted)
  SDVTList VTs = DAG.getVTList(MVT::i32, MVT::Other);
  SDValue dynAlloc = DAG.getNode(PatmosISD::DYNALLOC, dl, VTs, chain,
                                 DAG.getTargetConstant(0, MVT::i32));


  // get value of stack pointer
  SDValue readSP = DAG.getCopyFromReg(dynAlloc.getValue(1), dl,
                                      Patmos::RSP, MVT::i32);

  // reserve space on stack, i.e., sp -= size
  SDValue updateSP = DAG.getNode(ISD::SUB, dl, MVT::i32, readSP, size);

  // copy value back to stack pointer
  SDValue writeSP = DAG.getCopyToReg(readSP.getValue(1), dl, Patmos::RSP,
                                     updateSP);


  // merge results
  SDValue Ops[2] = { dynAlloc, writeSP };
  return DAG.getMergeValues(Ops, 2, dl);
}





SDValue
PatmosTargetLowering::LowerVASTART(SDValue Op, SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  PatmosMachineFunctionInfo &PMFI = *MF.getInfo<PatmosMachineFunctionInfo>();

  // get VarArgsFI, i.e., the FI used to access the variadic parameters of the 
  // current function
  DebugLoc dl = Op.getDebugLoc();
  SDValue VarArgsFI = DAG.getFrameIndex(PMFI.getVarArgsFI(), getPointerTy());

  // get the VarArgsFI and store it to the given address.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), // chain
                      dl,
                      VarArgsFI, // VarArgsFI
                      Op.getOperand(1), // destination address
                      MachinePointerInfo(SV), false, false, 0);
}


const char *PatmosTargetLowering::getTargetNodeName(unsigned Opcode) const {
  switch (Opcode) {
  default: return NULL;
  case PatmosISD::RET_FLAG:           return "PatmosISD::RET_FLAG";
  case PatmosISD::CALL:               return "PatmosISD::CALL";
  case PatmosISD::DYNALLOC:           return "PatmosISD::DYNALLOC";
  case PatmosISD::MUL:                return "PatmosISD::MUL";
  case PatmosISD::MULU:               return "PatmosISD::MULU";
  }
}
