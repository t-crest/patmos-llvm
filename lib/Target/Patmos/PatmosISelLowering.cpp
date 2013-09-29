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
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/GlobalAlias.h"
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
#include "llvm/MC/MCExpr.h"
using namespace llvm;


PatmosTargetLowering::PatmosTargetLowering(PatmosTargetMachine &tm) :
  TargetLowering(tm, new TargetLoweringObjectFileELF()),
  Subtarget(*tm.getSubtargetImpl()) {

  TD = getDataLayout();

  // Set up the register classes.
  // SRegs are not used for computations.
  addRegisterClass(MVT::i32, &Patmos::RRegsRegClass);
  addRegisterClass(MVT::i1,  &Patmos::PRegsRegClass);

  // Compute derived properties from the register classes
  computeRegisterProperties();

  // Provide all sorts of operation actions

  // Division is expensive
  setIntDivIsCheap(false);
  // Select is not
  setSelectIsExpensive(false);
  // Jump is Expensive. Don't create extra control flow for 'and', 'or'
  // condition branches.
  setJumpIsExpensive(true);

  setStackPointerRegisterToSaveRestore(Patmos::RSP);
  setBooleanContents(ZeroOrOneBooleanContent);

  // We require word alignment at least (in log2 bytes here), if code requires 
  // an other alignment, e.g., due to the method-cache, it will be handled 
  // later.
  setMinFunctionAlignment(2);
  setPrefFunctionAlignment(2);

  // Enable using divmod functions
  setLibcallName(RTLIB::SDIVREM_I32, "__divmodsi4");
  setLibcallName(RTLIB::UDIVREM_I32, "__udivmodsi4");
  setLibcallName(RTLIB::SDIVREM_I64, "__divmoddi4");
  setLibcallName(RTLIB::UDIVREM_I64, "__udivmoddi4");

  setOperationAction(ISD::LOAD,   MVT::i1, Custom);
  setLoadExtAction(ISD::EXTLOAD,  MVT::i1, Promote);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1, Promote);
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i1, Promote);

  setOperationAction(ISD::STORE, MVT::i1, Custom);

  setOperationAction(ISD::SIGN_EXTEND, MVT::i1, Promote);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i1, Promote);
  setOperationAction(ISD::ANY_EXTEND,  MVT::i1, Promote);
  // NB: Several operations simply do not get promoted, e.g.,
  //     arithmetic operations like add, sub, ...
  //     We try to solve them by isel patterns, e.g. add i1 -> xor i1

  // Expand to S/UMUL_LOHI
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
  setOperationAction(ISD::CTTZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF, MVT::i32, Expand);
  setOperationAction(ISD::CTPOP, MVT::i32, Expand);

  setOperationAction(ISD::SIGN_EXTEND, MVT::i8,  Expand);
  setOperationAction(ISD::SIGN_EXTEND, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND, MVT::i32, Expand);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i8,  Expand);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i16, Expand);
  setOperationAction(ISD::ZERO_EXTEND, MVT::i32, Expand);
  setOperationAction(ISD::ANY_EXTEND,  MVT::i8, Expand);
  setOperationAction(ISD::ANY_EXTEND,  MVT::i16, Expand);
  setOperationAction(ISD::ANY_EXTEND,  MVT::i32, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i8,  Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i16, Expand);
  setOperationAction(ISD::SIGN_EXTEND_INREG, MVT::i32, Expand);

  setOperationAction(ISD::ROTL , MVT::i32, Expand);
  setOperationAction(ISD::ROTR , MVT::i32, Expand);

  setOperationAction(ISD::SHL_PARTS, MVT::i32,   Expand);
  setOperationAction(ISD::SRA_PARTS, MVT::i32,   Expand);
  setOperationAction(ISD::SRL_PARTS, MVT::i32,   Expand);

  setOperationAction(ISD::SELECT_CC, MVT::i8,    Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i16,   Expand);
  setOperationAction(ISD::SELECT_CC, MVT::i32,   Expand);
  setOperationAction(ISD::SELECT_CC, MVT::Other, Expand);
  setOperationAction(ISD::BR_CC,     MVT::i1,    Expand);
  setOperationAction(ISD::BR_CC,     MVT::i8,    Expand);
  setOperationAction(ISD::BR_CC,     MVT::i16,   Expand);
  setOperationAction(ISD::BR_CC,     MVT::i32,   Expand);
  setOperationAction(ISD::BR_CC,     MVT::Other, Expand);

  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32, Expand);

  // handling of variadic parameters
  setOperationAction(ISD::VASTART, MVT::Other, Custom);
  setOperationAction(ISD::VAARG  , MVT::Other, Expand);
  setOperationAction(ISD::VACOPY , MVT::Other, Expand);
  setOperationAction(ISD::VAEND  , MVT::Other, Expand);

  // TODO expand floating point stuff?
}


SDValue PatmosTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {

  switch (Op.getOpcode()) {
    case ISD::LOAD:               return LowerLOAD(Op,DAG);
    case ISD::STORE:              return LowerSTORE(Op,DAG);
    case ISD::SMUL_LOHI:
    case ISD::UMUL_LOHI:          return LowerMUL_LOHI(Op, DAG);
    case ISD::VASTART:            return LowerVASTART(Op, DAG);
    case ISD::FRAMEADDR:          return LowerFRAMEADDR(Op, DAG);
    default:
      llvm_unreachable("unimplemented operation");
  }
}

EVT PatmosTargetLowering::getSetCCResultType(EVT VT) const {
  // All our compare results should be i1
  return MVT::i1;
}

const MCExpr * PatmosTargetLowering::LowerCustomJumpTableEntry(
                          const MachineJumpTableInfo * MJTI,
                          const MachineBasicBlock * MBB, unsigned uid,
                          MCContext &OutContext) const
{
  // Note: see also PatmosMCInstLower::LowerSymbolOperand
  return MCSymbolRefExpr::Create(MBB->getSymbol(), OutContext);
}


//===----------------------------------------------------------------------===//
//                      Custom Lower Operation
//===----------------------------------------------------------------------===//

SDValue PatmosTargetLowering::LowerLOAD(SDValue Op, SelectionDAG &DAG) const {
  LoadSDNode *load = static_cast<LoadSDNode*>(Op.getNode());

  assert(load->getMemoryVT() == MVT::i1);

  SDValue newLoad = DAG.getLoad(ISD::UNINDEXED, ISD::ZEXTLOAD, MVT::i32,
                                Op.getDebugLoc(), load->getChain(),
                                load->getBasePtr(), load->getOffset(), MVT::i8,
                                load->getMemOperand());

  SDValue newTrunc = DAG.getZExtOrTrunc(newLoad, Op.getDebugLoc(), MVT::i1);

  SDValue Ops[2] = { newTrunc, newLoad.getOperand(0) };
  return DAG.getMergeValues(Ops, 2, Op.getDebugLoc());
}


SDValue PatmosTargetLowering::LowerSTORE(SDValue Op, SelectionDAG &DAG) const {
  StoreSDNode *store = static_cast<StoreSDNode*>(Op.getNode());

  assert(store->getMemoryVT() == MVT::i1);

  SDValue newVal = DAG.getZExtOrTrunc(store->getValue(), Op.getDebugLoc(),
                                      MVT::i32);

  return DAG.getTruncStore(store->getChain(), Op.getDebugLoc(), newVal,
                           store->getBasePtr(), MVT::i1,
                           store->getMemOperand());
}

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


SDValue PatmosTargetLowering::LowerFRAMEADDR(SDValue Op, SelectionDAG &DAG) const {
  // check the depth
  assert((cast<ConstantSDNode>(Op.getOperand(0))->getZExtValue() == 0) &&
         "Frame address can only be determined for current frame.");

  MachineFrameInfo *MFI = DAG.getMachineFunction().getFrameInfo();
  MFI->setFrameAddressIsTaken(true);
  EVT VT = Op.getValueType();
  DebugLoc DL = Op.getDebugLoc();
  SDValue FrameAddr = DAG.getCopyFromReg(DAG.getEntryNode(), DL,
                                         Patmos::RFP, VT);
  return FrameAddr;
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
PatmosTargetLowering::LowerCall(CallLoweringInfo &CLI,
                                SmallVectorImpl<SDValue> &InVals) const {
  // Patmos target does not yet support tail call optimization.
  CLI.IsTailCall = false;

  switch (CLI.CallConv) {
  default:
    llvm_unreachable("Unsupported calling convention");
  case CallingConv::Fast:
  case CallingConv::C:
    return LowerCCCCallTo(CLI, InVals);
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

  CCState CCInfo(CallConv, isVarArg, MF,
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
          RegInfo.createVirtualRegister(&Patmos::RRegsRegClass);
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

  // Analyze return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_Patmos);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // Copy the result values into the output registers.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");

    Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(),
                             OutVals[i], Flag);

    // Guarantee that all emitted copies are stuck together,
    // avoiding something bad.
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  unsigned Opc = PatmosISD::RET_FLAG;

  RetOps[0] = Chain;  // Update chain.

  if (Flag.getNode())
    RetOps.push_back(Flag);

  // Return
  return DAG.getNode(Opc, dl, MVT::Other, &RetOps[0], RetOps.size());
}




/// LowerCCCCallTo - functions arguments are copied from virtual regs to
/// (physical regs)/(stack frame), CALLSEQ_START and CALLSEQ_END are emitted.
/// TODO: sret.
SDValue
PatmosTargetLowering::LowerCCCCallTo(CallLoweringInfo &CLI,
                                     SmallVectorImpl<SDValue> &InVals) const
{
  SelectionDAG &DAG                     = CLI.DAG;
  DebugLoc &dl                          = CLI.DL;
  SmallVector<ISD::OutputArg, 32> &Outs = CLI.Outs;
  SmallVector<SDValue, 32> &OutVals     = CLI.OutVals;
  SmallVector<ISD::InputArg, 32> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool isVarArg                         = CLI.IsVarArg;

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


  // attach machine-level aliasing information
  MachineMemOperand *MMO = 
      DAG.getMachineFunction().getMachineMemOperand(CLI.MPI, 
	                                            MachineMemOperand::MOLoad,
					            4, 0);

  Chain = DAG.getMemIntrinsicNode(PatmosISD::CALL, dl,
                                  NodeTys, &Ops[0], Ops.size(),
                                  MVT::i32, MMO);

//   Chain = DAG.getNode(PatmosISD::CALL, dl, NodeTys, &Ops[0], Ops.size());
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
    assert((RVLocs[i].getLocReg() == Patmos::R1 ||
            RVLocs[i].getLocReg() == Patmos::R2) && "Invalid return register");
    // We only support i32 return registers, so we copy from i32, no matter what
    // the actual return type in RVLocs[i].getValVT() is.
    Chain = DAG.getCopyFromReg(Chain, dl, RVLocs[i].getLocReg(),
                               MVT::i32, InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

PatmosTargetLowering::ConstraintType PatmosTargetLowering::
getConstraintType(const std::string &Constraint) const
{
  // Patmos specific constrains
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
      //if (VT == MVT::i32) {
        return std::make_pair(0U, &Patmos::RRegsRegClass);
      //}
      //assert("Unexpected register type");
      //return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));
    case 'S':
      if (VT == MVT::i32) {
        return std::make_pair(0U, &Patmos::SRegsRegClass);
      }
      assert("Unexpected register type");
      return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));
    case 'P':
      if (VT == MVT::i1) {
        return std::make_pair(0U, &Patmos::PRegsRegClass);
      }
      assert("Unexpected register type");
      return std::make_pair(0U, static_cast<const TargetRegisterClass*>(0));

    // TODO support for i,n,m,o,X,f,.. (immediates, floats?, memory, labels,..) ??

    }
  }
  // Handle '{$<regname>}'
  if (Constraint.size() > 2 && Constraint[0] == '{' && Constraint[1] == '$') {
    std::string Stripped = "{" + Constraint.substr(2);
    return TargetLowering::getRegForInlineAsmConstraint(Stripped, VT);
  }
  // Handle everything else ('{<regname}, ..)
  return TargetLowering::getRegForInlineAsmConstraint(Constraint, VT);
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
  case PatmosISD::MUL:                return "PatmosISD::MUL";
  case PatmosISD::MULU:               return "PatmosISD::MULU";
  }
}
