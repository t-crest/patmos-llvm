//===- PatmosInstrInfo.cpp - Patmos Instruction Information ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Patmos implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "Patmos.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "llvm/Function.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#define GET_INSTRINFO_CTOR
#include "PatmosGenInstrInfo.inc"


using namespace llvm;

PatmosInstrInfo::PatmosInstrInfo(PatmosTargetMachine &tm)
  : PatmosGenInstrInfo(Patmos::ADJCALLSTACKDOWN, Patmos::ADJCALLSTACKUP),
    RI(tm, *this), TM(tm) {}

bool PatmosInstrInfo::findCommutedOpIndices(MachineInstr *MI,
                                            unsigned &SrcOpIdx1,
                                            unsigned &SrcOpIdx2) const {
  switch (MI->getOpcode())
  {
    case Patmos::ADDr:
    case Patmos::ORr:
    case Patmos::ANDr:
    case Patmos::XORr:
    case Patmos::NORr:
      SrcOpIdx1 = 3;
      SrcOpIdx2 = 4;
      return true;
    case Patmos::MUL:
    case Patmos::MULU:
      SrcOpIdx1 = 2;
      SrcOpIdx2 = 3;
      return true;
    default:
      llvm_unreachable("Unexpected commutable machine instruction.");
  }
  return false;
}

void PatmosInstrInfo::copyPhysReg(MachineBasicBlock &MBB,
                                  MachineBasicBlock::iterator I, DebugLoc DL,
                                  unsigned DestReg, unsigned SrcReg,
                                  bool KillSrc) const {
  unsigned Opc;
  if (Patmos::RRegsRegClass.contains(DestReg, SrcReg)) {
    // General purpose register
    Opc = Patmos::MOV;
    AddDefaultPred(BuildMI(MBB, I, DL, get(Opc), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::PRegsRegClass.contains(DestReg, SrcReg)) {
    // Predicate register
    Opc = Patmos::PMOV;
    AddDefaultPred(BuildMI(MBB, I, DL, get(Opc), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::SRegsRegClass.contains(DestReg)) {
    assert(Patmos::RRegsRegClass.contains(SrcReg));
    AddDefaultPred(BuildMI(MBB, I, DL, get(Patmos::MTS), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else if (Patmos::SRegsRegClass.contains(SrcReg)) {
    assert(Patmos::RRegsRegClass.contains(DestReg));
    AddDefaultPred(BuildMI(MBB, I, DL, get(Patmos::MFS), DestReg))
      .addReg(SrcReg, getKillRegState(KillSrc));

  } else {
    llvm_unreachable("Impossible reg-to-reg copy");
  }

}

void PatmosInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                    unsigned SrcReg, bool isKill, int FrameIndex,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::SWC)))
      .addFrameIndex(FrameIndex).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)); // value to store
  }
  else if (RC == &Patmos::PRegsRegClass) {
    // As clients of this function (esp. InlineSpiller, foldMemoryOperands)
    // assume that the store instruction is the last one emitted, we cannot
    // simply emit two predicated stores.
    // We also cannot use RTR as it might be used for large FI offset
    // computation.
    // We work around this restriction by using a pseudo-inst that
    // is expanded in PatmosRegisterInfo::eliminateFrameIndex()
    BuildMI(MBB, MI, DL, get(Patmos::PSEUDO_PREG_SPILL))
      .addFrameIndex(FrameIndex).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)); // predicate register
  }
  else llvm_unreachable("Register class not handled!");
}

void PatmosInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI,
                     unsigned DestReg, int FrameIdx,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::LWC), DestReg))
      .addFrameIndex(FrameIdx).addImm(0); // address
  }
  else if (RC == &Patmos::PRegsRegClass) {
    // Clients assume the last instruction inserted to be a load instruction.
    // Again, we work around this with a pseudo instruction that is expanded
    // during FrameIndex elimination.
    BuildMI(MBB, MI, DL, get(Patmos::PSEUDO_PREG_RELOAD), DestReg)
      .addFrameIndex(FrameIdx).addImm(0); // address
  }
  else llvm_unreachable("Register class not handled!");
}


void PatmosInstrInfo::insertNoop(MachineBasicBlock &MBB,
      MachineBasicBlock::iterator MI) const {
  DebugLoc DL;
  AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::NOP)).addImm(0));
}


bool PatmosInstrInfo::fixOpcodeForGuard(MachineInstr *MI) const {
  using namespace Patmos;
  unsigned opc = MI->getOpcode();
  int newopc = -1;

  if (MI->isBranch()) {
    if (isPredicated(MI)) {
      // unconditional branch -> conditional branch
      switch (opc) {
        case BCu: newopc = BC; break;
        case Bu:  newopc = B ; break;
        default:
          assert(MI->isConditionalBranch());
          break;
      }
    } else { // NOT predicated
      // conditional branch -> unconditional branch
      switch (opc) {
        case BC: newopc = BCu; break;
        case B:  newopc = Bu ; break;
        default:
          assert(MI->isUnconditionalBranch());
          break;
      }
    }
  }
  if (newopc != -1) {
    // we have sth to rewrite
    MI->setDesc(get(newopc));
    return true;
  }
  return false;
}

////////////////////////////////////////////////////////////////////////////////
//
// Branch handling
//


MachineBasicBlock *PatmosInstrInfo::
getBranchTarget(const MachineInstr *MI) const {
  // can handle only direct branches
  assert(MI->isBranch() && !MI->isIndirectBranch() &&
         "Not a direct branch instruction!");
  return MI->getOperand(2).getMBB();
}

bool PatmosInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  // If the client does not want to only simplify the branch,
  // the output arguments must be initialized.
  assert(AllowModify || (TBB==0 && FBB==0 && Cond.size()==0));

  // Start from the bottom of the block and work up, examining the
  // terminator instructions.
  MachineBasicBlock::iterator I = MBB.end();

  while (I != MBB.begin()) {
    --I;

    if (I->isDebugValue())
      continue;

    // Working from the bottom, when we see a non-terminator inst, we're done.
    if (!isUnpredicatedTerminator(I))
      break;

    // A terminator that isn't a (direct) branch can't easily be handled
    // by this analysis.
    if (!I->isBranch() || I->isIndirectBranch())
      return true;

    // Handle Unconditional branches
    if (!isPredicated(I)) {
      // fix instruction, if necessary
      if (!I->isUnconditionalBranch()) fixOpcodeForGuard(I);
      // TBB is used to indicate the unconditional destination.
      TBB = getBranchTarget(I);
      if (AllowModify) {
        // If the block has any instructions after an uncond branch, delete them.
        while (llvm::next(I) != MBB.end())
          llvm::next(I)->eraseFromParent();
      }
      continue;
    }

    // Handle conditional branches
    if (isPredicated(I)) {
      // fix instruction, if necessary
      if (!I->isConditionalBranch()) fixOpcodeForGuard(I);
      // we only treat the first conditional branch in a row
      if (Cond.size() > 0)
        return true;
      // Get branch condition
      int i = I->findFirstPredOperandIdx();
      assert(i != -1 );
      Cond.push_back(I->getOperand(i));   // reg
      Cond.push_back(I->getOperand(i+1)); // flag
      // We've processed an unconditional branch before,
      // the unconditional target goes to FBB now
      if (TBB) FBB = TBB;
      // target of conditional branch goes to TBB
      TBB = getBranchTarget(I);
      continue;
    }
    // we explicitly leave or continue.
    llvm_unreachable("AnalyzeBranch error.");
  }
  // left the loop? then we're done
  return false;
}


unsigned PatmosInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator I = MBB.end();
  unsigned Count = 0;
  while (I != MBB.begin()) {
    --I;
    if (I->isDebugValue())
      continue;
    if (!I->isBranch()) break; // Not a branch
    // Remove the branch.
    I->eraseFromParent();
    I = MBB.end();
    ++Count;
  }
  return Count;
}

unsigned
PatmosInstrInfo::InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                              MachineBasicBlock *FBB,
                              const SmallVectorImpl<MachineOperand> &Cond,
                              DebugLoc DL) const {
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 2 || Cond.size() == 0) &&
         "Patmos branch conditions should have two components (reg+imm)!");

  if (FBB == 0) {
    // One-way branch.
    if (Cond.empty()) { // Unconditional branch
      AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BCu))).addMBB(TBB);
    } else { // Conditional branch.
      BuildMI(&MBB, DL, get(Patmos::BC))
        .addOperand(Cond[0]).addOperand(Cond[1])
        .addMBB(TBB);
    }
    return 1;
  }

  // Two-way Conditional branch.
  BuildMI(&MBB, DL, get(Patmos::BC))
    .addOperand(Cond[0]).addOperand(Cond[1])
    .addMBB(TBB);
  AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BCu)))
    .addMBB(FBB);
  return 2;
}

bool PatmosInstrInfo::
ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond) const {
  // invert the flag
  int64_t invflag = Cond[1].getImm();
  Cond[1].setImm( (invflag)?0:-1 );
  return false; //success
}


////////////////////////////////////////////////////////////////////////////////
//
// Predication and If-Conversion
//

bool PatmosInstrInfo::isPredicated(const MachineInstr *MI) const {
  int i = MI->findFirstPredOperandIdx();
  if (i != -1) {
    unsigned preg = MI->getOperand(i).getReg();
    int      flag = MI->getOperand(++i).getImm();
    return (preg!=Patmos::NoRegister && preg!=Patmos::P0) || flag;
  }
  // no predicates at all
  return false;
}

bool PatmosInstrInfo::isUnpredicatedTerminator(const MachineInstr *MI) const {
  if (!MI->isTerminator()) return false;

  // Conditional branch is a special case.
  if (MI->isBranch() && isPredicated(MI))
    return true;

  return !isPredicated(MI);
}

bool PatmosInstrInfo::
PredicateInstruction(MachineInstr *MI,
                     const SmallVectorImpl<MachineOperand> &Pred) const {
  assert(!MI->isBundle() &&
         "PatmosInstrInfo::PredicateInstruction() can't handle bundles");

  if (MI->isPredicable()) {
    assert(!isPredicated(MI) &&
           "Cannot predicate an instruction already predicated.");
    // find first predicate operand
    int i = MI->findFirstPredOperandIdx();
    assert(i != -1);
    MachineOperand &PO1 = MI->getOperand(i);
    MachineOperand &PO2 = MI->getOperand(i+1);
    assert(PO1.isReg() && PO2.isImm() &&
        "Unexpected Patmos predicate operand");
    PO1.setReg(Pred[0].getReg());
    PO2.setImm(Pred[1].getImm());
    return true;
  }
  return false;
}


bool PatmosInstrInfo::
SubsumesPredicate(const SmallVectorImpl<MachineOperand> &Pred1,
                  const SmallVectorImpl<MachineOperand> &Pred2) const {
  assert( Pred1.size()==2 && Pred2.size()==2 );

  // True always subsumes all others
  unsigned preg1 = Pred1[0].getReg();
  int      flag1 = Pred1[1].getImm();
  if ((preg1==Patmos::NoRegister || preg1==Patmos::P0) && !flag1)
    return true;

  // Equal predicates subsume each other
  if (preg1==Pred2[0].getReg() && flag1==Pred2[1].getImm())
    return true;

  // False never subsumes anything (except false)

  // safe side
  return false;
}


bool PatmosInstrInfo::DefinesPredicate(MachineInstr *MI,
                                       std::vector<MachineOperand> &Pred)
                                       const {
  bool flag = false;

  for (unsigned i = 0; i < MI->getNumOperands(); i++) {
    const MachineOperand &MO = MI->getOperand(i);
    if ( MO.isReg() && MO.isDef() &&
        Patmos::PRegsRegClass.contains(MO.getReg())) {
      Pred.push_back(MO);
      flag = true;
    }
  }
  return flag;
}
