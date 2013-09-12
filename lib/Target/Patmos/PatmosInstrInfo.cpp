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
#include "PatmosAsmPrinter.h"
#include "PatmosInstrInfo.h"
#include "PatmosMachineFunctionInfo.h"
#include "PatmosTargetMachine.h"
#include "PatmosHazardRecognizer.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/CodeGen/DFAPacketizer.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/PseudoSourceValue.h"
#include "llvm/MC/MCSectionELF.h"
#include "llvm/Support/ELF.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
//#include "llvm/Support/Debug.h"
//#include "llvm/Support/raw_ostream.h"

#define GET_INSTRINFO_CTOR
#include "PatmosGenInstrInfo.inc"
#include "PatmosGenDFAPacketizer.inc"


using namespace llvm;

PatmosInstrInfo::PatmosInstrInfo(PatmosTargetMachine &tm)
  : PatmosGenInstrInfo(Patmos::ADJCALLSTACKDOWN, Patmos::ADJCALLSTACKUP),
    PTM(tm), RI(tm, *this), PST(*tm.getSubtargetImpl()) {}

bool PatmosInstrInfo::findCommutedOpIndices(MachineInstr *MI,
                                            unsigned &SrcOpIdx1,
                                            unsigned &SrcOpIdx2) const {
  switch (MI->getOpcode())
  {
    case Patmos::ADDr: case Patmos::ADDr_ow:
    case Patmos::ORr:  case Patmos::ORr_ow:
    case Patmos::ANDr: case Patmos::ANDr_ow:
    case Patmos::XORr: case Patmos::XORr_ow:
    case Patmos::NORr: case Patmos::NORr_ow:
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
                    unsigned SrcReg, bool isKill, int FrameIdx,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {
  DebugLoc DL;
  if (MI != MBB.end()) DL = MI->getDebugLoc();

  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  MachineMemOperand *MMO =
  MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(FrameIdx),
                          MachineMemOperand::MOStore,
                          MFI.getObjectSize(FrameIdx),
                          MFI.getObjectAlignment(FrameIdx));

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::SWC)))
      .addFrameIndex(FrameIdx).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)) // value to store
      .addMemOperand(MMO);
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
      .addFrameIndex(FrameIdx).addImm(0) // address
      .addReg(SrcReg, getKillRegState(isKill)) // predicate register
      .addMemOperand(MMO);
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

  MachineFunction &MF = *MBB.getParent();
  MachineFrameInfo &MFI = *MF.getFrameInfo();
  MachineMemOperand *MMO =
  MF.getMachineMemOperand(MachinePointerInfo::getFixedStack(FrameIdx),
                          MachineMemOperand::MOLoad,
                          MFI.getObjectSize(FrameIdx),
                          MFI.getObjectAlignment(FrameIdx));

  if (RC == &Patmos::RRegsRegClass) {
    AddDefaultPred(BuildMI(MBB, MI, DL, get(Patmos::LWC), DestReg))
      .addFrameIndex(FrameIdx).addImm(0) // address
      .addMemOperand(MMO);
  }
  else if (RC == &Patmos::PRegsRegClass) {
    // Clients assume the last instruction inserted to be a load instruction.
    // Again, we work around this with a pseudo instruction that is expanded
    // during FrameIndex elimination.
    BuildMI(MBB, MI, DL, get(Patmos::PSEUDO_PREG_RELOAD), DestReg)
      .addFrameIndex(FrameIdx).addImm(0) // address
      .addMemOperand(MMO);
  }
  else llvm_unreachable("Register class not handled!");
}


void PatmosInstrInfo::insertNoop(MachineBasicBlock &MBB,
      MachineBasicBlock::iterator MI) const {
  DebugLoc DL;
  BuildMI(MBB, MI, DL, get(Patmos::NOP))
    .addReg(Patmos::NoRegister).addImm(0);
}


bool PatmosInstrInfo::isSchedulingBoundary(const MachineInstr *MI,
                                            const MachineBasicBlock *MBB,
                                            const MachineFunction &MF) const {
  // Debug info is never a scheduling boundary.
  if (MI->isDebugValue())
    return false;

  // Terminators and labels can't be scheduled around.
  if (MI->getDesc().isTerminator() || MI->isLabel())
    return true;

  // TODO check if we have any other scheduling boundaries (STCs,..)
  //      Ideally, we would like to schedule even over branches and calls
  //      and model everything else as hazards and dependencies.

  return false;
}

ScheduleHazardRecognizer *PatmosInstrInfo::CreateTargetHazardRecognizer(
                              const TargetMachine *TM,
                              const ScheduleDAG *DAG) const
{
  const InstrItineraryData *II = TM->getInstrItineraryData();
  return new PatmosHazardRecognizer(PTM, II, DAG, false);
}

ScheduleHazardRecognizer *PatmosInstrInfo::CreateTargetMIHazardRecognizer(
                                const InstrItineraryData *II,
                                const ScheduleDAG *DAG) const
{
  return new PatmosHazardRecognizer(PTM, II, DAG, false);
}

ScheduleHazardRecognizer *PatmosInstrInfo::CreateTargetPostRAHazardRecognizer(
                                const InstrItineraryData *II,
                                const ScheduleDAG *DAG) const
{
  return new PatmosHazardRecognizer(PTM, II, DAG, true);
}

DFAPacketizer *PatmosInstrInfo::
CreateTargetScheduleState(const TargetMachine *TM,
                           const ScheduleDAG *DAG) const {
  const InstrItineraryData *II = TM->getInstrItineraryData();
  return TM->getSubtarget<PatmosGenSubtargetInfo>().createDFAPacketizer(II);
}



bool PatmosInstrInfo::fixOpcodeForGuard(MachineInstr *MI) const {
  using namespace Patmos;

  if (MI->isBundle()) {
    bool changed = false;

    MachineBasicBlock::instr_iterator it = MI;

    while ((++it)->isBundledWithPred()) {
      changed |= fixOpcodeForGuard(it);
    }

    return changed;
  }

  unsigned opc = MI->getOpcode();
  int newopc = -1;

  if (MI->isBranch()) {
    if (isPredicated(MI)) {
      // unconditional branch -> conditional branch
      switch (opc) {
        case BRu:   newopc = BR;   break;
        case BRRu:  newopc = BRR;  break;
        case BRTu:  newopc = BRT;  break;
        case BRCFu: newopc = BRCF; break;
        case BRCFRu:newopc = BRCFR;break;
        case BRCFTu:newopc = BRCFT;break;
        default:
          assert(MI->isConditionalBranch() ||
                 (MI->isIndirectBranch() && MI->isBarrier()) );
          break;
      }
    } else { // NOT predicated
      // conditional branch -> unconditional branch
      switch (opc) {
        case BR:   newopc = BRu;   break;
        case BRR:  newopc = BRRu;  break;
        case BRT:  newopc = BRTu;  break;
        case BRCF: newopc = BRCFu; break;
        case BRCFR:newopc = BRCFRu;break;
        case BRCFT:newopc = BRCFTu;break;
        default:
          assert(MI->isUnconditionalBranch() ||
                 (MI->isIndirectBranch() && MI->isBarrier()) );
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

bool PatmosInstrInfo::isStackControl(const MachineInstr *MI) const {
  switch (getPatmosFormat(MI->getDesc().TSFlags)) {
    case PatmosII::FrmSTCi:
    case PatmosII::FrmSTCr:
      return true;
    default: return false;
  }
}

bool PatmosInstrInfo::isSideEffectFreeSRegAccess(const MachineInstr *MI)
                                                                      const {
  unsigned opc = MI->getOpcode();

  if  (opc==Patmos::MTS || opc==Patmos::MFS) {
    // MTS sreg <- r (sreg is operand 0)
    // MFS r <- sreg (sreg is operand 3)
    unsigned sreg = MI->getOperand( (opc==Patmos::MFS) ? 3 : 0 ).getReg();

    // check SRegs
    BitVector safeSRegs(getRegisterInfo().getNumRegs());
    safeSRegs.set(Patmos::S0);
    safeSRegs.set(Patmos::SL);
    safeSRegs.set(Patmos::SH);
    if (safeSRegs.test(sreg))
      return true;
  }
  return false;
}

unsigned PatmosInstrInfo::getMemType(const MachineInstr *MI) const {
  assert(MI->mayLoad() || MI->mayStore());

  if (MI->isBundle()) {
    // find mem instruction in bundle (does not need to be the first
    // instruction, they might be sorted later!)
    MachineBasicBlock::const_instr_iterator II = MI; ++II;
    while (II->isInsideBundle() && !II->mayLoad() && !II->mayStore()) {
      ++II;
    }
    return getMemType(II);
  }

  // FIXME: Maybe there is a better way to get this info directly from
  //        the instruction definitions in the .td files
  using namespace Patmos;
  unsigned opc = MI->getOpcode();
  switch (opc) {
    case LWS: case LHS: case LBS: case LHUS: case LBUS:
    case SWS: case SHS: case SBS:
      return PatmosII::MEM_S;
    case LWL: case LHL: case LBL: case LHUL: case LBUL:
    case SWL: case SHL: case SBL:
      return PatmosII::MEM_L;
    case  LWC: case  LHC: case  LBC: case  LHUC: case  LBUC:
    case DLWC: case DLHC: case DLBC: case DLHUC: case DLBUC:
    case  SWC: case  SHC: case  SBC:
      return PatmosII::MEM_C;
    case  LWM: case  LHM: case  LBM: case  LHUM: case  LBUM:
    case DLWM: case DLHM: case DLBM: case DLHUM: case DLBUM:
    case  SWM: case  SHM: case  SBM:
      return PatmosII::MEM_M;
    default: llvm_unreachable("Unexpected memory access instruction!");
  }

}

bool PatmosInstrInfo::isPseudo(const MachineInstr *MI) const {
  if (MI->isDebugValue())
    return true;

  // We must emit inline assembly
  if (MI->isInlineAsm())
    return false;

  // We check if MI has any functional units mapped to it.
  // If it doesn't, we ignore the instruction.
  const MCInstrDesc& TID = MI->getDesc();
  unsigned SchedClass = TID.getSchedClass();
  const InstrStage* IS = PST.getInstrItineraryData().beginStage(SchedClass);
  unsigned FuncUnits = IS->getUnits();
  return !FuncUnits;
}

void PatmosInstrInfo::skipPseudos(MachineBasicBlock &MBB,
    MachineBasicBlock::instr_iterator &II) const
{
  while (II != MBB.instr_end() && isPseudo(II)) {
    II++;
  }
}

void PatmosInstrInfo::skipPseudos(MachineBasicBlock &MBB,
    MachineBasicBlock::iterator &II) const
{
  // TODO we should check if a bundle contains only pseudos (optionally).
  while (II != MBB.instr_end() && !II->isBundle() && isPseudo(II)) {
    II++;
  }
}

/// nextNonPseudo - Get the next non-pseudo instruction or bundle.
MachineBasicBlock::iterator PatmosInstrInfo::nextNonPseudo(
                                  MachineBasicBlock &MBB,
                                  const MachineBasicBlock::iterator &II) const
{
  MachineBasicBlock::iterator J = next(II);
  skipPseudos(MBB, J);
  return J;
}


const MachineInstr *PatmosInstrInfo::hasOpcode(const MachineInstr *MI,
                                               int Opcode) const {
  if (MI->isBundle()) {
    MachineBasicBlock::const_instr_iterator II = MI; ++II;

    while (II->isInsideBundle()) {
      if (II->getOpcode() == Opcode) return II;
      II++;
    }

    return 0;
  } else {
    return MI->getOpcode() == Opcode ? MI : 0;
  }
}

const MachineInstr *PatmosInstrInfo::getFirstMI(const MachineInstr *MI) const {
  if (MI->isBundle()) {
    MachineBasicBlock::const_instr_iterator I = MI;
    return next(I);
  }
  return MI;
}

PatmosInstrAnalyzer *PatmosInstrInfo::createPatmosInstrAnalyzer(
                                                         MCContext &Ctx) const {
  // PIA is deleted by AsmPrinter
  PatmosInstrAnalyzer *PIA = new PatmosInstrAnalyzer(Ctx);

  // PTM.getTargetLowering()->getObjFileLowering() might not yet be
  // initialized, so we create a new section object for this temp context
  const MCSection* TS = Ctx.getELFSection(".text",
                                          ELF::SHT_PROGBITS, 0,
                                          SectionKind::getText());
  PIA->SwitchSection(TS);

  return PIA;
}

unsigned int PatmosInstrInfo::getInstrSize(const MachineInstr *MI) const {
  if (MI->isInlineAsm()) {
    // TODO is there a way to get the current context?
    MCContext Ctx(*PTM.getMCAsmInfo(),
                  *PTM.getRegisterInfo(), *PTM.getInstrInfo(), 0);

    // PIA is deleted by AsmPrinter
    PatmosInstrAnalyzer *PIA = createPatmosInstrAnalyzer(Ctx);

    PatmosAsmPrinter PAP(PTM, *PIA);
    PAP.EmitInlineAsm(MI);

    return PIA->getSize();
  }
  else if (MI->isBundle()) {
    const MachineBasicBlock *MBB = MI->getParent();
    MachineBasicBlock::const_instr_iterator I = MI, E = MBB->instr_end();
    unsigned Size = 0;
    while ((++I != E) && I->isInsideBundle()) {
      Size += getInstrSize(I);
    }
    return Size;
  }
  else {
    // trust the desc..
    return MI->getDesc().getSize();
  }
}

bool PatmosInstrInfo::hasCall(const MachineInstr *MI) const {
  if (MI->isInlineAsm()) {
    // TODO is there a way to get the current context?
    MCContext Ctx(*PTM.getMCAsmInfo(),
                  *PTM.getRegisterInfo(), *PTM.getInstrInfo(), 0);

    // PIA is deleted by AsmPrinter
    PatmosInstrAnalyzer *PIA = createPatmosInstrAnalyzer(Ctx);

    PatmosAsmPrinter PAP(PTM, *PIA);
    PAP.EmitInlineAsm(MI);

    return PIA->hasCall();
  }
  else {
    // trust the desc..
    return MI->isCall();
  }
}

const Function *PatmosInstrInfo::getCallee(const MachineInstr *MI) const
{
  const Function *F = NULL;

  if (MI->isBundle()) {
    MachineBasicBlock::const_instr_iterator it = MI;

    while ((++it)->isBundledWithPred()) {
      if (!it->isCall()) continue;
      if (F) return NULL;
      F = getCallee(it);
      if (!F) return NULL;
    }

    return F;
  }

  // get target
  const MachineOperand &MO(MI->getOperand(2));

  // try to find the target of the call
  if (MO.isGlobal()) {
    // is the global value a function?
    F = dyn_cast<Function>(MO.getGlobal());
  }
  else if (MO.isSymbol()) {
    // find the function in the current module
    const Module &M = *MI->getParent()->getParent()->getFunction()->getParent();

    F = dyn_cast_or_null<Function>(M.getNamedValue(MO.getSymbolName()));
  }

  return F;
}

unsigned PatmosInstrInfo::getIssueWidth(const MachineInstr *MI) const
{
  if (MI->isInlineAsm())
    return PST.getSchedModel()->IssueWidth;

  return PST.getIssueWidth(MI->getDesc().SchedClass);
}

bool PatmosInstrInfo::canIssueInSlot(const MCInstrDesc &MID,
                                     unsigned Slot) const
{
  return PST.canIssueInSlot(MID.getSchedClass(), Slot);
}

bool PatmosInstrInfo::canIssueInSlot(const MachineInstr *MI,
                                     unsigned Slot) const
{
  if (MI->isPseudo()) return true;
  return canIssueInSlot(MI->getDesc(), Slot);
}

int PatmosInstrInfo::getOperandLatency(const InstrItineraryData *ItinData,
                              const MachineInstr *DefMI, unsigned DefIdx,
                              const MachineInstr *UseMI,
                              unsigned UseIdx) const
{
  if (UseMI->isInlineAsm()) {
    // For inline asm we do not have a use cycle for our operands, so we use
    // the default def latency instead.
    return getDefOperandLatency(ItinData, DefMI, DefIdx);
  }

  return TargetInstrInfo::getOperandLatency(ItinData, DefMI, DefIdx,
                                                      UseMI, UseIdx);
}

int PatmosInstrInfo::getDefOperandLatency(const InstrItineraryData *ItinData,
                                          const MachineInstr *DefMI,
                                          unsigned DefIdx) const
{
  int Latency = TargetInstrInfo::getDefOperandLatency(ItinData, DefMI, DefIdx);

  const MachineOperand &MO = DefMI->getOperand(DefIdx);

  // Patmos specific: GPRs are always bypassed and read in cycle 1, so we can
  // reduce the latency of edges to ExitSU by 1.
  if (MO.isReg() && MO.isDef() && RI.isRReg(MO.getReg()) && Latency > 0) {
    Latency--;
  }

  return Latency;
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

bool PatmosInstrInfo::mayFallthrough(MachineBasicBlock &MBB) const {

  // Look back 1 slot further than the call to catch the case where a SENS
  // is scheduled after an noreturn call delay slot.
  int maxLookback = PST.getCFLDelaySlotCycles(false) + 1;

  // find last terminator
  for(MachineBasicBlock::reverse_iterator t(MBB.rbegin()),
      te(MBB.rend()); t != te && maxLookback >= 0; t++)
  {
    MachineInstr *mi = &*t;

    if (!mi->isPseudo(MachineInstr::AllInBundle)) {
      maxLookback--;
    }

    if (mi->isCall()) {
      const Function *F = getCallee(mi);
      if (F && F->hasFnAttribute(Attribute::NoReturn)) {
        return false;
      }
    }

    // skip non-terminator instructions
    if (!mi->isTerminator()) {
      continue;
    }

    // fix opcode for branch instructions to set barrier flag correctly
    fixOpcodeForGuard(mi);

    return !mi->isBarrier();
  }

  return true;
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
      AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BRu))).addMBB(TBB);
    } else { // Conditional branch.
      BuildMI(&MBB, DL, get(Patmos::BR))
        .addOperand(Cond[0]).addOperand(Cond[1])
        .addMBB(TBB);
    }
    return 1;
  }

  // Two-way Conditional branch.
  BuildMI(&MBB, DL, get(Patmos::BR))
    .addOperand(Cond[0]).addOperand(Cond[1])
    .addMBB(TBB);
  AddDefaultPred(BuildMI(&MBB, DL, get(Patmos::BRu)))
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

bool PatmosInstrInfo::haveDisjointPredicates(const MachineInstr* MI1,
                                             const MachineInstr* MI2) const
{
  if (!MI1->getDesc().isPredicable() || !MI2->getDesc().isPredicable()) {
    return false;
  }

  unsigned Pos1 = MI1->getDesc().getNumDefs();
  unsigned Pos2 = MI2->getDesc().getNumDefs();

  return MI1->getOperand(Pos1).getReg() == MI2->getOperand(Pos2).getReg() &&
         MI1->getOperand(Pos1+1).getImm() != MI2->getOperand(Pos2+1).getImm();
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

    // Fix opcode from uncond. to cond.
    fixOpcodeForGuard(MI);
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
