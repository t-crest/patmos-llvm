//===- PatmosInstrInfo.h - Patmos Instruction Information -------*- C++ -*-===//
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

#ifndef _LLVM_TARGET_PATMOS_INSTRINFO_H_
#define _LLVM_TARGET_PATMOS_INSTRINFO_H_

#include "llvm/ADT/SmallSet.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCNullStreamer.h"
#include "PatmosRegisterInfo.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "MCTargetDesc/PatmosBaseInfo.h"

#define GET_INSTRINFO_HEADER
#include "PatmosGenInstrInfo.inc"


namespace llvm {

class PatmosTargetMachine;
class PatmosSubtarget;

// TODO move this class into a separate header, track call sites and stack
// cache control instructions, use in CallGraphBuilder, ...
class PatmosInstrAnalyzer : public MCNullStreamer {
  const MCInstrInfo &MII;
  unsigned count;
  unsigned size;
  bool call;
public:
  PatmosInstrAnalyzer(MCContext &ctx)
    : MCNullStreamer(ctx), MII(*ctx.getInstrInfo()), count(0), size(0),
      call(false)
    {
    }

  void reset() {
    count = 0;
    size = 0;
    call = false;
  }

  unsigned getCount() const { return count; }

  unsigned getSize() const { return size; }

  bool hasCall() const { return call; }

  virtual void EmitInstruction(const MCInst &Inst) {
    const MCInstrDesc &MID = MII.get(Inst.getOpcode());
    count++;
    size += MID.getSize();
    call |= MID.isCall();
  }
};

class PatmosInstrInfo : public PatmosGenInstrInfo {
  PatmosTargetMachine &PTM;
  const PatmosRegisterInfo RI;
  const PatmosSubtarget &PST;
public:
  explicit PatmosInstrInfo(PatmosTargetMachine &TM);

  virtual ~PatmosInstrInfo() {}

  /// getRegisterInfo - TargetInstrInfo is a superset of MRegister info.  As
  /// such, whenever a client has an instance of instruction info, it should
  /// always be able to get register info as well (through this method).
  ///
  virtual const TargetRegisterInfo &getRegisterInfo() const { return RI; }

  const PatmosRegisterInfo &getPatmosRegisterInfo() const { return RI; }

  /// findCommutedOpIndices - If specified MI is commutable, return the two
  /// operand indices that would swap value. Return false if the instruction
  /// is not in a form which this routine understands.
  virtual bool findCommutedOpIndices(MachineInstr *MI, unsigned &SrcOpIdx1,
                                     unsigned &SrcOpIdx2) const;

  void copyPhysReg(MachineBasicBlock &MBB,
                   MachineBasicBlock::iterator I, DebugLoc DL,
                   unsigned DestReg, unsigned SrcReg,
                   bool KillSrc) const;

  void storeRegToStackSlot(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator MI,
                           unsigned SrcReg, bool isKill,
                           int FrameIndex,
                           const TargetRegisterClass *RC,
                           const TargetRegisterInfo *TRI) const;
  void loadRegFromStackSlot(MachineBasicBlock &MBB,
                            MachineBasicBlock::iterator MI,
                            unsigned DestReg, int FrameIdx,
                            const TargetRegisterClass *RC,
                            const TargetRegisterInfo *TRI) const;

  /// isStoreToStackSlot - If the specified machine instruction is a direct
  /// store to a stack slot, return the virtual or physical register number of
  /// the source reg along with the FrameIndex of the loaded stack slot.  If
  /// not, return 0.  This predicate must return 0 if the instruction has
  /// any side effects other than storing to the stack slot.
  unsigned isStoreToStackSlot(const MachineInstr *MI,
                              int &FrameIndex) const;

  /// isLoadFromStackSlot - If the specified machine instruction is a direct
  /// load from a stack slot, return the virtual or physical register number of
  /// the destination along with the FrameIndex of the loaded stack slot.  If
  /// not, return 0.  This predicate must return 0 if the instruction has
  /// any side effects other than loading from the stack slot.
  unsigned isLoadFromStackSlot(const MachineInstr *MI,
                               int &FrameIndex) const;

  /// insertNoop - Insert a noop into the instruction stream at the specified
  /// point.
  virtual void insertNoop(MachineBasicBlock &MBB,
                          MachineBasicBlock::iterator MI) const;


  /// expandPostRAPseudo - This function is called for all pseudo instructions
  /// that remain after register allocation. Many pseudo instructions are
  /// created to help register allocation. This is the place to convert them
  /// into real instructions. The target can edit MI in place, or it can insert
  /// new instructions and erase MI. The function should return true if
  /// anything was changed.
  virtual bool expandPostRAPseudo(MachineBasicBlock::iterator MI) const;

  /// isSchedulingBoundary - Test if the given instruction should be
  /// considered a scheduling boundary.
  virtual bool isSchedulingBoundary(const MachineInstr *MI,
                                              const MachineBasicBlock *MBB,
                                              const MachineFunction &MF) const;

  virtual DFAPacketizer*
  CreateTargetScheduleState(const TargetMachine *TM,
                            const ScheduleDAG *DAG) const;

  /// fixOpcodeForGuard - If the MCID opcode is for an unconditional
  /// instruction (e.g. by the isBarrier flag), but the predicate says
  /// otherwise (and vice versa), rewrite the instruction accordingly.
  /// Returns true iff the instruction was rewritten.
  bool fixOpcodeForGuard(MachineInstr *MI) const;

  /// findPrevDelaySlotEnd - Find the end of the previous delay slot, if any.
  /// \param II - The instruction from where to start, will be set to the last
  ///             checked instruction, i.e. the branch if a delay slot is found.
  /// \param Cycles - Maximum number of cycles to search for an end of a delay
  ///                 slot, or -1 to search to start of BB.
  /// \return 0 or positive indicates the number of instructions between II and
  ///         the last instruction of the previous delay slot end, i.e., the
  ///         number of instructions II can be moved up before entering a delay
  ///         slot. A negative value indicates how many instructions after II
  ///         are in the same delay slot as II, including II.
  int findPrevDelaySlotEnd(MachineBasicBlock &MBB,
                           MachineBasicBlock::iterator &II,
                           int Cycles = -1) const;

  /// moveTo - Move an instruction to a given target instruction, either
  /// replacing a NOP at the target or bundling it with the instruction at the
  /// target, if possible.
  /// \param Target The pointer to the instruction to replace or bundle with the
  ///               the source. Will be set to the new instruction or the bundle.
  /// \param Source The instruction to move.
  /// \param Pred If set, set the guard of the Source instruction to the
  ///             predicate defined by Pred on a successful move.
  /// \param Negate If true, negate the predicate when setting a new predicate.
  /// \return True on a successful move, false if the instruction has not been
  ///         moved.
  bool moveTo(MachineBasicBlock &MBB,
              MachineBasicBlock::iterator &Target,
              MachineBasicBlock::iterator &Source,
              SmallVectorImpl<MachineOperand> *Pred = NULL,
              bool Negate = false) const;

  /// moveUp - Move an instruction up by its delay slot cycles.
  /// Assumes the instruction does not have any dependency to previous
  /// instructions.
  /// Returns the number of cycles not rescheduled after the instruction.
  unsigned moveUp(MachineBasicBlock &MBB,
                  MachineBasicBlock::iterator &II) const;

  /// moveUp - Move an instruction up by a given number of cycles if possible.
  /// Assumes the instruction does not have any dependency to previous
  /// instructions.
  /// Returns the number of cycles not rescheduled after the instruction.
  unsigned moveUp(MachineBasicBlock &MBB,
                  MachineBasicBlock::iterator &II, unsigned Cycles) const;

  /// isStackControl - Return true if the instruction controls the stack cache.
  bool isStackControl(const MachineInstr *MI) const;

  /// isSideEffectFreeSRegAccess- return true if the instruction is a MTS/MFS
  /// to/from a special register without side-effects
  bool isSideEffectFreeSRegAccess(const MachineInstr *MI) const;

  /// getMemType - Return the type for Patmos' typed memory accesses.
  /// MI must be either a load or a store instruction.
  PatmosII::MemType getMemType(const MachineInstr *MI) const;

  /// isPseudo - check if the given machine instruction is emitted, i.e.,
  /// if the instruction is either inline asm or has some FU assigned to it.
  bool isPseudo(const MachineInstr *MI) const;

  /// skipPseudos - Increment II to the next non-pseudo instruction if II is a
  /// pseudo instruction.
  void skipPseudos(MachineBasicBlock &MBB,
                   MachineBasicBlock::instr_iterator &II) const;

  /// skipPseudos - Increment II to the next non-pseudo instruction if II is a
  /// pseudo instruction. This assumes that bundles contain at least one
  /// non-pseudo instruction.
  void skipPseudos(MachineBasicBlock &MBB,
                   MachineBasicBlock::iterator &II) const;

  /// nextNonPseudo - Get the previous non-pseudo instruction or bundle.
  MachineBasicBlock::iterator prevNonPseudo(MachineBasicBlock &MBB,
                   const MachineBasicBlock::iterator &II) const;

  /// nextNonPseudo - Get the next non-pseudo instruction or bundle.
  MachineBasicBlock::iterator nextNonPseudo(MachineBasicBlock &MBB,
                   const MachineBasicBlock::iterator &II) const;

  /// recedeCycles - Move the iterator back by a given number of cycles, skipping
  /// pseudo instructions.
  /// \return false when receding the iterator stopped due to inline asm.
  bool recedeCycles(MachineBasicBlock &MBB,
                    MachineBasicBlock::iterator &II, unsigned Cycles,
                    bool StopOnInlineAsm = true) const;

  /// advanceCycles - Move the iterator down by a given number of cycles,
  /// skipping pseudo instructions.
  /// \return false when advancing the iterator stopped due to inline asm.
  bool advanceCycles(MachineBasicBlock &MBB,
                     MachineBasicBlock::iterator &II, unsigned Cycles,
                     bool StopOnInlineAsm = true) const;

  /// hasOpcode - check if the given instruction has the given opcode, or if
  /// the bundle contains an instruction with the opcode if this is a bundle.
  /// Returns either the first instruction in the bundle matching the opcode,
  /// the instruction itself, or 0 if no instruction matches the opcode.
  const MachineInstr *hasOpcode(const MachineInstr *MI, int Opcode) const;

  /// hasRegUse - Check if the given instruction uses any register.
  bool hasRegUse(const MachineInstr *MI) const;

  // getFirstMI - Return MI or the first 'real' instruction if MI is a bundle.
  const MachineInstr* getFirstMI(const MachineInstr *MI) const;

  PatmosInstrAnalyzer *createPatmosInstrAnalyzer(MCContext &Ctx) const;

  /// getInstrSize - get the size of an instruction.
  /// Correctly deals with inline assembler and bundles.
  unsigned int getInstrSize(const MachineInstr *MI) const;

  /// hasCall - check if there is a call in this instruction.
  /// Correctly deals with inline assembler and bundles.
  bool hasCall(const MachineInstr *MI) const;

  /// mayStall - return true if the MI might cause a memory access that might
  /// miss and stall the CPU. Not checking for instruction fetch related stalls.
  bool mayStall(const MachineInstr *MI) const;

  /// mayStall - return true if the MBB might cause a memory access that might
  /// miss and stall the CPU. Not checking for instruction fetch related stalls.
  bool mayStall(const MachineBasicBlock &MBB) const;

  /// canRemoveFromSchedule - check if the given instruction can be removed
  /// without creating any hazards to surrounding instructions.
  bool canRemoveFromSchedule(MachineBasicBlock &MBB,
                             const MachineBasicBlock::iterator &II) const;

  /// getCallee - try to get the called function, or null if this is not a
  /// call, if the call target is unknown or if there is more than one callee.
  const Function *getCallee(const MachineInstr *MI) const;

  /// getCallees - add all known call targets of an instruction or a bundle.
  /// \return false if there might be additional call targets.
  bool getCallees(const MachineInstr *MI,
                  SmallSet<const Function*,2> &Callees) const;

  /// getIssueWidth - Get the number of slots required for this instruction.
  /// For instructions that must be scheduled on its own this returns the
  /// maximum issue width of the processor.
  unsigned getIssueWidth(const MachineInstr *MI) const;

  /// Check if we can issue an instruction in a given slot
  bool canIssueInSlot(const MCInstrDesc &MID, unsigned Slot) const;

  bool canIssueInSlot(const MachineInstr *MI, unsigned Slot) const;

  virtual int getOperandLatency(const InstrItineraryData *ItinData,
                                const MachineInstr *DefMI, unsigned DefIdx,
                                const MachineInstr *UseMI,
                                unsigned UseIdx) const;

  virtual int getDefOperandLatency(const InstrItineraryData *ItinData,
                                   const MachineInstr *DefMI,
                                   unsigned DefIdx) const;

  /////////////////////////////////////////////////////////////////////////////
  // Branch handling
  /////////////////////////////////////////////////////////////////////////////


  /// getBranchTarget - Get the target machine basic block for direct branches
  MachineBasicBlock *getBranchTarget(const MachineInstr *MI) const;

  /// mayFalltrough - Check if the block might fall through to the next block.
  bool mayFallthrough(MachineBasicBlock &MBB) const;

  /// AnalyzeBranch - Analyze the branching code at the end of MBB, returning
  /// true if it cannot be understood (e.g. it's a switch dispatch or isn't
  /// implemented for a target).
  /// \see TargetInstrInfo::AnalyzeBranch
  virtual bool AnalyzeBranch(MachineBasicBlock &MBB, MachineBasicBlock *&TBB,
                             MachineBasicBlock *&FBB,
                             SmallVectorImpl<MachineOperand> &Cond,
                             bool AllowModify = false) const;

  /// RemoveBranch - Remove the branching code at the end of the specific MBB.
  /// This is only invoked in cases where AnalyzeBranch returns success. It
  /// returns the number of instructions that were removed.
  virtual unsigned RemoveBranch(MachineBasicBlock &MBB) const;

  /// InsertBranch - Insert branch code into the end of the specified
  /// MachineBasicBlock.  The operands to this method are the same as those
  /// returned by AnalyzeBranch.  This is only invoked in cases where
  /// AnalyzeBranch returns success. It returns the number of instructions
  /// inserted.
  /// \see TargetInstrInfo::InsertBranch
  virtual unsigned InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
                                MachineBasicBlock *FBB,
                                const SmallVectorImpl<MachineOperand> &Cond,
                                DebugLoc DL) const;

  /// ReverseBranchCondition - Reverses the branch condition of the specified
  /// condition list, returning false on success and true if it cannot be
  /// reversed.
  virtual bool ReverseBranchCondition(SmallVectorImpl<MachineOperand> &Cond)
                                      const;




  /////////////////////////////////////////////////////////////////////////////
  // Predication and IfConversion
  /////////////////////////////////////////////////////////////////////////////

  /// isPredicated - If the instruction has other than default predicate
  /// operands (p0), return true.
  /// Return false if the branch instruction has default predicate operands.
  virtual bool isPredicated(const MachineInstr *MI) const;

  /// isUnpredicatedTerminator - Returns true if the instruction is a
  /// terminator instruction that has not been predicated.
  /// IMPORTANT: returns also true for conditional branches,
  ///            they are an exception
  virtual bool isUnpredicatedTerminator(const MachineInstr *MI) const;

  /// haveDisjointPredicates - check if the predicates of the two instructions
  /// can never be true at the same time (but they might be false at the same
  /// time).
  bool haveDisjointPredicates(const MachineInstr* MI1,
                              const MachineInstr* MI2) const;


  /// getPrediate - load the guards of an instruction into Pred. If the
  /// instruction is a bundle, get all predicates of the bundle.
  /// Return true if any predicate is found.
  bool getPredicateOperands(const MachineInstr* MI,
                            SmallVectorImpl<MachineOperand> &Pred) const;

  /// negatePredicate - invert the flag of the guard of the instruction.
  /// Return true on success.
  bool NegatePredicate(MachineInstr *MI) const;

  /// PredicateInstruction - Convert the instruction into a predicated
  /// instruction. It returns true if the operation was successful.
  virtual
  bool PredicateInstruction(MachineInstr *MI,
                            const SmallVectorImpl<MachineOperand> &Pred) const;

  /// SubsumesPredicate - Returns true if the first specified predicate
  /// subsumes the second.
  /// For Patmos, the default predicate subsumes all others.
  /// For all other cases, predicate equality is checked.
  virtual
  bool SubsumesPredicate(const SmallVectorImpl<MachineOperand> &Pred1,
                         const SmallVectorImpl<MachineOperand> &Pred2) const;

  /// DefinesPredicate - If the specified instruction defines any predicate
  /// register, it returns true as well as the defined predicate register.
  /// NOTE: currently this is the only place where only one operand is put
  ///       into the Pred vector.
  virtual bool DefinesPredicate(MachineInstr *MI,
                                std::vector<MachineOperand> &Pred) const;

  /// isProfitableToIfCvt - Return true if it's profitable to predicate
  /// instructions with accumulated instruction latency of "NumCycles"
  /// of the specified basic block, where the probability of the instructions
  /// being executed is given by Probability, and Confidence is a measure
  /// of our confidence that it will be properly predicted.
  virtual
  bool isProfitableToIfCvt(MachineBasicBlock &MBB, unsigned NumCycles,
                           unsigned ExtraPredCycles,
                           const BranchProbability &Probability) const {

    const MCInstrDesc &MCID = prior(MBB.end())->getDesc();
    if (MCID.isReturn() || MCID.isCall())
      return false;
    if (NumCycles > 8)
      return false;

    // We do not handle predicated instructions that may stall the pipeline
    // properly in the cache analyses, so we do not convert them for now.
    return !mayStall(MBB);
  }

  /// isProfitableToIfCvt - Second variant of isProfitableToIfCvt, this one
  /// checks for the case where two basic blocks from true and false path
  /// of a if-then-else (diamond) are predicated on mutally exclusive
  /// predicates, where the probability of the true path being taken is given
  /// by Probability, and Confidence is a measure of our confidence that it
  /// will be properly predicted.
  virtual bool
  isProfitableToIfCvt(MachineBasicBlock &TMBB,
                      unsigned NumTCycles, unsigned ExtraTCycles,
                      MachineBasicBlock &FMBB,
                      unsigned NumFCycles, unsigned ExtraFCycles,
                      const BranchProbability &Probability) const {
    const MCInstrDesc &TMCID = prior(TMBB.end())->getDesc();
    if (TMCID.isReturn() || TMCID.isCall())
      return false;
    const MCInstrDesc &FMCID = prior(FMBB.end())->getDesc();
    if (FMCID.isReturn() || FMCID.isCall())
      return false;
    if ((NumTCycles + NumFCycles) > 16)
      return false;

    // We do not handle predicated instructions that may stall the pipeline
    // properly in the cache analyses, so we do not convert them for now.
    return !mayStall(TMBB) && !mayStall(FMBB);
  }

  /// isProfitableToDupForIfCvt - Return true if it's profitable for
  /// if-converter to duplicate instructions of specified accumulated
  /// instruction latencies in the specified MBB to enable if-conversion.
  /// The probability of the instructions being executed is given by
  /// Probability, and Confidence is a measure of our confidence that it
  /// will be properly predicted.
  virtual bool
  isProfitableToDupForIfCvt(MachineBasicBlock &MBB, unsigned NumCycles,
                            const BranchProbability &Probability) const {
    const MCInstrDesc &MCID = prior(MBB.end())->getDesc();
    if (MCID.isReturn() || MCID.isCall())
      return false;
    return NumCycles <= 4;
  }

}; // PatmosInstrInfo

static inline
const MachineInstrBuilder &AddDefaultPred(const MachineInstrBuilder &MIB) {
  // predicate: always true
  return MIB.addReg(Patmos::NoRegister).addImm(0);
}


static inline
bool HasALUlVariant(unsigned Opcode, unsigned &ALUlOpcode) {
  using namespace Patmos;

  switch (Opcode) {
  case ADDi:  ALUlOpcode = ADDl;  return true;
  case SUBi:  ALUlOpcode = SUBl;  return true;
  case XORi:  ALUlOpcode = XORl;  return true;
  // No need for ALUl versions of SL, SR, SRA: they only use 5bit immediates anyway
  case ORi:   ALUlOpcode = ORl;   return true;
  case ANDi:  ALUlOpcode = ANDl;  return true;
  case LIi:   ALUlOpcode = LIl;   return true;
  case CADDi: ALUlOpcode = CADDl; return true;
  default: return false;
  }
}

/// HasPCrelImmediate - check if the instruction with the given opcode and
/// MID has a PC relative immediate (Format == CFLi && Opcode == BR/BRu).
static inline
bool HasPCRELImmediate(unsigned Opcode, const MCInstrDesc &MID) {
  uint64_t Format = (MID.TSFlags & PatmosII::FormMask);
  // CFLi and Opcode == BR/BRu => immediate is PCrel
  if (Format != PatmosII::FrmCFLi) return false;
  return (Opcode == Patmos::BR || Opcode == Patmos::BRu 
          || Opcode == Patmos::BRND || Opcode == Patmos::BRNDu);
}


} // end namespace llvm

#endif // _LLVM_TARGET_PATMOS_INSTRINFO_H_
