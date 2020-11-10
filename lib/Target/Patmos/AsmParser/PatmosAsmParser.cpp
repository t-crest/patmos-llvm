//===-- PatmosAsmParser.cpp - Parse Patmos assembly to MCInst instructions ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "PatmosInstrInfo.h"
#include "InstPrinter/PatmosInstPrinter.h"
#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "MCTargetDesc/PatmosMCAsmInfo.h"
#include "MCTargetDesc/PatmosMCAsmInfo.h"
#include "MCTargetDesc/PatmosTargetStreamer.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/OwningPtr.h"

using namespace llvm;

namespace {
struct PatmosOperand;

class PatmosAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;
  const MCInstrInfo &MII;

  // Remember if we are inside of a bundle marker
  bool InBundle;

  // keep track of the bundle bit of the last instructions
  unsigned BundleCounter;

  unsigned IssueWidth;

  PrintBytesLevel ParseBytes;

  MCAsmParser &getParser() const { return Parser; }
  MCAsmLexer &getLexer() const { return Parser.getLexer(); }

  void Warning(SMLoc L, const Twine &Msg) { Parser.Warning(L, Msg); }
  bool Error(SMLoc L, const Twine &Msg) { return Parser.Error(L, Msg); }

  #define GET_ASSEMBLER_HEADER
  #include "PatmosGenAsmMatcher.inc"

public:
  PatmosAsmParser(MCSubtargetInfo &sti, MCAsmParser &parser,
                  const MCInstrInfo &mii)
    : MCTargetAsmParser(), Parser(parser), MII(mii),
      InBundle(false), BundleCounter(0)
  {
    IssueWidth = sti.getSchedModel()->IssueWidth;

    switch (parser.getAssemblerDialect()) {
    case 0: ParseBytes = PrintAsEncoded; break;
    case 1: ParseBytes = PrintCallAsBytes; break;
    case 2: ParseBytes = PrintAllAsBytes; break;
    }
  }

  virtual bool ParsePrefix(SMLoc &PrefixLoc, SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                           bool &HasPrefix, StringRef PrevToken);

  virtual bool ParseInstruction(ParseInstructionInfo &Info, StringRef Name, SMLoc NameLoc,
                                SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  virtual bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc);

  virtual bool ParseDirective(AsmToken DirectiveID);

  virtual bool MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                               SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                               MCStreamer &Out, unsigned &ErrorInfo, 
			       bool MatchingInlineAsm);

  void EatToEndOfStatement();

private:
  bool ParseOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands, unsigned OpNo);

  bool ParseRegister(SmallVectorImpl<MCParsedAsmOperand*> &Operands, bool EmitError = true);

  /// ParseRegister - This version does not lex the last token so the end token can be retrieved
  bool ParseRegister(unsigned &RegNo, bool Required);

  bool ParseMemoryOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  /// ParsePredicateOperand - parse a predicate operand including an optional negate flag. Adds two
  /// operands.
  /// \param checkClass - if true, only add the flag operand if the register is a predicate register
  bool ParsePredicateOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands, bool checkClass = false);

  bool ParseImmediate(SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  /// ParseToken - Check if the Lexer is currently over the given token kind, and add it as operand if so.
  bool ParseToken(SmallVectorImpl<MCParsedAsmOperand*> &Operands, AsmToken::TokenKind Kind);

  /// isPredSrcOperand - Check whether the operand might be a predicate source operand (i.e., has a negate flag)
  bool isPredSrcOperand(StringRef Mnemonic, unsigned OpNo);

  bool ParseDirectiveWord(unsigned Size, SMLoc L);

  bool ParseDirectiveFStart(SMLoc L);
};

/// PatmosOperand - Instances of this class represent a parsed Patmos machine
/// instruction.
struct PatmosOperand : public MCParsedAsmOperand {
    enum KindTy {
      Token,
      Immediate,
      Register,
      Memory
    } Kind;

    SMLoc StartLoc, EndLoc;

    struct TokOp {
      const char *Data;
      unsigned Length;
    };

    struct RegOp {
      unsigned RegNum;
    };

    struct ImmOp {
      const MCExpr *Val;
    };

    struct MemOp {
      unsigned Base;
      const MCExpr *Off;
    };

    union {
      struct TokOp Tok;
      struct RegOp Reg;
      struct ImmOp Imm;
      struct MemOp Mem;
    };

    PatmosOperand(KindTy K) : MCParsedAsmOperand(), Kind(K) {}
  public:
    PatmosOperand(const PatmosOperand &o) : MCParsedAsmOperand() {
      Kind = o.Kind;
      StartLoc = o.StartLoc;
      EndLoc = o.EndLoc;
      switch (Kind) {
      case Register:
        Reg = o.Reg;
        break;
      case Immediate:
        Imm = o.Imm;
        break;
      case Token:
        Tok = o.Tok;
        break;
      case Memory:
        Mem = o.Mem;
        break;
      }
    }

    /// getStartLoc - Get the location of the first token of this operand.
    SMLoc getStartLoc() const { return StartLoc; }

    /// getEndLoc - Get the location of the last token of this operand.
    SMLoc getEndLoc() const { return EndLoc; }

    unsigned getReg() const {
      assert(Kind == Register && "Invalid access!");
      return Reg.RegNum;
    }

    const MCExpr *getImm() const {
      assert(Kind == Immediate && "Invalid access!");
      return Imm.Val;
    }

    unsigned getMemBase() const {
      assert(Kind == Memory && "Invalid access!");
      return Mem.Base;
    }

    const MCExpr* getMemOff() const {
      assert(Kind == Memory && "Invalid access!");
      return Mem.Off;
    }

    bool isToken() const { return Kind == Token; }
    bool isImm() const { return Kind == Immediate; }
    bool isMem() const { return Kind == Memory; }
    bool isReg() const { return Kind == Register; }

    void addExpr(MCInst &Inst, const MCExpr *Expr) const {
      // Add as immediate when possible.  Null MCExpr = 0.
      if (Expr == 0)
        Inst.addOperand(MCOperand::CreateImm(0));
      else if (const MCConstantExpr *CE = dyn_cast<MCConstantExpr>(Expr))
        Inst.addOperand(MCOperand::CreateImm(CE->getValue()));
      else
        Inst.addOperand(MCOperand::CreateExpr(Expr));
    }

    void addRegOperands(MCInst &Inst, unsigned N) const {
      assert(N == 1 && "Invalid number of operands!");
      Inst.addOperand(MCOperand::CreateReg(getReg()));
    }

    void addImmOperands(MCInst &Inst, unsigned N) const {
      assert(N == 1 && "Invalid number of operands!");
      addExpr(Inst, getImm());
    }

    void addMemOperands(MCInst &Inst, unsigned N) const {
      assert(N == 2 && "Invalid number of operands!");

      Inst.addOperand(MCOperand::CreateReg(getMemBase()));

      addExpr(Inst, getMemOff());
    }

    StringRef getToken() const {
      assert(Kind == Token && "Invalid access!");
      return StringRef(Tok.Data, Tok.Length);
    }

    virtual void print(raw_ostream &OS) const;

    static PatmosOperand *CreateToken(StringRef Str, SMLoc S) {
      PatmosOperand *Op = new PatmosOperand(Token);
      Op->Tok.Data = Str.data();
      Op->Tok.Length = Str.size();
      Op->StartLoc = S;
      Op->EndLoc = S;
      return Op;
    }

    static PatmosOperand *CreateReg(unsigned RegNum, SMLoc S, SMLoc E) {
      PatmosOperand *Op = new PatmosOperand(Register);
      Op->Reg.RegNum = RegNum;
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }

    static PatmosOperand *CreateImm(const MCExpr *Val, SMLoc S, SMLoc E) {
      PatmosOperand *Op = new PatmosOperand(Immediate);
      Op->Imm.Val = Val;
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }

    static PatmosOperand *CreateConstant(int value, SMLoc S, SMLoc E, MCContext &Ctx) {
      PatmosOperand *Op = new PatmosOperand(Immediate);
      Op->Imm.Val = MCConstantExpr::Create(value, Ctx);
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }

    static PatmosOperand *CreateFlag(bool flag, SMLoc S, SMLoc E, MCContext &Ctx) {
      return CreateConstant(flag ? 1 : 0, S, E, Ctx);
    }

    static PatmosOperand *CreateMem(unsigned Base, const MCExpr *Off, SMLoc S,
                                    SMLoc E) {
      PatmosOperand *Op = new PatmosOperand(Memory);
      Op->Mem.Base = Base;
      Op->Mem.Off = Off;
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }
};

} // end anonymous namespace.

void PatmosOperand::print(raw_ostream &OS) const {
  switch (Kind) {
  case Immediate:
    getImm()->print(OS);
    break;
  case Register:
    OS << "<register ";
    OS << PatmosInstPrinter::getRegisterName(getReg()) << ">";
    break;
  case Token:
    OS << "'" << getToken() << "'";
    break;
  case Memory: {
    OS << "<memory ";
    OS << getMemBase();
    OS << ", ";

    OS << getMemOff();
    OS << ">";
    }
    break;
  }
}


/// @name Auto-generated Match Functions
/// {

#define GET_REGISTER_MATCHER
#define GET_MATCHER_IMPLEMENTATION
#include "PatmosGenAsmMatcher.inc"

/// }


bool PatmosAsmParser::
MatchAndEmitInstruction(SMLoc IDLoc, unsigned &Opcode,
                        SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                        MCStreamer &Out, unsigned &ErrorInfo, 
			bool MatchingInlineAsm) {
  MCInst Inst;
  SMLoc ErrorLoc;
  
  // Extract the predicate operands, as 'MatchInstructionImpl' cannot 
  // handle them. They will be reinstated later.
  auto p = Operands[1];
  auto flag = Operands[2];
  Operands.erase(next(Operands.begin()));
  Operands.erase(next(Operands.begin()));

  switch (MatchInstructionImpl(Operands, Inst, ErrorInfo, MatchingInlineAsm, 0))
  {
  default: break;
  case Match_Success:
  {
    const MCInstrDesc &MID = MII.get(Inst.getOpcode());

    // The predicate operands must be the first input operands
    // i.e. after the output operands (also called definitions)
    auto insert_pred_at = MID.getNumDefs();
    auto insert_pred_flag_at = insert_pred_at + 1;

    // Insert the predicate register
    Inst.insert(
      next(Inst.begin(), insert_pred_at),
      MCOperand::CreateReg(static_cast<PatmosOperand*>(p)->getReg())
    );

    // Insert the predicate flag.
    int64_t flag_value;
    static_cast<PatmosOperand*>(flag)->getImm()->EvaluateAsAbsolute(flag_value);
    // For some instructions a stray immediate operand is present at this point
    // that should not be there. It is a remnant of a '=' being represented by it
    // before the call to 'MatchInstructionImpl'.
    // We overwrite this immediate value with the flag, if its there.
    // Otherwise we make a new immediate for the flag.
    //
    // Note:
    // I don't know why the immediate is there for instruction with '=', 
    // it probably has something to do
    // with how the tablegen is implemented (which dictates how 'MatchInstructionImpl' 
    // works). If this is fixed in the tablegen, such that the immediate is no longer
    // there at this point, a new immediate should always be created for the flag.	
    // For now, the if statement below is a workaround.
    if ( !next(Inst.begin(),insert_pred_flag_at)->isImm() || Inst.getNumOperands()<3 ) {
      Inst.insert(next(Inst.begin(),insert_pred_flag_at), MCOperand::CreateImm(0));
    }
    next(Inst.begin(),insert_pred_flag_at)->setImm(flag_value);

    // Add bundle marker
    Inst.addOperand(MCOperand::CreateImm(InBundle));

    // If we have an ALUi immediate instruction and the immediate does not fit
    // 12bit, use ALUl version of instruction
    uint64_t Format = MID.TSFlags & PatmosII::FormMask;
    unsigned ImmOpNo = getPatmosImmediateOpNo( MID.TSFlags );
    bool ImmSigned = isPatmosImmediateSigned( MID.TSFlags );
    bool HasImm = hasPatmosImmediate( MID.TSFlags );

    if (HasImm) {
      MCOperand &MCO = Inst.getOperand( ImmOpNo );

      // If we have an immediate, shift the value according to the asm dialect
      if (MCO.isImm() && (ParseBytes == PrintAllAsBytes ||
          (ParseBytes == PrintCallAsBytes && Inst.getOpcode() == Patmos::CALL)))
      {
        unsigned ImmShift = getPatmosImmediateShift( MID.TSFlags );
        MCO.setImm(MCO.getImm() / (1 << ImmShift));
      }
    }

    unsigned ALUlOpcode;
    if (Format == PatmosII::FrmALUi && HasImm) {
      MCOperand &MCO = Inst.getOperand( ImmOpNo );

      if (MCO.isExpr()) {
        if (HasALUlVariant(Inst.getOpcode(), ALUlOpcode)){
          if (InBundle) {
            return Error(IDLoc, "long immediate instruction cannot be in the second slot of a bundle");
          } else {
            // If we have an expression and can use ALUl, do so
            Inst.setOpcode(ALUlOpcode);
            // ALUl counts as two operations
            BundleCounter++;
          }
        }
      } else {
        assert(MCO.isImm() && "expected immediate operand for ALUi format");

        if (!isUInt<12>(MCO.getImm())) {
          if (isUInt<12>(-MCO.getImm()) && Inst.getOpcode() == Patmos::LIi) {
            // Make this an sub instead
            MCO.setImm(-MCO.getImm());
            Inst.setOpcode(Patmos::LIin);
          }
          else if (HasALUlVariant(Inst.getOpcode(), ALUlOpcode)) {
            if (InBundle || BundleCounter) {
              return Error(IDLoc, "immediate size requires ALUl but ALUl must "
                                  "not appear inside bundles");
            }
            Inst.setOpcode(ALUlOpcode);
            // ALUl counts as two operations
            BundleCounter++;

            if (!isInt<32>(MCO.getImm()) && !isUInt<32>(MCO.getImm())) {
              return Error(IDLoc,"immediate operand too large for ALUl format");
            }
          }
          else {
            return Error(IDLoc, "immediate operand too large for ALUi format "
                                "and ALUl is not available for this opcode");
          }
        }
      }
      if (BundleCounter > IssueWidth) {
        return Error(IDLoc, "operand size required ALUl instruction, but this "
                            "makes the bundle too large");
      }
    }
    else if (Format == PatmosII::FrmALUl) {
      if (InBundle || BundleCounter) {
        return Error(IDLoc, "ALUl instruction cannot be bundled");
      }
      // ALUl counts as two instructions
      BundleCounter++;

      MCOperand &MCO = Inst.getOperand( ImmOpNo );

      // No immediates larger than 32bit allowed ..
      if (MCO.isImm() && !isInt<32>(MCO.getImm()) && !isUInt<32>(MCO.getImm()))
      {
        return Error(IDLoc, "immediate operand too large for ALUl format");
      }
    }

    if (Format == PatmosII::FrmCFLi || Format == PatmosII::FrmSTCi) {
      const MCOperand &MCO = Inst.getOperand(ImmOpNo);
      if (!MCO.isExpr()) {
        assert(MCO.isImm() && "expected immediate operand for ALUi format");

        if ((Format == PatmosII::FrmSTCi && !isUInt<18>(MCO.getImm())) ||
            ( ImmSigned && !isInt<22>(MCO.getImm())) ||
            (!ImmSigned && !isUInt<22>(MCO.getImm()))) {
          return Error(IDLoc, "immediate operand is out of range");
        }
      }
    }

    if (Format == PatmosII::FrmSTT || Format == PatmosII::FrmLDT) {
      const MCOperand &MCO = Inst.getOperand(ImmOpNo);
      if (!MCO.isExpr()) {
        assert(MCO.isImm() && "expected immediate operand for ALUi format");

        if (( ImmSigned && !isInt<7>(MCO.getImm())) ||
            (!ImmSigned && !isUInt<7>(MCO.getImm()))) {
          return Error(IDLoc, "immediate offset is out of range");
        }
      }
    }

    if (BundleCounter >= IssueWidth) {
      Error(IDLoc, "the bundle consists of too many instructions");
      return true;
    }

    BundleCounter = InBundle ? BundleCounter + 1 : 0;

    Out.EmitInstruction(Inst);
    return false;
  }
  case Match_MissingFeature:
    return Error(IDLoc, "instruction use requires an option to be enabled");
  case Match_MnemonicFail:
      return Error(IDLoc, "unrecognized instruction mnemonic");
  case Match_InvalidOperand:
    ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((PatmosOperand*)Operands[ErrorInfo])->getStartLoc();
      if (ErrorLoc == SMLoc()) ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction or syntax mismatch");
  }

  llvm_unreachable("Implement any new match types added!");
}

bool PatmosAsmParser::
ParseRegister(SmallVectorImpl<MCParsedAsmOperand*> &Operands, bool EmitError) {
  MCAsmLexer &Lexer = getLexer();
  SMLoc S = Lexer.getLoc();

  unsigned RegNo = 0;
  if (ParseRegister(RegNo, false)) {
    // syntax error
    return true;
  }
  if (RegNo == 0) {
    // missing register
    return !EmitError || Error(S, "Missing register name");
  }

  SMLoc E = Lexer.getLoc();
  Lexer.Lex();

  Operands.push_back(PatmosOperand::CreateReg(RegNo, S, E));

  return false;
}

bool PatmosAsmParser::
ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) {
  if (ParseRegister(RegNo, false)) {
    return true;
  }
  getLexer().Lex();
  return false;
}

bool PatmosAsmParser::
ParseRegister(unsigned &RegNo, bool Required) {
  MCAsmLexer &Lexer = getLexer();

  if (Lexer.getKind() == AsmToken::Dollar) {
    Lexer.Lex();
  } else {
    return Required;
  }
  if (Lexer.getKind() == AsmToken::Identifier) {
    StringRef RegName = Lexer.getTok().getIdentifier();
    RegNo = MatchRegisterName(RegName);

    // Handle alternative register names
    if (!RegNo) {
      RegNo = StringSwitch<unsigned>(RegName)
        .Case("sl", Patmos::SL)
        .Case("sh", Patmos::SH)
        .Case("ss", Patmos::SS)
        .Case("st", Patmos::ST)
        .Case("srb", Patmos::SRB)
        .Case("sro", Patmos::SRO)
        .Case("sxb", Patmos::SXB)
        .Case("sxo", Patmos::SXO)
        .Default(0);
    }

    // If name does not match after $ prefix, this is always an error
    return (RegNo == 0) && Error(Lexer.getLoc(), "register name not valid");
  }
  // Syntax error: $ and no identifier is always an error
  return Error(Lexer.getLoc(), "register prefix $ is not followed by a register name");
}

bool PatmosAsmParser::
ParseMemoryOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands)  {
  MCAsmLexer &Lexer = getLexer();
  SMLoc StartLoc = Lexer.getLoc();

  if (ParseToken(Operands, AsmToken::LBrac)) {
    return true;
  }

  // try to match rN +/- Imm, rN, or Imm

  if (ParseRegister(Operands, false)) {

    // add default register
    SMLoc EndLoc = Lexer.getLoc();
    Operands.push_back(PatmosOperand::CreateReg(Patmos::R0, StartLoc, EndLoc));

  } else {

    if (Lexer.is(AsmToken::RBrac)) {
      // Default offset
      SMLoc E = Lexer.getLoc();
      Operands.push_back(PatmosOperand::CreateConstant(0, E, E, getParser().getContext()));

      return ParseToken(Operands, AsmToken::RBrac);

    } else if (Lexer.is(AsmToken::Plus)) {
      // lex away the plus symbol, leave a minus, fail on everything else
      Lexer.Lex();
    } else if (Lexer.isNot(AsmToken::Minus)) {
      return Error(Lexer.getLoc(), "invalid separator between register and offset");
    }
  }

  if (ParseImmediate(Operands)) {
    return true;
  }

  return ParseToken(Operands, AsmToken::RBrac);
}

bool PatmosAsmParser::
ParsePredicateOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands, bool checkClass)  {
  MCAsmLexer &Lexer = getLexer();
  SMLoc StartLoc = Lexer.getLoc();

  bool flag = false;
  if (Lexer.is(AsmToken::Exclaim)) {
    flag = true;
    Lexer.Lex();
  }

  SMLoc RegLoc = Lexer.getLoc();

  if (ParseRegister(Operands)) {
    return true;
  }

  if (checkClass) {
    PatmosOperand *Op = (PatmosOperand*)Operands.back();
    if (!Op->isReg()) return Error(Lexer.getLoc(), "magic happened: we found a register but the operand is not a register");

    // TODO There really should be a nicer way of doing this, but we do not have access to the RegisterInfo stuff here
    if (PatmosInstPrinter::getRegisterName(Op->getReg())[0] != 'p') {
      // Not a predicate register, do not emit a flag operand
      if (flag) {
        Error(StartLoc, "Negation of registers other than predicates is invalid.");
      }
      return false;
    }
  }

  Operands.push_back(PatmosOperand::CreateFlag(flag, StartLoc, RegLoc, getParser().getContext()));

  return false;
}

bool PatmosAsmParser::
ParseOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands, unsigned OpNo)  {
  MCAsmLexer &Lexer = getLexer();

  // Handle all the various operand types here: Imm, reg, memory, predicate, label
  if (Lexer.is(AsmToken::LBrac)) {
    return ParseMemoryOperand(Operands);
  }
  if (Lexer.is(AsmToken::Exclaim)) {
    // we never allow a negated predicate as first out operand
    if (OpNo == 0) {
      return Error(Lexer.getLoc(), "destination predicate cannot be negated");
    }
    return ParsePredicateOperand(Operands);
  }
  if (Lexer.is(AsmToken::Dollar)) {

    StringRef Mnemonic = ((PatmosOperand*)Operands[0])->getToken();
    if (isPredSrcOperand(Mnemonic, OpNo)) {
      return ParsePredicateOperand(Operands, true);
    }

    return ParseRegister(Operands);
  }
  if (Lexer.is(AsmToken::Identifier)) {
    // Parse it as a label
    return ParseImmediate(Operands);
  }

  // Parse as immediate or some other form of symbolic expression
  return ParseImmediate(Operands);
}

bool PatmosAsmParser::ParseImmediate(SmallVectorImpl<MCParsedAsmOperand*> &Operands) {
  MCAsmLexer &Lexer = getLexer();
  SMLoc S = Lexer.getLoc();

  const MCExpr *EVal;
  switch (Lexer.getKind()) {
  default: return true;
  case AsmToken::LParen:
  case AsmToken::Plus:
  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::Identifier:
    SMLoc E;
    if (getParser().parseExpression(EVal, E))
      return true;

    Operands.push_back(PatmosOperand::CreateImm(EVal, S, E));
    return false;
  }
}

bool PatmosAsmParser::ParseToken(SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                                 AsmToken::TokenKind Kind)
{
  MCAsmLexer &Lexer = getLexer();

  if (Lexer.isNot(Kind)) {
    return Error(Lexer.getLoc(), "unexpected token");
  }

  Operands.push_back(PatmosOperand::CreateToken(Lexer.getTok().getString(), Lexer.getLoc()));
  Lexer.Lex();

  return false;
}


bool PatmosAsmParser::
ParsePrefix(SMLoc &PrefixLoc, SmallVectorImpl<MCParsedAsmOperand*> &Operands,
    bool &HasGuard, StringRef PrevToken)
{
  MCAsmLexer &Lexer = getLexer();

  if (Lexer.is(AsmToken::RCurly)) {
    // Try to recover..
    InBundle = false;
    BundleCounter = 0;
    // TODO we either need to go back to the previous instruction (but
    // that one might already be emitted and deleted!) or handle this earlier.
    return Error(Lexer.getLoc(), "Closing bracket must appear immediately "
           "after the instruction (for now).");
  }

  // Check if we start a new bundle
  if (PrevToken == "{" || Lexer.is(AsmToken::LCurly)) {
    if (InBundle) {
      return Error(Lexer.getLoc(), "previous bundle has not been closed.");
    }
    InBundle = true;
    if (PrevToken != "{") {
      Lexer.Lex();
    }

    // Allow newline(s) following '{'
    while (Lexer.is(AsmToken::EndOfStatement) &&
           Lexer.getTok().getString() != ";")
    {
      // TODO accept # comments
      Lexer.Lex();
    }
  }

  // If it starts with '(', assume this is a guard, and try to parse it, otherwise skip
  if (PrevToken != "(") {
    if (Lexer.isNot(AsmToken::LParen)) {
      return false;
    }
    Lexer.Lex();
  }

  HasGuard = true;

  if (ParsePredicateOperand(Operands)) {
    return true;
  }

  if (Lexer.isNot(AsmToken::RParen)) {
    return true;
  }
  Lexer.Lex();

  return false;
}

bool PatmosAsmParser::
ParseInstruction(ParseInstructionInfo &Info, StringRef Name, SMLoc NameLoc,
                 SmallVectorImpl<MCParsedAsmOperand*> &Operands)
{
  bool HasGuard = false;
  ParsePrefix(NameLoc, Operands, HasGuard, Name);

  MCAsmLexer &Lexer = getLexer();
  if (Name == "{" || Name == "(") {
    // The prefix has some tokens. Therefore, 'Name' doesn't contain
    // the mnemonic. We need it to do so.	
    if (Lexer.isNot(AsmToken::Identifier) && Lexer.isNot(AsmToken::String)) {
      return Error(Lexer.getLoc(), "Couldn't find Mnemonic");
    }
    Name = getTok().getIdentifier();
    NameLoc = Lexer.getLoc();
    Lex();
  }

  // The first operand is the token for the instruction name
  // We need to split at . in mnemonic names, this is the way the matcher
  // expects it.
  size_t Next = Name.find('.');
  StringRef Mnemonic = Name.slice(0, Next);

  Operands.insert(Operands.begin(),
                  PatmosOperand::CreateToken(Mnemonic, NameLoc));

  if (Next != StringRef::npos) {
    // there is a format/modifier token in mnemonic, add as first operand
    StringRef Format = Name.slice(Next, StringRef::npos);
    Operands.insert(Operands.begin() + 1,
                    PatmosOperand::CreateToken(Format, NameLoc));
  }

  // If this instruction has no guard, we just add a default one.
  // We do not yet know if the instruction actually requires one, so we might need to undo this
  // if we do not find a match (if we actually have instructions that have no guard).
  if (!HasGuard) {
    Operands.push_back(PatmosOperand::CreateReg(Patmos::P0, NameLoc, NameLoc));
    Operands.push_back(PatmosOperand::CreateFlag(false, NameLoc, NameLoc,
                                                 getParser().getContext()));
  }

  unsigned OpNo = 0;

  // If there are no more operands then finish
  while (Lexer.isNot(AsmToken::EndOfStatement)) {

    // last instruction in a bundle?
    if (Lexer.is(AsmToken::RCurly)) {
      if (!InBundle) {
        SMLoc TokLoc = Lexer.getLoc();
        EatToEndOfStatement();
        return Error(TokLoc, "found bundle end marker without a matching "
                             "start marker");
      }
      InBundle = false;
      Lexer.Lex();
      return false;
    }

    if (Lexer.is(AsmToken::Comma)) {
      // we do not start with a comma before any operands
      if (OpNo == 0) {
        SMLoc TokLoc = Lexer.getLoc();
        EatToEndOfStatement();
        return Error(TokLoc, "comma before first operand");
      }
      Lex();
    } else if (Lexer.is(AsmToken::Equal)) {
      // add it as a token for the matcher
      // TODO if somebody writes something like 'r1, r2, r3' instead of 'r1 = r2, r3', he will
      //      get a 'register type mismatch' error for 'r3', which is *very* confusing.
      if (ParseToken(Operands, AsmToken::Equal)) {
        EatToEndOfStatement();
        return true;
      }
    } else if (OpNo > 0) {
      // We need some separation between operands
      SMLoc TokLoc = Lexer.getLoc();
      EatToEndOfStatement();
      return Error(TokLoc, "missing separator between operands or instructions");
    }

    if (ParseOperand(Operands, OpNo)) {
      EatToEndOfStatement();
      return true;
    }

    OpNo++;
  }

  return false;
}

bool PatmosAsmParser::
ParseDirective(AsmToken DirectiveID) {
  StringRef IDVal = DirectiveID.getIdentifier();
  if (IDVal == ".word")
    return ParseDirectiveWord(4, DirectiveID.getLoc());
  if (IDVal == ".half" || IDVal == ".hword")
    return ParseDirectiveWord(2, DirectiveID.getLoc());
  if (IDVal == ".fstart")
    return ParseDirectiveFStart(DirectiveID.getLoc());
  return true;
}

/// ParseDirectiveWord
///  ::= .word [ expression (, expression)* ]
bool PatmosAsmParser::ParseDirectiveWord(unsigned Size, SMLoc L) {
  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    for (;;) {
      const MCExpr *Value;
      SMLoc E;
      if (getParser().parseExpression(Value, E))
        return true;

      getParser().getStreamer().EmitValue(Value, Size);

      if (getLexer().is(AsmToken::EndOfStatement))
        break;

      // FIXME: Improve diagnostic.
      if (getLexer().isNot(AsmToken::Comma))
        return Error(L, "unexpected token in directive");
      Parser.Lex();
    }
  }

  Parser.Lex();
  return false;
}

/// ParseDirectiveFStart
///  ::= .fstart [ symbol , length, align ]
bool PatmosAsmParser::ParseDirectiveFStart(SMLoc L) {
  if (getLexer().is(AsmToken::EndOfStatement)) {
    return Error(L, "missing arguments to .fstart directive");
  }

  const MCSymbol *Start;
  const MCExpr *StartExpr;
  SMLoc E;
  if (getParser().parseExpression(StartExpr, E)) {
    return true;
  }
  if (StartExpr->getKind() == MCExpr::SymbolRef) {
    const MCSymbolRefExpr *SymRef = dyn_cast<MCSymbolRefExpr>(StartExpr);
    Start = &SymRef->getSymbol();
  } else {
    return Error(L, "first parameter of this directive must be a symbol name");
  }

  if (getLexer().isNot(AsmToken::Comma))
    return Error(L, "unexpected token in directive");
  Parser.Lex();

  const MCExpr *Length;
  if (getParser().parseExpression(Length, E)) {
    return true;
  }

  if (getLexer().isNot(AsmToken::Comma))
    return Error(L, "unexpected token in directive");
  Parser.Lex();

  int64_t Align;
  if (getParser().parseAbsoluteExpression(Align)) {
    return true;
  }
  if (Align < 0) {
    return Error(L, "alignment value must be a positive value");
  }

  if (getLexer().isNot(AsmToken::EndOfStatement)) {
    return Error(L, "unexpected token in directive");
  }
  Parser.Lex();

  PatmosTargetStreamer &PTS = static_cast<PatmosTargetStreamer&>(
                                 getParser().getStreamer().getTargetStreamer());

  PTS.EmitFStart(Start, Length, (unsigned)Align);

  return false;
}

bool PatmosAsmParser::isPredSrcOperand(StringRef Mnemonic, unsigned OpNo)
{
  // only src operands, only combine ops
  if (OpNo == 0) return false;

  // We check if the src op is actually a predicate register later in the
  // parse method
  if (Mnemonic == "por"  || Mnemonic == "pand" ||
      Mnemonic == "pxor") return true;
  if (Mnemonic == "pmov" || Mnemonic == "pnot" ||
      Mnemonic == "pset" || Mnemonic == "pclr") return true;
  // Note that mov might actually move between predicate and registers
  // (in the future)
  if (Mnemonic == "mov") return true;

  return false;
}

void PatmosAsmParser::EatToEndOfStatement() {
  MCAsmLexer &Lexer = getLexer();
  while (Lexer.isNot(AsmToken::EndOfStatement) &&
         Lexer.isNot(AsmToken::LCurly) &&
         Lexer.isNot(AsmToken::Eof)) {
    Lexer.Lex();
  }
}


extern "C" void LLVMInitializePatmosAsmParser() {
  RegisterMCAsmParser<PatmosAsmParser> X(ThePatmosTarget);
}

