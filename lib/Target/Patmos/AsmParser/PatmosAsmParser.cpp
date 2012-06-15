//===-- PatmosAsmParser.cpp - Parse Patmos assembly to MCInst instructions ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PatmosMCTargetDesc.h"
#include "MCTargetDesc/PatmosBaseInfo.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCTargetAsmParser.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Twine.h"

using namespace llvm;

namespace {
struct PatmosOperand;

class PatmosAsmParser : public MCTargetAsmParser {
  MCAsmParser &Parser;

  MCAsmParser &getParser() const { return Parser; }
  MCAsmLexer &getLexer() const { return Parser.getLexer(); }

  void Warning(SMLoc L, const Twine &Msg) { Parser.Warning(L, Msg); }
  bool Error(SMLoc L, const Twine &Msg) { return Parser.Error(L, Msg); }

  #define GET_ASSEMBLER_HEADER
  #include "PatmosGenAsmMatcher.inc"

public:
  PatmosAsmParser(MCSubtargetInfo &sti, MCAsmParser &parser)
    : MCTargetAsmParser(), Parser(parser) {
  }

  virtual bool ParsePrefix(SMLoc &PrefixLoc, SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                           bool &HasPrefix);

  virtual bool ParseInstruction(StringRef Name, SMLoc NameLoc,
                                SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  virtual bool ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc);

  virtual bool ParseDirective(AsmToken DirectiveID);

  virtual bool MatchAndEmitInstruction(SMLoc IDLoc,
                               SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                               MCStreamer &Out);

private:
  bool ParseRegister(unsigned &RegNo);

  bool ParseOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  bool ParseMemoryOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  bool ParsePredicateOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands);

  PatmosOperand* ParseImmediate();
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

    union {
      struct {
        const char *Data;
        unsigned Length;
      } Tok;

      struct {
        unsigned RegNum;
      } Reg;

      struct {
        const MCExpr *Val;
      } Imm;

      struct {
        unsigned Base;
        unsigned OffReg;
        const MCExpr *Off;
      } Mem;
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

    unsigned getMemOffReg() const {
      assert(Kind == Memory && "Invalid access!");
      return Mem.OffReg;
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

      unsigned RegOff = getMemOffReg();
      if (RegOff)
        Inst.addOperand(MCOperand::CreateReg(RegOff));
      else
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

    static PatmosOperand *CreateFlag(bool flag, SMLoc S, SMLoc E, MCContext &Ctx) {
      PatmosOperand *Op = new PatmosOperand(Immediate);
      Op->Imm.Val = MCConstantExpr::Create(flag, Ctx);
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }

    static PatmosOperand *CreateMem(unsigned Base, const MCExpr *Off, SMLoc S,
                                    SMLoc E) {
      PatmosOperand *Op = new PatmosOperand(Memory);
      Op->Mem.Base = Base;
      Op->Mem.Off = Off;
      Op->Mem.OffReg = 0;
      Op->StartLoc = S;
      Op->EndLoc = E;
      return Op;
    }

    static PatmosOperand *CreateMem(unsigned Base, unsigned Off, SMLoc S,
                                    SMLoc E) {
      PatmosOperand *Op = new PatmosOperand(Memory);
      Op->Mem.Base = Base;
      Op->Mem.OffReg = Off;
      Op->Mem.Off = 0;
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
    OS << "<register R";
    OS << getPatmosRegisterNumbering(getReg()) << ">";
    break;
  case Token:
    OS << "'" << getToken() << "'";
    break;
  case Memory: {
    OS << "<memory R";
    OS << getPatmosRegisterNumbering(getMemBase());
    OS << ", ";

    unsigned RegOff = getMemOffReg();
    if (RegOff)
      OS << "R" << getPatmosRegisterNumbering(RegOff);
    else
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
MatchAndEmitInstruction(SMLoc IDLoc,
                        SmallVectorImpl<MCParsedAsmOperand*> &Operands,
                        MCStreamer &Out) {
  MCInst Inst;
  SMLoc ErrorLoc;
  unsigned ErrorInfo;

  switch (MatchInstructionImpl(Operands, Inst, ErrorInfo)) {
  default: break;
  case Match_Success:
    Out.EmitInstruction(Inst);
    return false;
  case Match_MissingFeature:
    return Error(IDLoc, "instruction use requires an option to be enabled");
  case Match_MnemonicFail:
      return Error(IDLoc, "unrecognized instruction mnemonic");
  case Match_ConversionFail:
    return Error(IDLoc, "unable to convert operands to instruction");
  case Match_InvalidOperand:
    ErrorLoc = IDLoc;
    if (ErrorInfo != ~0U) {
      if (ErrorInfo >= Operands.size())
        return Error(IDLoc, "too few operands for instruction");

      ErrorLoc = ((PatmosOperand*)Operands[ErrorInfo])->getStartLoc();
      if (ErrorLoc == SMLoc()) ErrorLoc = IDLoc;
    }

    return Error(ErrorLoc, "invalid operand for instruction");
  }

  llvm_unreachable("Implement any new match types added!");
}

bool PatmosAsmParser::
ParseRegister(unsigned &RegNo) {
  if (getLexer().getKind() != AsmToken::Identifier) {
    return true;
  }
  RegNo = MatchRegisterName(getLexer().getTok().getIdentifier());
  return (RegNo == 0);
}

bool PatmosAsmParser::
ParseRegister(unsigned &RegNo, SMLoc &StartLoc, SMLoc &EndLoc) {
  if (getLexer().getKind() == AsmToken::Identifier) {
    return ParseRegister(RegNo);
  }
  return false;
}

bool PatmosAsmParser::
ParseMemoryOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands)  {

  return false;
}

bool PatmosAsmParser::
ParsePredicateOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands)  {

  return false;
}

bool PatmosAsmParser::
ParseOperand(SmallVectorImpl<MCParsedAsmOperand*> &Operands)  {

  return false;
}


PatmosOperand *PatmosAsmParser::ParseImmediate() {
  SMLoc S = Parser.getTok().getLoc();
  SMLoc E = SMLoc::getFromPointer(Parser.getTok().getLoc().getPointer() - 1);

  const MCExpr *EVal;
  switch (getLexer().getKind()) {
  default: return 0;
  case AsmToken::LParen:
  case AsmToken::Plus:
  case AsmToken::Minus:
  case AsmToken::Integer:
  case AsmToken::Identifier:
    if (getParser().ParseExpression(EVal))
      return 0;

    return PatmosOperand::CreateImm(EVal, S, E);
  }
}

bool PatmosAsmParser::
ParsePrefix(SMLoc &PrefixLoc, SmallVectorImpl<MCParsedAsmOperand*> &Operands, bool &HasPrefix) {
  MCAsmLexer &Lexer = getLexer();

  if (Lexer.isNot(AsmToken::LParen)) {
    return false;
  }

  // If it starts with '(', assume this is a guard, and try to parse it
  Lexer.Lex();

  bool flag = false;
  if (Lexer.is(AsmToken::Exclaim)) {
    flag = true;
    Lexer.Lex();
  }

  unsigned RegNo;
  if (!ParseRegister(RegNo)) {
    return true;
  }

  Lexer.Lex();

  if (Lexer.isNot(AsmToken::RParen)) {
    return true;
  }

  SMLoc EndLoc = Lexer.getLoc();
  Lexer.Lex();

  Operands.push_back(PatmosOperand::CreateReg(RegNo, PrefixLoc, EndLoc));
  Operands.push_back(PatmosOperand::CreateFlag(flag, PrefixLoc, EndLoc, getParser().getContext()));

  return false;
}

bool PatmosAsmParser::
ParseInstruction(StringRef Name, SMLoc NameLoc,
                 SmallVectorImpl<MCParsedAsmOperand*> &Operands)
{
  // The first operand is the token for the instruction name
  Operands.insert(Operands.begin(), PatmosOperand::CreateToken(Name,NameLoc));

  // If this instruction has no guard, we just add a default one.
  // We do not yet know if the instruction actually requires one, so we might need to undo this
  // if we do not find a match (if we actually have instructions that have no guard).
  bool addedDefault = false;
  if (Operands.size() == 1) {
    addedDefault = true;
    Operands.push_back(PatmosOperand::CreateReg(Patmos::P0, NameLoc, NameLoc));
    Operands.push_back(PatmosOperand::CreateFlag(false, NameLoc, NameLoc, getParser().getContext()));
  }

  unsigned OpNo = 0;

  MCAsmLexer &Lexer = getLexer();

  // If there are no more operands then finish
  while (Lexer.isNot(AsmToken::EndOfStatement)) {

    // we have a bundled operation?
    if (Lexer.is(AsmToken::PipePipe)) {

      // TODO handle bundle marker somehow.. add as last operand??

      // consume bundle marker
      Lex();
      return false;
    }

    if (Lexer.is(AsmToken::Comma)) {
      if (OpNo == 0) return true;
      Lex();
    } else if (Lexer.is(AsmToken::Equal)) {
      // We only accept one '=' and only after the first operand
      // TODO this also accepts some invalid formats since we do not check the opcode!
      if (OpNo != 1) return true;
      Lex();
    } else if (OpNo > 0) {
      // We need some separation between operands
      return true;
    }

    if (!ParseOperand(Operands)) {
      return true;
    }
  }

  return false;
}

bool PatmosAsmParser::
ParseDirective(AsmToken DirectiveID) {
  return true;
}



extern "C" void LLVMInitializePatmosAsmLexer();

extern "C" void LLVMInitializePatmosAsmParser() {
  RegisterMCAsmParser<PatmosAsmParser> X(ThePatmosTarget);
  LLVMInitializePatmosAsmLexer();
}

