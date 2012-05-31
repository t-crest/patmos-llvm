//===-- PatmosAsmLexer.cpp - Tokenize Patmos assembly to AsmTokens --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/PatmosBaseInfo.h"

#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCParser/MCAsmLexer.h"
#include "llvm/MC/MCParser/MCParsedAsmOperand.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCTargetAsmLexer.h"

#include "llvm/Support/TargetRegistry.h"

#include "llvm/ADT/StringSwitch.h"

#include <string>
#include <map>

using namespace llvm;

namespace {

class PatmosAsmLexer : public MCTargetAsmLexer {
  const MCAsmInfo &AsmInfo;

  const AsmToken &lexDefinite() {
    return getLexer()->Lex();
  }

  AsmToken LexTokenUAL();
protected:
  typedef std::map <std::string, unsigned> rmap_ty;

  rmap_ty RegisterMap;

  void InitRegisterMap(const MCRegisterInfo *info) {
    unsigned numRegs = info->getNumRegs();

    for (unsigned i = 0; i < numRegs; ++i) {
      const char *regName = info->getName(i);
      if (regName)
        RegisterMap[regName] = i;
    }

    // add register name aliases (any way to get this from altNames by tablegen?)
    RegisterMap["rsp"] = Patmos::RSP;
    RegisterMap["rfp"] = Patmos::RFP;
  }

  unsigned MatchRegisterName(StringRef Name) {
    rmap_ty::iterator iter = RegisterMap.find(Name.str());
    if (iter != RegisterMap.end())
      return iter->second;
    else
      return 0;
  }

  AsmToken LexToken() {
    if (!Lexer) {
      SetError(SMLoc(), "No MCAsmLexer installed");
      return AsmToken(AsmToken::Error, "", 0);
    }

    switch (AsmInfo.getAssemblerDialect()) {
    default:
      SetError(SMLoc(), "Unhandled dialect");
      return AsmToken(AsmToken::Error, "", 0);
    case 0:
      return LexTokenUAL();
    }
  }
public:
  PatmosAsmLexer(const Target &T, const MCRegisterInfo &MRI, const MCAsmInfo &MAI)
    : MCTargetAsmLexer(T), AsmInfo(MAI) {
    InitRegisterMap(&MRI);
  }
};

} // end anonymous namespace

AsmToken PatmosAsmLexer::LexTokenUAL() {
  const AsmToken &lexedToken = lexDefinite();

  switch (lexedToken.getKind()) {
  default: break;
  case AsmToken::Error:
    SetError(Lexer->getErrLoc(), Lexer->getErr());
    break;
  case AsmToken::Identifier: {
    std::string lowerCase = lexedToken.getString().lower();

    unsigned regID = MatchRegisterName(lowerCase);

    if (regID)
      return AsmToken(AsmToken::Register,
                      lexedToken.getString(),
                      static_cast<int64_t>(regID));
    break;
  }
  }

  return AsmToken(lexedToken);
}

extern "C" void LLVMInitializePatmosAsmLexer() {
  RegisterMCAsmLexer<PatmosAsmLexer> X(ThePatmosTarget);
}
