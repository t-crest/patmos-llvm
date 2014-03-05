//===-- PatmosTargetStreamer.cpp - Patmos Target Streamer Methods ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Patmos specific target streamer methods.
//
//===----------------------------------------------------------------------===//

#include "PatmosTargetStreamer.h"
#include "llvm/MC/MCELF.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"

using namespace llvm;

// pin vtable to this file
void PatmosTargetStreamer::anchor() {}

PatmosTargetAsmStreamer::PatmosTargetAsmStreamer(formatted_raw_ostream &OS)
    : OS(OS) {}

void PatmosTargetAsmStreamer::EmitFStart(const MCSymbol *Start, 
                               const MCExpr* Size, unsigned Alignment)
{
  OS << "\t.fstart\t" << *Start << ", " << *Size << ", " << Alignment << "\n";
}

MCELFStreamer &PatmosTargetELFStreamer::getStreamer() {
  return static_cast<MCELFStreamer &>(*Streamer);
}

void PatmosTargetELFStreamer::EmitFStart(const MCSymbol *Start, 
	      const MCExpr* Size, unsigned Alignment) 
{
  // Not used at the moment: This code is used to align the first *instruction*,
  // not the first word (i.e., the size word). Since we need to align the
  // size word, we can get by with an .align directive.
  //
  // Emit the padding and the size expression as fragment
  // We may emit at most Alignment + 3 bytes, in case emitting just the
  // expression will result in being one byte over the alignment. in this case
  // we need to emit Alignment - 1 bytes of padding + 4 bytes of expression.
  //MCExprAlignFragment *AF = new MCExprAlignFragment(Alignment, *Size, 4, 0, 1,
  //                                                  Alignment + 3);
  //AF->setEmitNops(true);
  //
  // TODO adding a fragment is not allowed from the outside, make this public
  // or add an EmitAlignedSymbol function to MCStreamer
  //getStreamer().insert(AF);

  getStreamer().EmitCodeAlignment(Alignment);
  getStreamer().EmitValue(Size, 4);

}

