set(LLVM_TARGET_DEFINITIONS Patmos.td)

tablegen(LLVM PatmosGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM PatmosGenInstrInfo.inc -gen-instr-info)
# CodeEmitter seems to be used for JIT only, same as MCCodeEmitter except no Fixups
#tablegen(LLVM PatmosGenCodeEmitter.inc -gen-emitter)
tablegen(LLVM PatmosGenMCCodeEmitter.inc -gen-emitter -mc-emitter)
#tablegen(LLVM PatmosGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM PatmosGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM PatmosGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM PatmosGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM PatmosGenCallingConv.inc -gen-callingconv)
tablegen(LLVM PatmosGenSubtargetInfo.inc -gen-subtarget)
#tablegen(LLVM PatmosGenIntrinsics.inc -gen-tgt-intrinsic)
tablegen(LLVM PatmosGenEDInfo.inc -gen-enhanced-disassembly-info)
tablegen(LLVM PatmosGenDisassemblerTables.inc -gen-disassembler)
add_public_tablegen_target(PatmosCommonTableGen)

add_llvm_target(PatmosCodeGen
  PatmosISelDAGToDAG.cpp
  PatmosISelLowering.cpp
  PatmosInstrInfo.cpp
  PatmosFrameLowering.cpp
  PatmosRegisterInfo.cpp
  PatmosSubtarget.cpp
  PatmosTargetMachine.cpp
  PatmosSelectionDAGInfo.cpp
  PatmosAsmPrinter.cpp
  PatmosMCInstLower.cpp
  PatmosELFWriterInfo.cpp
  PatmosDelaySlotFiller.cpp
  )

add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
