set(LLVM_TARGET_DEFINITIONS Patmos.td)

tablegen(LLVM PatmosGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM PatmosGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM PatmosGenMCCodeEmitter.inc -gen-emitter -mc-emitter)
#tablegen(LLVM PatmosGenMCPseudoLowering.inc -gen-pseudo-lowering)
tablegen(LLVM PatmosGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM PatmosGenAsmMatcher.inc -gen-asm-matcher)
tablegen(LLVM PatmosGenDAGISel.inc -gen-dag-isel -recursive-xform)
tablegen(LLVM PatmosGenCallingConv.inc -gen-callingconv)
tablegen(LLVM PatmosGenSubtargetInfo.inc -gen-subtarget)
tablegen(LLVM PatmosGenDFAPacketizer.inc -gen-dfa-packetizer)
#tablegen(LLVM PatmosGenIntrinsics.inc -gen-tgt-intrinsic)
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
  PatmosDelaySlotFiller.cpp
  PatmosFunctionSplitter.cpp
  PatmosDelaySlotKiller.cpp
  PatmosCallGraphBuilder.cpp
  PatmosStackCacheAnalysis.cpp
  PatmosExport.cpp
  PatmosBypassFromPML.cpp
  PatmosPostRAScheduler.cpp
  PatmosSchedStrategy.cpp
  PatmosPMLProfileImport.cpp
  PatmosEnsureAlignment.cpp
  )

add_dependencies(LLVMPatmosCodeGen intrinsics_gen)

add_subdirectory(AsmParser)
add_subdirectory(Disassembler)
add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
add_subdirectory(SinglePath)
