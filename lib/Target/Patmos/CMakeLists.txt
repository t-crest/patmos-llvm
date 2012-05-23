set(LLVM_TARGET_DEFINITIONS Patmos.td)

tablegen(LLVM PatmosGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM PatmosGenInstrInfo.inc -gen-instr-info)
tablegen(LLVM PatmosGenAsmWriter.inc -gen-asm-writer)
tablegen(LLVM PatmosGenDAGISel.inc -gen-dag-isel)
tablegen(LLVM PatmosGenCallingConv.inc -gen-callingconv)
tablegen(LLVM PatmosGenSubtargetInfo.inc -gen-subtarget)
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
  )

add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
