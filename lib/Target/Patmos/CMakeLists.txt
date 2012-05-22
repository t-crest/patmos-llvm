set(LLVM_TARGET_DEFINITIONS Patmos.td)

tablegen(PatmosGenRegisterInfo.inc -gen-register-info)
tablegen(PatmosGenInstrInfo.inc -gen-instr-info)
tablegen(PatmosGenAsmWriter.inc -gen-asm-writer)
tablegen(PatmosGenDAGISel.inc -gen-dag-isel)
tablegen(PatmosGenCallingConv.inc -gen-callingconv)
tablegen(PatmosGenSubtargetInfo.inc -gen-subtarget)
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
