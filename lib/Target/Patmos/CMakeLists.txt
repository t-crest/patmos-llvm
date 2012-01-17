set(LLVM_TARGET_DEFINITIONS Patmos.td)

llvm_tablegen(PatmosGenRegisterInfo.inc -gen-register-info)
llvm_tablegen(PatmosGenInstrInfo.inc -gen-instr-info)
llvm_tablegen(PatmosGenAsmWriter.inc -gen-asm-writer)
llvm_tablegen(PatmosGenDAGISel.inc -gen-dag-isel)
llvm_tablegen(PatmosGenCallingConv.inc -gen-callingconv)
llvm_tablegen(PatmosGenSubtargetInfo.inc -gen-subtarget)
add_public_tablegen_target(PatmosCommonTableGen)

add_llvm_target(PatmosCodeGen
  PatmosBranchSelector.cpp
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

add_llvm_library_dependencies(LLVMPatmosCodeGen
  LLVMAsmPrinter
  LLVMCodeGen
  LLVMCore
  LLVMMC
  LLVMPatmosAsmPrinter
  LLVMPatmosDesc
  LLVMPatmosInfo
  LLVMSelectionDAG
  LLVMSupport
  LLVMTarget
  )

add_subdirectory(InstPrinter)
add_subdirectory(TargetInfo)
add_subdirectory(MCTargetDesc)
