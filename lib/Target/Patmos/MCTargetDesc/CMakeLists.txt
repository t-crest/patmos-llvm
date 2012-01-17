add_llvm_library(LLVMPatmosDesc
  PatmosMCTargetDesc.cpp
  PatmosMCAsmInfo.cpp
  )

add_llvm_library_dependencies(LLVMPatmosDesc
  LLVMMC
  LLVMPatmosAsmPrinter
  LLVMPatmosInfo
  )

add_dependencies(LLVMPatmosDesc PatmosCommonTableGen)
