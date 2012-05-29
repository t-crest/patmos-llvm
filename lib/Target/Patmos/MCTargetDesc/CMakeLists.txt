add_llvm_library(LLVMPatmosDesc
  PatmosMCTargetDesc.cpp
  PatmosMCAsmInfo.cpp
  PatmosAsmBackend.cpp
  PatmosMCCodeEmitter.cpp
  PatmosELFObjectWriter.cpp
  )

add_dependencies(LLVMPatmosDesc PatmosCommonTableGen)

# Hack: we need to include 'main' target directory to grab private headers
#include_directories(${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}/..)
