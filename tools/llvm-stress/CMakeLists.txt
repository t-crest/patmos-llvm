set(LLVM_LINK_COMPONENTS bitreader asmparser bitwriter instrumentation scalaropts ipo codegen)

add_llvm_tool(llvm-stress
  llvm-stress.cpp
  )
