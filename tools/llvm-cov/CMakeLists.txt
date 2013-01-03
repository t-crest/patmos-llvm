set(LLVM_LINK_COMPONENTS instrumentation codegen)

add_llvm_tool(llvm-cov
  llvm-cov.cpp
  )
