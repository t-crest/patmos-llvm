set(LLVM_LINK_COMPONENTS bitreader codegen)

add_llvm_tool(llvm-prof
  llvm-prof.cpp
  )
