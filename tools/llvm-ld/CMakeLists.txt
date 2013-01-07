set(LLVM_LINK_COMPONENTS ipo scalaropts linker archive bitwriter vectorize codegen)

add_llvm_tool(llvm-ld
  Optimize.cpp
  llvm-ld.cpp
  )

add_dependencies(llvm-ld llvm-stub)
