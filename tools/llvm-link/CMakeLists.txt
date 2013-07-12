set(LLVM_LINK_COMPONENTS archive linker bitreader bitwriter asmparser irreader)

add_llvm_tool(llvm-link
  llvm-link.cpp
  LibraryLinker.cpp
  )
