set(LLVM_LINK_COMPONENTS
  BitReader
  BitWriter
  Core
  IRReader
  Linker
  Object
  Support
  )

add_llvm_tool(llvm-link
  llvm-link.cpp
  LibraryLinker.cpp
  )
