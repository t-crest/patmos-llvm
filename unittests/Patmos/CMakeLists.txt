set(LLVM_LINK_COMPONENTS
  PatmosSinglePath
  )

set(PatmosSource
  PredicatedBlockTest.cpp
  )

add_llvm_unittest(PatmosUnitTests
  ${PatmosSource}
  )
