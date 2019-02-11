add_custom_target(UnitTests)
set_target_properties(UnitTests PROPERTIES FOLDER "Tests")

include_directories(
	/usr/include/c++/7	# C++ std
	/usr/include		# C std
)

function(add_llvm_unittest test_dirname)
  add_unittest(UnitTests ${test_dirname} ${ARGN})
endfunction()

# We remove all non-patmos unit tests since we cannot trust they pass or fail
# Having only patmos unit tests allows us to use the make target UnitTests
# to build all Patmos unittests efficiently

#add_subdirectory(ADT)
#add_subdirectory(Analysis)
#add_subdirectory(Bitcode)
#add_subdirectory(CodeGen)
#add_subdirectory(DebugInfo)
#add_subdirectory(ExecutionEngine)
#add_subdirectory(IR)
#add_subdirectory(MC)
#add_subdirectory(Object)
#add_subdirectory(Option)
#add_subdirectory(Support)
#add_subdirectory(Transforms)

add_subdirectory(Patmos)