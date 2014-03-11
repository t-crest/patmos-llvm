# We need to execute this script at installation time because the
# DESTDIR environment variable may be unset at configuration time.
# See PR8397.

if(UNIX)
  set(LLVM_LINK_OR_COPY create_symlink)
  set(LLVM_DESTDIR $ENV{DESTDIR})
else()
  set(LLVM_LINK_OR_COPY copy)
endif()

# CMAKE_EXECUTABLE_SUFFIX is undefined on cmake scripts. See PR9286.
if( WIN32 )
  set(EXECUTABLE_SUFFIX ".exe")
else()
  set(EXECUTABLE_SUFFIX "")
endif()

set(bindir "${LLVM_DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/")
set(llvm-ar "llvm-ar${EXECUTABLE_SUFFIX}")
set(llvm-ranlib "llvm-ranlib${EXECUTABLE_SUFFIX}")

message("Creating llvm-ranlib executable based on ${llvm-ar}")

execute_process(
  COMMAND "${CMAKE_COMMAND}" -E ${LLVM_LINK_OR_COPY} "${llvm-ar}" "${llvm-ranlib}"
  WORKING_DIRECTORY "${bindir}")

