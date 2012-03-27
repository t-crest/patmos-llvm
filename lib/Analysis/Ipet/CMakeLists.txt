
# TODO this stuff should go to the top level CMakeLists.txt (at least the lpsolve stuff), have an 'LLVM_ENABLE_LPSOLVE' flag that enables
# building of IPET stuff ?
option(LLVM_ENABLE_IPET "Enable building the IPET solver (requires lpsolve)" ON)
set(LPSOLVE_LIBRARY_DIR "" CACHE PATH "Additional directory, where CMake should search for liblpsolve55.so")
set(LPSOLVE_INCLUDE_DIR "" CACHE PATH "Additional directory, where CMake should search for lp_lib.h or lpsolve/lp_lib.h")

# TODO this stuff should go to cmake/config-ix.cmake
if( LLVM_ENABLE_IPET )
  find_path(LPSOLVE_INCLUDE_PATH lp_lib.h PATHS ${LPSOLVE_INCLUDE_DIR})
  if( LPSOLVE_INCLUDE_PATH )
    set(LPSOLVE_HEADER lp_lib.h CACHE INTERNAL "")
    set(HAVE_LPLIB_H 1 CACHE INTERNAL "")
  else()
    find_path(LPSOLVE_INCLUDE_PATH lpsolve/lp_lib.h PATHS ${LPSOLVE_INCLUDE_DIR})
    if( LPSOLVE_INCLUDE_PATH )
      set(LPSOLVE_HEADER lpsolve/lp_lib.h CACHE INTERNAL "")
      set(HAVE_LPSOLVE_LPLIB_H 1 CACHE INTERNAL "")
    endif()
  endif()

  if( NOT LPSOLVE_HEADER )
    message(FATAL_ERROR "lpsolve includes are not found.")
  endif()

  # TODO we need the _pic.a file if we build an .so file, but for windows (and probably other non-ubuntu dists?) there is no such thing!
  find_library(LPSOLVE_LIBRARY_PATH lpsolve55_pic PATHS ${LPSOLVE_LIBRARY_DIR})
  if( NOT LPSOLVE_LIBRARY_PATH )
    message(FATAL_ERROR "liblpsolve55_pic is not found.")
  endif()

  list(APPEND CMAKE_REQUIRED_LIBRARIES ${LPSOLVE_LIBRARY_PATH})
  list(APPEND CMAKE_REQUIRED_INCLUDES ${LPSOLVE_INCLUDE_PATH})
  check_symbol_exists(make_lp ${LPSOLVE_HEADER} HAVE_LPSOLVE_CALL)
  list(REMOVE_ITEM CMAKE_REQUIRED_INCLUDES ${LPSOLVE_INCLUDE_PATH})
  list(REMOVE_ITEM CMAKE_REQUIRED_LIBRARIES ${LPSOLVE_LIBRARY_PATH})
else()
  unset(HAVE_LPSOLVE_LPLIB_H CACHE)
  unset(HAVE_LPLIB_H CACHE)
  unset(HAVE_LPSOLVE_CALL CACHE)
endif( LLVM_ENABLE_IPET )


if( LLVM_ENABLE_IPET )
  add_llvm_loadable_module( libLLVMIpet
    CostProvider.cpp
    FlowFactProvider.cpp
    Ipet.cpp
  )

  target_link_libraries( libLLVMIpet ${LPSOLVE_LIBRARY_PATH} )
endif()

