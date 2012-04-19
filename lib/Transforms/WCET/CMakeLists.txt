
# TODO this stuff should go to the top level CMakeLists.txt (at least the lpsolve stuff), have an 'LLVM_ENABLE_LPSOLVE' flag that enables
# building of IPET stuff ?
option(LLVM_ENABLE_IPET "Enable building the IPET solver (requires lpsolve)" ON)
set(LPSOLVE_LIBRARY_DIR "" CACHE PATH "Additional directory, where CMake should search for liblpsolve55.so")
set(LPSOLVE_INCLUDE_DIR "" CACHE PATH "Additional directory, where CMake should search for lp_lib.h or lpsolve/lp_lib.h")

# TODO this stuff should go to cmake/config-ix.cmake
if( LLVM_ENABLE_IPET )
  if ( NOT LPSOLVE_HEADER )
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
  endif()

  if( NOT LPSOLVE_HEADER )
    message(FATAL_ERROR "lpsolve includes are not found.")
  endif()

  # TODO we need the _pic.a file if we build an .so file, but for windows (and probably other non-ubuntu dists?) there is no such thing!
  find_library(LPSOLVE_LIBRARY_PATH lpsolve55 PATHS ${LPSOLVE_LIBRARY_DIR})
  find_library(COLAMD_LIBRARY_PATH  colamd    PATHS ${LPSOLVE_LIBRARY_DIR})
  if( NOT LPSOLVE_LIBRARY_PATH )
    message(FATAL_ERROR "liblpsolve55 is not found.")
  endif()

  list(APPEND CMAKE_REQUIRED_LIBRARIES ${LPSOLVE_LIBRARY_PATH} ${COLAMD_LIBRARY_PATH})
  list(APPEND CMAKE_REQUIRED_INCLUDES ${LPSOLVE_INCLUDE_PATH} ${COLAMD_LIBRARY_PATH})
  check_symbol_exists(make_lp ${LPSOLVE_HEADER} HAVE_LPSOLVE_CALL)
  list(REMOVE_ITEM CMAKE_REQUIRED_INCLUDES ${LPSOLVE_INCLUDE_PATH} ${COLAMD_LIBRARY_PATH})
  list(REMOVE_ITEM CMAKE_REQUIRED_LIBRARIES ${LPSOLVE_LIBRARY_PATH} ${COLAMD_LIBRARY_PATH})
else()
  unset(HAVE_LPSOLVE_LPLIB_H CACHE)
  unset(HAVE_LPLIB_H CACHE)
  unset(HAVE_LPSOLVE_CALL CACHE)
endif( LLVM_ENABLE_IPET )

set(SOURCES 
  CostProvider.cpp
  FlowFactProvider.cpp
  Criticality.cpp
)

if( LLVM_ENABLE_IPET )
  list(APPEND SOURCES 
    Ipet.cpp
  )
endif()

add_llvm_library( LLVMwcet ${SOURCES} )

add_llvm_library_dependencies( LLVMwcet
  LLVMAnalysis
  LLVMSupport
  LLVMTransformUtils
)

if( LLVM_ENABLE_IPET )
  target_link_libraries( LLVMwcet 
    ${LPSOLVE_LIBRARY_PATH} 
    ${COLAMD_LIBRARY_PATH}
  )
endif()

