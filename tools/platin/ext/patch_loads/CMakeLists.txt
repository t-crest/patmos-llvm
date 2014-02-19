cmake_minimum_required(VERSION 2.6)
project(patch_loads)

# Find libelf
find_library(ELF elf)
if (NOT ELF)
  message(FATAL_ERROR "libelf library not found.")
endif()
find_path(ELF_INCLUDE_DIRS gelf.h PATH_SUFFIXES libelf)
if (NOT ELF_INCLUDE_DIRS)
  message(FATAL_ERROR "libelf headers not found.")
endif()

include_directories(${ELF_INCLUDE_DIRS})

add_executable(patch_loads patch_loads.c)

target_link_libraries(patch_loads elf)
