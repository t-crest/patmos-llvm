project(patch_loads)

add_executable(patch_loads patch_loads.c)

target_link_libraries(patch_loads elf)
