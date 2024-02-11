# Tests folder level.

set(TEST_TARGET test_main)

add_executable(${TEST_TARGET} ${TEST_TARGET}.cpp "test_utils.h" )

add_test(NAME ${TEST_TARGET} COMMAND $<TARGET_FILE:${TEST_TARGET}>)

target_compile_features(${TEST_TARGET} PUBLIC cxx_std_23)

target_include_directories(${TEST_TARGET} PUBLIC . ../src/lib_main )

target_link_libraries(${TEST_TARGET} INTERFACE ${LIBRARY_NAME})


set(TEST_TARGET test_filter)
add_executable(${TEST_TARGET} ${TEST_TARGET}.cpp "test_utils.h" )
add_test(NAME ${TEST_TARGET} COMMAND $<TARGET_FILE:${TEST_TARGET}>)
target_compile_features(${TEST_TARGET} PUBLIC cxx_std_23)
target_include_directories(${TEST_TARGET} PUBLIC . ../src/lib_main )
target_link_libraries(${TEST_TARGET} INTERFACE ${LIBRARY_NAME})

set(CMAKE_EXPORT_COMPILE_COMMANDS "on")

# Enable Hot Reload for MSVC compilers if supported.
#if (POLICY CMP0141)
#  cmake_policy(SET CMP0141 NEW)
#  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
#endif()