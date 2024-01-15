# CMakeList.txt : CMake project for tests, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

set(TEST_TARGET test_main)
add_executable(${TEST_TARGET} ${TEST_TARGET}.cpp "test_utils.h")
add_test(NAME ${TEST_TARGET} COMMAND $<TARGET_FILE:${TEST_TARGET}>)
target_include_directories(${TEST_TARGET} PUBLIC . ../src )

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET test_main PROPERTY CXX_STANDARD 23)
endif()

set(TEST_TARGET test_filter)
add_executable(${TEST_TARGET} ${TEST_TARGET}.cpp "test_utils.h")
add_test(NAME ${TEST_TARGET} COMMAND $<TARGET_FILE:${TEST_TARGET}>)
target_include_directories(${TEST_TARGET} PUBLIC . ../src )

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET test_filter PROPERTY CXX_STANDARD 23)
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS "on")

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()