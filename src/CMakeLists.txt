# CMakeList.txt : CMake project for test driver program, include source and define
cmake_minimum_required (VERSION 3.8)

set(DRIVER_NAME test_driver)  # driver name (change if you wish)
add_executable (${DRIVER_NAME} "driver_main.cpp" "StreamToActionTranslator.h")
target_compile_features(${DRIVER_NAME} PUBLIC cxx_std_23)

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()