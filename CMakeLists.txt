# CMakeList.txt : CMake project for StreamToActionTranslator, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

project ("StreamToActionTranslator" LANGUAGES CXX)


set(DRIVER_NAME StreamToActionTranslatorApp)  # driver name (change if you wish)
add_executable (${DRIVER_NAME} "src/driver_main.cpp"  )
target_compile_features(${DRIVER_NAME} PUBLIC cxx_std_23)

enable_testing()
add_subdirectory(tests)
add_subdirectory(src)