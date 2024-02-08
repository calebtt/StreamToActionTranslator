# CMakeList.txt : CMake project for StreamToActionTranslator, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /std:c++latest")

project ("StreamToActionTranslator" LANGUAGES CXX)
set(LIBRARY_NAME StreamToActionTranslator)  # library name (change if you wish)
set(LIB_SOURCES lib_main/StreamToActionTranslator.h) # all library source files

# Note: The 'INTERFACE' keyword here is used because there is no .cpp source files for this library.
add_library(${LIBRARY_NAME} INTERFACE ${LIB_SOURCES})
target_compile_features(${LIBRARY_NAME} INTERFACE cxx_std_23)
target_link_libraries(${LIBRARY_NAME} INTERFACE ${LIB_SOURCES})

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo")
endif ()

enable_testing()
add_subdirectory(tests)