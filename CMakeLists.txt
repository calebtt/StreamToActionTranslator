# CMakeList.txt : CMake project for StreamToActionTranslator, include source and define
# project specific logic here.
#
cmake_minimum_required (VERSION 3.8)

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

project ("StreamToActionTranslator" CXX)


# Add source to this project's executable.
#add_executable (StreamToActionTranslator  "src/StreamToActionTranslator.h" "src/main.cpp")
add_library (StreamToActionTranslator OBJECT "src/StreamToActionTranslator.h" )

set(TARGET StreamToActionTranslator)
set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE CXX)

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET StreamToActionTranslator PROPERTY CXX_STANDARD 23)
endif()

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo")
endif ()

target_include_directories(${TARGET} PUBLIC /src )

enable_testing()
add_subdirectory(tests)
