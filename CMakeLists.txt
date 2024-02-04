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

set(SOURCES          # All .h/.cpp files in src/
	src/StreamToActionTranslator.h
    )

set(TESTFILES        # All .cpp files in tests/
    tests/rand_gen.h
    tests/test_utils.h
    tests/test_main.cpp
    tests/test_filter.cpp)

set(LIBRARY_NAME StreamToActionTranslator)  # library name (change if you wish)
set(DRIVER_NAME DriverMain)  # library name (change if you wish)

# Add source to this project's .lib/.dll/.exe
#add_executable (StreamToActionTranslator  "src/StreamToActionTranslator.h" "src/main.cpp")
add_library (${LIBRARY_NAME} OBJECT ${SOURCES})
add_executable (${DRIVER_NAME} "src/StreamToActionTranslator.h" "src/driver_main.cpp")

set(TARGET ${DRIVER_NAME})
set_target_properties(
    ${DRIVER_NAME}
      PROPERTIES
        CXX_STANDARD 23
        LINKER_LANGUAGE CXX
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO)

set(TARGET ${LIBRARY_NAME})
set_target_properties(
    ${LIBRARY_NAME}
      PROPERTIES
        CXX_STANDARD 23
        LINKER_LANGUAGE CXX
        CXX_STANDARD_REQUIRED YES
        CXX_EXTENSIONS NO)

#set_property(TARGET StreamToActionTranslator PROPERTY CXX_STANDARD 23)
#set_target_properties(${TARGET} PROPERTIES LINKER_LANGUAGE CXX)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo")
endif ()

#target_include_directories(${TARGET} PUBLIC "/src" )

enable_testing()
add_subdirectory(tests)