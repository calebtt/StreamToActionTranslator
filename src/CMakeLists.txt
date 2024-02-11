# src folder for test driver.

cmake_minimum_required (VERSION 3.8)

# Enable Hot Reload for MSVC compilers if supported.
if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

# # Set some vars
set(DRIVER_NAME TestDriverMain)  # exe name (change if you wish)
set(DRIVER_SOURCES test_driver_main.cpp)

# # Add executable (test driver main)
add_executable(${DRIVER_NAME} ${DRIVER_SOURCES})

# # Add directories
add_subdirectory(lib_main)

# # Target link libs (library names to link with, each sub-folder a proj)
target_link_libraries(${DRIVER_NAME} INTERFACE StreamToActionTranslator)

# # MEOOOOOW
#add_library(StreamToActionTranslator )

#project ("TestDriverMain" LANGUAGES CXX)
#target_include_directories(${DRIVER_NAME} lib_main)
#target_compile_features(${DRIVER_NAME} PUBLIC cxx_std_23)

