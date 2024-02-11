# lib_main project, builds the single header interface lib.

#project ("StreamToActionTranslator" LANGUAGES CXX)
set(LIBRARY_NAME StreamToActionTranslator)  # library name (change if you wish)
set(LIB_SOURCES StreamToActionTranslator.h) # all library source files

# Note: The 'INTERFACE' keyword here is used because there is no .cpp source files for this library.
add_library(${LIBRARY_NAME} INTERFACE ${LIB_SOURCES})
#add_library(${LIBRARY_NAME} OBJECT "")
target_sources(${LIBRARY_NAME} INTERFACE ${CMAKE_CURRENT_LIST_DIR}/StreamToActionTranslator.h)
target_compile_features(${LIBRARY_NAME} INTERFACE cxx_std_23)
target_include_directories(${LIBRARY_NAME} INTERFACE ${CMAKE_CURRENT_LIST_DIR})

target_link_libraries(${LIBRARY_NAME} INTERFACE ${LIB_SOURCES})
#target_include_directories(${LIBRARY_NAME} INTERFACE ${CMAKE_CURRENT_LIST_DIR})