cmake_minimum_required(VERSION 3.20)
project(tgplitw LANGUAGES C)

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

set(CMAKE_BUILD_TYPE Debug)
if(0)
    set(BISON_FLAGS "-Wcounterexamples --debug")
    set(FLEX_FLAGS "--debug")
else()
    set(BISON_FLAGS "-Wcounterexamples")
    set(FLEX_FLAGS "")
endif()

FLEX_TARGET(lexer 
    src/lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.y.c
    COMPILE_FLAGS ${FLEX_FLAGS})

BISON_TARGET(parser 
    src/parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.tab.c 
    COMPILE_FLAGS ${BISON_FLAGS})

ADD_FLEX_BISON_DEPENDENCY(lexer parser)

set(SOURCES
    src/main.c
    src/object.c
    src/allocator.c
    src/heap.c
    src/preprocess.c
    src/base64.c
    src/parser.c
    src/scope.c
    src/hash_table.c
    src/interpreter.c
    src/standard_library.c
    ${FLEX_lexer_OUTPUTS}
    ${BISON_parser_OUTPUTS}
)

add_executable(tgplitw ${SOURCES})

target_include_directories(tgplitw
    PRIVATE 
    ${FLEX_INCLUDE_DIRS} 
    ${CMAKE_CURRENT_BINARY_DIR} 
    ${CMAKE_CURRENT_SOURCE_DIR}/src
)

target_link_libraries(tgplitw
    PRIVATE
    ${FLEX_LIBRARIES}
    m
)

