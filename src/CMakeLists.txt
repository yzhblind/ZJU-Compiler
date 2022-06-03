# 参考Github项目的CMakeLists的写法 https://github.com/huangyangyi/Our-Pascal-Compiler/blob/master/src/CMakeLists.txt

cmake_minimum_required(VERSION 3.10)
project(ZJU-Compiler)

find_package(LLVM REQUIRED CONFIG)
include_directories(.)
include_directories(${CMAKE_CURRENT_BINARY_DIR})

include_directories(/usr/include/llvm)
include_directories(/usr/include/llvm-c)

set(CMAKE_CXX_STANDARD 14)

aux_source_directory(ast AST_SOURCE)

add_custom_command(
    OUTPUT pascal.l.cpp
    COMMAND flex -o pascal.l.cpp ${CMAKE_CURRENT_LIST_DIR}/scanner/pascal.l
    DEPENDS ${CMAKE_CURRENT_LIST_DIR}/scanner/pascal.l pascal.y.cpp
)

add_custom_command(
    OUTPUT pascal.y.hpp pascal.y.cpp 
    COMMAND bison -d -o pascal.y.cpp ${CMAKE_CURRENT_LIST_DIR}/parser/pascal.y
    DEPENDS ${CMAKE_CURRENT_LIST_DIR}/parser/pascal.y
)

add_executable(mypc
    ${AST_SOURCE} pascal.l.cpp
    pascal.y.cpp pascal.y.hpp main.cpp)