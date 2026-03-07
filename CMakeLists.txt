cmake_minimum_required(VERSION 3.20)
project(lumora VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

option(LUMORA_BUILD_EXTENSIONS "Build bundled extensions" ON)
option(LUMORA_BUILD_TESTS "Build test suite" ON)

add_library(lumora_core STATIC
    src/lexer/Lexer.cpp
    src/lexer/Token.cpp
    src/parser/Parser.cpp
    src/ast/AST.cpp
    src/ast/ASTDump.cpp
    src/sema/Sema.cpp
    src/codegen/IREmitter.cpp
    src/codegen/TypeMapper.cpp
    src/ext/ExtensionHost.cpp
    src/conf/Config.cpp
    src/driver/Pipeline.cpp
)

target_include_directories(lumora_core PUBLIC
    ${CMAKE_SOURCE_DIR}/include
)

target_compile_options(lumora_core PRIVATE
    -Wall -Wextra -Wpedantic -O2
)

add_executable(lumorac src/driver/main.cpp)
set_target_properties(lumorac PROPERTIES ENABLE_EXPORTS ON)
target_link_libraries(lumorac PRIVATE lumora_core dl)

if(LUMORA_BUILD_EXTENSIONS)
    add_subdirectory(extensions/example)
endif()

if(LUMORA_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
