cmake_minimum_required(VERSION 3.21)
project(token)

set(CMAKE_CXX_STANDARD 17)

add_executable(token main.cpp tokenization.cpp tokenization.h)
