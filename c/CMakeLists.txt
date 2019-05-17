cmake_minimum_required(VERSION 2.8 FATAL_ERROR)

project(SM_Engine C)

set(CMAKE_C_FLAGS	"${CMAKE_C_FLAGS} -g -O0 --std=gnu99")
add_executable(smEngine main.c sm_engine.c sm_data.c sm_funcs.c cmd_line_prms.c quickcheck4c.c)

