cmake_minimum_required(VERSION 3.10)
project(LPT_Project Fortran)

# 1. Set the Fortran Standard
set(CMAKE_Fortran_STANDARD 2003)

# 2. Define flags for Debug and Release modes
# Debug: Slow, but catches errors and allows line-by-line debugging
set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -Wextra -fcheck=all -fbounds-check")

# Release: Fast, uses CPU optimizations, removes debug checks
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -flto")

# 3. Find all source files
file(GLOB_RECURSE SOURCES "src/*.f90")

# 4. Create the executable
add_executable(main ${SOURCES})