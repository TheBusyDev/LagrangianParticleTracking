cmake_minimum_required(VERSION 3.10)
project(LPT Fortran)

# Set the Fortran Standard
set(CMAKE_Fortran_STANDARD 2003)

# Define flags for Debug and Release modes
set(CMAKE_Fortran_FLAGS_DEBUG   "-cpp -g -Wall -Wextra -fcheck=all -fbounds-check")
set(CMAKE_Fortran_FLAGS_RELEASE "-cpp -O3 -march=native -flto -ffast-math")
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modules")

# Find all source files
file(GLOB_RECURSE SOURCES "src/*.f90")

# Create the executable
add_executable(main ${SOURCES})

# Add plot target
add_custom_target(
  plot
  COMMAND python3 "${CMAKE_SOURCE_DIR}/scripts/plot.py"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
