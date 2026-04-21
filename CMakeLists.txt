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
set(EXEC "main")
add_executable(${EXEC} ${SOURCES})

# Create links
foreach(folder "examples" "scripts")
  file(
    CREATE_LINK
    "${CMAKE_SOURCE_DIR}/${folder}"
    "${CMAKE_CURRENT_BINARY_DIR}/${folder}"
    SYMBOLIC)
endforeach()

# Add run target
add_custom_target(
  run
  COMMAND ./${EXEC} ./examples/config.nml
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")

# Add plot target
add_custom_target(
  plot
  COMMAND python3 "./scripts/plot.py"
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
