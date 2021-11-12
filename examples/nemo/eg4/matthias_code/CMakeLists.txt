cmake_minimum_required(VERSION 3.17)
project(nemo_driver LANGUAGES CXX Fortran)

enable_language(CUDA)
find_package(CUDAToolkit)
set(CMAKE_CUDA_EXTENSIONS OFF)
set(CMAKE_CUDA_STANDARD 14)

# fetch grid tools
include(FetchContent)
find_package(GridTools 1.1.3)
if(NOT GridTools_FOUND)
  message(STATUS "Fetching GridTools...")  
  set(BUILD_TESTING OFF)
  set(INSTALL_TOOLS OFF)
  set(GT_INSTALL_EXAMPLES OFF)
  FetchContent_Declare(GridTools
    URL https://github.com/GridTools/gridtools/archive/v1.1.3.tar.gz
  )

  FetchContent_GetProperties(GridTools)
  if(NOT GridTools_POPULATED)
    FetchContent_Populate(GridTools)
    add_subdirectory(${gridtools_SOURCE_DIR} ${gridtools_BINARY_DIR})
  endif()
endif()

# dummy cpp driver
add_executable(driver_cpp driver_cpp.cpp)
target_compile_features(driver_cpp PUBLIC cxx_std_17)
target_compile_options(driver_cpp PRIVATE "-Wno-narrowing")

# nemo cpp as static lib
add_library(nemo_cpp STATIC wrapper_cpp.cpp res.cpp)
target_include_directories(nemo_cpp PRIVATE "/scratch/mroeth/dawn/dawn/src/")
target_include_directories(nemo_cpp PRIVATE "/scratch/mroeth/dawn/build/_deps/gridtools-src/include/")
target_include_directories(nemo_cpp PRIVATE "/scratch/mroeth/dawn/gtclang/src/")

# nemo cuda as static lib
set_source_files_properties(wrapper_cuda.cu res.cu PROPERTIES LANGUAGE CUDA )
add_library(nemo_cu STATIC wrapper_cuda.cu res.cu)
target_compile_options(nemo_cu PRIVATE $<$<COMPILE_LANGUAGE:CUDA>:-gencode arch=compute_70,code=sm_70>)
target_include_directories(nemo_cu PRIVATE "/scratch/mroeth/dawn/dawn/src/")
target_include_directories(nemo_cu PRIVATE "/scratch/mroeth/dawn/build/_deps/gridtools-src/include/")
target_include_directories(nemo_cu PRIVATE "/scratch/mroeth/dawn/gtclang/src/")

# need to explicitly link gridtools for cuda compilation (not sure why)
target_link_libraries(nemo_cu PUBLIC GridTools::gridtools)

# link each stencil to cpp driver
target_link_libraries(driver_cpp PRIVATE nemo_cpp)
target_link_libraries(driver_cpp PRIVATE nemo_cu)

# fortran driver
add_executable(driver_fortran driver.f90 res_cpp.f90 res_cuda.f90)

# link each stencil to fortran driver
target_link_libraries(driver_fortran PRIVATE nemo_cpp)
target_link_libraries(driver_fortran PRIVATE nemo_cu)


