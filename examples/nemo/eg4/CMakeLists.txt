##===------------------------------------------------------------------------------*- CMake -*-===##
##                          _                      
##                         | |                     
##                       __| | __ ___      ___ ___  
##                      / _` |/ _` \ \ /\ / / '_  | 
##                     | (_| | (_| |\ V  V /| | | |
##                      \__,_|\__,_| \_/\_/ |_| |_| - Compiler Toolchain
##
##
##  This file is distributed under the MIT License (MIT). 
##  See LICENSE.txt for details.
##
##===------------------------------------------------------------------------------------------===##

# Protobuf_DIR is <protobuf_root>/lib/cmake/protobuf
get_filename_component(camke_dir  "${Protobuf_DIR}" DIRECTORY)
get_filename_component(lib_dir "${camke_dir}" DIRECTORY)
get_filename_component(protobuf_root "${lib_dir}" DIRECTORY)

#
# Check the python libraries were installed in <protobuf_root>/python/
set(protobuf_python "${protobuf_root}/python")
set(protobuf_init_py "${protobuf_python}/google/protobuf/__init__.py")

if(NOT EXISTS "${protobuf_init_py}")
  message(WARNING 
    "Python module of protobuf is not installed correctly! Expected protobuf's __init__.py at:\n"
    "    ${protobuf_init_py}\n"
    "Disabling python module of Dawn."
  )
else()
  set(DAWN_INSTALL_PROTOBUF_MODULE "${protobuf_python}")

  set(config_in "${CMAKE_SOURCE_DIR}/examples/python/config.py.in")
  set(config_out "${CMAKE_BINARY_DIR}/examples/python/config.py")

  set(DAWN_INSTALL_MODULE ${CMAKE_BINARY_DIR}/python/)
  set(DAWN_INSTALL_DAWNCLIB ${CMAKE_INSTALL_PREFIX}/lib/libDawnC.so)

  configure_file(${config_in} ${config_out})

  # Copy to build directory
  file(COPY ${CMAKE_SOURCE_DIR}/python/ DESTINATION ${CMAKE_BINARY_DIR}/python
       FILES_MATCHING PATTERN "*.py")

  # Copy to install
  install(FILES ${config_out} 
	  DESTINATION ${DAWN_INSTALL_EXAMPLES_DIR}/python)
  install(DIRECTORY . 
	  DESTINATION ${DAWN_INSTALL_EXAMPLES_DIR}/python/
  )
endif()
