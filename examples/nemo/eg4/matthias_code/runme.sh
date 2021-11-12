INCLUDE="-I/home/rupert/proj/dawn/dawn/src -I/home/rupert/proj/dawn/build/_deps/gridtools-src/include"
# g++ -c ${INCLUDE} tra_adv_compute.cpp
g++ -c ${INCLUDE} wrapper_cpp.cpp
gfortran -c res_cpp.f90
gfortran -c driver.f90
gfortran -o tra_adv -I. res_cpp.o wrapper_cpp.o driver.o -lstdc++
