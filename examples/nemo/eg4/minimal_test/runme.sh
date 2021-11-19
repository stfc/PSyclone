INCLUDE="-I/home/rupert/proj/dawn/dawn/src -I/home/rupert/proj/dawn/build/_deps/gridtools-src/include"
# g++ -c ${INCLUDE} tra_adv_compute.cpp
g++ -g -c ${INCLUDE} wrapper_cpp.cpp
gfortran -g -c res_cpp.f90
gfortran -g -c driver.f90 -I/home/rupert/proj/dl_timer/src
gfortran -g -o tra_adv -I. res_cpp.o wrapper_cpp.o driver.o -fopenmp -lstdc++ -L/home/rupert/proj/dl_timer -ldl_timer_omp
export JPI=130
export JPJ=130
export JPK=31
export IT=100
./tra_adv
