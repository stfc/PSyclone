INCLUDE="-I/home/rupert/proj/dawn/dawn/src -I/home/rupert/proj/dawn/build/_deps/gridtools-src/include"
# g++ -c ${INCLUDE} tra_adv_compute.cpp
g++ -c ${INCLUDE} -O3 wrapper_cpp.cpp
gfortran -c res_cpp.f90
gfortran -c driver.f90
gfortran -o tra_adv -I. res_cpp.o wrapper_cpp.o driver.o -lstdc++
export JPI=130
export JPJ=130
export JPK=31
export IT=100
./tra_adv
# Checksum for domain  130 x 130 x  31 ( 100 iterations) = -0.3064615946252952E+20 0.1226639421040368E+25
