# Makefile for OpenCL utility modules
OBJECTS = clfortran.o ocl_params_mod.o ocl_utils_mod.o ocl_env_mod.o

all: ${OBJECTS}

%.o: %.f90
	${F90} ${F90FLAGS} -c $<

clean:
	rm -f *.o
	rm -f *~

allclean: clean
	rm -f *.mod *.MOD
