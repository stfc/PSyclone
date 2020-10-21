# We use the PSyData simple_timing implementation since it has no
# dependencies.
PROFILE_DIR ?= ../../../lib/profiling/simple_timing
PROFILE_LINK = -lsimple_timing
PROFILE_LIB = ${PROFILE_DIR}/libsimple_timing.a

NAME = tra_adv.exe

# Use gfortran by default
F90 ?= gfortran
F90FLAGS ?= -fopenmp

$(NAME): $(KERNELS) psy.o runner.o
	$(F90) $(F90FLAGS) $^ -o $@ -L$(PROFILE_DIR) $(PROFILE_LINK)

# Target that uses PSyclone to generate 'psy.f90'
psy.f90: tra_adv_mod.F90 omp_trans.py
	psyclone -s ./omp_trans.py -api nemo \
                     -opsy psy.f90 -l output tra_adv_mod.F90

# Make sure the infrastructure library is compiled, so the mods are found
$(KERNELS): $(INF_LIB)

# Creating psy.o and runner.o requires that the profiling lib be compiled
runner.o psy.o: ${PROFILE_LIB}

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(PROFILE_DIR) -c $<

${PROFILE_LIB}:
	${MAKE} -C ${PROFILE_DIR}

clean:
	rm -f psy.f90 *.o *.mod

allclean: clean
	rm -f ${NAME}
	${MAKE} -C ${PROFILE_DIR}  clean
