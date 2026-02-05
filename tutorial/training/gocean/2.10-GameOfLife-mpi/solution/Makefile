EXE = gol
API = gocean

# The MPI flag is used to indicate if a MPI enabled compiler is used.
# If not, the transformations will still use distributed memory, but
# compilation will be disabled.
MPI ?= yes
F90 ?= mpif90

# Trigger the flag -dm, and linking of MPI library in common.mk:
USE_MPI = yes
SCRIPT ?= openmp.py
include ../../common.mk

test:
	SCRIPT=./openmp.py $(MAKE) test-default
	@# Make sure we get a halo exchange:
	cat time_step_alg_mod_psy.f90 | grep halo_exchange >/dev/null
	@# Note that this will also test loop_fuse.py
	SCRIPT=./openmp_combined.py $(MAKE) test-default
	cat time_step_alg_mod_psy.f90 | grep halo_exchange >/dev/null


# MPI is disabled for github CI, so have tests for both with and without MPI
ifeq ($(MPI), yes)
run: $(EXE)
	mpirun -np 4 ./$(EXE) ../../gol-lib/config.glider

# Call the default test (which checks the output file), but do a second test to make
# sure we get a 2x2 domain decomposition, otherwise the job was not running properly
# with MPI
TS=time_step_alg_mod
test_run: test
	bash -c "if [[ $$(make run | grep -c 'go_decompose: using grid of *2x *2') != 1 ]]; then \
		echo 'Job is not running properly with MPI'; \
	fi"


else
test_run: test
	bash -c "if [[ $$(make run | grep -c 'go_decompose: using grid of *1x *1') != 1 ]]; then \
		echo 'Job is not running properly with MPI'; \
	fi"

# No MPI enabled compiler is available
run: $(EXE)
	./$(EXE) ../../gol-lib/config.glider

endif

