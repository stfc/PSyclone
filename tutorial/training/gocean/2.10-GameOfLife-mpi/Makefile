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
include ../common.mk

run: $(EXE)
	mpirun -np 4 ./$(EXE) ../gol-lib/config.glider

# Call the default test (which checks the output file), but do a second test to make
# sure we get a 2x2 domain decomposition, otherwise the job was not running properly
# with MPI
test: test-default
	@bash -c "if [[ $$(make run | grep -c 'go_decompose: using grid of   2x  2') != 1 ]]; then \
		echo 'Job is not running properly with MPI'; \
	fi"
