EXE = gol
API = gocean
# If this flag is set to no, the code will not be executed during `test_run`.
# This allows to run all tests even if no GPU support is available (either
# because of hardware, or software like compiler-support)
USE_GPU ?= yes

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran with managed memory - the scripts currently only support
# managed memory.
F90 ?= nvfortran
F90FLAGS ?= -O3 -mp=gpu -gpu=managed -Minfo=all

SCRIPT=omp_offload.py
include ../../common.mk

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# PSyclone: create alg and psy layer files:
%.f90: %.x90 Makefile omp_offload.py
	$(PSYCLONE) -s ./omp_offload.py -oalg $*.f90 -opsy $*_psy.f90 $<

.PHONY: test_run

# First trigger the default test (i.e. run the transformation), then
# check for the expected OpenMP directives in the output file:
TS=time_step_alg_mod_psy.f90
test: test-default
	cat $(TS) | grep "omp target" > /dev/null
	cat $(TS) | grep "omp teams distribute parallel" > /dev/null
	cat $(TS) | grep "omp declare target" > /dev/null

ifeq ($(USE_GPU),yes)

	# If we are running with GPU support, trigger the above test target,
	# and then actual execution test
test_run: test test_run-default

else

	# Otherwise without GPU support, execute test and compile, but
	# don't try to execute:
test_run: test compile

endif
