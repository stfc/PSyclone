EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran without managed memory by default
F90 ?= nvfortran
F90FLAGS ?= -O3 -acc -Minfo=all

SCRIPT = openacc.py
include ../../common.mk

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# First trigger the default test (i.e. run the transformation), then
# check for the expected OpenACC directives in the output file:
TS=time_step_alg_mod_psy.f90
test: test-default
	cat $(TS) | grep "acc enter data copyin" > /dev/null
	cat $(TS) | grep "acc parallel default" > /dev/null
	cat $(TS) | grep "acc loop independent collapse" > /dev/null

# Trigger the above test target, and the running test
test_run: test test_run-default
