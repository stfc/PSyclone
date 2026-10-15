EXE = gol
API = gocean
# If this flag is set to no, the code will not be executed during `test_run`.
# This allows to run all tests even if no GPU support is available (either
# because of hardware, or software like compiler-support)
USE_GPU ?= yes

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

.PHONY: test_run

# First trigger the default test (i.e. run the transformation), then
# check for the expected OpenACC directives in the output file:
TS=time_step_alg_mod_psy.f90
test: test-default
	cat $(TS) | grep "acc enter data copyin" > /dev/null
	cat $(TS) | grep "acc parallel default" > /dev/null
	cat $(TS) | grep "acc loop independent collapse" > /dev/null

ifeq ($(USE_GPU), yes)

	# If we are running with GPU support, trigger the above test target,
	# and then actual execution test
test_run: test test_run-default

else

	# Otherwise without GPU support, execute test and compile, but
	# don't try to execute:
test_run: test compile

endif
