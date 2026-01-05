EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

# Use nvfortran by default
F90 ?= nvfortran
F90FLAGS ?= -O3 -acc -Minfo=all

SCRIPT = openacc.py
include ../../common.mk

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# We can't simply use the default test, since it will require to recompile
# the infrastructure files. So only test the transformation here.
# If compilation is to be tested, use `make test-default`
TS=time_step_alg_mod_psy.f90
test: test-default
	cat $(TS) | grep "acc enter data copyin" > /dev/null
	cat $(TS) | grep "acc parallel default" > /dev/null
	cat $(TS) | grep "acc loop independent collapse" > /dev/null
