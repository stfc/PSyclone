EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

SCRIPT ?= openmp.py
include ../../common.mk

test:
	SCRIPT=./openmp.py $(MAKE) test-default
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *parallel *do *default"
	SCRIPT=./openmp_combined.py  $(MAKE) test-default
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *parallel *default"
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *do *schedule"
	SCRIPT=./task.py  $(MAKE) test-default
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *parallel *default"
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *single"
	cat time_step_alg_mod_psy.f90 | grep -iq "omp *taskloop"
	@# The fuse_loop scripts are used in openmp and openmp_combined

test_run: #test
	$(MAKE) clean
	SCRIPT=./openmp.py $(MAKE) test_run-default
	@# Make sure it was actually compiled with threading:
	OMP_NUM_THREADS=2 $(MAKE) run | grep "Using *2 *threads" > /dev/null

	$(MAKE) clean
	SCRIPT=./openmp_combined.py $(MAKE) test_run-default
	OMP_NUM_THREADS=2 $(MAKE) run | grep  "Using *2 *threads" > /dev/null

	@# Taskloop is not supported by nvfortran, so disable this test for now:
	@#$(MAKE) clean
	@#SCRIPT=./task.py  $(MAKE) test_run-default
	@#OMP_NUM_THREADS=2 $(MAKE) run | grep  "Using *2 *threads" > /dev/null
