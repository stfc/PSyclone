EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

SCRIPT ?= openmp.py
include ../../common.mk

test:
	SCRIPT=./openmp.py $(MAKE) test-default
	SCRIPT=./openmp_combined.py  $(MAKE) test-default
	SCRIPT=./task.py  $(MAKE) test-default
	@# Also verify the fuse loops scripts still work:
	SCRIPT=./fuse_loops.py $(MAKE) test-default
	SCRIPT=./fuse_loops_last.py  $(MAKE) test-default

test_run:
	$(MAKE) clean
	SCRIPT=./openmp.py $(MAKE) test_run-default
	@#TODO 3253
	# $(MAKE) clean
	# SCRIPT=./openmp_combined.py $(MAKE) test_run-default
	@echo openmp_combined does not compile atm due to #3253
	# $(MAKE) clean
	# SCRIPT=./task.py  $(MAKE) test_run-default
	@echo task.py does not compile atm due to #3253
