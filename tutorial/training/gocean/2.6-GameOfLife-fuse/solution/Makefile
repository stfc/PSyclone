EXE = gol
API = gocean

SCRIPT ?= fuse_loops.py
include ../../common.mk

TS=time_step_alg_mod

test:
	SCRIPT=fuse_loops.py $(MAKE) test-default
	SCRIPT=fuse_loops_last.py $(MAKE) test-default
	SCRIPT=full_inline_loops.py $(MAKE) test-default \
	| grep "potential replacement born"

test_run:
	$(MAKE) clean
	SCRIPT=fuse_loops.py $(MAKE) test_run-default
	$(MAKE) clean
	SCRIPT=fuse_loops_last.py $(MAKE) test_run-default
	$(MAKE) clean
	@# First run make test-default to grep for expected output
	SCRIPT=full_inline_loops.py $(MAKE) test-default \
	| grep "potential replacement born"
	@# Then build and run:
	SCRIPT=full_inline_loops.py $(MAKE) test_run-default

