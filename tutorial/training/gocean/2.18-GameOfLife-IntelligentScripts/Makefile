EXE = gol
API = gocean

OBJ = time_step_alg_mod.o time_step_alg_mod_psy.o

SCRIPT=fuse_loops.py
include ../common.mk

# Dependencies:
# -------------
# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

TS=time_step_alg_mod
test: test-default
	$(PSYCLONE) -s ./fuse_loops_index.py -oalg $(TS).f90 -opsy $(TS)_psy.f90 $(TS).x90
	@# Count the number of outer loops (do j) to check that loops were fused
	@bash -c "if [[ $$(cat $(TS)_psy.f90 | grep -c 'do j') != 2 ]]; then \
		echo 'We should only have two outer loops in $(TS)_psy.f90';     \
		exit 1; \
	fi"

test_run: test test_run-default
