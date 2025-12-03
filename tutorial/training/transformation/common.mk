F90 ?= gfortran
F90FLAGS ?= -Wall -g -O0

# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../../..)

# The GOL directory is used for KGO in testing
GOL_DIR = $(ROOT_DIR)/tutorial/training/gocean/gol-lib

PSYCLONE = psyclone --config $(ROOT_DIR)/config/psyclone.cfg -l output $(DM)

default: $(EXE)

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# Generic compilation rule
# ------------------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

%.o: %.F90
	$(F90) -c $(F90FLAGS) $<

.PHONY: run-default allclean-default clean-default test-default

compile-default: $(EXE)

run-default: $(EXE)
	./$(EXE) $(GOL_DIR)/config.glider

test-default: $(EXE)
	make --no-print-directory  run | tail -n 12 | diff -b - $(GOL_DIR)/glider.correct

clean-default:
	rm -f *.o $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# The target allclean cleans all also all remote dependencies
allclean-default: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean

# A sneaky way to allow a Makefile including this one to override
# 'clean' without a warning (overriding recipe)
%:  %-default
	@  true
