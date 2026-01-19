ifeq ($(MPI), yes)
	F90 ?= mpif90
else
	F90 ?= gfortran
endif
F90FLAGS ?= -Wall -g -O0

# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up three from this file
ROOT_DIR := $(abspath $(dir $(this_file))../../..)

# Path of the included simplified LFRic infrastructure files:
LFRIC_PATH = $(ROOT_DIR)/external/lfric_infrastructure/src
LFRIC_LIB ?= $(LFRIC_PATH)/liblfric.a

# This sets up LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk
F90FLAGS += $(LFRIC_INCLUDE_FLAGS)

LIBS +=  $(LFRIC_LIB)

LDFLAGS += $(LIBS)

PSYCLONE = psyclone --config $(ROOT_DIR)/config/psyclone.cfg --psykal-dsl $(API) -l output

default: $(EXE)

.precious: main_alg.f90 main_alg_psy.f90


# Generic compilation rule
# ------------------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

%.o: %.F90
	$(F90) -c $(F90FLAGS) $<

# External libraries - LFRic infrastructure
# -----------------------------------------
$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

$(OBJ): $(LFRIC_LIB)

.PHONY: run-default allclean-default clean-default test-default transform

compile-default: $(EXE)

run-default: $(EXE)
	./$(EXE) $(GOL_DIR)/config.glider

test-default: $(EXE)
	make --no-print-directory  run | tail -n 12 | diff -b - $(GOL_DIR)/glider.correct

clean-default:
	rm -f *.o $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# The target allclean cleans the local directory, and then also all
# remote dependencies.
allclean-default: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean

# A sneaky way to allow a Makefile including this one to override
# 'clean' without a warning (overriding recipe)
%:  %-default
	@  true
