F90 ?= gfortran

# While only some examples use openmp, it is important that they
# are actually compiled with openmp (since errors in threading might
# otherwise not show up), and it doesn't affect non-threaded code.
F90FLAGS ?= -Wall -g -O0 -fopenmp

# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../../..)

ifeq ($(API), gocean)
	INF_DIR ?= $(ROOT_DIR)/external/dl_esm_inf/finite_difference
	INF_INC = $(INF_DIR)/src
	ifeq ($(MPI), yes)
		INF_LIB ?= $(INF_DIR)/src/lib_dm_fd.a
		DM = -dm
	else
		INF_LIB ?= $(INF_DIR)/src/lib_fd.a
		DM = -nodm
	endif
endif

GOL_DIR = $(ROOT_DIR)/tutorial/training/gocean/gol-lib
GOL_LIB = $(GOL_DIR)/libgol.a

# For examples that do not use golib, but want to use
# the remaining flags set here:
ifneq ($(IGNORE_GOL_LIB), yes)
	LIBS = $(GOL_LIB)
	F90FLAGS += -I$(GOL_DIR)
endif
LIBS +=  $(INF_LIB)

LDFLAGS += $(LIBS)
F90FLAGS += -I$(INF_INC)

PSYCLONE = psyclone --config $(ROOT_DIR)/config/psyclone.cfg --psykal-dsl $(API) -l output $(DM) -d $(GOL_DIR)

default: $(EXE)

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# External libs
# -------------
$(GOL_LIB): $(INF_LIB)
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR)

$(INF_LIB):
	$(MAKE) MPI=$(MPI) F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)/src


#
.PHONY: allclean-default clean-default test-default test_run-default \
		transform-default

test_run-default: $(EXE)
	make --no-print-directory  run | tail -n 12 | diff -b - $(GOL_DIR)/glider.correct

compile-default: $(EXE)

run-default: $(EXE)
	./$(EXE) $(GOL_DIR)/config.glider

# Testing, which does not run on github, typicall should just
# check the transform target. Though multiple examples will define
# their own test target (e.g. to verify several transformations, errors, ...)
test-default: transform

PSYCLONE_COMMAND = $(PSYCLONE) -oalg time_step_alg_mod.f90 -opsy \
					time_step_alg_mod_psy.f90 time_step_alg_mod.x90
ifdef SCRIPT
	PSYCLONE_COMMAND += -s $(SCRIPT)
endif

transform-default:
	$(PSYCLONE_COMMAND)

clean-default:
	rm -f *.o $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# The target allclean cleans all also all remote dependencies
allclean-default: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean

# PSyclone: create alg and psy layer files:
ifdef SCRIPT
time_step_alg_mod.f90: time_step_alg_mod.x90 Makefile $(SCRIPT)
	$(PSYCLONE) -oalg time_step_alg_mod.f90 -opsy time_step_alg_mod_psy.f90 \
		-s $(SCRIPT) time_step_alg_mod.x90
else
time_step_alg_mod.f90: time_step_alg_mod.x90 Makefile $(SCRIPT)
	$(PSYCLONE) -oalg time_step_alg_mod.f90 -opsy time_step_alg_mod_psy.f90 \
		time_step_alg_mod.x90
endif

# A sneaky way to allow a Makefile including this one to override
# targets without a warning (overriding recipe)
%:  %-default
	@  true

# Dependencies for compilation
# ----------------------------
OBJ ?= time_step_alg_mod.o time_step_alg_mod_psy.o

# sources need golib
$(OBJ): $(GOL_LIB)

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Ensure that PSyclone has finished (which will also create the psy file)
time_step_alg_mod_psy.f90: time_step_alg_mod.f90
time_step_alg_mod.o: time_step_alg_mod_psy.o

# Generic compilation rule
# ------------------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

%.o: %.F90
	$(F90) -c $(F90FLAGS) $<
