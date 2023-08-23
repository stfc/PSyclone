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
# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../..)

ifeq ($(API), gocean)
	# For now till MPI support is merged into dl_esm_info and PSyclone is updated.
	# INF_DIR ?= $(ROOT_DIR)/external/dl_esm_inf/finite_difference
	INF_DIR ?= $(HOME)/work/dl_esm_inf/finite_difference
	INF_INC = $(INF_DIR)/src
	ifeq ($(MPI), yes)
		INF_LIB ?= $(INF_DIR)/src/lib_dm_fd.a
	else
		INF_LIB ?= $(INF_DIR)/src/lib_fd.a
	endif
endif

GOL_DIR = $(ROOT_DIR)/tutorial/training/gol-lib
GOL_LIB = $(GOL_DIR)/libgol.a

INCL = -I$(INF_INC) -I$(GOL_DIR)
LIBS = $(GOL_LIB) $(INF_LIB)

LDFLAGS += $(LIBS)
F90FLAGS += $(INCL)

PSYCLONE=psyclone --config $(ROOT_DIR)/config/psyclone.cfg \
		-l output -d $(GOL_DIR)

default: $(EXE)

.precious: time_step_alg_mod.f90 time_step_alg_mod_psy.f90

$(OBJ): $(GOL_LIB)

# External libs
# -------------
$(GOL_LIB): $(INF_LIB)
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR)

$(INF_LIB):
	echo "NOOOOO " $(MPI)
	$(MAKE) MPI=$(MPI) F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)/src

#$(INF_DIR)/src/lib_dm_fd.a:
#	echo "YESSSS"
#	# Specialisation to provide the MPI=yes flag
#	$(MAKE) MPI=yes F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)/src

# Generic compilation rule
# ------------------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

.PHONY: clean allclean run test

clean:
	rm -f *.o $(EXE) *.mod time_step_alg_mod.f90 time_step_alg_mod_psy.f90

# The target allclean cleans all also all remote dependencies
allclean: clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_INC) clean
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(GOL_DIR) clean