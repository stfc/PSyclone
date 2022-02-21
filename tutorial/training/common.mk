F90 ?= gfortran
F90FLAGS ?= -Wall -g -O0

# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))
# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../..)

ifeq ($(API), gocean)
	#INF_DIR ?= $(ROOT_DIR)/external/dl_esm_inf/finite_difference
	INF_DIR ?= $(HOME)/work/dl_esm_inf/finite_difference
	INF_INC = $(INF_DIR)/src
	INF_LIB = $(INF_DIR)/src/lib_fd.a
endif

INCL = -I$(INF_INC)
LIBS = -L$(INF_INC) -l_fd

LDFLAGS += $(LIBS)
F90FLAGS += $(INCL)

default: $(EXE)

# External libs
# -------------
$(INF_LIB):
	$(MAKE) F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)
$(INF_DIR)/src/lib_dm.a:
	$(MAKE) MPI=yes F90FLAGS="$(F90FLAGS)" -C $(INF_DIR)

# Compilation rules
# -----------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<
