F90 ?= gfortran
F90FLAGS ?= -Wall -g -O0

ROOT_DIR := $(abspath $(dir $(this_file))../../..)

ifeq ($(API), gocean)
	INF_DIR ?= $(ROOT_DIR)/external/dl_esm_inf/finite_difference
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

# Compilation rules
# -----------------
%.o: %.f90
	$(F90) -c $(F90FLAGS) $<
