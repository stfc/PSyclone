# MAKEFILE_LIST is a Gnu-make variable that contains all of the
# arguments passed to the first invocation of Make. The last entry
# in this list is the current file.
this_file := $(abspath $(lastword $(MAKEFILE_LIST)))

# PSyclone directory is up two from this file
ROOT_DIR := $(abspath $(dir $(this_file))../../..)
PSYCLONE = psyclone --config $(ROOT_DIR)/config/psyclone.cfg --psykal-dsl lfric

# Path of the included LFRic infrastructure files (for the examples
# that can be compiled)
LFRIC_PATH=$(ROOT_DIR)/external/lfric_infrastructure/src
LFRIC_NAME=lfric
LFRIC_LIB=$(LFRIC_PATH)/lib$(LFRIC_NAME).a

.PHONY: allclean clean compile default run test transform

# Add a default target for all examples to run `make test`
default: transform

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"
F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

# This sets up LFRIC_INCLUDE_FLAGS
include $(LFRIC_PATH)/lfric_include_flags.mk
F90FLAGS += $(LFRIC_INCLUDE_FLAGS)

# External libraries - LFRic infrastructure
# -----------------------------------------
$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH)

# File creation rules
# -------------------
%.o: %.f90
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) -c $<
