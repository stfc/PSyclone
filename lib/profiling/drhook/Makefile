# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?=
# Path to the Dr Hook installation. It defaults to the assumed location
# of the Dr Hook directory next to a clone of PSyclone repository.
# Overwrite for a custom location.
DRHOOK_ROOT ?= ./../../../../drhook
# Path to the DrHook "include" files. Overwrite for a custom location.
DRHOOK_INCLUDE ?= $(DRHOOK_ROOT)/include
DRHOOK_MODULES ?= $(DRHOOK_ROOT)/build/module
# -----------------------------------------------------------------------------

F90FLAGS += -I$(DRHOOK_INCLUDE) -I$(DRHOOK_MODULES)

PSYDATA_LIB_NAME = drhook_psy
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = drhook_psy.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(OBJS)
	ar rs $(PSYDATA_LIB) $(OBJS)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB)

allclean: clean
