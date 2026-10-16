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
# Path to the 'dl_timer' installation. It defaults to the assumed
# location of the dl_timer library next to a clone of PSyclone
# repository. Overwrite for a custom location.
DL_TIMER_ROOT ?= $(PSYDATA_LIB_DIR)/../../dl_timer
# Path to the 'stfc/dl_timer' "include" files. Overwrite for a custom location.
DL_TIMER_INCLUDE ?= $(DL_TIMER_ROOT)/src
# Path to the PSyclone wrapper libraries. It defaults to the relative path to
# the top-level 'lib' directory. Overwrite for a custom location.
PSYDATA_LIB_DIR ?= ./../..
# -----------------------------------------------------------------------------

F90FLAGS += -I$(DL_TIMER_INCLUDE)

PSYDATA_LIB_NAME = dl_timer_psy
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = dl_timer.o psy_data_base.o

PROCESS_ARGS = -prefix=profile_ -types= -dims=
PROCESS = $$($(PSYDATA_LIB_DIR)/get_python.sh) $(PSYDATA_LIB_DIR)/process.py

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(OBJS)
	ar rs $(PSYDATA_LIB) $(OBJS)

dl_timer.o:	psy_data_base.o

psy_data_base.f90: $(PSYDATA_LIB_DIR)/psy_data_base.jinja
	$(PROCESS) $(PROCESS_ARGS) $< > psy_data_base.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) psy_data_base.*

allclean: clean
