# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# ----------- Default "make" values, can be overwritten by the user -----------
F90 ?= gfortran
F90FLAGS ?=

# -----------------------------------------------------------------------------

PSYDATA_LIB_NAME = tau_psy
PSYDATA_LIB = lib$(PSYDATA_LIB_NAME).a

OBJS = tau_psy.o

default: $(PSYDATA_LIB)

.PHONY: default clean allclean

$(PSYDATA_LIB): $(OBJS)
	ar rs $(PSYDATA_LIB) $(OBJS)

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod $(PSYDATA_LIB) *pomp.f90 *opari.inc

allclean: clean
