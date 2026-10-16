# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 and F90FLAGS
# environment variables. To use the NVIDIA compiler and enable
# openacc compilation, use:
#
# export F90=nvfortran
# export F90FLAGS="-acc=gpu -Minfo=all"

PSYROOT=../../..

include ../lfric_common.mk

GENERATED_FILES = *.o *.mod $(EXEC) main_alg.f90 main_psy.f90 \
                  other_alg_mod_psy.f90 other_alg_mod_alg.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g

OBJ = main_psy.o main_alg.o other_alg_mod_psy.o other_alg_mod_alg.o \
  testkern_w0_kernel_mod.o

EXEC = example_openacc

# PSyData profiling wrapper library.
# Default is to use the 'template' library which simply prints entry and exit
# messages to stdout.
PROFILE_PATH=$(PSYROOT)/lib/profiling/template
PROFILE_LIB=$(PROFILE_PATH)/libdummy.a
PROFILE_LINK=-ldummy
# For NVIDIA profiling
#PROFILE_PATH=$(PSYROOT)/lib/profiling/nvidia
#PROFILE_LIB=$(PROFILE_PATH)/libnvtx_prof.a
#PROFILE_LINK=-lnvtx_prof -lnvtx3interop -cuda

.PHONY: transform compile run

transform:
	rm -f main_psy.f90
	${MAKE} main_psy.f90
	${MAKE} other_alg_mod_psy.f90

# Instruct PSyclone to automatically put profiling calipers around every
# invoke.
%_psy.f90: %.x90
	${PSYCLONE} -api lfric -dm -s ./acc_parallel.py  --profile invokes \
	-opsy $*_psy.f90 -oalg $*_alg.f90 $<

compile: transform ${EXEC}

run: compile
	./$(EXEC)

$(EXEC): $(PROFILE_LIB) $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS)  $(OBJ) -o $(EXEC) \
	 -L${PROFILE_PATH} $(PROFILE_LINK) $(LDFLAGS)

$(PROFILE_LIB):
	$(MAKE) -C $(PROFILE_PATH)

# Dependencies
main_psy.o:	other_alg_mod_psy.o testkern_w0_kernel_mod.o
main_alg.o:	other_alg_mod_alg.o main_psy.o

%.o: %.F90
	$(F90) $(F90FLAGS) -I$(PROFILE_PATH) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I$(PROFILE_PATH) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90
other_alg_mod_alg.f90: other_alg_mod_psy.f90

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(PROFILE_PATH) allclean
