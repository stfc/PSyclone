# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 environment
#
# export F90=gfortran
# export F90FLAGS="-Wall -g -fcheck=bound"

include ../../lfric_common.mk

GENERATED_FILES = *.o *.mod  $(EXEC) main_alg.f90 main_psy.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

EXEC = example

# TODO #3114: refactor this example to work with the new infrastructure
compile: transform
	@echo "No compilation supported for lfric/eg17/full_example_netcdf"

run: compile
	@echo "No run targets for lfric/eg17/full_example_netcdf"

transform: main_psy.f90

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) -L$(LDFLAGS)

$(LFRIC_LIB):
	$(MAKE) -C $(LFRIC_PATH) -f ../Makefile

# Dependencies
main_psy.o:	testkern_w0_kernel_mod.o
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: $(LFRIC_LIB)

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

%_psy.f90:	%.x90
	${PSYCLONE} -api lfric -opsy $*_psy.f90 -oalg $*_alg.f90 $<

allclean: clean
	make -C $(LFRIC_PATH) -f ../Makefile allclean
