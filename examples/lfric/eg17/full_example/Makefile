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

GENERATED_FILES = main_psy.f90 main_alg.f90 *.o *.mod $(EXEC)

F90 ?= gfortran
F90FLAGS ?= -Wall -g -fcheck=bound

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

EXEC = example

run: compile
	./example

compile: transform $(EXEC)

$(EXEC): $(LFRIC_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) -o $(EXEC) \
	-L$(LFRIC_PATH) -l$(LFRIC_NAME) $(LDFLAGS)

# Dependencies
main_psy.o:	testkern_w0_kernel_mod.o
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: $(LFRIC_LIB)

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_alg.f90: main_psy.f90

transform: main_psy.f90

%_psy.f90:	%.x90
	${PSYCLONE} -api lfric --keep-comments --keep-directives -opsy $*_psy.f90 -oalg $*_alg.f90 $<

allclean: clean
	make -C $(LFRIC_PATH) -f ../Makefile allclean
