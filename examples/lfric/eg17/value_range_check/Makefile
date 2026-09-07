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

PSYROOT=../../../..

include ../../lfric_common.mk

GENERATED_FILES += *.o *.mod $(EXEC) main_alg.f90 main_psy.f90

F90 ?= gfortran
F90FLAGS ?= -Wall -g -ffree-line-length-none

OBJ = main_psy.o main_alg.o testkern_w0_kernel_mod.o

VALUE_RANGE_DIR ?= $(PSYROOT)/lib/value_range_check/lfric
VALUE_RANGE_NAME = _value_range_check

VALUE_RANGE_LIB=$(VALUE_RANGE_DIR)/lib$(VALUE_RANGE_NAME).a

EXEC = value_range_check

F90FLAGS += -I$(VALUE_RANGE_DIR)

transform: main_psy.f90 main_alg.f90
	
compile: transform $(EXEC)

run: compile
	@# Run application and grep for the expected value range message:
	PSY_VALUE_RANGE="field1_data=0:7" ./$(EXEC) 2>&1 | grep "at index/indices"

$(EXEC): $(LFRIC_LIB) $(VALUE_RANGE_LIB) $(OBJ)
	$(F90) $(F90FLAGS) $(LFRIC_INCLUDE_FLAGS) $(OBJ) -o $(EXEC) \
		  -L$(VALUE_RANGE_DIR) -l$(VALUE_RANGE_NAME)            \
		  -L$(LFRIC_PATH) -l$(LFRIC_NAME) $(LDFLAGS)

$(VALUE_RANGE_LIB): $(LFRIC_LIB)
	$(MAKE) -C $(VALUE_RANGE_DIR)

value_range_process:
	$(MAKE) -C $(VALUE_RANGE_DIR)

# Dependencies
main_alg.f90: main_psy.f90

main_psy.o:	testkern_w0_kernel_mod.o $(VALUE_RANGE_LIB) $(LFRIC_LIB)
main_alg.o:	main_psy.o
testkern_w0_kernel_mod.o: $(LFRIC_LIB)

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

# Keep the generated psy and alg files
.precious: main_psy.f90 main_alg.f90

main_psy.f90:	main.x90
	${PSYCLONE} -api lfric -s ./value_range_check_transformation.py \
	-d . -d $(VALUE_RANGE_DIR)                                      \
	-d $(LFRIC_PATH)                                                \
	-nodm -opsy main_psy.f90 -oalg main_alg.f90 main.x90

allclean: clean
	$(MAKE) -C $(LFRIC_PATH) allclean
	$(MAKE) -C $(VALUE_RANGE_DIR) allclean
	# Remove the value range check object files:
	$(MAKE) -C $(VALUE_RANGE_DIR)/.. allclean
