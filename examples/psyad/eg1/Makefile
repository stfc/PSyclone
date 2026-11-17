# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg
GENERATED_FILES += testkernadj_mod.f90 testkern*.o testkern*.mod \
                   test_harness.*

# The first command will write the adjoint kernel to stdout. The second will
# write both it and the corresponding test harness to separate files.
transform:
	$(PSYAD) testkern_mod.f90 -a field1 field2 field3
	$(PSYAD) -t -otest test_harness.f90 -oad testkernadj_mod.f90 \
                 testkern_mod.f90 -a field1 field2 field3

compile: transform
	$(F90) $(F90FLAGS) -c testkern_mod.f90
	$(F90) $(F90FLAGS) -c testkernadj_mod.f90
	$(F90) $(F90FLAGS) -c test_harness.f90
	$(F90) $(F90FLAGS) -o test_harness.exe testkern_mod.o \
                  testkernadj_mod.o test_harness.o

run: compile
	@echo ""
	@echo "Running PSyAD-generated test harness..."
	@./test_harness.exe
	@echo "...done."

include ../../top_level.mk
