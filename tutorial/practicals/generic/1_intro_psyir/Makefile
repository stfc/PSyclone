# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../../common.mk

.PHONY: transform

transform:
	$(PSYCLONE) -s ./view_trans.py -o /dev/null tra_adv.F90

compile: transform
	@echo "No compilation targets for this example"

run: compile
	@echo "No run targets for this example"

# (output.f90 is created when following the tutorial instructions)
clean:
	rm -f output.f90 *.o *.mod *~

allclean: clean
	rm -f *.exe output.dat
