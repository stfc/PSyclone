# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../common.mk

# We need to add the default target for this Makefile before the
# include statement (which will add 'test' as default).
transform:
	$(PSYCLONE) -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

# For PSyclone CI:
test:
	$(MAKE) transform 2>&1 | grep "Kernel file 'summation_w0_to_w3_kernel_mod\."\
	"\[fF\]90' not found in" >/dev/null

test_run: test

# Not actually required, but to avoid errors when cleaning top level
clean:
allclean:
