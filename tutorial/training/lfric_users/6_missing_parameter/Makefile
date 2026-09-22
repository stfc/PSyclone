# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# We need to add the default target for this Makefile before the
# include statement (which will add 'test' as default).
transform:
	$(PSYCLONE) -nodm -l output -opsy main_alg_psy.f90 -oalg main_alg.f90 main_alg.x90

include ../common.mk

test:
	$(MAKE) transform 2>&1 | grep "Parse Error: Kernel 'summation_w0_to_w3_kernel_type' "\
	"called from the algorithm layer with an insufficient number of arguments as specified "\
	"by the metadata. Expected at least '2' but found '1'" >/dev/null

test_run: test

# Not actually required, but to avoid errors when cleaning top level
clean:
allclean:
