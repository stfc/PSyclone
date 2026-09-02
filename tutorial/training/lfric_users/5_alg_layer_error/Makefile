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
	$(MAKE) transform 2>&1 | grep "kernel call 'no_setval_c' must either "\
	"be named in a use statement" >/dev/null

test_run: test

# Not actually required, but to avoid errors when cleaning top level
clean:
allclean:
