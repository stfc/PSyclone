# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Makefile to test the example transformation scripts contained in
# this directory.

.PHONY: clean transform

transform:
	${MAKE} -C ../ output_1.f90
	${MAKE} TRANS_SCRIPT=solutions/kernels_trans.py -C ../ output_2.f90
	${MAKE} TRANS_SCRIPT=solutions/collapse_trans.py -C ../ output_openacc.f90

clean:
	${MAKE} -C ../ clean
