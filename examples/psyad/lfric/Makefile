# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../top_level.mk

transform:
	${MAKE} -C tangent_linear transform
	${MAKE} -C tangent_linear_tweaked transform
compile:
	${MAKE} -C tangent_linear compile
	${MAKE} -C tangent_linear_tweaked compile
run:
	${MAKE} -C tangent_linear run
	${MAKE} -C tangent_linear_tweaked run
clean:
	${MAKE} -C tangent_linear clean
	${MAKE} -C tangent_linear_tweaked clean
allclean:
	${MAKE} -C tangent_linear allclean
	${MAKE} -C tangent_linear_tweaked allclean
