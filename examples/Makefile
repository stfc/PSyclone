# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

.DEFAULT_GOAL := transform

.PHONY: transform run compile notebook clean allclean

transform compile run notebook clean allclean:
	${MAKE} -C gocean $@
	${MAKE} -C nemo $@
	${MAKE} -C lfric $@
	${MAKE} -C psyir $@
	${MAKE} -C psyad $@
	${MAKE} -C stub_generation $@
