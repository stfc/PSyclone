# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

.DEFAULT_GOAL := transform

ALL_TARGETS = transform compile run clean allclean
.PHONY: $(ALL_TARGETS)

$(ALL_TARGETS):
	$(MAKE) -C LFRic $@
	$(MAKE) -C generic $@
