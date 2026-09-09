# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Include file for 'top-level' Makefiles found in the directories immediately
# below the one containing this file.
#
# Provides support for 'all', 'compile', 'transform' (the default),
# 'run', clean' and 'allclean' targets for directories listed in TUTORIALS.
# All an including Makefile needs to do is set TUTORIALS appropriately.

run_TUTORIALS=$(addprefix run_,$(TUTORIALS))
compile_TUTORIALS=$(addprefix compile_,$(TUTORIALS))
notebook_TUTORIALS=$(addprefix notebook_,$(TUTORIALS))
clean_TUTORIALS=$(addprefix clean_,$(TUTORIALS))
allclean_TUTORIALS=$(addprefix allclean_,$(TUTORIALS))

run: ${run_TUTORIALS}
compile: ${compile_TUTORIALS}
transform: ${TUTORIALS}
clean: ${clean_TUTORIALS}
allclean: ${allclean_TUTORIALS}

.PHONY: ${TUTORIALS} $(all_TUTORIALS) ${compile_TUTORIALS} ${clean_TUTORIALS} \
        ${allclean_TUTORIALS}

$(TUTORIALS):
	${MAKE} -C $@ transform

$(run_TUTORIALS):
	${MAKE} -C $(patsubst run_%,%,$@) run

$(compile_TUTORIALS):
	${MAKE} -C $(patsubst compile_%,%,$@) compile

$(clean_TUTORIALS):
	${MAKE} -C $(patsubst clean_%,%,$@) clean

$(allclean_TUTORIALS):
	${MAKE} -C $(patsubst allclean_%,%,$@) allclean
