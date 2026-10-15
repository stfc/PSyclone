# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# Include file for 'top-level' Makefiles found in the directories immediately
# below the one containing this file.
#
# Provides support for 'all', 'compile', 'transform' (the default), 'notebook',
# 'run', clean' and 'allclean' targets for directories listed in EXAMPLES.
# All an including Makefile needs to do is set EXAMPLES appropriately.

run_EXAMPLES=$(addprefix run_,$(EXAMPLES))
compile_EXAMPLES=$(addprefix compile_,$(EXAMPLES))
notebook_EXAMPLES=$(addprefix notebook_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))

transform: ${EXAMPLES}
compile: ${compile_EXAMPLES}
run: ${run_EXAMPLES}
notebook: ${notebook_EXAMPLES}
clean: ${clean_EXAMPLES}
allclean: ${allclean_EXAMPLES}

.PHONY: ${EXAMPLES} $(all_EXAMPLES) ${compile_EXAMPLES} ${clean_EXAMPLES} \
        ${notebook_EXAMPLES} ${allclean_EXAMPLES}

$(EXAMPLES):
	${MAKE} -C $@ transform

$(run_EXAMPLES):
	${MAKE} -C $(patsubst run_%,%,$@) run

$(compile_EXAMPLES):
	${MAKE} -C $(patsubst compile_%,%,$@) compile

$(notebook_EXAMPLES):
	${MAKE} -C $(patsubst notebook_%,%,$@) notebook

$(clean_EXAMPLES):
	${MAKE} -C $(patsubst clean_%,%,$@) clean

$(allclean_EXAMPLES):
	${MAKE} -C $(patsubst allclean_%,%,$@) allclean
