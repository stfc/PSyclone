# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Laboratory

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
