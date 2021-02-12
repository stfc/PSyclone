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
# Modified J. Henrichs, Bureau of Meteorology

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
