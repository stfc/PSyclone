# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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

include ../../common.mk

.PHONY: vanilla inline loopfuse

transform: vanilla inline loopfuse

# Location of the stripped-down LFRic infrastructure source
LFRIC_INC_DIR = ../../../src/psyclone/tests/test_files/lfric/infrastructure

# The '-d ../code' argument specifies that PSyclone should search the
# '../code' directory when looking for user-supplied kernels.

# The '-s my_script.py' argument specifies that PSyclone should apply
# the transformation script 'my_script.py.'

vanilla:
	${PSYCLONE} -api lfric -nodm -d ../code -I${LFRIC_INC_DIR} -s ./print_psyir_trans.py ./multi_invoke_mod.x90

inline:
	${PSYCLONE} -api lfric -nodm -d ../code -I${LFRIC_INC_DIR} -s ./module_inline_trans.py ./multi_invoke_mod.x90

loopfuse:
	${PSYCLONE} -api lfric -nodm -d ../code -I${LFRIC_INC_DIR} -s ./loop_fuse_trans.py ./multi_invoke_mod.x90

compile: transform
	@echo "No compilation supported for lfric/eg2"

run: compile
	@echo "No run targets for lfric/eg2"
