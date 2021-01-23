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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab

include ../../common.mk

FORTRAN_FILES = tridiagonal_solve.f90 if_example.f90 copy_stencil.f90 hori_diff.f90

.PHONY: ${FORTRAN_FILES} intrinsic tra_adv

transform: ${FORTRAN_FILES} intrinsic tra_adv

${FORTRAN_FILES}:
	${PSYCLONE} -s ./sir_trans.py -api nemo $@ -opsy /dev/null

# intrinsic_example.f90 and tracer_advection.f90 make use of a
# different transformation script
intrinsic:
	${PSYCLONE} -s ./sir_trans_all.py -api nemo intrinsic_example.f90 \
-opsy /dev/null
tra_adv:
	${PSYCLONE} -s ./sir_trans_all.py -api nemo tra_adv_compute.F90 \
-opsy /dev/null

# We don't do any compilation for this example
compile: transform
	@echo "No compilation supported for nemo/eg4"

run: compile
	@echo "No run targets for nemo/eg4"
