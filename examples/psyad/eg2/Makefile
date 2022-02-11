# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory

include ../../common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg
GENERATED_FILES += 

# The first command will write the adjoint kernel to stdout. The
# second, if supported, would write both it and the corresponding test
# harness to separate files. Note, one of the variables provided in
# the psyad command line below (res_dot_product) did not exist in the
# original code and is a by-product of psyad replacing a dot_product
# intrinsic with equivalent code. This and all other local variables
# will not need to be specified on the command line once issue #1556
# has been addressed.
transform:
	$(PSYAD) tl_hydrostatic_kernel_mod.F90 -a r_u exner theta moist_dyn_gas moist_dyn_tot moist_dyn_fac grad_term theta_v_e theta_v_at_quad grad_theta_v_at_quad exner_e exner_at_quad res_dot_product
	@echo "Harness generation not yet supported"

# It is not yet possible to compile the adjoint code into an object
# file and run it, as the metadata is incorrect (see issue #1453) and
# the harness generation does not understand all of the argument
# datatypes and how to initialise them (see issue #1555).
compile: transform
	@echo "Harness compilation not yet supported"

run: compile
	@echo "Harness running not yet supported"
