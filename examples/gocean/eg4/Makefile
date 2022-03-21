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
# -----------------------------------------------------------------------------
# Authors: A. R. Porter and S. Siso, STFC Daresbury Laboratory
# Modified J. Henrichs, Bureau of Meteorology

include ../../common.mk

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= ../../../external
INF_DIR ?= ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/lib_fd.a

GENERATED_FILES += *.cl kern_use_var_*_mod.f90 psy_gen.f90 \
	alg_gen.f90 *.o *.mod *.exe
KERNELS=data_mod.o kern_use_var_mod.o

.PHONY: basic opencl openacc others compile-acc


transform: basic opencl openacc others

basic:
	${PSYCLONE} -api "gocean1.0" alg_kern_use_var.f90

opencl:
	${PSYCLONE} -api "gocean1.0" -s ./ocl_transform.py alg_kern_use_var.f90

openacc:
	${PSYCLONE} -api "gocean1.0" -s ./acc_transform.py -opsy psy_gen.f90 \
		 -oalg alg_gen.f90  alg_kern_use_var.f90

${INF_LIB}:
	${MAKE} -C ${INF_DIR} fd_lib

psy_gen.f90 alg_gen.f90: openacc

%.o: %.f90
	# Currently only compatible with F90=pgf90
	${F90} -Minfo -acc -ta=tesla:cc70 ${LDFLAGS} -I${INF_INC} -c $<

compile-acc: ${INF_LIB} alg_gen.f90 ${KERNELS} psy_gen.o
	${F90} -acc ${LDFLAGS} -I${INF_INC} alg_gen.f90 psy_gen.o ${INF_LIB} -o alg_openacc.exe

others:
	# The following currently fail as work is in progress to support transforming
	# kernels that access nested modules or call kernels from modules (TODO #342).
	# ${PSYCLONE} -api "gocean1.0" -s ./acc_transform.py alg_kern_call_kern.f90
	# ${PSYCLONE} -api "gocean1.0" -s ./acc_transform.py alg_kern_nested_use.f90

compile: transform
	@echo "No compilation targets for example gocean/eg4"

run: compile
	@echo "No run targets for example gocean/eg4"

# Override allclean rule to include INF_DIR library
allclean: clean
	${MAKE} -C ${INF_DIR} distclean
