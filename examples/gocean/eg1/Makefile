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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified J. Henrichs, Bureau of Meteorology

include ../../common.mk

GENERATED_FILES += invoke_0_dag invoke_0_dag.png *.cl ompt_alg.f90 ompt_psy.f90
ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg

KERNELS=compute_cu_mod.o compute_cv_mod.o compute_z_mod.o compute_h_mod.o \
		compute_unew_mod.o compute_vnew_mod.o compute_pnew_mod.o time_smooth_mod.o \
		infrastructure_mod.o

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= ../../../external
INF_DIR ?= ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/lib_fd.a

.PHONY: basic openmp mpi loop_fuse dag openacc opencl openmp_taskloop

transform: basic openmp mpi loop_fuse dag openacc opencl openmp_taskloop

# The dl_esm_inf library
fd_lib:
	${MAKE} -C ${INF_DIR} F90=${F90} fd_lib

dm_fd_lib:
	${MAKE} -C ${INF_DIR} F90=${F90} dm_fd_lib

ompt_psy.f90 ompt_alg.f90: shallow_alg.f90
	${PSYCLONE} -nodm -s ./openmp_taskloop_trans.py -api gocean1.0 \
		-opsy ompt_psy.f90 -oalg ompt_alg.f90 shallow_alg.f90

openmp_taskloop: ompt_psy.f90 ompt_alg.f90

# TODO: Cannot compile this example until #1474 is resolved.
#compile-openmptaskloop: ompt_psy.f90 ompt_alg.f90 ${KERNELS} fd_lib
#	${F90} ${LDFLAGS} -I${INF_INC} ompt_psy.f90 ompt_alg.f90 ${KERNELS} \
	${INF_LIB} -o shallow_ompt.exe

compile: transform
	@echo "No compile targets for example gocean/eg1, see #1474"

%.o: %.f90
	${F90} ${LDFLAGS} -I${INF_INC} -c $<

run: compile
	@echo "No run targets for example gocean/eg1"

basic:
	$(ENV) ${PYTHON} ./runme.py

openmp:
	$(ENV) ${PYTHON} ./runme_openmp.py


mpi:
	${PSYCLONE} -api gocean1.0 shallow_alg.f90

loop_fuse:
	$(ENV) ${PYTHON} ./runme_loop_fuse.py

dag:
	$(ENV) ${PYTHON} ./runme_dag.py

openacc:
	$(ENV) ${PYTHON} ./runme_openacc.py

# The "--kernel-renaming single" parameter avoids generating duplicate
# versions of OpenCL kernels called multiple times.
opencl:
	${PSYCLONE} -nodm -s ./opencl_transformation.py --kernel-renaming single \
		-api gocean1.0 shallow_alg.f90
