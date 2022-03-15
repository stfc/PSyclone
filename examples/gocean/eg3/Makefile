# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Author: A. R. Porter and S. Siso, STFC Daresbury Laboratory
# Modified J. Henrichs, Bureau of Meteorology

include ../../common.mk

KERNELS=compute_cu_mod.o compute_cv_mod.o compute_z_mod.o compute_h_mod.o

GENERATED_FILES += opencl_kernels_*.cl psy_gen.f90 alg_gen.f90 ${KERNELS} \
	${KERNELS:.o=.mod} alg_opencl.exe allkernels.cl alg_gen_dm.f90 \
	psy_gen_dm.f90 *.exe *.o

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= ../../../external
INF_DIR ?= ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/lib_fd.a
FORTCL_DIR ?= ${SHARED_DIR}/FortCL
FORTCL_INC = ${FORTCL_DIR}/src
FORTCL_LIB = ${FORTCL_DIR}/src/libFortCL.a

transform:
	${PSYCLONE} -api "gocean1.0" -s ./ocl_trans.py alg.f90

# The dl_esm_inf library
fd_lib:
	${MAKE} -C ${INF_DIR} F90=${F90} fd_lib

dm_fd_lib:
	${MAKE} -C ${INF_DIR} F90=${F90} dm_fd_lib

# The FortCL library
${FORTCL_LIB}:
	${MAKE} -C ${FORTCL_DIR} F90=${F90}


# PSyclone command that stores the output code into files
psy_gen.f90 alg_gen.f90: alg.f90
	${PSYCLONE} -nodm -api "gocean1.0" -s ./ocl_trans.py -opsy psy_gen.f90 \
		 -oalg alg_gen.f90 alg.f90

psy_gen_dm.f90 alg_gen_dm.f90: alg.f90
	${PSYCLONE} -dm -api "gocean1.0" -s ./ocl_trans.py -opsy psy_gen_dm.f90 \
		 -oalg alg_gen_dm.f90 alg.f90

compile-ocl: fd_lib ${FORTCL_LIB} ${KERNELS} psy_gen.o alg_gen.f90
	${F90} ${LDFLAGS} -I${INF_INC} alg_gen.f90 psy_gen.o ${INF_LIB} ${FORTCL_LIB} \
		-lOpenCL -o alg_opencl.exe
	@echo "OpenCL compilation complete!"
	@echo "Use 'FORTCL_KERNELS_FILE=opencl_kernels_0.cl ./alg_opencl.exe' to run."

compile-mpi-ocl: dm_fd_lib ${FORTCL_LIB} ${KERNELS} psy_gen_dm.o alg_gen_dm.f90
	# TODO dl_esm_inf/#54: The generated binary will not execute properly until
	# this issue is fixed.
	${F90} ${LDFLAGS} -I${INF_INC} alg_gen_dm.f90 psy_gen_dm.o ${INF_LIB} \
		${FORTCL_LIB} -lOpenCL -o alg_dm_opencl.exe
	@echo "OpenCL compilation complete!"
	@echo "Use 'FORTCL_KERNELS_FILE=opencl_kernels_0.cl mpirun -n <NUM_RANKS>" \
	 "./alg_dm_opencl.exe' to run."

%.o: %.f90
	${F90} ${LDFLAGS} -I${INF_INC} -I${FORTCL_INC} -c $<

compile: compile-ocl

run: compile
	@echo "No run targets for example gocean/eg3"

# Override allclean rule to include INF_DIR library
allclean: clean
	${MAKE} -C ${INF_DIR} distclean
	${MAKE} -C ${FORTCL_DIR} allclean
