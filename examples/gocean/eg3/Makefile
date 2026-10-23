# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

# The compiler to use may be specified via the F90 environment
# variable.
#
# export F90=mpifc
# export F90FLAGS="-O1"
# export LDFLAGS="-lOpenCL"

LDFLAGS ?= -lOpenCL
include ../../common.mk

KERNELS=compute_cu_mod.o compute_cv_mod.o compute_z_mod.o compute_h_mod.o

GENERATED_FILES += opencl_kernels_*.cl psy_gen.f90 alg_gen.f90 ${KERNELS} \
	${KERNELS:.o=.mod} alg_opencl.exe allkernels.cl alg_gen_dm.f90 \
	psy_gen_dm.f90 *.exe *.o

# Location of the infrastructure code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR ?= ../../../external
INF_DIR ?= ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/lib_fd.a
FORTCL_DIR ?= ${SHARED_DIR}/FortCL
FORTCL_INC = ${FORTCL_DIR}/src
FORTCL_LIB = ${FORTCL_DIR}/src/libFortCL.a

transform:
	${PSYCLONE} -api "gocean"  -I${INF_INC} -s ./ocl_trans.py alg.f90

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
	${PSYCLONE} -nodm -api "gocean" -s ./ocl_trans.py -opsy psy_gen.f90 \
		  -I${INF_INC} -oalg alg_gen.f90 alg.f90

psy_gen_dm.f90 alg_gen_dm.f90: alg.f90
	${PSYCLONE} -dm -api "gocean" -s ./ocl_trans.py -opsy psy_gen_dm.f90 \
		  -I${INF_INC} -oalg alg_gen_dm.f90 alg.f90

compile-ocl: fd_lib ${FORTCL_LIB} ${KERNELS} psy_gen.o alg_gen.f90
	${F90} ${F90FLAGS} -I${INF_INC} alg_gen.f90 psy_gen.o ${INF_LIB} ${FORTCL_LIB} \
		${LDFLAGS} -o alg_opencl.exe
	@echo "OpenCL compilation complete!"
	@echo "Use 'FORTCL_KERNELS_FILE=opencl_kernels_0.cl ./alg_opencl.exe' to run."

compile-mpi-ocl: dm_fd_lib ${FORTCL_LIB} ${KERNELS} psy_gen_dm.o alg_gen_dm.f90
	# TODO dl_esm_inf/#54: The generated binary will not execute properly until
	# this issue is fixed.
	${F90} ${F90FLAGS} -I${INF_INC} alg_gen_dm.f90 psy_gen_dm.o ${INF_LIB} \
		${FORTCL_LIB} ${LDFLAGS} -o alg_dm_opencl.exe
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
