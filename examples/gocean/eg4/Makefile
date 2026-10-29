# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

include ../../common.mk

# The compiler to use may be specified via the F90 environment
# variable.
#
# export F90=nvfortran
# export F90FLAGS = -Minfo -acc -gpu=cc70
# export LDFLAGS=""
# Currently only compatible with F90=nvfortran

# Location of the infrastructure code (which is a submodule of the
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
	${PSYCLONE} -api "gocean"  -I./ -I${INF_INC} alg_kern_use_var.f90

opencl:
	${PSYCLONE} -api "gocean"  -I./ -I${INF_INC} -s ./ocl_transform.py \
                 alg_kern_use_var.f90

openacc:
	${PSYCLONE} -api "gocean"  -I./ -I${INF_INC} -s ./acc_transform.py \
                 -opsy psy_gen.f90 -oalg alg_gen.f90  alg_kern_use_var.f90

${INF_LIB}:
	${MAKE} -C ${INF_DIR} fd_lib

psy_gen.f90 alg_gen.f90: openacc
alg_gen.o: psy_gen.o

%.o: %.f90
	${F90} ${F90FLAGS} -I${INF_INC} -c $<

compile-acc: ${INF_LIB} ${KERNELS} alg_gen.o psy_gen.o
	${F90} ${F90FLAGS} ${LDFLAGS} -I${INF_INC} \
		${KERNELS} alg_gen.o psy_gen.o \
		${INF_LIB} -o alg_openacc.exe

others:
	# The following currently fail as work is in progress to support transforming
	# kernels that access nested modules or call kernels from modules (TODO #342).
	# ${PSYCLONE} -api "gocean" -I${INF_INC} -s ./acc_transform.py alg_kern_call_kern.f90
	# ${PSYCLONE} -api "gocean" -I${INF_INC} -s ./acc_transform.py alg_kern_nested_use.f90

compile: transform
	@echo "No compilation targets for example gocean/eg4"

run: compile
	@echo "No run targets for example gocean/eg4"

# Override allclean rule to include INF_DIR library
allclean: clean
	${MAKE} -C ${INF_DIR} distclean
