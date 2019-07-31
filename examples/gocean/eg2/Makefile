# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council
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

# Makefile for the 2nd GOcean example. Uses PSyclone (which must
# be installed) to generate Fortran with OpenACC directives for
# a single-kernel example. The `profile` target builds the same
# example but instruments it for profiling with the NVIDIA NVTX
# library (via the PSyclone profiling API). The location of this
# library can be supplied via the NVTX_DIR variable, e.g.:
#
#     make NVTX_DIR=/apps/packages/cuda/10.0/lib64 profile
#
# The compiler to use must be specified via the F90 environment
# variable. e.g. to use PGI and OpenACC:
#
# export F90=pgf90
# export F90FLAGS="-O1 -acc -ta=tesla,cc70 -Minfo=all"
# export LDFLAGS="-acc -ta=tesla,cc70"

# Location of the infrastucture code (which is a submodule of the
# PSyclone git repo).
SHARED_DIR = ../../../external
INF_DIR = ${SHARED_DIR}/dl_esm_inf/finite_difference
INF_INC = ${INF_DIR}/src
INF_LIB = ${INF_DIR}/src/lib_fd.a

# Location of the PSyclone profiling wrapper
PROFILE_DIR = ../../../lib/profiling/nvidia
PROFILE_LIB = ${PROFILE_DIR}/libnvtx_prof.a

# Location of the NVTX library
NVTX_DIR ?= /usr/local/lib64/cuda10.0
NVTX_LINK = -L${NVTX_DIR} -lnvToolsExt

.PHONY: all kernels profile clean allclean

all: single.exe

# Target to make the same application but instrumented for profiling
profile: single_prof.exe

# The dl_esm_inf library
${INF_LIB}:
	${MAKE} -C ${INF_DIR}

# The profiling wrapper library
${PROFILE_LIB}:
	${MAKE} -C ${PROFILE_DIR}

# Listing psy.f90 before kernels ensures that PSyclone is run to generate
# the transformed kernels.
single.exe: ${INF_LIB} psy.f90 kernels psy.o alg_gen.o
	${F90} ${LDFLAGS} inc_field_*mod.o psy.o alg_gen.o ${INF_LIB} -o single.exe

# Listing psy_prof.f90 before kernels ensures that PSyclone is run to generate
# the transformed kernels.
single_prof.exe: ${INF_LIB} ${PROFILE_LIB} psy_prof.f90 kernels psy_prof.o alg_prof.o
	${F90} ${LDFLAGS} inc_field_*mod.o psy_prof.o alg_prof.o ${INF_LIB} \
${PROFILE_LIB} ${NVTX_LINK} -o single.exe

psy.f90 alg_gen.f90: alg.f90
	psyclone -l -api "gocean1.0" -s ./acc_transform.py -opsy psy.f90 \
-oalg alg_gen.f90 alg.f90

psy_prof.f90 alg_prof.f90: alg.f90
	psyclone -l -api "gocean1.0" -s ./acc_prof_transform.py -opsy psy_prof.f90 \
-oalg alg_prof.f90 alg.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -I${INF_INC} -I${PROFILE_DIR} -c $<

# Gnu Make caches the contents of the current working directory at
# start-up. Therefore wildcards do not pick-up any files generated
# during execution.
kernels:
	${MAKE} inc_field_mod.o inc_field_0_mod.o

clean:
	rm -f *.o *.mod *~

# Delete generated Fortran and exe
allclean: clean
	rm -f inc_field_*_mod.f90
	rm -f psy*.f90 alg_gen.f90 alg_prof.f90 single*.exe
