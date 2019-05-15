# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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

# Makefile for the 3rd NEMO example. Uses PSyclone (which must
# be installed) to generate Fortran with OpenACC directives for
# the tracer-advection benchmark. Depends upon the dl_timer
# library (bitbucket.org/apeg/dl_timer) which must be installed
# and the location specified in SHARED_DIR below.
#
# The compiler to use must be specified via the F90 environment
# variable. e.g. to use PGI and OpenACC:
#
# export F90=pgf90
# export F90FLAGS="-O1 -acc -ta=tesla,cc70 -Minfo=all"
# export LDFLAGS="-acc -ta=tesla,cc70"

# Location of the dl_timer library
SHARED_DIR = /home/aporter/MyInstalls
TIMER_DIR = ${SHARED_DIR}/dl_timer
TIMER_INC = ${TIMER_DIR}/src
TIMER_LIB = ${TIMER_DIR}/dl_timer_lib.a

.PHONY: all clean allclean dl_timer

all: traadv.exe

traadv.exe: dl_timer psy.o
	${F90} ${LDFLAGS} psy.o ${TIMER_LIB} -o traadv.exe

dl_timer:
	make -C ${TIMER_DIR} sm_lib

psy.f90:
	psyclone -l -api "nemo" -s ./kernels_trans.py -opsy psy.f90 ../code/tra_adv.F90

%.o: %.f90
	$(F90) $(F90FLAGS) -I${TIMER_INC} -c $<

clean:
	rm -f *.o *.mod *~

# Delete generated Fortran and exe
allclean: clean
	rm -f psy.f90 traadv.exe
	make -C ${TIMER_DIR} allclean
