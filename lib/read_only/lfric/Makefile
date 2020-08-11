#------------------------------------------------------------------------------
# BSD 2-Clause License
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
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#------------------------------------------------------------------------------
# Author: J. Henrichs, Australian Bureau of Meteorology


# This Makefile picks up the compiler to use plus any flags from
# environment variables. e.g.:
# 
# export F90=mpif90
# export F90FLAGS="-O3"
# export AR=ar

#TODO: #813 keeps track of adding enough files from LFRic to PSyclone to be
#      able to compile this library without the need of external
#      files (or #745, which will try to create a stand-alone example).

# This directory points to an external, compiled version of the LFRic,
# which is required to find the module files when compiling this library:
LFRIC_DIR ?= ../../../../../lfric/work/trunk/miniapps/gravity_wave/working/field
PROCESS_ARGS = -prefix=read_only_verify_ -types=real,int,double \
		       -dims=1,2,3,4
F90 ?= gfortran
F90FLAGS ?= -O0 -g
F90FLAGS += -I$(LFRIC_DIR)
LIB_NAME = lib_read_only.a

OBJS = read_only.o psy_data_base.o read_only_base.o

default: $(LIB_NAME)

$(LIB_NAME): $(OBJS)
	${AR} ${ARFLAGS} ${LIB_NAME} $^

# Give a useful error message if field_mod cannot be found
$(LFRIC_DIR)/field_mod.mod:
	$(error "Can not find field_mod.mod in $(LFRIC_DIR).")

read_only.o: $(LFRIC_DIR)/field_mod.mod read_only.f90 \
	psy_data_base.o read_only_base.f90

read_only.o: read_only_base.o

%.f90:	%.jinja
	../../process.py $(PROCESS_ARGS) $< > $*.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

psy_data_base.f90: ../../psy_data_base.jinja
	../../process.py $(PROCESS_ARGS) $< > psy_data_base.f90

read_only_base.f90: ../read_only_base.jinja
	../../process.py $(PROCESS_ARGS) $< > read_only_base.f90

clean:
	rm -f *.o *.mod $(LIB_NAME) psy_data_base.* read_only_base.*
