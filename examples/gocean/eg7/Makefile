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

include ../../common.mk

GENERATED_FILES += alg.f90 psy.f90 *.o *.mod

DL_DIR = ../../../external/dl_esm_inf/finite_difference/src
READ_ONLY_DIR = ../../../lib/read_only/dl_esm_inf
F90FLAGS += -I$(DL_DIR)
LIB_NAME = lib_read_only.a


# The two kernels used in the application.
KERNELS = init_field_mod.o update_field_mod.o

# The name of the executable
EXEC = read_only_test


all: transform compile

transform: alg.f90

compile: $(KERNELS) alg.o psy.o $(READ_ONLY_DIR)/$(LIB_NAME)
	$(F90)  $(KERNELS) alg.o psy.o -o $(EXEC)                   \
	-L$(READ_ONLY_DIR) -l_read_only \
	-L $(DL_DIR) -l_fd
	

alg.f90 psy.f90: test.x90 read_only_transform.py
	psyclone -nodm -l -api "gocean1.0" -s ./read_only_transform.py\
			 --config ../../../config/psyclone.cfg \
		     -opsy psy.f90 -oalg alg.f90 test.x90

alg.o:	psy.o 

psy.o:	$(KERNELS)

%.o: %.f90
	$(F90) $(F90FLAGS) -I $(DL_DIR) -I $(READ_ONLY_DIR) -c $<
