# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified: R. W. Ford, STFC Daresbury Laboratory

include ../../common.mk

CONFIG_ENV = PSYCLONE_CONFIG=${PSYCLONE_DIR}/config/psyclone.cfg
GENERATED_FILES += testkernadj_mod.f90 testkern*.o testkern*.mod \
                   test_harness.*

# The first command will write the adjoint kernel to stdout. The second will
# write both it and the corresponding test harness to separate files.
transform:
	$(PSYAD) testkern_mod.f90 -a field1 field2 field3
	$(PSYAD) -t -otest test_harness.f90 -oad testkernadj_mod.f90 \
                 testkern_mod.f90 -a field1 field2 field3

compile: transform
	$(F90) $(F90FLAGS) -c testkern_mod.f90
	$(F90) $(F90FLAGS) -c testkernadj_mod.f90
	$(F90) $(F90FLAGS) -c test_harness.f90
	$(F90) $(F90FLAGS) -o test_harness.exe testkern_mod.o \
                  testkernadj_mod.o test_harness.o

run: compile
	@echo ""
	@echo "Running PSyAD-generated test harness..."
	@./test_harness.exe
	@echo "...done."

include ../../top_level.mk
