# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, STFC Daresbury Lab,
#             I. Kavcic, Met Office

# ----------- Default "make" values, can be overwritten by the user -----------
# Compiler and compiler flags
F90 ?= gfortran
F90FLAGS ?= -g
# -----------------------------------------------------------------------------

# We don't build dl_timer, drhook, nvidia, Vernier, and tau by default
# since they require external libraries to be available.
ALL_LIBS = lfric_timer simple_timing template

.PHONY: default all $(ALL_LIBS) clean allclean \
	dl_timer drhook nvidia tau vernier

default: all

all: $(ALL_LIBS)

# Invoke make in the corresponding subdirectory, and also support
# the wrapper that need additional software to be installed
$(ALL_LIBS) dl_timer drhook nvidia tau vernier:
		$(MAKE) -C $@

clean:
	$(foreach lib, $(ALL_LIBS), $(MAKE) -C $(lib) allclean; )

allclean: clean
	# These libs are not cleaned by 'clean' (since they
	# depend on external libraries)
	$(MAKE) -C dl_timer clean
	$(MAKE) -C drhook clean
	$(MAKE) -C nvidia clean
	$(MAKE) -C tau clean
	$(MAKE) -C vernier clean
