# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology


# We want to avoid that a user accidentally starts this Makefile, since then
# unexpected transformations might be applied. So we disable the default
# target here and ask the user to select a different Makefile.
default:
	@echo "Please use one of the transformation-specific Makefiles, e.g."
	@echo "make -f Makefile.extract_one"

PSYDATA_PATH?=$(PSYCLONE_RELPATH)/lib/nan_test/lfric
PSYDATA_LIB_NAME?=_nan_test

# Target for testing the tutorials with compilation (use only one PSyData wrapper)
compile:
	make -f solutions/Makefile.nan_all

run: compile
	./time_evolution

# Target for CI, only do the transformation (use all transformation scripts)
transform:
	make clean
	make -f solutions/Makefile.extract_one time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.extract_all time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.nan_all time_evolution_alg_mod_psy.f90
	make clean
	make -f solutions/Makefile.readonly_all time_evolution_alg_mod_psy.f90

# This will define a 'transform' target, that tutorial checking will use
include Makefile.inc
