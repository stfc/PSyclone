# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council
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
# Author: M. Naylor, University of Cambridge, UK

F90 ?= gfortran
F90FLAGS ?= -O2 -fopenmp

.PHONY: all
all: matmul matmul_tiled matmul_omp matmul_omp_tiled
	./matmul
	./matmul_tiled
	./matmul_omp
	./matmul_omp_tiled

.PHONY: clean
clean:
	rm -f matmul matmul_tiled matmul_omp matmul_omp_tiled
	rm -f matmul_tiled.F90 matmul_omp.F90 matmul_omp_tiled.F90

matmul_tiled.F90: matmul.F90 tile.py
	psyclone -s tile.py matmul.F90 -o matmul_tiled.F90

matmul_omp.F90: matmul.F90 omp.py
	psyclone -s omp.py matmul.F90 -o matmul_omp.F90

matmul_omp_tiled.F90: matmul.F90 omp-tile.py
	psyclone -s omp-tile.py matmul.F90 -o matmul_omp_tiled.F90

%: %.F90
	$(F90) $(F90FLAGS) $< -o $@
