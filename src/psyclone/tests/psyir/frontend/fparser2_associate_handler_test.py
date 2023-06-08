# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab


''' Performs pytest tests for the support for ASSOCIATE blocks in the
    fparser2 PSyIR front-end. '''


from psyclone.psyir.nodes import Assignment, Routine


def test_basic_associate(fortran_reader, fortran_writer):
    '''Check that a basic associate block is correctly handled by the
    frontend.'''
    code = '''
program test_alloc
  use grid_mod, only: grid
  integer, parameter :: ndof = 8
  real, dimension(ndof) :: var1
  var1(:) = 10.0
  associate(easy => var1(3), hard => grid%data)
  easy = 5.0
  hard(:) = 0.0
  end associate
end program test_alloc
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert len(routine.children) == 3
    assert isinstance(routine.children[1], Assignment)
    assert "easy" not in routine.symbol_table
    assert "hard" not in routine.symbol_table
    assert routine.children[1].lhs.symbol.name == "var1"
    output = fortran_writer(psyir)
    assert ("  var1(:) = 10.0\n"
            "  var1(3) = 5.0\n"
            "  grid%data(:) = 0.0\n" in output)
