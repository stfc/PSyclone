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


from fparser.two import Fortran2003
from psyclone.psyir.nodes import Assignment, CodeBlock, Routine
from psyclone.tests.utilities import Compile


def test_basic_associate(fortran_reader, fortran_writer, tmpdir):
    '''Check that a basic associate block is correctly handled by the
    frontend.'''
    code = '''
program test_assoc
  type :: grid_type
    integer :: nx
    real, dimension(10) :: data
  end type
  type(grid_type) :: grid
  real, dimension(10) :: var1
  var1(:) = 10.0
  associate(easy => var1(3), hard => grid%data)
  easy = 5.0
  hard(:) = 0.0
  end associate
end program test_assoc
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
    assert Compile(tmpdir).string_compiles(output)


def test_associate_dependency(fortran_reader):
    '''Check that we get a CodeBlock if our substitution of expressions
    inside the associate construct would change the semantics of the code.'''
    code = '''
program test_assoc
  use grid_mod, only: grid
  real, dimension(10) :: var1
  integer :: i
  var1(:) = 10.0
  i = 1
  associate(hard => grid%data(i))
  ! We write to 'i' here. However, in Fortran that would make no difference
  ! to the fact that 'hard' is equivalent to 'grid%data(1)'. Therefore, we
  ! cannot substitute 'hard' with 'grid%data(i)'.
  i = 2
  hard = 0.0
  end associate
  var1(:) = 5.0
  associate(tricky => grid%data(1:grid%nx))
  grid%nx = 5
  tricky = -1.0
  end associate
end program test_assoc
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert len(routine.children) == 5
    assert isinstance(routine.children[2], CodeBlock)
    assert isinstance(routine.children[4], CodeBlock)


def test_associate_cblock(fortran_reader):
    '''Test that a CodeBlock within an associate construct results in the
    whole construct being put in a CodeBlock (unless the CodeBlock does
    not access any associate names).'''
    code = '''
program test_assoc
  use grid_mod, only: grid
  real, dimension(10) :: var1
  integer :: i
  var1(:) = 10.0
  associate(easy => grid)
  easy%data = 0.0
  ! This write will result in a CodeBlock that does access an associate
  ! name ('easy').
  write(*,*) easy%data
  end associate
  i = 5
  ! A second associate construct that also will contain a CodeBlock (because of
  ! the WRITE) but is fine because the CodeBlock does not access any
  ! associate names.
  associate(trivial => grid)
  trivial%data(1:3) = -1.0
  write (*,*) i
  end associate
end program test_assoc
'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert len(routine.children) == 5
    assert isinstance(routine.children[1], CodeBlock)
    assert isinstance(routine.children[1].get_ast_nodes[0],
                      Fortran2003.Associate_Construct)
    assert isinstance(routine.children[3], Assignment)
    assert isinstance(routine.children[4], CodeBlock)
