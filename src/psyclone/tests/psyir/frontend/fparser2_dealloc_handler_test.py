# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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


''' Performs pytest tests on the support for deallocate statements in the
    fparser2 PSyIR front-end. '''


from psyclone.psyir.nodes import IntrinsicCall, Reference, StructureReference


def test_deallocate_handler(fortran_reader):
    '''Check that a various forms of deallocate are correctly captured by
    the frontend.

    '''
    code = '''
program test_dealloc
  use some_mod, only: my_var
  implicit none
  integer :: ierr
  real, allocatable, dimension(:, :) :: var1, var2, var3
  deallocate(var1)
  deallocate(var2, var3, stat=ierr)
  deallocate(my_var%data)
end program test_dealloc
'''
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(IntrinsicCall)
    assert len(calls) == 3
    call = calls[0]
    assert len(call.children) == 1
    assert isinstance(call.children[0], Reference)
    assert call.children[0].symbol.name == "var1"
    call = calls[1]
    assert call.argument_names == [None, None, "STAT"]
    assert call.children[1].symbol.name == "var3"
    call = calls[2]
    assert isinstance(call.children[0], StructureReference)
