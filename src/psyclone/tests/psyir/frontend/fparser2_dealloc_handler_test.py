# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


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
    assert len(call.arguments) == 1
    assert isinstance(call.children[0], Reference)
    assert call.arguments[0].symbol.name == "var1"
    call = calls[1]
    assert call.argument_names == [None, None, "STAT"]
    assert call.arguments[1].symbol.name == "var3"
    call = calls[2]
    assert isinstance(call.arguments[0], StructureReference)
