
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the handling of pointers in the fparser2
    PSyIR front-end. '''

from psyclone.psyir.nodes import CodeBlock, Assignment


def test_pointer_assignments(fortran_reader):
    '''
    Test that pointer assignments are parsed as Assignment with the is_pointer
    attributes set to True. Also when accessing derived types no CodeBlocks
    must be produced.
    '''
    test_module = '''
    subroutine mysub()
        use other_symbols
        integer, target :: a = 1
        integer, pointer :: b => null()

        b => a
        field(3,c)%pointer => b
    end subroutine
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    assert not file_container.walk(CodeBlock)
    assignments = file_container.walk(Assignment)
    assert len(assignments) == 2
    for assignment in assignments:
        assert assignment.is_pointer is True


def test_unsupported_pointer_assignments(fortran_reader):
    '''
    Test that pointer assignments that have an array-accessor syntax
    on the inner element are not supported.
    '''
    test_module = '''
    subroutine mysub()
        use other_symbols

        array(3:) => ptr
        field(3,c)%array_of_pointer(1:) => ptr
        field(3,c)%array_of_pointer(1:3) => ptr
    end subroutine
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    assert file_container.walk(CodeBlock)
    assert not file_container.walk(Assignment)
