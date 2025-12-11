# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified A. B. G. Chalk, STFC Daresbury La

'''Module containing pytest tests for the _program_handler method in
the class Fparser2Reader. This handler deals with the translation of
the fparser2 Program construct to PSyIR.'''

import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import CodeBlock, FileContainer
from psyclone.psyir.symbols import RoutineSymbol


def test_empty_program_handler(parser):
    '''Test that an empty program handler still returns a FileContainer
    with no contents.
    '''
    code = ""
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert psyir.parent is None
    assert len(psyir.children) == 0


def test_program_handler(parser):
    '''Test that program handler works with multiple program units of
    different types.

    '''
    code = (
        "module a\n"
        "end module\n"
        "program b\n"
        "end program b\n"
        "subroutine c\n"
        "end subroutine")
    expected = (
        "module a\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module a\n"
        "program b\n\n\n"
        "end program b\n"
        "subroutine c()\n\n\n"
        "end subroutine c\n")
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, FileContainer)
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected


def test_program_handler_no_module(parser):
    ''' Test that the FileContainer contains the relevant RoutineSymbol
    nodes when we have no containing module.'''
    code = '''
    subroutine a()
    end subroutine
    real function b(val)
       real :: val
       b = val
    end function
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert psyir.parent is None
    assert len(psyir.symbol_table.symbols) == 2
    assert isinstance(psyir.symbol_table.symbols[0], RoutineSymbol)
    assert psyir.symbol_table.symbols[0].name == 'a'
    assert isinstance(psyir.symbol_table.symbols[1], RoutineSymbol)
    assert psyir.symbol_table.symbols[1].name == 'b'


def test_program_handler_routine_and_module(parser):
    '''Xfailing test to show that a module specified doesn't result in a
    symbol being created in the parent FileContainer.'''
    code = '''
    module other
      integer :: x
    end module
    subroutine mysub(a)
    use other, only: x
    integer, intent(inout) :: a
    a = a + 1
    end subroutine
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert psyir.parent is None
    assert len(psyir.symbol_table.symbols) != 2
    pytest.xfail(reason="Issue #2201: Module specifiers in "
                        "FileContainers don't produce symbols.")


def test_program_handler_routine_and_module_and_program(parser):
    '''
    Xfailing test handling a FileContainer containing a program, module
    and subroutine. Currently the FileContainer only contains a symbol for
    the subroutine.
    '''
    code = '''
    program test
    end program test
    module other
      integer :: x
    end module
    subroutine mysub(a)
    integer, intent(inout) :: a
    a = a + 1
    end subroutine
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert psyir.parent is None
    assert isinstance(psyir.symbol_table.lookup("mysub"), RoutineSymbol)
    assert len(psyir.symbol_table.symbols) != 3
    pytest.xfail(reason="Issues #2201: Program and module specifiers in "
                        "FileContainers don't produce symbols.")


def test_non_named_program_with_subroutine(parser):
    '''Test to show the behaviour of a non-named program whose
    first declared element is a contained subroutine results in a CodeBlock.'''
    code = '''
    subroutine mysub(x)
    integer, intent(inout) :: a
    a = a + 1
    end subroutine
    end
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert isinstance(psyir.children[0], CodeBlock)


def test_non_named_program_with_variables_and_subroutine(parser):
    '''Test to show non-named program with both variables
    and a contained subroutine results in a CodeBlock.'''
    code = '''
    integer :: b, c
    contains
    subroutine mysub(x)
    integer, intent(inout) :: a
    a = a + 1
    end subroutine
    end
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    assert isinstance(psyir, FileContainer)
    assert isinstance(psyir.children[0], CodeBlock)
