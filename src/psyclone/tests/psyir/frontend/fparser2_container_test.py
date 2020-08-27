# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' Performs py.test tests on the container generation in the fparser2 PSyIR
    front-end. '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import RoutineSymbol


def test_generate_container(parser):
    ''' Test that generate_container creates a PSyIR container with the
    contents of the given fparser2 fortran module.'''
    dummy_module = '''
    module dummy_mod
        use mod1
        use mod2, only: var1
        real :: modvar1
    contains
        subroutine dummy_code(f1, f2, f3)
            real(wp), dimension(:,:), intent(in)  :: f1
            real(wp), dimension(:,:), intent(out)  :: f2
            real(wp), dimension(:,:) :: f3
            f2 = f1 + 1
        end subroutine dummy_code
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_module)
    ast = parser(reader)
    processor = Fparser2Reader()
    container = processor.generate_container(ast)
    assert isinstance(container, Container)
    assert not container.children
    assert container.symbol_table
    assert container.symbol_table.lookup("modvar1")
    assert container.symbol_table.lookup("var1")
    assert container.symbol_table.lookup("mod1")
    assert container.symbol_table.lookup("mod2")


def test_generate_container_two_modules(parser):
    ''' Tests the fparser2Reader generate_container method raises an exception
    when more than one fparser2 module node is provided.
    '''
    reader = FortranStringReader("module dummy1_mod\n"
                                 "end module dummy1_mod\n"
                                 "module dummy2_mod\n"
                                 "end module dummy2_mod\n")
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test kernel with two modules
    with pytest.raises(GenerationError) as error:
        _ = processor.generate_container(ast)
    assert "Could not process" in str(error.value)
    assert "Just one module definition per file supported." in str(error.value)


def test_generate_container_routine_names(parser):
    ''' Test that the generate_container method correctly populates the
    SymbolTable with entries for routines contained within the module. '''
    reader = FortranStringReader("module dummy1_mod\n"
                                 "  public my_func\n"
                                 "contains\n"
                                 "  subroutine my_sub()\n"
                                 "    implicit none\n"
                                 "    write(*,*) my_func()\n"
                                 "  end subroutine my_sub\n"
                                 "  function my_func()\n"
                                 "    integer :: my_func\n"
                                 "    my_func = 1\n"
                                 "  end function my_func\n"
                                 "end module dummy1_mod\n")
    ast = parser(reader)
    processor = Fparser2Reader()
    container = processor.generate_container(ast)
    func = container.symbol_table.lookup("my_func")
    assert isinstance(func, RoutineSymbol)
    sub = container.symbol_table.lookup("my_sub")
    assert isinstance(sub, RoutineSymbol)


def test_access_stmt_no_unqualified_use_error(parser):
    ''' Check that we raise the expected error if an undeclared symbol is
    listed in an access statement and there are no unqualified use
    statements to bring it into scope. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "integer :: flag\n"
        "public var3\n"
        "end module modulename")
    fparser2spec = parser(reader)
    with pytest.raises(SymbolError) as err:
        processor.generate_container(fparser2spec)
    assert ("module 'modulename': 'var3' is listed in an accessibility "
            "statement as being" in str(err.value))
