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
# -----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the container generation in the fparser2 PSyIR
    front-end. '''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes import Container, FileContainer, KernelSchedule
from psyclone.psyir.symbols import Symbol, RoutineSymbol


def test_generate_container(parser):
    ''' Test that generate_container creates a PSyIR container with the
    contents of the given fparser2 fortran module.'''
    dummy_module = '''
    module dummy_mod
        use mod1
        use mod2, only: var1
        implicit none
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
    assert container.symbol_table.lookup("dummy_code")


def test_generate_container_no_module(parser):
    ''' Check that generate_container returns None if the parse tree does
    not contain a module. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "program my_prog\n"
        "integer :: flag\n"
        "end program")
    ast = parser(reader)
    processor = Fparser2Reader()
    container = processor.generate_container(ast)
    assert container is None


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


def test_access_stmt_no_unqualified_use(parser):
    '''Check that no error is raised if an undeclared symbol is listed in
    an access statement and there are no unqualified use statements to
    bring it into scope. This is now the job of the back-end.

    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "integer :: flag\n"
        "public var3\n"
        "end module modulename")
    fparser2spec = parser(reader)
    processor.generate_container(fparser2spec)


def test_default_public_container(parser):
    ''' Test when all symbols default to public within a module and some
    are explicitly listed as being private. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "public\n"
        "integer, private :: var1\n"
        "integer :: var2\n"
        "integer :: var3\n"
        "private var3\n"
        "end module modulename")
    fparser2spec = parser(reader)
    container = processor.generate_container(fparser2spec)
    assert "var1" in container.symbol_table
    assert (container.symbol_table.lookup("var1").visibility ==
            Symbol.Visibility.PRIVATE)
    assert (container.symbol_table.lookup("var2").visibility ==
            Symbol.Visibility.PUBLIC)
    assert (container.symbol_table.lookup("var3").visibility ==
            Symbol.Visibility.PRIVATE)


def test_default_private_container(parser):
    ''' Test that symbols get the correct visibilities when the Fortran
    specifies that the default is private within a module. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "integer, public :: var1\n"
        "integer :: var2\n"
        "integer :: var3\n"
        "public var3\n"
        "end module modulename")
    fparser2spec = parser(reader)
    container = processor.generate_container(fparser2spec)
    assert "var1" in container.symbol_table
    assert (container.symbol_table.lookup("var1").visibility ==
            Symbol.Visibility.PUBLIC)
    assert (container.symbol_table.lookup("var2").visibility ==
            Symbol.Visibility.PRIVATE)
    assert (container.symbol_table.lookup("var3").visibility ==
            Symbol.Visibility.PUBLIC)


def test_access_stmt_undeclared_symbol(parser):
    ''' Check that we create a Symbol if a name appears in an access statement
    but is not explicitly declared. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "use some_mod\n"
        "private\n"
        "integer :: var3\n"
        "public var3, var4\n"
        "private var5\n"
        "end module modulename")
    fparser2spec = parser(reader)
    container = processor.generate_container(fparser2spec)
    sym_table = container.symbol_table
    assert "var3" in sym_table
    assert sym_table.lookup("var3").visibility == Symbol.Visibility.PUBLIC
    assert "var4" in sym_table
    var4 = sym_table.lookup("var4")
    assert isinstance(var4, Symbol)
    assert var4.visibility == Symbol.Visibility.PUBLIC
    var5 = sym_table.lookup("var5")
    assert isinstance(var5, Symbol)
    assert var5.visibility == Symbol.Visibility.PRIVATE


def test_process_access_statements_invalid(parser):
    ''' Tests for the process_access_statements() method when an
    invalid parse tree is encountered. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "use some_mod\n"
        "private\n"
        "integer :: var3\n"
        "public var3, var4\n"
        "end module modulename")
    fparser2spec = parser(reader).children[0].children[1]
    # Break the parse tree created by fparser2
    fparser2spec.children[1].items = ('not-private', None)
    with pytest.raises(InternalError) as err:
        processor.process_access_statements([fparser2spec])
    assert ("Failed to process 'not-private'. Found an accessibility "
            "attribute of 'not-private' but expected either 'public' or "
            "'private'" in str(err.value))


def test_access_stmt_no_module(parser):
    ''' Check that we raise the expected error if we encounter multiple access
    statements (without access-id-lists) that are not within a module (this is
    invalid Fortran but fparser doesn't catch it). '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "use some_mod\n"
        "private\n"
        "integer :: var3\n"
        "public\n"
        "end module modulename")
    module = parser(reader).children[0]
    assert isinstance(module, Fortran2003.Module)
    # Break 'module' so that it is no longer an instance of Fortran2003.Module
    module.__class__ = Fortran2003.Program
    spec_part = module.children[1]
    with pytest.raises(GenerationError) as err:
        processor.process_access_statements([spec_part])
    assert ("Found multiple access statements with omitted access-id-lists "
            "and no enclosing Module. Both of these things are invalid "
            "Fortran." in str(err.value))


def test_access_stmt_routine_name(parser):
    ''' Check that we create a Symbol for something named in an access
    statement that is not a variable. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "public my_routine\n"
        "contains\n"
        "  subroutine my_routine()\n"
        "  end subroutine my_routine\n"
        "end module modulename")
    fparser2spec = parser(reader)
    container = processor.generate_container(fparser2spec)
    sym = container.symbol_table.lookup("my_routine")
    assert isinstance(sym, RoutineSymbol)
    assert sym.visibility == Symbol.Visibility.PUBLIC


def test_public_private_symbol_error(parser):
    ''' Check that we raise the expected error when a symbol is listed as
    being both PUBLIC and PRIVATE. (fparser2 doesn't check for this.) '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "public var3\n"
        "private var4, var3\n"
        "end module modulename")
    fparser2spec = parser(reader)
    with pytest.raises(GenerationError) as err:
        processor.generate_container(fparser2spec)
    assert ("Symbols ['var3'] appear in access statements with both PUBLIC "
            "and PRIVATE" in str(err.value))


def test_multiple_access_stmt_error(parser):
    ''' Check that we raise the expected error when we encounter multiple
    access statements. (fparser2 doesn't check for this.) '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "private\n"
        "public var3\n"
        "private\n"
        "end module modulename")
    fparser2spec = parser(reader)
    with pytest.raises(GenerationError) as err:
        processor.generate_container(fparser2spec)
    assert ("Module 'modulename' contains more than one access statement with"
            in str(err.value))


def test_broken_access_spec(parser):
    ''' Check that we raise the expected InternalError if the parse tree for
    an access-spec on a variable declaration is broken. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "module modulename\n"
        "integer, private :: var3\n"
        "end module modulename\n")
    fparser2spec = parser(reader).children[0].children[1]
    # Break the parse tree
    access_spec = fparser2spec.children[0].children[1].children[0]
    access_spec.string = "not-private"
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, fparser2spec.children, [])
    assert "Unexpected Access Spec attribute 'not-private'" in str(err.value)


def test_unsupported_implicit_part(parser):
    '''
    Test that an unsupported implicit statment results in the expected error.
    '''
    fake_parent = FileContainer("dummy")
    processor = Fparser2Reader()
    code = '''\
      module my_modulename
        implicit real (a-h,o-z)
        integer, private :: var3
      end module my_modulename'''
    reader = FortranStringReader(code)
    fparser2spec = parser(reader).children[0].children[1]
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(fake_parent, fparser2spec.children, [])
    assert ("implicit variable declarations not supported but found "
            "'IMPLICIT REAL(A - H, O - Z)'" in str(err.value))


def test_unsupported_format_stmt(parser):
    '''
    Test that an unsupported format statement appearing in the implicit-part
    of a specification-part in a module results in the expected error.
    '''
    fake_parent = FileContainer("dummy")
    processor = Fparser2Reader()
    code = '''\
      module my_modulename
5       FORMAT (1E12.4, I10)
        integer, private :: var3
      end module my_modulename'''
    reader = FortranStringReader(code)
    fparser2spec = parser(reader).children[0].children[1]
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(fake_parent, fparser2spec.children, [])
    assert ("Error processing implicit-part: Format statements are not "
            "supported but found 'FORMAT(1E12.4, I10)'" in str(err.value))
