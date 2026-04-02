# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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

''' Performs py.test tests on the handling of interface blocks in the fparser2
    PSyIR front-end. '''

import itertools
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    DataSymbol, DataTypeSymbol, GenericInterfaceSymbol, INTEGER_TYPE,
    NoType, RoutineSymbol, Symbol, SymbolTable, UnsupportedFortranType)


@pytest.mark.parametrize("mod_txt", ["", "module "])
def test_named_interface(fortran_reader, mod_txt):
    ''' Test that the frontend creates a GenericInterfaceSymbol
    for a named interface block.'''
    dummy_module = f'''
    module dummy_mod
      private
      public eos
      interface eos
        {mod_txt}procedure eos_insitu, eos_insitu_2d
        {mod_txt}procedure eos_insitu_3d
      end interface
    contains
      subroutine eos_insitu(f1, f2)
          real, intent(in)  :: f1
          real, intent(out) :: f2
          f2 = f1 + 1
      end subroutine eos_insitu
      subroutine eos_insitu_2d(f1, f2)
          real, dimension(:,:), intent(in)  :: f1
          real, dimension(:,:), intent(out) :: f2
          f2(:,:) = f1(:,:) + 1
      end subroutine eos_insitu_2d
      subroutine eos_insitu_3d(f1, f2)
          real, dimension(:,:,:), intent(in)  :: f1
          real, dimension(:,:,:), intent(out) :: f2
          f2(:,:,:) = f1(:,:,:) + 1
      end subroutine eos_insitu_3d
    end module dummy_mod
    '''
    file_container = fortran_reader.psyir_from_source(dummy_module)
    container = file_container.children[0]
    assert isinstance(container, Container)
    eos = container.symbol_table.lookup("eos")
    assert isinstance(eos, GenericInterfaceSymbol)
    assert isinstance(eos.datatype, NoType)
    assert eos.visibility == Symbol.Visibility.PUBLIC
    names = [rsym.symbol.name for rsym in eos.routines]
    assert "eos_insitu" in names
    assert "eos_insitu_2d" in names
    assert "eos_insitu_3d" in names


@pytest.mark.parametrize("mod_txt", ["", "module "])
def test_named_interface_declared(fortran_reader, fortran_writer, mod_txt):
    ''' Test that the frontend creates a RoutineSymbol of
    UnsupportedFortranType for a named interface block when the symbol
    name has already been declared (as will be the case for a structure
    constructor or a subroutine with the same name). '''
    test_module = f'''
    module test_mod
      type test
      end type test
      interface test
        {mod_txt}procedure test_code
      end interface test
    contains
      real function test_code()
      end function test_code
    end module test_mod
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    container = file_container.children[0]
    assert isinstance(container, Container)
    # type symbol
    assert container.symbol_table.lookup("test")
    test_symbol = container.symbol_table.lookup("test")
    assert isinstance(test_symbol, DataTypeSymbol)
    # interface symbol
    assert container.symbol_table.lookup("_psyclone_internal_test")
    test_symbol = container.symbol_table.lookup("_psyclone_internal_test")
    assert isinstance(test_symbol, RoutineSymbol)
    assert isinstance(test_symbol.datatype, UnsupportedFortranType)
    assert test_symbol.datatype.declaration == (
        f"interface test\n"
        f"  {mod_txt}procedure test_code\n"
        f"end interface test")
    assert test_symbol.visibility == Symbol.Visibility.PUBLIC

    test_module = f'''
    module test_mod
      private
      interface test
        {mod_txt}procedure test, test_code
      end interface test
      public test
    contains
      subroutine test_code()
      end subroutine test_code
      subroutine test()
      end subroutine test
    end module test_mod
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    container = file_container.children[0]
    assert isinstance(container, Container)
    # routine symbol
    assert container.symbol_table.lookup("test")
    test_symbol = container.symbol_table.lookup("test")
    assert isinstance(test_symbol, RoutineSymbol)
    # interface symbol
    assert container.symbol_table.lookup("_psyclone_internal_test")
    test_symbol = container.symbol_table.lookup("_psyclone_internal_test")
    assert isinstance(test_symbol, RoutineSymbol)
    assert isinstance(test_symbol.datatype, UnsupportedFortranType)
    assert test_symbol.datatype.declaration == (
        f"interface test\n"
        f"  {mod_txt}procedure test, test_code\n"
        f"end interface test")
    assert test_symbol.visibility == Symbol.Visibility.PUBLIC


def test_named_interface_no_routine_symbol(fortran_reader):
    '''
    Test that an interface is handled correctly if it refers to symbols
    for which we don't have type information (because they are imported).
    '''
    test_module = '''
    module test_mod
      use other_mod
      use some_mod, only: other_routine
      private
      interface test
        module procedure :: test_code_r4, test_code_r8
      end interface test
      public :: test
      interface boundary
        module procedure :: other_routine, test_code_r4
      end interface boundary
    contains

    end module test_mod
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    container = file_container.children[0]
    table = container.symbol_table
    isym = table.lookup("test")
    assert isinstance(isym, GenericInterfaceSymbol)
    bsym = table.lookup("boundary")
    assert isinstance(bsym, GenericInterfaceSymbol)
    assert bsym.routines[0].symbol is table.lookup("other_routine")
    for name in ["test_code_r4", "test_code_r8", "other_routine"]:
        assert isinstance(table.lookup(name), RoutineSymbol)


def test_named_interface_wrong_symbol_type(f2008_parser):
    '''
    Check that we get the expected error if we find a Symbol of the wrong
    type when searching for the routines referenced by an interface.

    We can't get to this error very easily so we resort to adding a clashing
    symbol to the SymbolTable before doing the processing.

    '''
    test_module = '''
    module test_mod
      use other_mod
      interface test
        module procedure :: test_code_r4, test_code_r8
      end interface test
    end module test_mod
'''
    reader = FortranStringReader(test_module)
    ptree = f2008_parser(reader)
    node = walk(ptree, Fortran2003.Interface_Block)[0]
    table = SymbolTable()
    # Add a DataSymbol with the same name as one of the routines referred to by
    # the interface.
    table.new_symbol("test_code_r4", symbol_type=DataSymbol,
                     datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    # This should raise an InternalError...
    with pytest.raises(InternalError) as err:
        processor._process_interface_block(node, table, {})
    assert ("Expected 'test_code_r4' referenced by generic interface 'test' "
            "to be a Symbol or a RoutineSymbol but found 'DataSymbol'" in
            str(err.value))


@pytest.mark.parametrize("use_stmt", ["use some_mod, only: other_sub", ""])
def test_named_interface_with_body(fortran_reader, use_stmt):
    '''
    Test that an INTERFACE that does not exclusively use
    '[MODULE] PROCEDURE :: name-list' is captured as a RoutineSymbol of
    UnsupportedFortranType. We test for the cases where an interface contains
    only subroutine definitions and when it contains a mixture of procedure
    statements and subroutine definitions.
    '''
    mod_procedure = "procedure :: other_sub\n"
    routine1 = '''\
         subroutine test_code(arg)
           real, intent(in) :: arg
         end subroutine test_code
'''
    routine2 = '''\
         subroutine test_code2d(arg)
           real, dimension(:, :), intent(in) :: arg
         end subroutine test_code2d
'''
    if not use_stmt:
        other_sub_body = '''\
      subroutine other_sub(arg)
        complex :: arg
      end subroutine other_sub
'''
    else:
        # 'other_sub' brought in by use statement.
        other_sub_body = ""

    # Generate the various orderings of the three components that will
    # make up the interface body.
    body_parts = [mod_procedure, routine1, routine2]
    perms = list(itertools.permutations(body_parts))
    # Add the case where there is no 'procedure' statement at all.
    perms.append(["", routine1, routine2])
    for parts in perms:
        test_code = f'''
    module test_mod
      {use_stmt}
      implicit none
      interface test
         {parts[0]}
         {parts[1]}
         {parts[2]}
      end interface test
    contains
      subroutine some_sub()
        call test_code(1.0)
      end subroutine some_sub
      {other_sub_body}
    end module test_mod
'''
        file_container = fortran_reader.psyir_from_source(test_code)
        container = file_container.children[0]
        assert isinstance(container, Container)
        table = container.symbol_table
        if mod_procedure in parts:
            # Should have a RoutineSymbol named 'other_sub' because of the
            # procedure statement in the interface body.
            assert isinstance(table.lookup("other_sub"), RoutineSymbol)
        elif use_stmt:
            # Not mentioned in the interface body so not specialised to a
            # RoutineSymbol.
            assert type(table.lookup("other_sub")) is Symbol
        test_symbol = table.lookup("test")
        assert isinstance(test_symbol, RoutineSymbol)
        assert isinstance(test_symbol.datatype, UnsupportedFortranType)
        # Check that the stored declaration text is correct.
        decln_lines = test_symbol.datatype.declaration.split("\n")
        body_lines = (
            f"interface test\n"
            f"{parts[0]}"
            f"{parts[1]}"
            f"{parts[2]}"
            f"end interface test").split("\n")
        for stored, expected in zip(decln_lines, body_lines):
            assert stored.strip() == expected.strip()


GENERIC_INTERFACE_CODE = '''
module dummy_mod
  interface
    subroutine ext1(x, y, z)
      real, dimension(100, 100) :: x, y, z
    end subroutine ext1
    subroutine ext2(x, z)
      real x
      complex (kind = 4) z(2000)
    end subroutine ext2
    function ext3(p, q)
      logical ext3
      integer p(1000)
      logical q(1000)
    end function ext3
  end interface
end module dummy_mod
'''

OPERATOR_INTERFACE_CODE = '''
module dummy_mod
  interface operator ( * )
    function boolean_and (b1, b2)
      logical, intent (in) :: b1 (:), b2 (size (b1))
      logical :: boolean_and (size (b1))
    end function boolean_and
  end interface operator ( * )
end module dummy_mod
'''

ASSIGNMENT_INTERFACE_CODE = '''
module dummy_mod
  interface assignment ( = )
    subroutine logical_to_numeric (n, b)
      integer, intent (out) :: n
      logical, intent (in) :: b
    end subroutine logical_to_numeric
    subroutine char_to_string (s, c)
      use string_module
      ! contains definition of type string
      type (string), intent (out) :: s ! a variable-length string
      character (*), intent (in) :: c
    end subroutine char_to_string
  end interface assignment ( = )
end module dummy_mod
'''

ABSTRACT_INTERFACE_CODE = '''
module dummy_mod
  abstract interface
    subroutine update_interface(field_name, field_proxy, times, xios_id)
      import i_def, field_proxy_type
      character(len=*),        intent(in) :: field_name
      type(field_proxy_type),  intent(inout) :: field_proxy
      integer(i_def),          intent(in) :: times(:)
      character(len=*),        intent(in) :: xios_id
    end subroutine update_interface
  end interface
end module dummy_mod
'''


@pytest.mark.parametrize("code, start, end", [
    (GENERIC_INTERFACE_CODE, "interface", "end interface"),
    (OPERATOR_INTERFACE_CODE,
     "interface operator(*)", "end interface operator(*)"),
    (ASSIGNMENT_INTERFACE_CODE,
     "interface assignment(=)", "end interface assignment(=)"),
    (ABSTRACT_INTERFACE_CODE, "abstract interface", "end interface")])
def test_unnamed_interface(fortran_reader, code, start, end):
    ''' Test that the frontend creates a RoutineSymbol of
    UnsupportedFortranType for the supplied unnamed interfaces. '''
    file_container = fortran_reader.psyir_from_source(code)
    container = file_container.children[0]
    assert isinstance(container, Container)
    # interface symbol
    interface_symbol = container.symbol_table.lookup(
        "_psyclone_internal_interface")
    assert isinstance(interface_symbol, RoutineSymbol)
    assert isinstance(interface_symbol.datatype, UnsupportedFortranType)
    assert interface_symbol.datatype.declaration.startswith(start)
    assert interface_symbol.datatype.declaration.endswith(end)
    assert interface_symbol.visibility == Symbol.Visibility.PUBLIC
