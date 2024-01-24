# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the support for declarations of UnsupportedType in
   the psyclone.psyir.backend.fortran module.'''

import os
import pytest

from psyclone.errors import InternalError
from psyclone.configuration import Config
from psyclone.psyir.backend.fortran import (
    add_accessibility_to_unsupported_declaration)
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.symbols import (
    ArgumentInterface, DataSymbol, UnresolvedType, RoutineSymbol,
    UnsupportedType, UnsupportedFortranType, ImportInterface, ContainerSymbol,
    Symbol, SymbolTable, INTEGER_TYPE)
from psyclone.tests.utilities import Compile


def test_fw_unsupported_decln_error(monkeypatch, fortran_writer):
    ''' Check that the FortranWriter raises the expected error if it
    encounters an UnsupportedType that is not an UnsupportedFortranType. '''
    # We can't create an UnsupportedType() object directly as it is abstract.
    # Therefore we create a symbol of UnsupportedFortranType and then
    # monkeypatch it.
    sym = DataSymbol("b", UnsupportedFortranType("int b;"))
    monkeypatch.setattr(sym.datatype, "__class__", UnsupportedType)
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_vardecl(sym)
    assert ("DataSymbol 'b' is of 'UnsupportedType' type. This is not "
            "supported by the Fortran backend" in str(err.value))


def test_fw_unsupported_decln(fortran_writer):
    ''' Check that the FortranWriter recreates a declaration that is of
    UnsupportedFortranType. '''
    sym = DataSymbol("b", UnsupportedFortranType("integer, value :: b"))
    assert "integer, value :: b" in fortran_writer.gen_vardecl(sym)


def test_fw_unsupported_interface_decln(tmpdir, fortran_writer):
    ''' Check that the backend recreates an interface declaration stored as
    an unsupported type and adds an appropriate access statement. '''
    container = Container("my_mod")
    interface_code = ("interface eos\n"
                      "  module procedure eos1d, eos2d\n"
                      "end interface")
    sym = RoutineSymbol("eos", UnsupportedFortranType(interface_code),
                        visibility=Symbol.Visibility.PRIVATE)
    container.symbol_table.add(sym)
    code = fortran_writer(container)
    assert interface_code in code
    assert "private :: eos\n" in code
    container.symbol_table.add(
        RoutineSymbol("my_sub", visibility=Symbol.Visibility.PUBLIC))
    code = fortran_writer(container)
    assert "private :: eos\n" in code
    # Default visibility is public so there should be no explicit 'public ::'
    # statements.
    assert "public ::" not in code
    # Add remaining structures so that we can generate compilable code
    container.symbol_table.add(RoutineSymbol("eos1d"))
    container.symbol_table.add(RoutineSymbol("eos2d"))
    # We have to make the interfaces to the two 'module procedures' different
    # so we give 'eos1d' an integer argument.
    arg = DataSymbol("var1", datatype=INTEGER_TYPE,
                     interface=ArgumentInterface())
    eos1d_table = SymbolTable()
    eos1d_table.add(arg)
    eos1d_table.specify_argument_list([arg])
    container.addchild(Routine.create("eos1d", eos1d_table, []))
    container.addchild(Routine.create("eos2d", SymbolTable(), []))
    container.addchild(Routine.create("my_sub", SymbolTable(), []))
    assert Compile(tmpdir).string_compiles(fortran_writer(container))


def test_fw_unsupportedtype_routine_symbols_error(fortran_writer):
    ''' Check that the backend raises the expected error if a RoutineSymbol
    which is not imported or a Fortran interface (currently an
    UnsupportedFortanType) is found by the gen_decls. This symbols are
    implicitly declared by the routine definition.

    '''
    class OtherType(UnsupportedType):
        ''' UnsupportedType is abstract so sub-class it for this test '''
        def __str__(self):
            return "OtherType"

    container = Container("my_mod")
    container.symbol_table.add(RoutineSymbol("eos", OtherType("some code")))
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_decls(container.symbol_table)
    assert ("Symbol 'eos' is a RoutineSymbol which is not imported nor an "
            "interface (UnsupportedFortranType). This is already implicitly "
            "declared by the routine itself and should not be provided "
            "to 'gen_vardecl'." in str(err.value))


def test_fw_unsupportedtype_nonlocal_routine_symbols_error(fortran_writer):
    ''' Check that the backend raises the expected error if a RoutineSymbol
    of UnsupportedFortranType is encountered. '''
    container = Container("my_mod")
    csym = ContainerSymbol("other_mod")
    container.symbol_table.add(csym)
    sym = RoutineSymbol("eos", UnsupportedFortranType("some code"),
                        interface=ImportInterface(csym))
    container.symbol_table.add(sym)
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_vardecl(sym)
    assert (" Symbol 'eos' is brought into scope from a Fortran USE statement "
            "and should be generated by 'gen_use' instead of 'gen_vardecl'."
            in str(err.value))


def test_fw_add_accessibility_errors():
    ''' Check that the add_accessibility_to_unsupported_declaration() method
    raises the expected errors. '''
    with pytest.raises(TypeError) as err:
        add_accessibility_to_unsupported_declaration("hello")
    assert str(err.value) == "Expected a Symbol but got 'str'"
    with pytest.raises(TypeError) as err:
        add_accessibility_to_unsupported_declaration(
            DataSymbol("var", INTEGER_TYPE))
    assert ("Expected a Symbol of UnsupportedFortranType but symbol 'var' has "
            "type 'Scalar<INTEGER, UNDEFINED>'" in str(err.value))
    # Missing :: separator in declaration
    sym = DataSymbol("var", UnsupportedFortranType("real var"))
    with pytest.raises(NotImplementedError) as err:
        add_accessibility_to_unsupported_declaration(sym)
    assert ("Cannot add accessibility information to an UnsupportedFortranType"
            " that does not have '::' in its original declaration: 'real var'"
            in str(err.value))
    # Private symbol that has 'public' in its declaration
    sym = DataSymbol("var", UnsupportedFortranType("real, puBlic :: var"),
                     visibility=Symbol.Visibility.PRIVATE)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unsupported_declaration(sym)
    assert ("Symbol 'var' of UnsupportedFortranType has private visibility but"
            " its associated declaration specifies that it is public: 'real, "
            "puBlic :: var'" in str(err.value))
    # Public symbol that has 'private' in its declaration
    sym = DataSymbol("var", UnsupportedFortranType("real, pRivate :: var"),
                     visibility=Symbol.Visibility.PUBLIC)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unsupported_declaration(sym)
    assert ("Symbol 'var' of UnsupportedFortranType has public visibility but "
            "its associated declaration specifies that it is private: 'real, "
            "pRivate :: var'" in str(err.value))
    # Missing declaration text
    sym = DataSymbol("var", UnsupportedFortranType(""),
                     visibility=Symbol.Visibility.PUBLIC)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unsupported_declaration(sym)
    assert ("Symbol 'var' is of UnsupportedFortranType but the "
            "associated declaration text is empty." in str(err.value))


def test_fw_add_accessibility():
    ''' Check that the add_accessibility_to_unsupported_declaration() method
    works as expected. '''
    sym = DataSymbol("var", UnsupportedFortranType("real, target :: var"),
                     visibility=Symbol.Visibility.PUBLIC)
    result = add_accessibility_to_unsupported_declaration(sym)
    assert result == "real, target, public :: var"
    sym = DataSymbol("var", UnsupportedFortranType("real, target :: var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unsupported_declaration(sym)
    assert result == "real, target, private :: var"
    sym = DataSymbol("var", UnsupportedFortranType(
                                 "type :: var\n"
                                 "  public\n"
                                 "  integer :: flag\n"
                                 "end type var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unsupported_declaration(sym)
    assert result == ("type, private :: var\n"
                      "  public\n"
                      "  integer :: flag\n"
                      "end type var")
    sym = DataSymbol("var", UnsupportedFortranType(
                                 "type :: var\n"
                                 "  integer, private :: id\n"
                                 "  integer, public :: flag\n"
                                 "end type var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unsupported_declaration(sym)
    assert result == ("type, private :: var\n"
                      "  integer, private :: id\n"
                      "  integer, public :: flag\n"
                      "end type var")


def test_generating_unsupportedtype_routine_imports(
        fortran_reader, tmpdir, monkeypatch, fortran_writer):
    ''' Tests that generating UnsupportedType imported RoutineSymbols (if their
    UnresolvedType is resolved) are not misinterpreted as interfaces.'''

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
          module a_mod
              contains
              character(len=3) function unsupported_type_symbol()
                 unsupported_type_symbol = 'a'
              end function unsupported_type_symbol
          end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
          module test_mod
              use a_mod, only: unsupported_type_symbol
              contains
              subroutine test()
                  integer :: a
                  a = unsupported_type_symbol()
              end subroutine test
          end module test_mod
      ''')
    module = psyir.children[0]
    symbol = module.symbol_table.lookup('unsupported_type_symbol')
    module.symbol_table.resolve_imports()

    # symbol is now a RoutineSymbol
    assert isinstance(symbol, RoutineSymbol)
    # but its datatype has not been resolved and is still a UnresolvedType
    # instead of UnsupportedFortranType
    assert not isinstance(symbol.datatype, UnsupportedFortranType)
    assert isinstance(symbol.datatype, UnresolvedType)

    # And the output does not mistake it for an interface
    code = fortran_writer(psyir)
    assert "use a_mod, only : unsupported_type_symbol" in code
    assert "interface" not in code.lower()


def test_fw_save_common(fortran_reader, fortran_writer):
    '''
    Check that a SAVE statement involving a named Common Block is correctly
    re-generated.
    '''
    code = '''
      module my_mod
        save :: var3, /my_common/
        common /my_common/ a, b
        integer :: var1
        integer :: a, b
        integer :: var3
      end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    assert "SAVE :: /my_common/\n" in output
    assert "integer, save, public :: var3\n" in output
