# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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

'''Performs pytest tests on the support for declarations of unknown type in
   the psyclone.psyir.backend.fortran module'''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import \
    add_accessibility_to_unknown_declaration
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.symbols import Symbol, DataSymbol, RoutineSymbol, \
    UnknownType, UnknownFortranType, ImportInterface, ContainerSymbol, \
    SymbolTable, ArgumentInterface, INTEGER_TYPE
from psyclone.tests.utilities import Compile


def test_fw_unknown_decln_error(monkeypatch, fortran_writer):
    ''' Check that the FortranWriter raises the expected error if it
    encounters an UnknownType that is not an UnknownFortranType. '''
    # We can't create an UnknownType() object directly as it is abstract.
    # Therefore we create a symbol of UnknownFortranType and then
    # monkeypatch it.
    sym = DataSymbol("b", UnknownFortranType("int b;"))
    monkeypatch.setattr(sym.datatype, "__class__", UnknownType)
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_vardecl(sym)
    assert ("DataSymbol 'b' is of 'UnknownType' type. This is not supported by"
            " the Fortran backend" in str(err.value))


def test_fw_unknown_decln(fortran_writer):
    ''' Check that the FortranWriter recreates a declaration that is of
    UnknownFortranType. '''
    sym = DataSymbol("b", UnknownFortranType("integer, value :: b"))
    assert "integer, value :: b" in fortran_writer.gen_vardecl(sym)


def test_fw_unknown_interface_decln(tmpdir, fortran_writer):
    ''' Check that the backend recreates an interface declaration stored as
    an unknown type and adds an appropriate access statement. '''
    container = Container("my_mod")
    interface_code = ("interface eos\n"
                      "  module procedure eos1d, eos2d\n"
                      "end interface")
    sym = RoutineSymbol("eos", UnknownFortranType(interface_code),
                        visibility=Symbol.Visibility.PRIVATE)
    assert interface_code in fortran_writer.gen_vardecl(sym)
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


def test_fw_unknowntype_routine_symbols_error(fortran_writer):
    ''' Check that the backend raises the expected error if a RoutineSymbol
    of UnknownType or non-local interface is encountered.

    '''
    class OtherType(UnknownType):
        ''' UnknownType is abstract so sub-class it for this test '''
        def __str__(self):
            return "OtherType"

    container = Container("my_mod")
    container.symbol_table.add(RoutineSymbol("eos", OtherType("some code")))
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_decls(container.symbol_table)
    assert ("RoutineSymbol 'eos' is of 'OtherType' type. This is not supported"
            " by the Fortran backend" in str(err.value))


def test_fw_add_accessibility_errors():
    ''' Check that the add_accessibility_to_unknown_declaration() method
    raises the expected errors. '''
    with pytest.raises(TypeError) as err:
        add_accessibility_to_unknown_declaration("hello")
    assert str(err.value) == "Expected a Symbol but got 'str'"
    with pytest.raises(TypeError) as err:
        add_accessibility_to_unknown_declaration(
            DataSymbol("var", INTEGER_TYPE))
    assert ("Expected a Symbol of UnknownFortranType but symbol 'var' has "
            "type 'Scalar<INTEGER, UNDEFINED>'" in str(err.value))
    # Missing :: separator in declaration
    sym = DataSymbol("var", UnknownFortranType("real var"))
    with pytest.raises(NotImplementedError) as err:
        add_accessibility_to_unknown_declaration(sym)
    assert ("Cannot add accessibility information to an UnknownFortranType "
            "that does not have '::' in its original declaration: 'real var'"
            in str(err.value))
    # Private symbol that has 'public' in its declaration
    sym = DataSymbol("var", UnknownFortranType("real, puBlic :: var"),
                     visibility=Symbol.Visibility.PRIVATE)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unknown_declaration(sym)
    assert ("Symbol 'var' of UnknownFortranType has private visibility but "
            "its associated declaration specifies that it is public: 'real, "
            "puBlic :: var'" in str(err.value))
    # Public symbol that has 'private' in its declaration
    sym = DataSymbol("var", UnknownFortranType("real, pRivate :: var"),
                     visibility=Symbol.Visibility.PUBLIC)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unknown_declaration(sym)
    assert ("Symbol 'var' of UnknownFortranType has public visibility but "
            "its associated declaration specifies that it is private: 'real, "
            "pRivate :: var'" in str(err.value))
    # Missing declaration text
    sym = DataSymbol("var", UnknownFortranType(""),
                     visibility=Symbol.Visibility.PUBLIC)
    with pytest.raises(InternalError) as err:
        add_accessibility_to_unknown_declaration(sym)
    assert ("Symbol 'var' is of UnknownFortranType but the "
            "associated declaration text is empty." in str(err.value))


def test_fw_add_accessibility():
    ''' Check that the add_accessibility_to_unknown_declaration() method
    works as expected. '''
    sym = DataSymbol("var", UnknownFortranType("real, target :: var"),
                     visibility=Symbol.Visibility.PUBLIC)
    result = add_accessibility_to_unknown_declaration(sym)
    assert result == "real, target, public :: var"
    sym = DataSymbol("var", UnknownFortranType("real, target :: var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unknown_declaration(sym)
    assert result == "real, target, private :: var"
    sym = DataSymbol("var", UnknownFortranType("type :: var\n"
                                               "  public\n"
                                               "  integer :: flag\n"
                                               "end type var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unknown_declaration(sym)
    assert result == ("type, private :: var\n"
                      "  public\n"
                      "  integer :: flag\n"
                      "end type var")
    sym = DataSymbol("var", UnknownFortranType("type :: var\n"
                                               "  integer, private :: id\n"
                                               "  integer, public :: flag\n"
                                               "end type var"),
                     visibility=Symbol.Visibility.PRIVATE)
    result = add_accessibility_to_unknown_declaration(sym)
    assert result == ("type, private :: var\n"
                      "  integer, private :: id\n"
                      "  integer, public :: flag\n"
                      "end type var")
