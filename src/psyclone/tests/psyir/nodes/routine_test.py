# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

''' This module contains the pytest tests for the Routine class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Routine, Assignment, Reference, Literal
from psyclone.psyir.symbols import (REAL_TYPE, DataSymbol,
                                    SymbolTable, RoutineSymbol)
from psyclone.tests.utilities import check_links


def test_routine_constructor():
    ''' Check the constructor and associated type checking. '''
    with pytest.raises(TypeError) as err:
        Routine(1)
    assert "must be a str but got" in str(err.value)
    with pytest.raises(TypeError) as err:
        Routine("hello", is_program=1)
    assert "'is_program' must be a bool" in str(err.value)
    node = Routine("hello")
    assert node._name == "hello"


def test_routine_properties():
    ''' Check the various properties of the Routine class. '''
    node1 = Routine("hello")
    assert node1.dag_name == "routine_hello_0"
    assert node1.return_symbol is None
    assert node1.is_program is False
    assert node1.name == "hello"
    # Give the Routine a child to get full coverage of __str__ method
    node1.addchild(Assignment())
    assert "Routine[name:'hello']:\nAssignment" in str(node1)

    node2 = Routine("bonjour")
    assert node2.is_program is False

    node3 = Routine("gutentag", is_program=True)
    assert node3.is_program


def test_routine_name_setter():
    ''' Check the name setter property of the Routine class updates its
    name and its associated Routine symbol. '''

    node = Routine("hello")  # The constructor has an implicit name setter
    # Check the associated RoutineSymbol has been created
    assert "hello" in node.symbol_table
    assert isinstance(node.symbol_table.lookup("hello"), RoutineSymbol)
    # Check with an incorrect value type
    with pytest.raises(TypeError) as err:
        node.name = 3
    assert "must be a str but got" in str(err.value)

    # Perform a successful name change
    node.name = "goodbye"
    assert node.name == "goodbye"
    # Check that the previous symbol has been deleted and the new one created
    assert "welcome" not in node.symbol_table
    assert "goodbye" in node.symbol_table
    # Check that the 'own_routine_symbol' tag has been updated
    assert node.symbol_table.lookup_with_tag('own_routine_symbol').name == \
        "goodbye"


def test_routine_create_invalid():
    '''Test that the create method in the Routine class raises the
    expected exceptions if the provided input is invalid.

    '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("x", REAL_TYPE)
    symbol_table.add(symbol)
    children = [Assignment.create(Reference(symbol),
                                  Literal("1", REAL_TYPE))]

    # name is not a string.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create(1, symbol_table, children)
    assert ("name argument in create method of Routine class "
            "should be a string but found 'int'.") in str(excinfo.value)

    # symbol_table not a SymbolTable.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", "invalid", children)
    assert ("symbol_table argument in create method of Routine class "
            "should be a SymbolTable but found 'str'.") in str(excinfo.value)

    # children not a list.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", symbol_table, "invalid")
    assert ("children argument in create method of Routine class "
            "should be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", symbol_table, ["invalid"])
    assert (
        "child of children argument in create method of Routine class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))

    # return_symbol is not a Symbol
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", symbol_table, [], return_symbol="wrong")
    assert ("return_symbol argument should be a DataSymbol but found 'str'"
            in str(excinfo.value))


def test_routine_create():
    '''Test that the create method correctly creates a Routine instance. '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    symbol_table.add(symbol)
    assignment = Assignment.create(Reference(symbol),
                                   Literal("0.0", REAL_TYPE))
    kschedule = Routine.create("mod_name", symbol_table, [assignment],
                               is_program=True, return_symbol=symbol)
    assert isinstance(kschedule, Routine)
    check_links(kschedule, [assignment])
    assert kschedule.symbol_table is symbol_table
    assert kschedule.is_program
    assert kschedule.return_symbol is symbol
