# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psyclone.psyir.symbols.symboltable file '''

import re
import os
from collections import OrderedDict
import pytest
from psyclone.configuration import Config
from psyclone.psyir.nodes import (
    CodeBlock, Container, KernelSchedule,
    Literal, Reference, Assignment, Routine, Schedule)
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ContainerSymbol, \
    AutomaticInterface, ImportInterface, ArgumentInterface, \
    ScalarType, ArrayType, UnresolvedType, REAL_TYPE, INTEGER_TYPE, Symbol, \
    SymbolError, RoutineSymbol, NoType, StructureType, DataTypeSymbol, \
    UnsupportedFortranType, UnresolvedInterface, CommonBlockInterface
from psyclone.errors import InternalError


def create_hierarchy():
    '''Utility routine that creates a symbol table hierarchy with a
    symbol in each symbol table.

    :returns: two symbol tables created in a hierarchy.
    :rtype: (:py:class:`psyclone.psyir.symbols.SymbolTable`, \
        :py:class:`psyclone.psyir.symbols.SymbolTable`)

    '''
    schedule_symbol_table = SymbolTable()
    symbol1 = DataSymbol("symbol1", INTEGER_TYPE)
    schedule_symbol_table.add(symbol1, tag="symbol1_tag")
    schedule = KernelSchedule.create("my_kernel", schedule_symbol_table, [])
    container_symbol_table = SymbolTable()
    symbol2 = DataSymbol("symbol2", INTEGER_TYPE)
    container_symbol_table.add(symbol2, tag="symbol2_tag")
    _ = Container.create("my_container", container_symbol_table,
                         [schedule])
    return (schedule_symbol_table, container_symbol_table)


def test_instance():
    '''Test that a SymbolTable is created with the expected initial values
    and that it raises an exception if any arguments are invalid.

    '''
    sym_table = SymbolTable()
    assert isinstance(sym_table._symbols, OrderedDict)
    assert not sym_table._symbols
    assert sym_table._argument_list == []
    assert sym_table._tags == {}
    assert sym_table._node is None
    assert sym_table._default_visibility is Symbol.Visibility.PUBLIC

    with pytest.raises(TypeError) as info:
        _ = SymbolTable(node="hello")
    # Error checked in the attribute setter test

    schedule = Schedule()
    schedule.symbol_table.detach()
    sym_table = SymbolTable(node=schedule)
    assert isinstance(sym_table._symbols, OrderedDict)
    assert not sym_table._symbols
    assert sym_table._argument_list == []
    assert sym_table._tags == {}
    assert sym_table._node is schedule
    assert sym_table._default_visibility is Symbol.Visibility.PUBLIC

    sym_table = SymbolTable(default_visibility=Symbol.Visibility.PUBLIC)
    assert sym_table._default_visibility == Symbol.Visibility.PUBLIC

    with pytest.raises(TypeError) as info:
        SymbolTable(default_visibility=1)
    assert ("Default visibility must be an instance of psyir.symbols.Symbol."
            "Visibility but got 'int'" in str(info.value))


def test_default_vis_symbol_table():
    ''' Test the setter and getter for the default_visibility property. '''
    sym_table = SymbolTable()
    assert sym_table.default_visibility is Symbol.Visibility.PUBLIC
    sym_table.default_visibility = Symbol.Visibility.PRIVATE
    assert sym_table.default_visibility == Symbol.Visibility.PRIVATE
    with pytest.raises(TypeError) as info:
        sym_table.default_visibility = 1
    assert ("Default visibility must be an instance of psyir.symbols.Symbol."
            "Visibility but got 'int'" in str(info.value))


def test_parent_symbol_table():
    '''Check that the parent_symbol_table() method behaves as expected with the
    scope_limit argument.

    '''
    inner_symbol_table, outer_symbol_table = create_hierarchy()
    inner_scope = inner_symbol_table.node

    assert inner_symbol_table.parent_symbol_table() is outer_symbol_table
    assert outer_symbol_table.parent_symbol_table() is None

    # Limit recursion by the scope_limit parameter
    assert inner_symbol_table.parent_symbol_table(
            scope_limit=inner_scope) is None

    # Provide a wrong scope_limit parameter
    with pytest.raises(TypeError) as err:
        _ = inner_symbol_table.parent_symbol_table(scope_limit=2)
    assert ("The scope_limit argument '2', is not of type `Node`." in
            str(err.value))


def test_symboltable_is_empty():
    '''Test that a symbol table is correctly flagged as empty/non-empty.

    '''
    sym_table = SymbolTable()
    assert sym_table.is_empty() is True

    # Add a symbol so the table is now not empty anymore
    sym = ContainerSymbol("my_mod")
    sym_table.add(sym)
    assert sym_table.is_empty() is False

    # Now remove the symbol again, the table should be empty
    sym_table.remove(sym)
    assert sym_table.is_empty() is True


def test_next_available_name_1():
    '''Test that the next_available_name method returns names that are not
    already in the symbol table.

    '''
    # Create a symbol table containing a symbol
    sym_table = SymbolTable()
    sym_table.add(ContainerSymbol("my_mod"))

    # Check we can generate a new symbol name (and add it to the symbol
    # table as this is required for further testing).
    name = sym_table.next_available_name()
    assert name == "psyir_tmp"
    sym_table.add(DataSymbol(name, REAL_TYPE))

    # Check we return the expected symbol name when there is a
    # supplied root name.
    assert sym_table.next_available_name(root_name="my_name") == "my_name"
    # Check we return a new symbol by appending an integer index to
    # the root name when the names clash.
    name = sym_table.next_available_name(root_name="my_mod")
    assert name == "my_mod_1"
    sym_table.add(ContainerSymbol(name))
    # Check that the name-clash checking is not case sensitive
    name = sym_table.next_available_name(root_name="my_MOD")
    assert name == "my_MOD_2"
    name = sym_table.next_available_name(root_name="my_mod_1")
    assert name == "my_mod_1_1"
    # Check we return a new name by appending an integer index to
    # the default name when the names clash.
    name = sym_table.next_available_name()
    assert name == "psyir_tmp_1"
    sym_table.add(DataSymbol(name, REAL_TYPE))
    assert sym_table.next_available_name() == "psyir_tmp_2"
    # Check that clashes with symbols in a second symbol table are also
    # avoided.
    table2 = SymbolTable()
    table2.add(DataSymbol("psyir_tmp_2", REAL_TYPE))
    assert sym_table.next_available_name(other_table=table2) == "psyir_tmp_3"


def test_next_available_name_2():
    '''Test that the next_available_name method returns an internal name if
    the supplied root_name argument is an empty string.

    '''
    sym_table = SymbolTable()
    name = sym_table.next_available_name(root_name="")
    assert name == "psyir_tmp"


def test_next_available_name_3():
    '''Test that the next_available_name method returns an internal name if
    the supplied root_name argument is None.

    '''
    sym_table = SymbolTable()
    name = sym_table.next_available_name(root_name=None)
    assert name == "psyir_tmp"


def test_next_available_name_4():
    '''Test that the next_available_name method raises the expected exception
    if an argument has the wrong type.

    '''
    sym_table = SymbolTable()
    with pytest.raises(TypeError) as excinfo:
        _ = sym_table.next_available_name(root_name=7)
    assert ("Argument root_name should be of type str or NoneType but found "
            "'int'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        _ = sym_table.next_available_name(shadowing=7)
    assert ("Argument 'shadowing' should be of type bool but found "
            "'int'." in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        _ = sym_table.next_available_name(root_name="groot", other_table="am")
    assert ("argument 'other_table' should be of type SymbolTable but found "
            "'str'." in str(excinfo.value))


def test_new_symbol_5():
    '''Check that next_available_name in the SymbolTable class behaves as
    expected with the shadowing flag being a) explicitly set to
    False, b) explicitly set to True and c) using the default value
    (False).

    '''
    schedule_symbol_table, container_symbol_table = create_hierarchy()

    # A clash in this symbol table should return a unique symbol
    for arg in {}, {"shadowing": True}, {"shadowing": False}:
        assert (schedule_symbol_table.next_available_name("symbol1", **arg)
                == "symbol1_1")
    # A clash in an ancestor symbol table will not be checked if
    # shadowing is False
    for arg in {}, {"shadowing": False}:
        assert (schedule_symbol_table.next_available_name("symbol2", **arg)
                == "symbol2_1")
    assert schedule_symbol_table.next_available_name(
        "symbol2", shadowing=True) == "symbol2"
    # A clash with a symbol in a child symbol table is not checked
    for arg in {}, {"shadowing": True}, {"shadowing": False}:
        assert (container_symbol_table.next_available_name("symbol1", **arg)
                == "symbol1")


def test_add_1():
    '''Test that the add method inserts new symbols in the symbol table,
    but raises appropriate errors when provided with an invalid symbol
    or duplicate declaration.

    '''
    sym_table = SymbolTable()

    # Declare a symbol
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    array_type = ArrayType(REAL_TYPE, [5, 1])
    sym_table.add(DataSymbol("var1", array_type,
                             interface=ImportInterface(my_mod)))
    assert sym_table._symbols["my_mod"].name == "my_mod"
    var1_symbol = sym_table._symbols["var1"]
    assert var1_symbol.name == "var1"
    assert (var1_symbol.datatype.intrinsic ==
            ScalarType.Intrinsic.REAL)
    assert (var1_symbol.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    var1_datatype = var1_symbol.datatype
    assert len(var1_datatype.shape) == 2
    assert isinstance(var1_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(var1_datatype.shape[0].upper, Literal)
    assert var1_datatype.shape[0].upper.value == "5"
    assert (var1_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (var1_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(var1_datatype.shape[1], ArrayType.ArrayBounds)
    assert isinstance(var1_datatype.shape[1].upper, Literal)
    assert var1_datatype.shape[1].upper.value == "1"
    assert (var1_datatype.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (var1_datatype.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert var1_symbol.interface.container_symbol == my_mod

    # Declare a duplicate name symbol
    with pytest.raises(KeyError) as error:
        sym_table.add(DataSymbol("var1", REAL_TYPE))
    assert ("Symbol table already contains a symbol with name "
            "'var1'.") in str(error.value)
    with pytest.raises(KeyError) as error:
        sym_table.add(DataSymbol("vAR1", REAL_TYPE))
    assert ("Symbol table already contains a symbol with name "
            "'vAR1'.") in str(error.value)

    # Test that an exception is raised if a non-symbol is added
    with pytest.raises(InternalError) as error:
        sym_table.add("string-not-symbol")
    assert "Symbol 'string-not-symbol' is not a symbol, but 'str'" in \
        str(error.value)


def test_add_with_tags_1():
    '''Test that the add method with a tag inserts new symbols in the symbol
    table and raises appropriate errors.'''
    sym_table = SymbolTable()

    sym1 = Symbol("symbol_notag")
    sym2 = Symbol("symbol_tag1")
    sym3 = Symbol("symbol_tag2")
    sym_table.add(sym1)
    assert not sym_table._tags  # No tag added if none given
    sym_table.add(sym2, tag="tag1")
    sym_table.add(sym3, tag="tag2")

    assert len(sym_table._symbols) == 3
    assert len(sym_table._tags) == 2
    assert "tag1" in sym_table._tags
    assert sym_table._tags["tag1"] == sym2
    assert "tag2" in sym_table._tags
    assert sym_table._tags["tag2"] == sym3

    with pytest.raises(KeyError) as error:
        sym_table.add(DataSymbol("var1", REAL_TYPE), tag="tag1")
    assert ("This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'tag1' for the symbol 'symbol_tag1', "
            "so it can not be associated with symbol 'var1'."
            in str(error.value))


def test_add_with_tags_hierachical():
    '''Check that add(tag=xxx) in a symbol_table hierarchy works as
    expected.

    '''
    schedule_symbol_table, container_symbol_table = create_hierarchy()
    symbol1 = schedule_symbol_table.lookup("symbol1")
    symbol3 = DataSymbol("symbol3", INTEGER_TYPE)

    # A clash of tags in this symbol table should raise an exception.
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.add(symbol3, tag="symbol1_tag")
        assert (
            "This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'symbol1_tag' for the symbol 'symbol1: "
            "DataSymbol<Scalar<INTEGER, UNDEFINED>, Local>', so it can not be "
            "associated with symbol 'symbol3'." in str(info.value))
    # A clash of tags in an ancestor symbol table should raise an exception.
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.add(symbol3, tag="symbol2_tag")
        assert (
            "This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'symbol2_tag' for the symbol 'symbol2: "
            "DataSymbol<Scalar<INTEGER, UNDEFINED>, Local>', so it can not be "
            "associated to symbol 'symbol3'." in str(info.value))
    # A clash of tags with a child symbol table is not checked for now.
    container_symbol_table.add(symbol1, tag="symbol1_tag")


def test_symbols_imported_from():
    ''' Test the Symbol Table symbols_imported_from() method. '''
    sym_table = SymbolTable()
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    assert sym_table.symbols_imported_from(my_mod) == []
    var1 = DataSymbol("var1", REAL_TYPE, interface=AutomaticInterface())
    sym_table.add(var1)
    assert sym_table.symbols_imported_from(my_mod) == []
    var2 = DataSymbol("var2", INTEGER_TYPE,
                      interface=ImportInterface(my_mod))
    assert sym_table.symbols_imported_from(my_mod) == []
    sym_table.add(var2)
    assert sym_table.symbols_imported_from(my_mod) == [var2]
    var3 = DataSymbol("var3", INTEGER_TYPE,
                      interface=ImportInterface(my_mod))
    sym_table.add(var3)
    imported_symbols = sym_table.symbols_imported_from(my_mod)
    assert var3 in imported_symbols
    assert var2 in imported_symbols
    # Passing something that is not a ContainerSymbol is an error
    with pytest.raises(TypeError) as err:
        sym_table.symbols_imported_from(var2)
    assert "expects a ContainerSymbol but got an object of type" in \
        str(err.value)
    # Passing a ContainerSymbol that is not in the SymbolTable is an error
    with pytest.raises(KeyError) as err:
        sym_table.symbols_imported_from(ContainerSymbol("another_mod"))
    assert "Could not find 'another_mod' in " in str(err.value)
    # Passing a ContainerSymbol that is not in the SymbolTable but that has
    # the same name as one that is is an error
    with pytest.raises(KeyError) as err:
        sym_table.symbols_imported_from(ContainerSymbol("my_mod"))
    assert ("The 'my_mod' entry in this SymbolTable is not the supplied "
            "ContainerSymbol" in str(err.value))


def test_remove_genericsymbols():
    ''' Test that the remove method removes generic symbols from the symbol
    table. Also check that it disassociates any existing tags to the symbol'''

    sym_table = SymbolTable()
    symbol_a = Symbol("a")
    symbol_b = Symbol("b")
    symbol_c = Symbol("c")

    sym_table.add(symbol_a, tag="tag1")
    sym_table.add(symbol_b, tag="tag2")

    assert "a" in sym_table
    assert "tag1" in sym_table.tags_dict

    sym_table.remove(symbol_a)

    assert "a" not in sym_table
    assert "tag1" not in sym_table.tags_dict

    # Tag1 can now be reused
    sym_table.add(symbol_c, tag="tag1")


def test_remove_routineymbols():
    ''' Test that the remove method removes RoutineSymbols from the symbol
    table.'''
    sym_table = SymbolTable()
    symbol_a = RoutineSymbol("a")
    sym_table.add(symbol_a)
    assert "a" in sym_table
    sym_table.remove(symbol_a)
    assert "a" not in sym_table


def test_remove_containersymbols():
    '''Test that the remove method removes ContainerSymbols from the symbol
    table. Also checks that appropriate errors are raised when the method is
    provided with wrong parameters or if there are DataSymbols that reference
    the provided ContainerSymbol. '''
    sym_table = SymbolTable()

    # Declare a symbol
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    array_type = ArrayType(REAL_TYPE, [5, 1])
    sym_table.add(DataSymbol("var1", array_type,
                             interface=ImportInterface(my_mod)))
    var1 = sym_table.lookup("var1")
    assert var1
    assert sym_table.symbols_imported_from(my_mod) == [var1]
    # We should not be able to remove a Container if it is referenced
    # by an existing Symbol
    with pytest.raises(ValueError) as err:
        sym_table.remove(my_mod)
    assert ("Cannot remove ContainerSymbol 'my_mod' since symbols "
            "['var1'] are imported from it" in str(err.value))
    # Change the interface on var1
    var1.interface = AutomaticInterface()
    # We should now be able to remove the ContainerSymbol
    sym_table.remove(my_mod)
    with pytest.raises(KeyError) as err:
        sym_table.lookup("my_mod")
    assert "Could not find 'my_mod'" in str(err.value)
    # Attempting to remove it a second time is an error
    with pytest.raises(KeyError) as err:
        sym_table.remove(my_mod)
    assert ("Cannot remove Symbol 'my_mod' from symbol table because it does "
            "not" in str(err.value))
    # Attempting to remove a Symbol that is not in the table but that has
    # the same name as an entry in the table is an error
    sym_table.add(ContainerSymbol("my_mod"))
    with pytest.raises(InternalError) as err:
        sym_table.remove(ContainerSymbol("my_mod"))
    assert ("Symbol with name 'my_mod' in this symbol table is not the "
            "same" in str(err.value))


def test_remove_unsupported_types():
    ''' Test that the remove method raises appropriate errors when trying to
    remove unsupported types.'''
    sym_table = SymbolTable()

    # Attempt to supply something that is not a Symbol
    with pytest.raises(TypeError) as err:
        sym_table.remove("broken")
    assert ("remove() expects a Symbol argument but found: 'str'."
            in str(err.value))

    # We should not be able to remove a Symbol that is not currently supported
    var1 = DataSymbol("var1", REAL_TYPE)
    sym_table.add(var1)
    with pytest.raises(NotImplementedError) as err:
        sym_table.remove(var1)
    assert ("remove() currently only supports generic Symbol, ContainerSymbol "
            "and RoutineSymbol types but got: 'DataSymbol'" in str(err.value))


@pytest.mark.parametrize("sym_name", ["var1", "vAr1", "VAR1"])
def test_remove_case_insensitive(sym_name):
    ''' Check that the remove method works, irrespective of the case of the
    Symbol name. '''
    sym_table = SymbolTable()
    symbol1 = Symbol(sym_name)
    sym_table.add(symbol1)
    assert "var1" in sym_table
    sym_table.remove(symbol1)
    assert "var1" not in sym_table


def test_swap_symbol():
    ''' Test the SymbolTable.swap() method. '''
    symbol1 = Symbol("var1")
    sym_table = SymbolTable()
    sym_table.add(symbol1)
    # Test the checks on argument types.
    with pytest.raises(TypeError) as err:
        sym_table.swap(symbol1, "var2")
    assert ("Symbol to add must be of type Symbol but got 'str'" in
            str(err.value))
    with pytest.raises(TypeError) as err:
        sym_table.swap("var2", symbol1)
    assert ("Symbol to remove must be of type Symbol but got 'str'" in
            str(err.value))
    # Test that we reject attempts to swap symbols with different names.
    symbol2 = DataSymbol("var2", INTEGER_TYPE, initial_value=6)
    with pytest.raises(SymbolError) as err:
        sym_table.swap(symbol1, symbol2)
    assert ("Cannot swap symbols that have different names, got: 'var1' and "
            "'var2'" in str(err.value))
    # Finally, check that the method correctly adds the new symbol to the
    # table and removes the old one (even if the case of the name of the
    # new symbol differs from the original).
    symbol3 = DataSymbol("Var1", REAL_TYPE)
    sym_table.swap(symbol1, symbol3)
    assert sym_table.lookup("var1") is symbol3
    assert symbol1 not in sym_table._symbols


def test_check_for_clashes_imports():
    '''Test the check_for_clashes method for two tables that import the same
    symbol from different tables.'''
    table1 = SymbolTable()
    table2 = SymbolTable()
    csym1 = ContainerSymbol("ford")
    table1.add(csym1)
    csym2 = ContainerSymbol("Ford")
    table2.add(csym2)
    clash1 = DataSymbol("Prefect", INTEGER_TYPE,
                        interface=ImportInterface(csym1))
    table1.add(clash1)
    clash2 = DataSymbol("prefect", INTEGER_TYPE,
                        interface=ImportInterface(csym2))
    table2.add(clash2)
    # No clash as the containers are the same, just with different
    # capitalisation.
    table1.check_for_clashes(table2)
    # Now create a clash between variables that have different capitalisation.
    csym3 = ContainerSymbol("arthur")
    table1.add(csym3)
    clash3 = DataSymbol("dent", INTEGER_TYPE, interface=ImportInterface(csym3))
    table1.add(clash3)
    clash4 = DataSymbol("DENT", INTEGER_TYPE, interface=ImportInterface(csym2))
    table2.add(clash4)
    with pytest.raises(SymbolError) as err:
        table1.check_for_clashes(table2)
    assert ("This table has an import of 'dent' from Container 'arthur' but "
            "the supplied table imports it from Container 'Ford'." in
            str(err.value))


def test_check_for_clashes_cannot_rename():
    '''Test the check_for_clashes() method works as expected when a name clash
    cannot be resolved by renaming.'''
    table1 = SymbolTable()
    table2 = SymbolTable()
    csym1 = ContainerSymbol("vogon")
    table1.add(csym1)
    table1.add(DataSymbol("slab", INTEGER_TYPE))
    csym2 = ContainerSymbol("fleet")
    table2.add(csym2)
    table2.add(DataSymbol("slab", UnresolvedType(),
                          interface=ImportInterface(csym2)))
    # 'slab' in table1 can be renamed.
    table1.check_for_clashes(table2)
    # Add another clash where one symbol is imported and the other cannot
    # be renamed because it is a routine argument.
    table1.add(DataSymbol("prostetnic", INTEGER_TYPE,
                          interface=ArgumentInterface()))
    table2.add(DataSymbol("prostetnic", UnresolvedType(),
                          interface=ImportInterface(csym2)))
    for (tab1, tab2) in [(table1, table2), (table2, table1)]:
        with pytest.raises(SymbolError) as err:
            tab1.check_for_clashes(tab2)
        assert ("for symbol 'prostetnic' that cannot be resolved by renaming "
                "one of the instances because:" in str(err.value))
        assert ("- PSyclone SymbolTable error: Cannot rename symbol "
                "'prostetnic' because it is imported (from Container 'fleet')"
                in str(err.value))
        assert ("- PSyclone SymbolTable error: Cannot rename symbol "
                "'prostetnic' because it is a routine argument and as such "
                "may be named in a Call." in str(err.value))
    # Add a clash between two symbols where neither is a Container or has an
    # ImportInterface.
    del table1._symbols["prostetnic"]
    table1.add(DataSymbol("jeltz", INTEGER_TYPE,
                          interface=ArgumentInterface()))
    table2.add(DataSymbol("jeltz", INTEGER_TYPE,
                          interface=ArgumentInterface()))
    with pytest.raises(SymbolError) as err:
        table1.check_for_clashes(table2)
    assert ("Cannot rename symbol 'jeltz' because it is a routine argument "
            "and as such may be named in a Call." in str(err.value))


def test_table_merge():
    ''' Test the SymbolTable.merge method. '''
    table1 = SymbolTable()
    table2 = SymbolTable()
    # Argument must be a table.
    with pytest.raises(TypeError) as err:
        table1.merge("zaphod")
    assert ("merge() expects a SymbolTable instance but got 'str'" in
            str(err.value))
    # Can merge empty tables.
    table1.merge(table2)
    assert not table1._symbols
    # Simple merge.
    table2.add(DataSymbol("beeblebrox", INTEGER_TYPE))
    # 'Own' routine symbol excluded.
    table2.add(RoutineSymbol("dent"), tag="own_routine_symbol")
    # Precision symbol should be included.
    wp_sym = DataSymbol("wp", INTEGER_TYPE, is_constant=True, initial_value=8)
    table2.add(wp_sym)
    table2.add(DataSymbol("marvin", ScalarType(ScalarType.Intrinsic.REAL,
                                               wp_sym)))
    table1.merge(table2)
    assert table1.lookup("beeblebrox")
    assert "dent" not in table1
    assert "marvin" in table1
    assert "wp" in table1
    # Different symbols with a name clash. This results in the Symbol in the
    # second table being renamed (as that preserves any references to it).
    table1 = SymbolTable()
    table2 = SymbolTable()
    table1.add(DataSymbol("theclash", INTEGER_TYPE))
    table2.add(DataSymbol("theclash", INTEGER_TYPE))
    table1.merge(table2)
    assert len(table1._symbols) == 2
    assert table1.lookup("theclash_1") is table2.lookup("theclash_1")
    # Arguments. By default they are included in a merge.
    table3 = SymbolTable()
    arg_sym = DataSymbol("trillian", INTEGER_TYPE,
                         interface=ArgumentInterface())
    table3.add(arg_sym)
    table3.specify_argument_list([arg_sym])
    table1.merge(table3)
    assert table1.lookup("trillian") is arg_sym
    # Check that arguments are ignored if requested.
    table4 = SymbolTable()
    arg_sym2 = DataSymbol("arthur", INTEGER_TYPE,
                          interface=ArgumentInterface())
    table4.add(arg_sym2)
    table4.specify_argument_list([arg_sym2])
    table1.merge(table4, include_arguments=False)
    assert "arthur" not in table1


def test_merge_container_syms():
    '''Test the merge method works as expected when the tables have
    ContainerSymbols.

    '''
    tab1 = SymbolTable()
    tab2 = SymbolTable()
    csym1 = ContainerSymbol("slartibartfast")
    tab2.add(csym1)
    wpsym = DataSymbol("wp", INTEGER_TYPE, interface=ImportInterface(csym1))
    tab2.add(wpsym)
    tab1.merge(tab2)
    assert "slartibartfast" in tab1
    assert "wp" in tab1
    # A second table which also imports wp as well as dp.
    tab3 = SymbolTable()
    csym2 = ContainerSymbol("slartibartfast")
    tab3.add(csym2)
    wpsym2 = DataSymbol("wp", INTEGER_TYPE, interface=ImportInterface(csym2))
    tab3.add(wpsym2)
    dpsym = DataSymbol("dp", INTEGER_TYPE,
                       interface=ImportInterface(csym2,
                                                 orig_name="different_name"))
    tab3.add(dpsym)
    tab1.merge(tab3)
    wp3 = tab1.lookup("wp")
    assert wp3.interface.container_symbol.name == "slartibartfast"
    dp3 = tab1.lookup("dp")
    assert dp3.interface.container_symbol.name == "slartibartfast"
    # Check that the dp import renaming is conserved
    assert dp3.interface.orig_name == "different_name"
    # A third table which imports wp from a *different* container.
    tab4 = SymbolTable()
    csym3 = ContainerSymbol("magrathea")
    tab4.add(csym3)
    wpsym3 = DataSymbol("wp", INTEGER_TYPE, interface=ImportInterface(csym3))
    tab4.add(wpsym3)
    with pytest.raises(SymbolError) as err:
        tab1.merge(tab4)
    err_txt = str(err.value)
    assert "Cannot merge Symbol Table:" in err_txt
    assert "due to unresolvable name clashes." in err_txt


def test_add_container_symbols_from_table():
    '''Test that the _add_container_symbols_from_table method copies Container
    symbols into the current table and updates any import interfaces.'''
    table1 = SymbolTable()
    table2 = SymbolTable()
    csym = ContainerSymbol("ford")
    # Put a Container symbol named 'arthur' in both tables.
    csym2 = ContainerSymbol("arthur")
    csym3 = csym2.copy()
    # The one in table2 will have a wildcard import.
    # pylint: disable=no-member
    csym2.wildcard_import = True
    table1.add(csym3)
    aclash = DataSymbol("aclash", INTEGER_TYPE)
    table1.add(aclash)
    asym = DataSymbol("prefect", INTEGER_TYPE, interface=ImportInterface(csym))
    bsym = DataSymbol("dent", INTEGER_TYPE, interface=ImportInterface(csym2))
    table2.add(csym)
    table2.add(csym2)
    table2.add(asym)
    table2.add(bsym)
    # Add a ContainerSymbol that will clash with a DataSymbol in the first
    # table.
    cclash = ContainerSymbol("aclash")
    # Add an import of a Symbol that will also clash with a DataSymbol in
    # the first table. As it is imported, we can't (currently) rename it so
    # we rename the Symbol in the first table.
    bclash = DataSymbol("bclash", INTEGER_TYPE,
                        interface=ImportInterface(cclash))
    table2.add(cclash)
    table2.add(bclash)
    bclash_in_1 = DataSymbol("bclash", INTEGER_TYPE)
    table1.add(bclash_in_1)
    table1._add_container_symbols_from_table(table2)
    assert table1.lookup("ford") is csym
    # The 'arthur' symbol object should still be the one originally in table1.
    # However, it should now have a wildcard import.
    assert table1.lookup("arthur") is csym3
    assert csym3.wildcard_import
    # Check that the import interface for a symbol in the second table has been
    # updated to point to the container in the first table.
    assert table2.lookup("dent").interface.container_symbol is csym3
    assert table1.lookup("aclash") is cclash
    # The original symbols should still be in the table but renamed.
    assert aclash in table1.symbols
    assert aclash.name != "aclash"
    assert bclash_in_1 in table1.symbols
    assert bclash_in_1.name != "bclash"


def test_add_symbols_from_table():
    '''Test for the 'internal' _add_symbols_from_table() method.'''
    table1 = SymbolTable()
    table2 = SymbolTable()
    csym = ContainerSymbol("ford")
    csym2 = csym.copy()
    table1.add(csym)
    table2.add(csym2)
    table1.add(DataSymbol("prefect", INTEGER_TYPE,
                          interface=ImportInterface(csym2)))
    table2.add(DataSymbol("prefect", INTEGER_TYPE,
                          interface=ImportInterface(csym2)))
    with pytest.raises(InternalError) as err:
        table1._add_symbols_from_table(table2)
    assert ("Symbol 'prefect' imported from 'ford' has not been updated to "
            "refer to the corresponding container in the current table."
            in str(err.value))


def test_swap_symbol_properties():
    ''' Test the symboltable swap_properties method '''
    # pylint: disable=too-many-statements

    symbol1 = DataSymbol("var1", INTEGER_TYPE, is_constant=True,
                         initial_value=7)
    symbol2 = DataSymbol("dim1", INTEGER_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READ))
    symbol3 = DataSymbol("dim2", INTEGER_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READ))
    array_type = ArrayType(REAL_TYPE, [Reference(symbol2), Reference(symbol3)])
    symbol4 = DataSymbol("var2", array_type,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READWRITE))
    sym_table = SymbolTable()
    sym_table.add(symbol1)

    # Raise exception if the first argument is not a symbol
    with pytest.raises(TypeError) as excinfo:
        sym_table.swap_symbol_properties(None, symbol1)
    assert ("Arguments should be of type 'Symbol' but found 'NoneType'."
            "") in str(excinfo.value)

    # Raise exception if the second argument is not a symbol
    with pytest.raises(TypeError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, "symbol")
    assert ("Arguments should be of type 'Symbol' but found 'str'."
            "") in str(excinfo.value)

    # Raise exception if the first symbol does not exist in the symbol table
    with pytest.raises(KeyError) as excinfo:
        sym_table.swap_symbol_properties(symbol4, symbol1)
    assert "Symbol 'var2' is not in the symbol table." in str(excinfo.value)

    # Raise exception if the second symbol does not exist in the symbol table
    with pytest.raises(KeyError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, symbol4)
    assert "Symbol 'var2' is not in the symbol table." in str(excinfo.value)

    # Raise exception if both symbols have the same name. The only way this
    # can currently occur is if they are the same symbol (as the normalised
    # symbol name is used as the key in the symbol table).
    with pytest.raises(ValueError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, symbol1)
    assert ("The symbols should have different names, but found 'var1' for "
            "both.") in str(excinfo.value)

    sym_table.add(symbol2)
    sym_table.add(symbol3)
    sym_table.add(symbol4)
    sym_table.specify_argument_list([symbol2, symbol3, symbol4])

    # Check that properties are swapped
    sym_table.swap_symbol_properties(symbol1, symbol4)

    assert symbol1.name == "var1"
    assert symbol1.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert symbol1.datatype.precision == ScalarType.Precision.UNDEFINED
    assert len(symbol1.datatype.shape) == 2
    assert symbol1.datatype.shape[0].upper.symbol == symbol2
    assert symbol1.datatype.shape[1].upper.symbol == symbol3
    assert symbol1.is_argument
    assert symbol1.initial_value is None
    assert symbol1.interface.access == ArgumentInterface.Access.READWRITE

    assert symbol4.name == "var2"
    assert symbol4.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert symbol4.datatype.precision == ScalarType.Precision.UNDEFINED
    assert not symbol4.shape
    assert symbol4.is_static
    assert symbol4.initial_value.value == "7"
    assert (symbol4.initial_value.datatype.intrinsic ==
            symbol4.datatype.intrinsic)
    assert (symbol4.initial_value.datatype.precision ==
            symbol4.datatype.precision)

    # Check symbol references are unaffected
    sym_table.swap_symbol_properties(symbol2, symbol3)
    assert symbol1.shape[0].upper.name == "dim1"
    assert symbol1.shape[1].upper.name == "dim2"

    # Check argument positions are updated. The original positions
    # were [dim1, dim2, var2]. They should now be [dim2, dim1, var1]
    assert sym_table.argument_list[0].name == "dim2"
    assert sym_table.argument_list[1].name == "dim1"
    assert sym_table.argument_list[2].name == "var1"


def test_lookup_1():
    '''Test that the lookup method retrieves symbols from the current
    symbol table if the name exists, otherwise it raises an error.

    '''
    sym_table = SymbolTable()
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                       ArrayType.Extent.ATTRIBUTE])

    sym_table.add(DataSymbol("var1", array_type))
    sym_table.add(DataSymbol("var2", INTEGER_TYPE))
    sym_table.add(DataSymbol("vAR3", REAL_TYPE))

    assert isinstance(sym_table.lookup("var1"), DataSymbol)
    assert sym_table.lookup("var1").name == "var1"
    assert sym_table.lookup("vAR1").name == "var1"
    assert isinstance(sym_table.lookup("var2"), DataSymbol)
    assert sym_table.lookup("var2").name == "var2"
    assert isinstance(sym_table.lookup("var3"), DataSymbol)
    assert sym_table.lookup("var3").name == "vAR3"

    with pytest.raises(KeyError) as error:
        sym_table.lookup("notdeclared")
    assert "Could not find 'notdeclared' in the Symbol Table." in \
        str(error.value)


def test_lookup_2():
    '''Test the visibility argument filtering functionality of the
    lookup() method.

    '''
    sym_table = SymbolTable()
    sym1 = Symbol("var1")
    sym_table.add(sym1)
    sym_table.add(Symbol("var2", visibility=Symbol.Visibility.PRIVATE))
    sym3 = Symbol("var3", visibility=Symbol.Visibility.PUBLIC)
    sym_table.add(sym3)
    # Default visibility is PUBLIC
    assert (sym_table.lookup("var1", visibility=Symbol.Visibility.PUBLIC)
            is sym1)
    assert (sym_table.lookup("var3", visibility=[Symbol.Visibility.PUBLIC])
            is sym3)
    # Check method accepts a list of visibilities
    assert (sym_table.lookup("var1", visibility=[Symbol.Visibility.PUBLIC])
            is sym1)
    assert (sym_table.lookup("var1", visibility=[Symbol.Visibility.PRIVATE,
                                                 Symbol.Visibility.PUBLIC])
            is sym1)
    # Check we get the expected error if the symbol exists but doesn't
    # have the requested visibility
    with pytest.raises(SymbolError) as err:
        sym_table.lookup("var2", visibility=Symbol.Visibility.PUBLIC)
    assert ("'var2' exists in the Symbol Table but has visibility 'PRIVATE' "
            "which does not" in str(err.value))
    # Pass an incorrect type for the visibility argument
    with pytest.raises(TypeError) as err:
        sym_table.lookup("var2", visibility="PUBLIC")
    assert ("the 'visibility' argument to lookup() must be an instance (or "
            "list of instances) of Symbol.Visibility but got 'str' when "
            "searching for symbol 'var2'" in str(err.value))


def test_lookup_3():
    '''Check that lookup() in the SymbolTable class raises the expected
    exception if the name argument has the wrong type.

    '''
    sym_table = SymbolTable()
    symbol = DataSymbol("var1", REAL_TYPE)
    with pytest.raises(TypeError) as info:
        _ = sym_table.lookup(symbol)
    assert ("Expected the name argument to the lookup() method to be a str "
            "but found 'DataSymbol'." in str(info.value))


def test_lookup_4():
    '''Check that lookup() in the SymbolTable class behaves as
    expected with the scope_limit argument.

    '''
    schedule_symbol_table, container_symbol_table = create_hierarchy()
    scope = schedule_symbol_table.node
    symbol1 = schedule_symbol_table.lookup("symbol1")
    symbol2 = container_symbol_table.lookup("symbol2")

    # raise an exception if the symbol is not found
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.lookup("does-not-exist")
    assert ("Could not find 'does-not-exist' in the Symbol Table."
            in str(info.value))
    # The symbol is in an ancestor symbol table. This will not be
    # found if the lookup scope_limit is below this ancestor.
    for arg in {}, {"scope_limit": None}:
        assert schedule_symbol_table.lookup(symbol2.name, **arg) is symbol2
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.lookup(symbol2.name, scope_limit=scope)
    assert ("Could not find 'symbol2' in the Symbol Table."
            in str(info.value))
    # A symbol in a child symbol table will not be found
    for arg in {}, {"scope_limit": None}, {"scope_limit": scope}:
        with pytest.raises(KeyError) as info:
            container_symbol_table.lookup(symbol1.name, **arg)
        assert ("Could not find 'symbol1' in the Symbol Table."
                in str(info.value))


def test_lookup_with_tag_1():
    '''Test that the lookup_with_tag method retrieves symbols from the symbol
    table if the tag exists, otherwise it raises an error.'''
    sym_table = SymbolTable()

    sym1 = Symbol("symbol_notag")
    sym2 = Symbol("symbol_tag1")
    sym3 = Symbol("symbol_tag2")
    sym_table.add(sym1)
    sym_table.add(sym2, tag="tag1")
    sym_table.add(sym3, tag="tag2")

    assert sym_table.lookup_with_tag("tag1").name == "symbol_tag1"
    assert sym_table.lookup_with_tag("tag2").name == "symbol_tag2"

    with pytest.raises(KeyError) as error:
        sym_table.lookup_with_tag("symbol_tag1")
    assert "Could not find the tag 'symbol_tag1' in the Symbol Table." in \
        str(error.value)


def test_lookup_with_tag_2():
    '''Check that lookup_with_tag() in the SymbolTable class raises the
    expected exception if the tag argument has the wrong type.

    '''
    sym_table = SymbolTable()
    with pytest.raises(TypeError) as info:
        _ = sym_table.lookup_with_tag(None)
    assert (
        "Expected the tag argument to the lookup_with_tag() method to be a "
        "str but found 'NoneType'." in str(info.value))


def test_lookup_with_tag_3():
    '''Check that lookup_with_tag() in the SymbolTable class behaves as
    expected with the scope_limit argument.

    '''
    schedule_symbol_table, container_symbol_table = create_hierarchy()
    scope = schedule_symbol_table.node
    symbol2 = container_symbol_table.lookup("symbol2")

    # raise an exception if the tag is not found
    for arg in {}, {"scope_limit": None}, {"scope_limit": scope}:
        with pytest.raises(KeyError) as info:
            schedule_symbol_table.lookup_with_tag("does-not-exist", **arg)
            assert ("Could not find the tag 'does-not-exist' in the Symbol "
                    "Table." in str(info.value))
    # The tag is in an ancestor symbol table. This will not be found if
    # scope_limit is the current scope
    for arg in {}, {"scope_limit": None}:
        assert (schedule_symbol_table.lookup_with_tag(
            "symbol2_tag", **arg) is symbol2)
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.lookup_with_tag(
            "symbol2_tag", scope_limit=scope)
    assert ("Could not find the tag 'symbol2_tag' in the Symbol Table."
            in str(info.value))
    # The tag is in a child symbol table so will not be found
    for arg in {}, {"scope_limit": None}, {"scope_limit": scope}:
        with pytest.raises(KeyError) as info:
            container_symbol_table.lookup_with_tag("symbol1_tag", **arg)
            assert ("Could not find the tag 'symbol1_tag' in the Symbol Table."
                    in str(info.value))


def test_has_wildcard_imports():
    ''' Test the has_wildcard_imports() method. '''
    sched_table, container_table = create_hierarchy()
    # We have no wildcard imports initially
    assert sched_table.has_wildcard_imports() is False
    assert container_table.has_wildcard_imports() is False
    csym = ContainerSymbol("some_mod")
    container_table.add(csym)
    # Adding a container symbol without a wildcard import has no effect
    assert container_table.has_wildcard_imports() is False
    # Now give it a wildcard import
    csym.wildcard_import = True
    assert container_table.has_wildcard_imports() is True
    assert sched_table.has_wildcard_imports() is True


def test_view():
    '''Test the view method of the SymbolTable class, it should return a
    representation of the full SymbolTable.'''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", INTEGER_TYPE))
    output = sym_table.view()
    assert "Symbol Table:\n" in output
    assert "var1" in output
    assert "var2" in output


def test_can_be_printed():
    '''Test that a SymbolTable instance can always be printed. (i.e. is
    initialised fully)'''
    sym_table = SymbolTable()
    my_mod = Container("my_mod")
    ex_mod = ContainerSymbol("external_mod")
    sym_table.add(ex_mod)
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", INTEGER_TYPE))
    sym_table.add(DataSymbol("var3", UnresolvedType(),
                             interface=ImportInterface(ex_mod)))

    sym_table_text = str(sym_table)
    # Check default header is generated
    assert "Symbol Table:\n" in sym_table_text
    assert "-------------\n" in sym_table_text

    # Check that it has the strings generated by the contained symbols
    assert str(sym_table.lookup("var1")) in sym_table_text
    assert str(sym_table.lookup("var2")) in sym_table_text
    assert str(sym_table.lookup("var3")) in sym_table_text

    # Checks more complex headers are also generated
    sym_table._node = my_mod
    sym_table_text = str(sym_table)
    assert "Symbol Table of Container 'my_mod':\n" in sym_table_text
    assert "-----------------------------------\n" in sym_table_text


def test_specify_argument_list():
    '''Test that the specify argument list method sets the argument_list
    with references to each DataSymbol and updates the DataSymbol attributes
    when needed.'''
    sym_table = SymbolTable()
    sym_v1 = DataSymbol("var1", REAL_TYPE)
    sym_table.add(sym_v1)
    sym_table.add(DataSymbol("var2", REAL_TYPE))
    sym_v1.interface = ArgumentInterface(ArgumentInterface.Access.UNKNOWN)
    sym_table.specify_argument_list([sym_v1])

    assert len(sym_table.argument_list) == 1
    assert sym_table.argument_list[0].is_argument
    assert sym_table.argument_list[0].interface.access == \
        ArgumentInterface.Access.UNKNOWN

    # Test that repeated calls still produce a valid argument list
    sym_table.specify_argument_list([sym_v1])
    assert len(sym_table.argument_list) == 1

    # Check that specifying the Interface allows us to specify how
    # the argument is accessed
    sym_v2 = sym_table.lookup("var2")
    sym_v2.interface = ArgumentInterface(ArgumentInterface.Access.READWRITE)
    sym_table.specify_argument_list([sym_v1, sym_v2])
    assert sym_table.argument_list[1].is_argument
    assert sym_table.argument_list[1].interface.access == \
        ArgumentInterface.Access.READWRITE


def test_specify_arg_list_errors():
    ''' Check that supplying specify_argument_list() with DataSymbols that
    don't have the correct Interface information raises the expected
    errors. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", REAL_TYPE))
    sym_v1 = sym_table.lookup("var1")
    # Attempt to say the argument list consists of "var1" which at this
    # point is just a local variable.
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err.value)
    assert "has an interface of type '" in str(err.value)
    # Now add an Interface for "var1" but of the wrong type
    sym_v1.interface = ImportInterface(ContainerSymbol("my_mod"))
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err.value)
    assert "has an interface of type '" in str(err.value)


def test_argument_list_errors():
    ''' Tests the internal sanity checks of the SymbolTable.argument_list
    property. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", REAL_TYPE))
    sym_table.add(DataSymbol("var3", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    # Manually put a local symbol into the internal list of arguments
    sym_table._argument_list = [sym_table.lookup("var1")]
    with pytest.raises(ValueError) as err:
        sym_table._validate_arg_list(sym_table._argument_list)
    pattern = ("Symbol \'var1.*\' is listed as a kernel argument but has an "
               "interface of type .* rather than ArgumentInterface")
    assert re.search(pattern, str(err.value)) is not None
    # Check that the argument_list property converts this error into an
    # InternalError
    with pytest.raises(InternalError) as err:
        _ = sym_table.argument_list
    assert re.search(pattern, str(err.value)) is not None
    # Check that we reject a symbol imported from a module
    with pytest.raises(ValueError) as err:
        sym_table._validate_arg_list([sym_table.lookup("var3")])
    # Manually put that symbol into the argument list
    sym_table._argument_list = [sym_table.lookup("var3")]
    pattern = (r"Symbol \'var3.*\' is listed as a kernel argument but has an "
               r"interface of type")
    assert re.search(pattern, str(err.value)) is not None
    # Check that the argument_list property converts this error into an
    # InternalError
    with pytest.raises(InternalError) as err:
        _ = sym_table.argument_list
    assert re.search(pattern, str(err.value)) is not None
    # Check that we get the expected TypeError if we provide a list containing
    # objects that are not Symbols
    with pytest.raises(TypeError) as err:
        sym_table._validate_arg_list(["Not a symbol"])
    assert "Expected a list of DataSymbols but found an object of type" \
        in str(err.value)


def test_validate_non_args():
    ''' Checks for the validation of non-argument entries in the
    SymbolTable. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", REAL_TYPE))
    sym_table.add(DataSymbol("var3", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    # Everything should be fine so far
    sym_table._validate_non_args()
    # Add an entry with an Argument interface
    sym_table.add(DataSymbol("var4", REAL_TYPE,
                             interface=ArgumentInterface()))
    # Since this symbol isn't in the argument list, the SymbolTable
    # is no longer valid
    with pytest.raises(ValueError) as err:
        sym_table._validate_non_args()
    pattern = (r"Symbol 'var4.* is not listed as a kernel argument and yet "
               "has an ArgumentInterface interface")
    assert re.search(pattern, str(err.value)) is not None


def test_contains():
    '''Test that the __contains__ method returns True if the given name
    is in the SymbolTable, otherwise returns False.'''
    sym_table = SymbolTable()

    sym_table.add(DataSymbol("var1", REAL_TYPE))
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    sym_table.add(DataSymbol("var2", array_type))

    assert "var1" in sym_table
    assert "var2" in sym_table
    assert "vAR2" in sym_table
    assert "var3" not in sym_table


def test_symbols():
    '''Test that the symbols property returns a list of the symbols in the
    SymbolTable.'''
    sym_table = SymbolTable()
    assert sym_table.symbols == []
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    sym_table.add(DataSymbol("var2", array_type))
    assert len(sym_table.symbols) == 2
    sym_table.add(DataSymbol("var3", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    assert len(sym_table.symbols) == 3


def test_automatic_datasymbols():
    '''Test that the automatic_datasymbols property returns a list with the
    symbols with local scope.'''
    sym_table = SymbolTable()
    assert [] == sym_table.automatic_datasymbols

    sym_table.add(DataSymbol("var1", REAL_TYPE))
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    sym_table.add(DataSymbol("var2", array_type))
    sym_table.add(DataSymbol("var3", REAL_TYPE))

    assert len(sym_table.automatic_datasymbols) == 3
    assert sym_table.lookup("var1") in sym_table.automatic_datasymbols
    assert sym_table.lookup("var2") in sym_table.automatic_datasymbols
    assert sym_table.lookup("var3") in sym_table.automatic_datasymbols
    sym_v1 = sym_table.lookup("var1")
    sym_v1.interface = ArgumentInterface(ArgumentInterface.Access.READWRITE)
    sym_table.specify_argument_list([sym_v1])

    assert len(sym_table.automatic_datasymbols) == 2
    assert sym_table.lookup("var1") not in sym_table.automatic_datasymbols
    assert sym_table.lookup("var2") in sym_table.automatic_datasymbols
    assert sym_table.lookup("var3") in sym_table.automatic_datasymbols

    sym_table.add(DataSymbol("var4", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    assert len(sym_table.automatic_datasymbols) == 2
    assert sym_table.lookup("var4") not in sym_table.automatic_datasymbols


def test_argument_datasymbols():
    ''' Test that the argument_datasymbols property returns a list of the
    correct symbols. '''
    sym_table = SymbolTable()
    assert sym_table.argument_datasymbols == []
    var1 = DataSymbol("var1", REAL_TYPE, interface=ArgumentInterface())
    sym_table.add(var1)
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    var2 = DataSymbol("var2", array_type, interface=ArgumentInterface())
    sym_table.add(var2)
    sym_table.add(DataSymbol("var3", REAL_TYPE))
    sym_table.specify_argument_list([var1, var2])
    assert sym_table.argument_datasymbols == [var1, var2]


def test_datatypesymbols():
    ''' Test that the datatypesymbols property returns a list of the
    correct symbols. '''
    sym_table = SymbolTable()
    assert sym_table.datatypesymbols == []
    region_type = StructureType.create([
        ("startx", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    region_sym = DataTypeSymbol("region_type", region_type)
    sym_table.add(region_sym)
    # Add other symbol types
    csym = ContainerSymbol("my_mod")
    sym_table.add(csym)
    # These should not appear as datatypesymbols
    assert sym_table.datatypesymbols == [region_sym]


def test_imported_symbols():
    '''Test that the imported_symbols property returns those Symbols with
    'global' scope (i.e. that represent data/code that exists outside
    the current scoping unit) and are not routine arguments.

    '''
    sym_table = SymbolTable()
    assert sym_table.imported_symbols == []
    # Add some local symbols
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    sym_table.add(DataSymbol("var2", array_type))
    assert sym_table.imported_symbols == []
    # Add a global symbol
    sym_table.add(DataSymbol("gvar1", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    assert sym_table.lookup("gvar1") in sym_table.imported_symbols
    sym_table.add(
        DataSymbol("gvar2", REAL_TYPE,
                   interface=ArgumentInterface(
                       ArgumentInterface.Access.READWRITE)))
    gsymbols = sym_table.imported_symbols
    assert len(gsymbols) == 1
    assert sym_table.lookup("gvar2") not in gsymbols
    # Add another global symbol
    sym_table.add(RoutineSymbol("my_sub", INTEGER_TYPE,
                                interface=ImportInterface(
                                    ContainerSymbol("my_mod"))))
    assert sym_table.lookup("my_sub") in sym_table.imported_symbols
    assert len(sym_table.imported_symbols) == 2


def test_unresolved_datasymbols():
    ''' Tests for the unresolved_datasymbols method. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("s1", INTEGER_TYPE))
    # Check that we get an empty list if everything is defined
    assert sym_table.unresolved_datasymbols == []
    # Add a symbol with a UnresolvedInterface
    rdef = DataSymbol("r_def", INTEGER_TYPE,
                      interface=UnresolvedInterface())
    sym_table.add(rdef)
    assert sym_table.unresolved_datasymbols == [rdef]


def test_precision_datasymbols():
    ''' Tests for the precision_datasymbols method. '''
    sym_table = SymbolTable()
    # Add a precision symbol
    rdef = DataSymbol("r_def", INTEGER_TYPE,
                      interface=UnresolvedInterface())
    sym_table.add(rdef)
    # Add a symbol that uses r_def for its precision
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, rdef)
    sym_table.add(DataSymbol("s2", scalar_type))
    # By default we should get this precision symbol
    assert sym_table.precision_datasymbols == [rdef]


def test_abstract_properties():
    '''Test that the SymbolTable abstract properties raise the appropriate
    error.'''
    sym_table = SymbolTable()

    with pytest.raises(NotImplementedError) as error:
        _ = sym_table.data_arguments
    assert "Abstract property. Which symbols are data arguments is " \
        "API-specific." in str(error.value)

    with pytest.raises(NotImplementedError) as error:
        _ = sym_table.iteration_indices
    assert "Abstract property. Which symbols are iteration indices is " \
        "API-specific." in str(error.value)


def test_copy_external_import():
    ''' Tests the SymbolTable copy_external_import method. '''

    symtab = SymbolTable()

    # Test input argument type checking
    with pytest.raises(TypeError) as error:
        symtab.copy_external_import("invalid_type")
    assert "The imported_var argument of SymbolTable.copy_external_import " \
        "method should be a DataSymbol, but found " \
        in str(error.value)

    with pytest.raises(TypeError) as error:
        symtab.copy_external_import(DataSymbol("var1", REAL_TYPE))
    assert "The imported_var argument of SymbolTable.copy_external_import " \
        "method should have an ImportInterface interface, but found " \
        "'AutomaticInterface'." \
        in str(error.value)

    # Copy an imported_var
    container = ContainerSymbol("my_mod")
    var = DataSymbol("a", UnresolvedType(),
                     interface=ImportInterface(container))
    symtab.copy_external_import(var)
    assert "a" in symtab
    assert "my_mod" in symtab
    assert var.interface.container_symbol.name == "my_mod"
    # The symtab items should be new copies not connected to the original
    assert symtab.lookup("a") != var
    assert symtab.lookup("my_mod") != container
    assert symtab.lookup("a").interface.container_symbol != container

    # Copy a second imported_var with a reference to the same external
    # Container
    container2 = ContainerSymbol("my_mod")
    var2 = DataSymbol("b", UnresolvedType(),
                      interface=ImportInterface(container2))
    symtab.copy_external_import(var2)
    assert "b" in symtab
    assert "my_mod" in symtab
    assert var2.interface.container_symbol.name == "my_mod"
    assert symtab.lookup("b") != var2
    assert symtab.lookup("my_mod") != container2
    assert symtab.lookup("b").interface.container_symbol != container2

    # The new imported_var should reuse the available container reference
    assert symtab.lookup("a").interface.container_symbol == \
        symtab.lookup("b").interface.container_symbol

    # The copy of imported_vars that already exist is supported
    var3 = DataSymbol("b", UnresolvedType(),
                      interface=ImportInterface(container2))
    symtab.copy_external_import(var3)

    # But if the symbol is different (e.g. points to a different container),
    # it should fail
    container3 = ContainerSymbol("my_other_mod")
    var4 = DataSymbol("b", UnresolvedType(),
                      interface=ImportInterface(container3))
    with pytest.raises(KeyError) as error:
        symtab.copy_external_import(var4)
    assert "Couldn't copy 'b: DataSymbol<UnresolvedType, Import(container=" \
           "'my_other_mod')>' into the SymbolTable. The name 'b' is already" \
           " used by another symbol." in str(error.value)

    # If the symbol is the same but the given tag in not in the symbol table,
    # the new tag should reference the existing symbol
    symtab.copy_external_import(var3, tag="anothertag")
    assert symtab.lookup_with_tag("anothertag").name == "b"

    # If a tag is given but this is already used, it should fail
    symtab.add(Symbol("symbol"), tag="tag")
    var5 = DataSymbol("c", UnresolvedType(),
                      interface=ImportInterface(container3))
    with pytest.raises(KeyError) as error:
        symtab.copy_external_import(var5, "tag")
    assert ("This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'tag' for the symbol 'symbol',"
            " so it can not be associated with symbol 'c'."
            in str(error.value))

    # It should also fail if the symbol exist and the tag is given to another
    # symbol
    with pytest.raises(KeyError) as error:
        symtab.copy_external_import(var3, "tag")
    assert " into the SymbolTable. The tag 'tag' is already used by another" \
        " symbol." in str(error.value)

    # If the tag does not already exist, the tag is associated with the new
    # symbol
    var6 = DataSymbol("d", UnresolvedType(),
                      interface=ImportInterface(container3))
    symtab.copy_external_import(var6, "newtag")
    assert symtab.lookup_with_tag("newtag").name == "d"


def test_normalization():
    ''' Tests the SymbolTable normalize method lower cases the strings '''
    assert SymbolTable._normalize("aAbB") == "aabb"


def test_shallow_copy():
    ''' Tests the SymbolTable shallow copy generated new top-level containers
    but keeps the same objects in the symbol table'''

    # Create an initial SymbolTable
    symtab = SymbolTable(default_visibility=Symbol.Visibility.PRIVATE)
    dummy = Schedule(symbol_table=symtab)
    sym1 = DataSymbol("symbol1", INTEGER_TYPE,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    sym2 = Symbol("symbol2")
    symtab.add(sym1)
    symtab.add(sym2, tag="tag1")
    symtab.specify_argument_list([sym1])

    # Create a copy and check the contents are the same
    symtab2 = symtab.shallow_copy()
    assert "symbol1" in symtab2
    assert symtab2.lookup("symbol1") == sym1
    assert symtab2.lookup_with_tag("tag1") == sym2
    assert symtab2.scope is dummy
    assert sym1 in symtab2.argument_list
    assert symtab2.default_visibility == Symbol.Visibility.PRIVATE

    # Add new symbols in both symbols tables and check they are not added
    # to the other symbol table
    symtab.add(Symbol("st1"))
    symtab2.add(Symbol("st2"))
    assert "st1" in symtab
    assert "st2" in symtab2
    assert "st2" not in symtab
    assert "st1" not in symtab2


def test_deep_copy():
    ''' Tests the SymbolTable deep copy generates a new SymbolTable with
    new identical copies of the symbols in the original symbol table'''

    # Create an initial SymbolTable
    symtab = SymbolTable(default_visibility=Symbol.Visibility.PRIVATE)
    dummy = Schedule(symbol_table=symtab)
    mod = ContainerSymbol("my_mod")
    sym1 = DataSymbol("symbol1", INTEGER_TYPE,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    sym2 = Symbol("symbol2", interface=ImportInterface(mod,
                                                       orig_name="altsym2"))
    sym3 = DataSymbol("symbol3", INTEGER_TYPE)
    symtab.add(mod)
    symtab.add(sym1)
    symtab.add(sym2, tag="tag1")
    symtab.add(sym3)
    symtab.specify_argument_list([sym1])

    # Create a copy and check the contents are the same
    symtab2 = symtab.deep_copy()
    assert "symbol1" in symtab2
    assert isinstance(symtab2.lookup("symbol1"), DataSymbol)
    assert symtab2.lookup("symbol1").datatype is INTEGER_TYPE
    assert "symbol2" in symtab2
    assert symtab2.lookup_with_tag("tag1") is symtab2.lookup("symbol2")
    assert symtab2.lookup("symbol1") in symtab2.argument_list
    assert symtab2._node is None
    assert symtab2.default_visibility == Symbol.Visibility.PRIVATE

    # But the symbols are not the same objects as the original ones
    assert symtab2.lookup("symbol1") is not sym1
    assert symtab2.lookup_with_tag("tag1") is not sym2
    assert sym1 not in symtab2.argument_list
    assert symtab2.lookup("symbol1") not in symtab.argument_list

    # Check that the internal links between ImportInterfaces and
    # ContainerSymbols have been updated
    assert symtab2.lookup("symbol2").interface.container_symbol is \
        symtab2.lookup("my_mod")
    # Check that the orig_name is copied across.
    assert symtab2.lookup("symbol2").interface.orig_name == "altsym2"

    # Add new symbols and rename symbols in both symbol tables and check
    # they are not added/renamed in the other symbol table
    symtab.add(Symbol("st1"))
    symtab.rename_symbol(symtab.lookup("symbol3"), "a")
    symtab2.add(Symbol("st2"))
    symtab2.rename_symbol(symtab2.lookup("symbol3"), "b")
    assert "st1" in symtab
    assert "st2" in symtab2
    assert "st2" not in symtab
    assert "st1" not in symtab2
    assert "a" in symtab
    assert "a" not in symtab2
    assert "b" in symtab2
    assert "b" not in symtab
    assert "symbol1" in symtab2
    assert "symbol2" in symtab
    assert "symbol3" not in symtab
    assert "symbol3" not in symtab2


def test_get_symbols():
    '''Check that the get_symbols method in the SymbolTable class
    behaves as expected.

    '''
    schedule_symbol_table = SymbolTable()
    symbol1 = DataSymbol("symbol1", INTEGER_TYPE)
    schedule_symbol_table.add(symbol1)

    # get_symbols() works when the symbol table is not attached to a node.
    all_symbols = schedule_symbol_table.get_symbols()
    assert len(all_symbols) == 1
    assert all_symbols[symbol1.name] is symbol1

    schedule = KernelSchedule.create("my_kernel", schedule_symbol_table, [])
    container_symbol_table = SymbolTable()
    symbol2 = DataSymbol("symbol2", INTEGER_TYPE)
    container_symbol_table.add(symbol2)
    _ = Container.create("my_container", container_symbol_table,
                         [schedule])

    # get_symbols() works when the symbol table is attached to a
    # node which has no parent.
    all_symbols = container_symbol_table.get_symbols()
    assert len(all_symbols) == 1
    assert all_symbols[symbol2.name] is symbol2

    # get_symbols() works when the symbol table has ancestor symbol
    # tables.
    all_symbols = schedule_symbol_table.get_symbols()
    assert len(all_symbols) == 3
    assert all_symbols[symbol1.name] is symbol1
    assert all_symbols[symbol2.name] is symbol2


def test_get_tags():
    '''Check that the get_tags method in the SymbolTable class
    behaves as expected.

    '''
    schedule_symbol_table = SymbolTable()
    symbol1 = DataSymbol("symbol1", INTEGER_TYPE)
    symbol1_tag = "symbol1_tag"
    schedule_symbol_table.add(symbol1, tag=symbol1_tag)

    # get_tags() works when the symbol table is not attached to a node.
    all_tags = schedule_symbol_table.get_tags()
    assert len(all_tags) == 1
    assert all_tags["symbol1_tag"] is symbol1

    schedule = KernelSchedule.create("my_kernel", schedule_symbol_table, [])
    container_symbol_table = SymbolTable()
    symbol2 = DataSymbol("symbol2", INTEGER_TYPE)
    symbol2_tag = "symbol2_tag"
    container_symbol_table.add(symbol2, tag=symbol2_tag)
    _ = Container.create("my_container", container_symbol_table,
                         [schedule])

    # get_tags() works when the symbol table is attached to a
    # node which has no parent.
    all_tags = container_symbol_table.get_tags()
    assert len(all_tags) == 1
    assert all_tags[symbol2_tag] is symbol2

    # get_tags() works when the symbol table has ancestor symbol
    # tables.
    all_tags = schedule_symbol_table.get_tags()
    assert len(all_tags) == 3
    assert all_tags[symbol1_tag] is symbol1
    assert all_tags[symbol2_tag] is symbol2


def test_symbols_tags_dict():
    '''Check that the symbols_dict, tags_dict and reverse_tags_dict properties
    work as expected.

    '''
    schedule_symbol_table = SymbolTable()
    assert schedule_symbol_table.symbols_dict == {}
    assert schedule_symbol_table.tags_dict == {}

    symbol1 = DataSymbol("symbol1", INTEGER_TYPE)
    symbol1_tag = "symbol1_tag"
    schedule_symbol_table.add(symbol1, tag=symbol1_tag)
    assert schedule_symbol_table.symbols_dict is schedule_symbol_table._symbols
    assert schedule_symbol_table.tags_dict is schedule_symbol_table._tags
    rdict = schedule_symbol_table.get_reverse_tags_dict()
    assert rdict[symbol1] == symbol1_tag


def test_new_symbol():
    '''Test that the new_symbol method creates and returns symbols as
    expected. '''
    # pylint: disable=unidiomatic-typecheck

    symtab = SymbolTable()

    # By default it creates a generic Symbols and no tags
    sym = symtab.new_symbol("generic")
    assert sym.name == "generic"
    assert symtab.lookup("generic") is sym
    assert type(sym) is Symbol
    assert not symtab.tags_dict

    # Doing it again it will find a new name
    sym = symtab.new_symbol("generic")
    assert sym.name == "generic_1"
    assert symtab.lookup("generic_1") is sym

    # It can also have tags
    sym = symtab.new_symbol("generic", tag="my_tag")
    assert sym.name == "generic_2"
    assert symtab.lookup_with_tag("my_tag") is sym

    # But tags can not be repeated
    with pytest.raises(KeyError) as error:
        sym = symtab.new_symbol("generic", tag="my_tag")
    assert ("This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'my_tag' for the symbol 'generic_2', "
            "so it can not be associated with symbol 'generic_3'."
            in str(error.value))

    # New symbols can be given a Symbol sub-type
    sym1 = symtab.new_symbol("routine", symbol_type=RoutineSymbol)
    sym2 = symtab.new_symbol("data", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE)
    assert sym1.name == "routine"
    assert sym2.name == "data"
    assert type(sym1) is RoutineSymbol
    assert type(sym2) is DataSymbol
    assert symtab.lookup("routine") is sym1
    assert symtab.lookup("data") is sym2
    # which will be initialised with default values
    assert sym1.visibility is Symbol.Visibility.PUBLIC
    assert sym2.visibility is Symbol.Visibility.PUBLIC
    assert isinstance(sym1.interface, AutomaticInterface)
    assert isinstance(sym2.interface, AutomaticInterface)
    assert isinstance(sym1.datatype, NoType)
    assert sym2.datatype is INTEGER_TYPE
    assert sym2.initial_value is None

    # The initialization parameters of new symbols can be given as
    # keyword parameters
    sym1 = symtab.new_symbol("routine",
                             symbol_type=RoutineSymbol,
                             datatype=UnresolvedType(),
                             visibility=Symbol.Visibility.PRIVATE)
    sym2 = symtab.new_symbol("data", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE,
                             visibility=Symbol.Visibility.PRIVATE,
                             is_constant=True,
                             initial_value=3)
    assert sym1.name == "routine_1"
    assert sym2.name == "data_1"
    assert type(sym1) is RoutineSymbol
    assert type(sym2) is DataSymbol
    assert symtab.lookup("routine_1") is sym1
    assert symtab.lookup("data_1") is sym2
    assert sym1.visibility is Symbol.Visibility.PRIVATE
    assert sym2.visibility is Symbol.Visibility.PRIVATE
    assert isinstance(sym1.datatype, UnresolvedType)
    assert sym2.datatype is INTEGER_TYPE
    assert sym2.initial_value is not None
    assert sym2.is_constant is True

    # Check that symbol_type only accepts symbols
    with pytest.raises(TypeError) as err:
        sym1 = symtab.new_symbol("wrong", symbol_type=str,
                                 visibility=Symbol.Visibility.PRIVATE)
    assert ("The symbol_type parameter should be a type class of Symbol or"
            " one of its sub-classes but found" in str(err.value))


def test_new_symbol_with_private_default_visibility():
    '''Test that the new_symbol method creates a symbol with the appropriate
    visibility if the symbol table has a PRIVATE default visibility. '''

    symtab = SymbolTable()
    symtab.default_visibility = Symbol.Visibility.PRIVATE

    # If nothing is specified, use the default symbol table visibility
    sym = symtab.new_symbol("generic")
    assert symtab.lookup("generic") is sym
    assert symtab.lookup("generic").visibility == Symbol.Visibility.PRIVATE

    # If visibility is specified, use the provide value
    sym = symtab.new_symbol("generic_2", visibility=Symbol.Visibility.PUBLIC)
    assert symtab.lookup("generic_2") is sym
    assert symtab.lookup("generic_2").visibility == Symbol.Visibility.PUBLIC


def test_find_or_create():
    ''' Tests the SymbolTable find_or_create method find existing symbols or
    otherwise creates a new symbol with the given properties. '''
    symtab = SymbolTable()
    existing_symbol = Symbol("existing")
    symtab.add(existing_symbol, tag="tag1")

    # If the given name exists, return the symbol
    assert symtab.find_or_create("existing") is existing_symbol

    # If the given name does not exist, create and return new symbol
    new1 = symtab.find_or_create("new1")
    assert isinstance(new1, Symbol)
    assert new1.name == "new1"
    assert new1 is symtab.find_or_create("new1")  # Which then is found

    # Creating symbols can have parameters passed to the new_symbol method
    new2 = symtab.find_or_create("new2",
                                 tag="mytag",
                                 symbol_type=DataSymbol,
                                 datatype=INTEGER_TYPE,
                                 visibility=Symbol.Visibility.PRIVATE,
                                 is_constant=True,
                                 initial_value=3)
    assert new2.name == "new2"
    assert isinstance(new2, DataSymbol)
    assert new2.datatype is INTEGER_TYPE
    assert new2.visibility is Symbol.Visibility.PRIVATE
    assert new2.initial_value.value == "3"
    assert new2.is_constant is True
    assert symtab.lookup_with_tag("mytag") is new2

    # Check that it fails if the named Symbol exists but is not of the
    # specified type.
    with pytest.raises(SymbolError) as err:
        symtab.find_or_create("new2", symbol_type=RoutineSymbol)
    assert ("Expected symbol with name 'new2' to be of type 'RoutineSymbol' "
            "but found type 'DataSymbol'." in str(err.value))

    # TODO #1057: It should also fail the symbol is found but the properties
    # are different than the requested ones.


def test_find_or_create_tag():
    ''' Tests the SymbolTable find_or_create_tag method '''
    # pylint: disable=unidiomatic-typecheck
    symtab = SymbolTable()
    existing_symbol = Symbol("existing")
    symtab.add(existing_symbol, tag="tag1")

    # If the given tag exists, return the symbol name
    assert symtab.find_or_create_tag("tag1") is existing_symbol

    # If the tag does not exist, create a new symbol with the tag
    tag2 = symtab.find_or_create_tag("tag2")
    assert isinstance(tag2, Symbol)
    assert symtab.lookup_with_tag("tag2") is tag2
    # By default it is a generic symbol with the same name as the tag
    assert type(tag2) is Symbol
    assert tag2.name == "tag2"

    # If the operation is repeated it returns the already created symbol
    tag2b = symtab.find_or_create_tag("tag2")
    assert tag2b is tag2

    # It can be given additional new_symbol parameters
    tag3 = symtab.find_or_create_tag("tag3",
                                     symbol_type=DataSymbol,
                                     datatype=INTEGER_TYPE,
                                     visibility=Symbol.Visibility.PRIVATE,
                                     is_constant=True,
                                     initial_value=3)
    assert symtab.lookup_with_tag("tag3") is tag3
    assert type(tag3) is DataSymbol
    assert tag3.visibility is Symbol.Visibility.PRIVATE
    assert tag3.datatype is INTEGER_TYPE
    assert tag3.is_constant is True
    assert tag3.initial_value is not None

    # It can be given a different root_name
    tag4 = symtab.find_or_create_tag("tag4", root_name="var")
    assert symtab.lookup_with_tag("tag4") is tag4
    assert symtab.lookup_with_tag("tag4").name == "var"

    # If the given suggested name of an already created tag is different it
    # doesn't matter.
    tag4b = symtab.find_or_create_tag("tag4", root_name="anothername")
    assert tag4 is tag4b
    assert tag4b.name == "var"

    # Check that it fails if the Symbol type is different than expected
    with pytest.raises(SymbolError) as err:
        symtab.find_or_create_tag("tag3", symbol_type=RoutineSymbol)
    assert ("Expected symbol with tag 'tag3' to be of type 'RoutineSymbol' "
            "but found type 'DataSymbol'." in str(err.value))

    # TODO #1057: It should also fail the symbol is found but the properties
    # are different than the requested ones.


def test_rename_symbol():
    '''Test that the rename_symbol method renames a symbol and the change
    affects all its references. Also check that it fails when the arguments
    are not what the method expects.'''
    # Prepare the symbol table hierarchy for the test
    schedule_symbol_table, _ = create_hierarchy()
    symbol = schedule_symbol_table.lookup("symbol1")
    symbol.constant_value = 3
    symbol2 = schedule_symbol_table.lookup("symbol2")

    # Create multiple references to the symbol
    array_type = ArrayType(REAL_TYPE, [Reference(symbol)])
    array = schedule_symbol_table.new_symbol("array", symbol_type=DataSymbol,
                                             datatype=array_type)
    sched = schedule_symbol_table.node
    ref1 = Reference(symbol2)
    ref2 = Reference(symbol)
    assignment = Assignment.create(ref1, ref2)
    sched.addchild(assignment)

    # Check that the names are as expected before and after renaming
    assert symbol.name == "symbol1"
    assert symbol is schedule_symbol_table.lookup("symbol1")
    assert sched[0].rhs.symbol.name == "symbol1"
    assert array.datatype.shape[0].upper.symbol.name == "symbol1"
    schedule_symbol_table.rename_symbol(symbol, "other")
    assert symbol.name == "other"
    assert symbol is schedule_symbol_table.lookup("other")
    assert sched[0].rhs.symbol.name == "other"
    assert array.datatype.shape[0].upper.symbol.name == "other"

    # The previous name should fail the lookup now
    with pytest.raises(KeyError) as err:
        schedule_symbol_table.lookup("symbol1")
    assert "Could not find 'symbol1' in the Symbol Table." in str(err.value)


def test_rename_symbol_errors():
    '''Test the various checks performed by the rename_symbol method.'''
    table = SymbolTable()
    symbol = DataSymbol("heart", INTEGER_TYPE)

    with pytest.raises(TypeError) as err:
        table.rename_symbol("not_a_symbol", "other")
    assert ("The symbol argument of rename_symbol() must be a Symbol, but "
            "found: 'str'." in str(err.value))

    with pytest.raises(ValueError) as err:
        table.rename_symbol(symbol, "somethingelse")
    assert ("The symbol argument of rename_symbol() must belong to this "
            "symbol_table instance, but " in str(err.value))

    table.add(symbol)
    with pytest.raises(TypeError) as err:
        table.rename_symbol(symbol, 3)
    assert ("The name argument of rename_symbol() must be a str, but "
            "found:" in str(err.value))

    # Cannot rename to something that already exists in the table.
    table.new_symbol("array")
    with pytest.raises(KeyError) as err:
        table.rename_symbol(symbol, "array")
    assert ("The name argument of rename_symbol() must not already exist in "
            "this symbol_table instance, but 'array' does." in str(err.value))

    with pytest.raises(KeyError) as err:
        table.rename_symbol(symbol, "aRRay")
    assert ("The name argument of rename_symbol() must not already exist in "
            "this symbol_table instance, but 'aRRay' does." in str(err.value))

    # Cannot rename a Container symbol.
    csym = ContainerSymbol("benjy")
    table.add(csym)
    with pytest.raises(SymbolError) as err:
        table.rename_symbol(csym, "frankie")
    assert ("Cannot rename symbol 'benjy' because it is a ContainerSymbol." in
            str(err.value))

    # Cannot rename an imported symbol.
    isym = DataSymbol("mouse", UnresolvedType(),
                      interface=ImportInterface(csym))
    table.add(isym)
    with pytest.raises(SymbolError) as err:
        table.rename_symbol(isym, "rodent")
    assert ("Cannot rename symbol 'mouse' because it is imported (from "
            "Container 'benjy')" in str(err.value))

    # Cannot rename a routine argument.
    asym = DataSymbol("frankie", INTEGER_TYPE, interface=ArgumentInterface())
    table.add(asym)
    table.specify_argument_list([asym])
    with pytest.raises(SymbolError) as err:
        table.rename_symbol(asym, "rodent")
    assert ("Cannot rename symbol 'frankie' because it is a routine argument "
            "and as such may be named in a Call." in str(err.value))

    # Cannot rename a common block symbol
    asym = DataSymbol("a", INTEGER_TYPE, interface=CommonBlockInterface())
    table.add(asym)
    with pytest.raises(SymbolError) as err:
        table.rename_symbol(asym, "b")
    assert ("Cannot rename symbol 'a' because it has a CommonBlock interface."
            in str(err.value))


def test_rename_codeblock_error(fortran_reader):
    '''Test that we refuse to rename a symbol that is referenced within a
    CodeBlock in the associated code.'''
    code = '''
module gold
  integer :: my_var, other_var

contains

  subroutine heart_of()
    other_var = 1.0
    my_var = 1.0

    write(*,*) my_var

  end subroutine heart_of

end module gold'''
    psyir = fortran_reader.psyir_from_source(code)
    cont = psyir.children[0]
    assert len(cont.walk(CodeBlock)) == 1
    # We can rename 'other_var' because it's not accessed in the CodeBlock
    table = cont.symbol_table
    ovar = table.lookup("other_var")
    table.rename_symbol(ovar, "new_name")
    assert table.lookup("new_name") is ovar
    # We can't rename 'my_var' because it is accessed in the CodeBlock
    with pytest.raises(SymbolError) as err:
        table.rename_symbol(table.lookup("my_var"), "ship")
    assert ("Cannot rename Symbol 'my_var' because it is accessed in a "
            "CodeBlock:\nWRITE(*, *) my_var" in str(err.value))


# resolve_imports

def test_resolve_imports(fortran_reader, tmpdir, monkeypatch):
    ''' Tests that the SymbolTable resolve_imports method works as expected
    when importing symbol information from external containers and respects
    the method optional keywords. '''

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            use other_mod
            integer :: a_1, a_2
            integer :: b_1  ! Name clash but it is not imported
        end module a_mod
        ''')
    filename = os.path.join(str(tmpdir), "b_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module b_mod
            integer, parameter :: b_1 = 10
            integer, save, pointer :: b_2
            integer :: not_used1
            integer :: not_used2
            integer :: not_used3
        end module b_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            use a_mod, only: a_2
            private :: a_2
            contains
            subroutine test()
                use a_mod, only: a_1
                use b_mod
                use c_mod  ! This module is not found in INCLUDE_PATH

                a_1 = b_1 + b_2
            end subroutine test
        end module test_mod
    ''')
    subroutine = psyir.walk(Routine)[0]
    # Add Generic unresolved reference to "not_used"
    subroutine.symbol_table.add(
            Symbol("not_used1", interface=ImportInterface(
                subroutine.symbol_table.lookup("b_mod"))))

    # After parsing the a_1, a_2, b_1, b_2, not_used1 and not_used2 will have
    # incomplete information because the modules are not resolved
    a_1 = subroutine.symbol_table.lookup('a_1')
    a_2 = subroutine.symbol_table.lookup('a_2')
    b_1 = subroutine.symbol_table.lookup('b_1')
    b_2 = subroutine.symbol_table.lookup('b_2')
    not_used1 = subroutine.symbol_table.lookup('not_used1')
    assert "not_used2" not in subroutine.symbol_table
    assert not isinstance(not_used1, DataSymbol)
    assert isinstance(a_1.interface, ImportInterface)
    assert not isinstance(a_1, DataSymbol)
    assert isinstance(a_2.interface, ImportInterface)
    assert not isinstance(a_2, DataSymbol)
    assert a_2.visibility == Symbol.Visibility.PRIVATE
    assert isinstance(b_1.interface, UnresolvedInterface)
    assert not isinstance(b_1, DataSymbol)
    assert isinstance(b_2.interface, UnresolvedInterface)
    assert not isinstance(b_2, DataSymbol)

    # Try with incorrect argument types
    with pytest.raises(TypeError) as err:
        subroutine.symbol_table.resolve_imports(symbol_target="a_1")
    assert ("The resolve_imports symbol_target argument must be a Symbol but "
            "found 'str' instead." in str(err.value))

    with pytest.raises(TypeError) as err:
        subroutine.symbol_table.resolve_imports(container_symbols="my_mod")
    assert ("The resolve_imports container_symbols argument must be a list "
            "but found 'str' instead." in str(err.value))

    with pytest.raises(TypeError) as err:
        subroutine.symbol_table.resolve_imports(container_symbols=["my_mod"])
    assert ("The resolve_imports container_symbols argument list elements "
            "must be ContainerSymbols, but found a 'str' instead."
            in str(err.value))

    # Try to resolve a symbol that is not in the provided container
    with pytest.raises(KeyError) as err:
        subroutine.symbol_table.resolve_imports(
                container_symbols=[subroutine.symbol_table.lookup('a_mod')],
                symbol_target=subroutine.symbol_table.lookup('b_1'))
    assert ("The target symbol 'b_1' was not found in any of the searched "
            "containers: ['a_mod']." in str(err.value))
    # We still haven't resolved anything inside a_mod or the b_1 symbol
    assert not isinstance(a_1, DataSymbol)
    assert not isinstance(b_1, DataSymbol)

    # Resolve only 'not_used3' from wildcard imports
    subroutine.symbol_table.resolve_imports(
            symbol_target=Symbol('not_used3'))
    not_used3 = subroutine.symbol_table.lookup('not_used3')
    assert isinstance(not_used3, DataSymbol)
    assert isinstance(not_used3.interface, ImportInterface)
    # This still does not resolve the other symbols in the same module
    assert not isinstance(b_1, DataSymbol)
    assert not isinstance(b_2, DataSymbol)

    # Resolve only b_2 symbol info
    subroutine.symbol_table.resolve_imports(
            symbol_target=subroutine.symbol_table.lookup('b_2'))
    assert isinstance(b_2, DataSymbol)
    assert isinstance(b_2.datatype, UnsupportedFortranType)
    assert isinstance(b_2.interface, ImportInterface)
    assert b_2.interface.container_symbol == \
           subroutine.symbol_table.lookup('b_mod')
    # Repeat but for the case where the specified symbol is not actually
    # referenced in the current symbol table and is brought in by a wildcard
    # import.
    subroutine.symbol_table.resolve_imports(
        symbol_target=DataSymbol("not_used3", UnresolvedType()))
    notused3 = subroutine.symbol_table.lookup("not_used3")
    assert notused3.datatype == INTEGER_TYPE
    # We still haven't resolved anything about a_mod or other b_mod symbols
    assert not isinstance(a_1, DataSymbol)
    assert not isinstance(b_1, DataSymbol)

    # Resolve all symbols that are in a_mod inside the subroutine
    subroutine.symbol_table.resolve_imports([
            subroutine.symbol_table.lookup('a_mod')])
    # This will resolve a_1 information
    assert isinstance(a_1, DataSymbol)
    assert a_1.datatype.intrinsic.name == 'INTEGER'
    assert isinstance(a_1.interface, ImportInterface)
    # ContainerSymbol names are not brought into the local scope
    assert "other_mod" not in subroutine.symbol_table
    # TODO #1540: And neither the nested symbol declarations inside
    # another wildcard import, but this could be processed.

    # The other symbols (including a_2 because it is not from this symbol
    # table) are unchanged. a_mod::b_1 is not resolved to the local b_1
    # because it knows that a_mod imports are not using a wildcard import
    # and therefore it must come from somewhere else.
    assert "not_used2" not in subroutine.symbol_table
    assert isinstance(a_2.interface, ImportInterface)
    assert not isinstance(a_2, DataSymbol)
    assert isinstance(b_1.interface, UnresolvedInterface)
    assert not isinstance(b_1, DataSymbol)

    # Now resolve all found containers (this will not fail for the
    # unavailable c_mod)
    subroutine.symbol_table.resolve_imports()

    # b_1 have all relevant info now
    assert isinstance(b_1, DataSymbol)
    assert b_1.datatype.intrinsic.name == 'INTEGER'
    assert b_1.initial_value.value == "10"
    # The interface is also updated updated now because we know where it comes
    # from
    assert isinstance(b_1.interface, ImportInterface)
    assert b_1.interface.container_symbol == \
           subroutine.symbol_table.lookup('b_mod')
    # not_used1 and not_used2 should now also exist and have all its properties
    # because the b_mod wildcard import imports them
    assert isinstance(subroutine.symbol_table.lookup('not_used1'), DataSymbol)
    assert isinstance(subroutine.symbol_table.lookup('not_used2'), DataSymbol)

    # a_2 is not yet resolved because it comes from another symbol table,
    # resolve that symbol table too
    assert not isinstance(a_2, DataSymbol)
    subroutine.parent.symbol_table.resolve_imports()
    # In this case check that the visibility stays PRIVATE
    assert isinstance(a_2, DataSymbol)
    assert a_2.visibility == Symbol.Visibility.PRIVATE


def test_resolve_imports_different_capitalization(
        fortran_reader, tmpdir, monkeypatch):
    ''' Tests that the SymbolTable resolve_imports method works as expected
    when importing symbols with different name capitalizations '''

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: SOME_name
        end module a_Mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            use a_mod, only: some_NAME
            private :: Some_namE
            contains
            subroutine test()
                somE_Name = soMe_name + 1
            end subroutine test
        end module test_mod
    ''')
    subroutine = psyir.walk(Routine)[0]
    subroutine.parent.symbol_table.resolve_imports()
    symbol = subroutine.symbol_table.lookup("SOME_NAME")
    # Datatype and visibility are correct despite different capitalizations
    assert symbol.datatype == INTEGER_TYPE
    assert symbol.visibility == Symbol.Visibility.PRIVATE


def test_resolve_imports_name_clashes(fortran_reader, tmpdir, monkeypatch):
    ''' Tests the SymbolTable resolve_imports method raises the appropriate
    errors when it finds name clashes. '''

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: not_a_name_clash
            integer :: name_clash
            private not_a_name_clash
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            contains
            subroutine test()
                use a_mod
                integer :: not_a_name_clash ! because its private in the module
                integer :: name_clash

                name_clash = name_clash + not_a_name_clash
            end subroutine test
        end module test_mod
    ''')
    subroutine = psyir.walk(Routine)[0]
    symtab = subroutine.symbol_table

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])

    with pytest.raises(SymbolError) as err:
        symtab.resolve_imports([symtab.lookup('a_mod')])
    assert ("Found a name clash with symbol 'name_clash' when importing "
            "symbols from container 'a_mod'." in str(err.value))


def test_resolve_imports_private_symbols(fortran_reader, tmpdir, monkeypatch):
    ''' Tests the SymbolTable resolve_imports respects the accessibility
    statements when importing symbol information from external containers. '''

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: name_public1
            integer, private :: name_clash
        end module a_mod
        ''')
    filename = os.path.join(str(tmpdir), "b_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module b_mod
            use a_mod
            ! The imported a_mod::name_public is private here, also name_clash
            private
            integer :: name_clash
            integer :: other_private
            integer, public :: name_public2
        end module b_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module test_mod
            use a_mod
            private name_public1

            contains

            subroutine test()
                use b_mod
                integer :: name_clash
            end subroutine test
        end module test_mod
    ''')
    subroutine = psyir.walk(Routine)[0]
    symtab = subroutine.symbol_table

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])

    # name_public1 exists before importing as a generic Symbol because it
    # is mentioned by the accessibility statement
    public1 = symtab.lookup("name_public1")
    # pylint: disable=unidiomatic-typecheck
    assert type(public1) is Symbol

    # This should succeed because all name clashes are protected by proper
    # private accessibility
    subroutine.parent.symbol_table.resolve_imports()
    symtab.resolve_imports()

    # Now we now that 'name_public1' is a DataSymbol
    assert isinstance(public1, DataSymbol)

    # name_public2 also has been imported because it is a public symbol
    assert "name_public2" in symtab
    # even though we capture that other symbols are private by default
    assert symtab.lookup("b_mod").container.symbol_table \
        .default_visibility == Symbol.Visibility.PRIVATE
    assert "other_private" not in symtab


def test_resolve_imports_with_datatypes(fortran_reader, tmpdir, monkeypatch):
    ''' Tests that the SymbolTable resolve_imports method work as expected when
    we are importing user-defined/derived types from an external container. '''
    filename = os.path.join(str(tmpdir), "my_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module my_mod
            type my_type
                integer :: field
                integer, dimension(10,10) :: array
            end type my_type
            type(my_type) :: global1
            type other_type
                type(my_type) :: value1
            end type other_type
        end module my_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            use my_mod
            type(my_type) :: local1

        end subroutine test
    ''')

    subroutine = psyir.walk(Routine)[0]
    symtab = subroutine.symbol_table
    # Add a generic Symbol definition of other_type
    symtab.add(Symbol("other_type",
                      interface=ImportInterface(symtab.lookup("my_mod"))))

    # Before resolving import
    # global1 doesn't exist because it is never mentioned
    assert "global1" not in symtab
    # Some symbols types / datatype are inferred
    assert isinstance(symtab.lookup("my_type"), DataTypeSymbol)
    assert symtab.lookup("local1").datatype == symtab.lookup("my_type")
    # but we don't know anything about the imported type
    assert isinstance(symtab.lookup("my_type").datatype, UnresolvedType)
    assert not isinstance(symtab.lookup("other_type"), DataTypeSymbol)

    # Set up include_path to import the proper modules and resolve symbols
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    symtab.resolve_imports()

    # The global1 exist and is a DataSymbol now
    assert isinstance(symtab.lookup("global1"), DataSymbol)

    # All symbols are of my_type type
    assert symtab.lookup("local1").datatype.name == "my_type"
    assert symtab.lookup("global1").datatype.name == "my_type"
    assert isinstance(symtab.lookup("other_type"), DataTypeSymbol)
    value1 = symtab.lookup("other_type").datatype.components["value1"]
    assert value1.datatype.name == "my_type"

    # And now the imported "my_type" type has more info
    my_type = symtab.lookup("my_type").datatype
    assert isinstance(my_type, StructureType)
    assert "field" in my_type.components
    assert "array" in my_type.components
    assert my_type.components["field"].datatype.intrinsic.name == "INTEGER"
    assert my_type.components["array"].datatype.shape[1].upper.value == "10"


@pytest.mark.parametrize('dependency_order', [['a_mod', 'b_mod'],
                                              ['b_mod', 'a_mod']])
def test_resolve_imports_common_symbol(fortran_reader, tmpdir, monkeypatch,
                                       dependency_order):
    ''' Tests the SymbolTable resolve_imports accepts symbols with the same
    name coming from different dependency paths and keeps the most specific
    information regardless of the import order. '''

    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: common_import
        end module a_mod
        ''')
    filename = os.path.join(str(tmpdir), "b_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module b_mod
            use a_mod, only: common_import
        end module b_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            use a_mod
            use b_mod

            common_import = common_import + 1
        end subroutine test
    ''')
    subroutine = psyir.walk(Routine)[0]
    symtab = subroutine.symbol_table

    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    for dependency in dependency_order:
        symtab.resolve_imports([symtab.lookup(dependency)])
    assert symtab.lookup("common_import").datatype.intrinsic.name == "INTEGER"


def test_resolve_imports_parent_scope(fortran_reader, tmpdir, monkeypatch):
    '''Test that resolve_imports() works as expected if a Symbol is brought
    into scope from a parent table (which does not itself contain the Symbol
    in question).'''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: some_var
            integer, parameter :: wp = kind(1.0)
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module b_mod
            use a_mod
            use other_mod
        contains
          subroutine my_sub()
            real(kind=wp) :: rvar
            some_var = some_var + 1_wp
          end subroutine
        end module b_mod
        ''')
    mod = psyir.children[0]
    subroutine = psyir.walk(Routine)[0]
    lit = subroutine.walk(Literal)[0]
    sym = lit.datatype.precision
    mod.symbol_table.resolve_imports(symbol_target=sym)
    # A new Symbol with the correct properties should have been added to the
    # table associated with the Container.
    new_sym = mod.symbol_table.lookup(sym.name)
    assert isinstance(new_sym.interface, ImportInterface)
    assert new_sym.interface.container_symbol.name == "a_mod"


def test_resolve_imports_from_child_symtab(
        fortran_reader, tmpdir, monkeypatch):
    '''Check that when an unresolved symbol is declared in a subroutine,
    resolve imports can resolve it from a parent module as long as
    there are no wildcard imports in the subroutine.

    '''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: some_var
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module b_mod
            use a_mod
        contains
          subroutine my_sub()
            some_var = 0.0
          end subroutine
        end module b_mod
        ''')
    mod = psyir.children[0]
    subroutine = psyir.walk(Routine)[0]
    assert "some_var" not in mod.symbol_table
    assert "some_var" in subroutine.symbol_table
    symbol = subroutine.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is Symbol
    mod.symbol_table.resolve_imports()
    assert "some_var" not in subroutine.symbol_table
    assert "some_var" in mod.symbol_table
    symbol = mod.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is DataSymbol
    assert isinstance(symbol.interface, ImportInterface)
    assert symbol.interface.container_symbol.name == "a_mod"


def test_resolve_imports_from_child_symtab_uft(
        fortran_reader, tmpdir, monkeypatch):
    '''Check that when an unresolved symbol is declared in a subroutine,
    resolve imports can resolve it from a parent module as an
    UnsupportedFortranType as long as there are no wildcard imports in the
    subroutine.

    '''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer, save, pointer :: some_var
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module b_mod
            use a_mod
        contains
          subroutine my_sub()
            some_var = 0.0
          end subroutine
        end module b_mod
        ''')
    mod = psyir.children[0]
    subroutine = psyir.walk(Routine)[0]
    assert "some_var" not in mod.symbol_table
    assert "some_var" in subroutine.symbol_table
    symbol = subroutine.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is Symbol
    mod.symbol_table.resolve_imports()
    assert "some_var" not in subroutine.symbol_table
    assert "some_var" in mod.symbol_table
    symbol = mod.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is DataSymbol
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    assert isinstance(symbol.interface, ImportInterface)
    assert symbol.interface.container_symbol.name == "a_mod"


def test_resolve_imports_from_child_symtabs(
        fortran_reader, tmpdir, monkeypatch):
    '''Check that when an unresolved symbol is declared in more than one
    subroutine, resolve imports can resolve it from a parent module as
    long as there are no wildcard imports in the subroutine. We also
    need to check that references to the new symbol still work when we
    remove (rather than move) the original symbol.

    '''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: some_var
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module b_mod
            use a_mod
        contains
          subroutine my_sub1()
            some_var = 0.0
          end subroutine
          subroutine my_sub2()
            some_var = 0.0
          end subroutine
        end module b_mod
        ''')
    mod = psyir.children[0]
    assert "some_var" not in mod.symbol_table
    for subroutine in psyir.walk(Routine):
        assert "some_var" in subroutine.symbol_table
        symbol = subroutine.symbol_table.lookup("some_var")
        # pylint: disable=unidiomatic-typecheck
        assert type(symbol) is Symbol
    mod.symbol_table.resolve_imports()
    for subroutine in psyir.walk(Routine):
        assert "some_var" not in subroutine.symbol_table
    assert "some_var" in mod.symbol_table
    symbol = mod.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is DataSymbol
    assert isinstance(symbol.interface, ImportInterface)
    assert symbol.interface.container_symbol.name == "a_mod"

    # Check that all References to the symbol have been updated,
    # i.e. that all References reference the new symbol. This has to
    # be dealt with by the implementation when the symbol we want to
    # reference already exists in the module symbol table and we want
    # to remove the symbol from a subroutine symbol table.
    some_var_symbol = mod.symbol_table.lookup("some_var")
    for reference in psyir.walk(Reference):
        assert reference.symbol is some_var_symbol


def test_resolve_imports_from_child_symtabs_utf(
        fortran_reader, tmpdir, monkeypatch):
    '''Check that when an unresolved symbol is declared in more than one
    subroutine, resolve imports can resolve it from a parent module
    where it is declared as an UnsupportedFortranType, as long as there
    are no wildcard imports in the subroutine.  We also need to check
    that references to the new symbol still work when we remove
    (rather than move) the original symbol.

    '''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer, save, pointer :: some_var
        end module a_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module b_mod
            use a_mod
        contains
          subroutine my_sub1()
            some_var = 0.0
          end subroutine
          subroutine my_sub2()
            some_var = 0.0
          end subroutine
        end module b_mod
        ''')
    mod = psyir.children[0]
    assert "some_var" not in mod.symbol_table
    for subroutine in psyir.walk(Routine):
        assert "some_var" in subroutine.symbol_table
        symbol = subroutine.symbol_table.lookup("some_var")
        # pylint: disable=unidiomatic-typecheck
        assert type(symbol) is Symbol
    mod.symbol_table.resolve_imports()
    for subroutine in psyir.walk(Routine):
        assert "some_var" not in subroutine.symbol_table
    assert "some_var" in mod.symbol_table
    symbol = mod.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(symbol) is DataSymbol
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    assert isinstance(symbol.interface, ImportInterface)
    assert symbol.interface.container_symbol.name == "a_mod"

    # Check that all References to the symbol have been updated,
    # i.e. that all References reference the new symbol. This has to
    # be dealt with by the implementation when the symbol we want to
    # reference already exists in the module symbol table and we want
    # to remove the symbol from a subroutine symbol table.
    some_var_symbol = mod.symbol_table.lookup("some_var")
    for reference in psyir.walk(Reference):
        assert reference.symbol is some_var_symbol


def test_resolve_imports_from_child_symtab_with_import(
        fortran_reader, tmpdir, monkeypatch):
    '''Check that when an unresolved symbol is declared in a subroutine
    with at least one wildcard use statement resolve imports can't
    resolve it from a parent. This shows one of the current
    limitations of resolve_imports i.e. it should be able to be done
    on routines as well as modules and should recurse up a hierarchy
    of symbol tables by default or have an option to do so. At the
    moment we end up with a symbol in the subroutine but also a
    datasymbol with the same name in the module symbol table.

    '''
    # Set up include_path to import the proper modules
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: some_var
            integer :: rau0 = 1
        end module a_mod
        ''')
    filename = os.path.join(str(tmpdir), "b_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module b_mod
            integer :: rau0 = 2
        end module b_mod
        ''')
    psyir = fortran_reader.psyir_from_source('''
        module c_mod
            use a_mod
        contains
          subroutine my_sub()
            use b_mod
            some_var = rau0
          end subroutine
        end module c_mod
        ''')
    mod = psyir.children[0]
    subroutine = mod.children[0]
    for symbol_name in ["some_var", "rau0"]:
        assert symbol_name not in mod.symbol_table
        assert symbol_name in subroutine.symbol_table
        symbol = subroutine.symbol_table.lookup(symbol_name)
        # pylint: disable=unidiomatic-typecheck
        assert type(symbol) is Symbol
    mod.symbol_table.resolve_imports()
    pytest.xfail(reason="issue #2331: Routine symbol table not checked "
                 "with resolve_imports")
    assert "rau0" in subroutine.symbol_table
    assert "rau0" not in mod.symbol_table
    data_symbol = subroutine.symbol_table.lookup("rau0")
    # pylint: disable=unidiomatic-typecheck
    assert type(data_symbol) is DataSymbol
    assert "some_var" not in subroutine.symbol_table
    assert "some_var" in mod.symbol_table
    data_symbol = mod.symbol_table.lookup("some_var")
    # pylint: disable=unidiomatic-typecheck
    assert type(data_symbol) is DataSymbol


def test_scope():
    ''' Test that the scope property returns the SymbolTable associated with
    the node. '''
    symtab = SymbolTable()
    assert symtab.scope is None

    schedule = Schedule()
    assert schedule.symbol_table.scope is schedule


def test_detach():
    ''' Test that the detach method of a symbol table detaches itself from its
    current scope and returns itself. '''

    # Create a symbol_table associated with a scope
    sym_table = SymbolTable()
    scope = Schedule(symbol_table=sym_table)

    assert sym_table._node is scope

    # Detach the symbol table
    assert sym_table.detach() is sym_table
    assert sym_table._node is not scope


def test_attach():
    ''' Test that the attach method binds a symboltable and a ScopingNode
    together. It checks that an appropriate error is raised if either object
    has an existing association.
    '''
    symtab = SymbolTable()
    not_a_scope = Literal("1", INTEGER_TYPE)

    with pytest.raises(TypeError) as err:
        symtab.attach(not_a_scope)
    assert ("A SymbolTable must be attached to a ScopingNode but found "
            "'Literal'." in str(err.value))

    scope = Schedule()
    with pytest.raises(ValueError) as err:
        symtab.attach(scope)
    assert ("The provided scope already has a symbol table attached to it. "
            "You may need to detach that one first." in str(err.value))

    scope.symbol_table.detach()
    symtab.attach(scope)

    scope2 = Schedule()
    scope2.symbol_table.detach()
    with pytest.raises(ValueError) as err:
        symtab.attach(scope2)
    assert ("The symbol table is already bound to another scope (Schedule[]). "
            "Consider detaching or deepcopying the symbol table first."
            in str(err.value))


def test_has_same_name():
    ''' Test that the _has_same_name utility accepts strings and symbols and
    returns whether the normalized names are the same.
    '''
    sym1 = Symbol('name')
    sym2 = Symbol('NaMe')
    different = Symbol('not_name')

    # It can compare symbols
    assert SymbolTable._has_same_name(sym1, sym2)
    assert not SymbolTable._has_same_name(sym1, different)

    # It can compare string
    assert SymbolTable._has_same_name("naME", "NamE")
    assert not SymbolTable._has_same_name("name", "NOT_NAME")

    # It can compare between symbols and strings
    assert SymbolTable._has_same_name(sym1, "NamE")
    assert not SymbolTable._has_same_name("name", different)


def test_equality():
    ''' Test that we can compare the equality of 2 symbol tables.

    TODO #1698: The current implementation is not sensitive to tags, order
    of arguments and visibilities.
    '''

    # An empty symbol table is equal to other empty symbol tables
    symtab1 = SymbolTable()
    symtab2 = SymbolTable()
    assert symtab1 == symtab2

    # Its not equal to any other type
    assert symtab1 != 3

    # If it has different symbols, its not equal
    symtab1.new_symbol("s1", symbol_type=RoutineSymbol)
    symtab2.new_symbol("s1", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    assert symtab1 != symtab2

    # If it has the same symbols is the same
    symtab2 = symtab1.deep_copy()
    assert symtab1 == symtab2

    # If the lhs symbol_table has more symbols its not equal
    symtab1.new_symbol("s2", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    assert symtab1 != symtab2

    # If the rhs symbol_table has more symbols its not equal
    symtab2 = symtab1.deep_copy()
    symtab2.new_symbol("s3")
    assert symtab1 != symtab2
