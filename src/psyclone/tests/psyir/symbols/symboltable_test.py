# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.symboltable file '''

from __future__ import absolute_import
import re
from collections import OrderedDict
import pytest
from psyclone.psyir.nodes import Schedule, Container, KernelSchedule, \
    Literal, Reference, Assignment
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ContainerSymbol, \
    LocalInterface, ImportInterface, ArgumentInterface, UnresolvedInterface, \
    ScalarType, ArrayType, DeferredType, REAL_TYPE, INTEGER_TYPE, Symbol, \
    SymbolError, RoutineSymbol, NoType, StructureType, DataTypeSymbol
from psyclone.errors import InternalError


def create_hierarchy():
    '''Utility routine that creates a symbol table hierarchy with a
    symbol in each symbol table.

    :returns: two symbol tables created in a hierachy.
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
    assert ("Optional node argument to SymbolTable should be a Schedule "
            "or a Container but found 'str'." in str(info.value))

    schedule = Schedule()
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
    name = sym_table.next_available_name(root_name="my_mod")
    assert name == "my_mod_2"
    name = sym_table.next_available_name(root_name="my_mod_1")
    assert name == "my_mod_1_1"
    # Check we return a new symbol by appending an integer index to
    # the default name when the names clash.
    name = sym_table.next_available_name()
    assert name == "psyir_tmp_1"
    sym_table.add(DataSymbol(name, REAL_TYPE))
    assert sym_table.next_available_name() == "psyir_tmp_2"


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
    assert ("Argument shadowing should be of type bool but found "
            "'int'." in str(excinfo.value))


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
            "<Scalar<INTEGER, UNDEFINED>, Local>', so it can not be associated"
            "with symbol 'symbol3'." in str(info.value))
    # A clash of tags in an ancestor symbol table should raise an exception.
    with pytest.raises(KeyError) as info:
        schedule_symbol_table.add(symbol3, tag="symbol2_tag")
        assert (
            "This symbol table, or an outer scope ancestor symbol table, "
            "already contains the tag 'symbol2_tag' for the symbol 'symbol2: "
            "<Scalar<INTEGER, UNDEFINED>, Local>', so it can not be associated"
            " to symbol 'symbol3'." in str(info.value))
    # A clash of tags with a child symbol table is not checked for now.
    container_symbol_table.add(symbol1, tag="symbol1_tag")


def test_symbols_imported_from():
    ''' Test the Symbol Table symbols_imported_from() method. '''
    sym_table = SymbolTable()
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    assert sym_table.symbols_imported_from(my_mod) == []
    var1 = DataSymbol("var1", REAL_TYPE, interface=LocalInterface())
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
    assert ("Cannot remove ContainerSymbol 'my_mod' because symbols "
            "['var1'] are imported from it" in str(err.value))
    # Change the interface on var1
    var1.interface = LocalInterface()
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
    symbol2 = DataSymbol("var2", INTEGER_TYPE, constant_value=6)
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


def test_swap_symbol_properties():
    ''' Test the symboltable swap_properties method '''

    symbol1 = DataSymbol("var1", INTEGER_TYPE, constant_value=7)
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
    assert("The symbols should have different names, but found 'var1' for "
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
    assert symbol1.constant_value is None
    assert symbol1.interface.access == ArgumentInterface.Access.READWRITE

    assert symbol4.name == "var2"
    assert symbol4.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert symbol4.datatype.precision == ScalarType.Precision.UNDEFINED
    assert not symbol4.shape
    assert symbol4.is_local
    assert symbol4.constant_value.value == "7"
    assert (symbol4.constant_value.datatype.intrinsic ==
            symbol4.datatype.intrinsic)
    assert (symbol4.constant_value.datatype.precision ==
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
    sym_table.add(DataSymbol("var3", REAL_TYPE))

    assert isinstance(sym_table.lookup("var1"), DataSymbol)
    assert sym_table.lookup("var1").name == "var1"
    assert isinstance(sym_table.lookup("var2"), DataSymbol)
    assert sym_table.lookup("var2").name == "var2"
    assert isinstance(sym_table.lookup("var3"), DataSymbol)
    assert sym_table.lookup("var3").name == "var3"

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


def test_view(capsys):
    '''Test the view method of the SymbolTable class, it should print to
    standard out a representation of the full SymbolTable.'''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", INTEGER_TYPE))
    sym_table.view()
    output, _ = capsys.readouterr()
    assert "Symbol Table:\n" in output
    assert "var1" in output
    assert "var2" in output


def test_can_be_printed():
    '''Test that a SymbolTable instance can always be printed. (i.e. is
    initialised fully)'''
    sym_table = SymbolTable()
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    sym_table.add(DataSymbol("var1", REAL_TYPE))
    sym_table.add(DataSymbol("var2", INTEGER_TYPE))
    sym_table.add(DataSymbol("var3", DeferredType(),
                             interface=ImportInterface(my_mod)))
    sym_table_text = str(sym_table)
    assert "Symbol Table:\n" in sym_table_text
    assert "var1" in sym_table_text
    assert "var2" in sym_table_text
    assert "\nmy_mod" in sym_table_text
    assert "Import(container='my_mod')" in sym_table_text


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


def test_local_datasymbols():
    '''Test that the local_datasymbols property returns a list with the
    symbols with local scope.'''
    sym_table = SymbolTable()
    assert [] == sym_table.local_datasymbols

    sym_table.add(DataSymbol("var1", REAL_TYPE))
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    sym_table.add(DataSymbol("var2", array_type))
    sym_table.add(DataSymbol("var3", REAL_TYPE))

    assert len(sym_table.local_datasymbols) == 3
    assert sym_table.lookup("var1") in sym_table.local_datasymbols
    assert sym_table.lookup("var2") in sym_table.local_datasymbols
    assert sym_table.lookup("var3") in sym_table.local_datasymbols
    sym_v1 = sym_table.lookup("var1")
    sym_v1.interface = ArgumentInterface(ArgumentInterface.Access.READWRITE)
    sym_table.specify_argument_list([sym_v1])

    assert len(sym_table.local_datasymbols) == 2
    assert sym_table.lookup("var1") not in sym_table.local_datasymbols
    assert sym_table.lookup("var2") in sym_table.local_datasymbols
    assert sym_table.lookup("var3") in sym_table.local_datasymbols

    sym_table.add(DataSymbol("var4", REAL_TYPE,
                             interface=ImportInterface(
                                 ContainerSymbol("my_mod"))))
    assert len(sym_table.local_datasymbols) == 2
    assert sym_table.lookup("var4") not in sym_table.local_datasymbols


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


def test_local_datatypesymbols():
    ''' Test that the local_datatypesymbols property returns a list of the
    correct symbols. '''
    sym_table = SymbolTable()
    assert sym_table.local_datatypesymbols == []
    region_type = StructureType.create([
        ("startx", INTEGER_TYPE, Symbol.Visibility.PUBLIC)])
    region_sym = DataTypeSymbol("region_type", region_type)
    sym_table.add(region_sym)
    # Add another DataTypeSymbol but have it imported from a Container (so it
    # is not local).
    csym = ContainerSymbol("my_mod")
    sym_table.add(csym)
    var1 = DataTypeSymbol("other_type", DeferredType(),
                          interface=ImportInterface(csym))
    sym_table.add(var1)
    var2 = DataSymbol("arg_var", region_type, interface=ArgumentInterface())
    sym_table.specify_argument_list([var2])
    assert sym_table.local_datatypesymbols == [region_sym]


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


def test_unresolved():
    ''' Tests for the get_unresolved_datasymbols method. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("s1", INTEGER_TYPE))
    # Check that we get an empty list if everything is defined
    assert sym_table.get_unresolved_datasymbols() == []
    # Add a symbol with a deferred interface
    rdef = DataSymbol("r_def", INTEGER_TYPE,
                      interface=UnresolvedInterface())
    sym_table.add(rdef)
    assert sym_table.get_unresolved_datasymbols() == ["r_def"]
    # Add a symbol that uses r_def for its precision
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, rdef)
    sym_table.add(DataSymbol("s2", scalar_type))
    # By default we should get this precision symbol
    assert sym_table.get_unresolved_datasymbols() == ["r_def"]
    # But not if we request that precision symbols be ignored
    assert sym_table.get_unresolved_datasymbols(ignore_precision=True) == []


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
        "'LocalInterface'." \
        in str(error.value)

    # Copy an imported_var
    container = ContainerSymbol("my_mod")
    var = DataSymbol("a", DeferredType(),
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
    var2 = DataSymbol("b", DeferredType(),
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
    var3 = DataSymbol("b", DeferredType(),
                      interface=ImportInterface(container2))
    symtab.copy_external_import(var3)

    # But if the symbol is different (e.g. points to a different container),
    # it should fail
    container3 = ContainerSymbol("my_other_mod")
    var4 = DataSymbol("b", DeferredType(),
                      interface=ImportInterface(container3))
    with pytest.raises(KeyError) as error:
        symtab.copy_external_import(var4)
    assert "Couldn't copy 'b: <DeferredType, Import(container=" \
           "'my_other_mod')>' into the SymbolTable. The name 'b' is already" \
           " used by another symbol." in str(error.value)

    # If the symbol is the same but the given tag in not in the symbol table,
    # the new tag should reference the existing symbol
    symtab.copy_external_import(var3, tag="anothertag")
    assert symtab.lookup_with_tag("anothertag").name == "b"

    # If a tag is given but this is already used, it should fail
    symtab.add(Symbol("symbol"), tag="tag")
    var5 = DataSymbol("c", DeferredType(),
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
    var6 = DataSymbol("d", DeferredType(),
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
    dummy = Schedule()
    symtab = SymbolTable(node=dummy,
                         default_visibility=Symbol.Visibility.PRIVATE)
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
    assert symtab2._node == dummy
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
    dummy = Schedule()
    symtab = SymbolTable(node=dummy,
                         default_visibility=Symbol.Visibility.PRIVATE)
    mod = ContainerSymbol("my_mod")
    sym1 = DataSymbol("symbol1", INTEGER_TYPE,
                      interface=ArgumentInterface(
                          ArgumentInterface.Access.READ))
    sym2 = Symbol("symbol2", interface=ImportInterface(mod))
    symtab.add(mod)
    symtab.add(sym1)
    symtab.add(sym2, tag="tag1")
    symtab.specify_argument_list([sym1])

    # Create a copy and check the contents are the same
    symtab2 = symtab.deep_copy()
    assert "symbol1" in symtab2
    assert isinstance(symtab2.lookup("symbol1"), DataSymbol)
    assert symtab2.lookup("symbol1").datatype is INTEGER_TYPE
    assert "symbol2" in symtab2
    assert symtab2.lookup_with_tag("tag1") is symtab2.lookup("symbol2")
    assert symtab2.lookup("symbol1") in symtab2.argument_list
    assert symtab2._node == dummy
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

    # Add new symbols and rename symbols in both symbol tables and check
    # they are not added/renamed in the other symbol table
    symtab.add(Symbol("st1"))
    symtab.rename_symbol(symtab.lookup("symbol1"), "a")
    symtab2.add(Symbol("st2"))
    symtab2.rename_symbol(symtab2.lookup("symbol2"), "b")
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
    assert "symbol1" not in symtab
    assert "symbol2" not in symtab2


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
    assert len(all_symbols) == 2
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
    assert len(all_tags) == 2
    assert all_tags[symbol1_tag] is symbol1
    assert all_tags[symbol2_tag] is symbol2


def test_symbols_tags_dict():
    '''Check that the symbols_dict and tags_dict properties work as
    expected.

    '''
    schedule_symbol_table = SymbolTable()
    assert schedule_symbol_table.symbols_dict == {}
    assert schedule_symbol_table.tags_dict == {}

    symbol1 = DataSymbol("symbol1", INTEGER_TYPE)
    symbol1_tag = "symbol1_tag"
    schedule_symbol_table.add(symbol1, tag=symbol1_tag)
    assert schedule_symbol_table.symbols_dict is schedule_symbol_table._symbols
    assert schedule_symbol_table.tags_dict is schedule_symbol_table._tags


def test_new_symbol():
    '''Test that the new_symbol method creates and returns symbols as
    expected. '''
    # pylint: disable=unidiomatic-typecheck

    symtab = SymbolTable()

    # By default it creates a generic Symbols and no tags
    sym = symtab.new_symbol("generic")
    assert sym.name == "generic"
    assert symtab.lookup("generic") is sym
    assert type(sym) == Symbol
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
    assert type(sym1) == RoutineSymbol
    assert type(sym2) == DataSymbol
    assert symtab.lookup("routine") is sym1
    assert symtab.lookup("data") is sym2
    # which will be initialised with default values
    assert sym1.visibility is Symbol.Visibility.PUBLIC
    assert sym2.visibility is Symbol.Visibility.PUBLIC
    assert isinstance(sym1.interface, LocalInterface)
    assert isinstance(sym2.interface, LocalInterface)
    assert isinstance(sym1.datatype, NoType)
    assert sym2.datatype is INTEGER_TYPE
    assert sym2.constant_value is None

    # The initialization parameters of new symbols can be given as
    # keyword parameters
    sym1 = symtab.new_symbol("routine",
                             symbol_type=RoutineSymbol,
                             datatype=DeferredType(),
                             visibility=Symbol.Visibility.PRIVATE)
    sym2 = symtab.new_symbol("data", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE,
                             visibility=Symbol.Visibility.PRIVATE,
                             constant_value=3)
    assert sym1.name == "routine_1"
    assert sym2.name == "data_1"
    assert type(sym1) == RoutineSymbol
    assert type(sym2) == DataSymbol
    assert symtab.lookup("routine_1") is sym1
    assert symtab.lookup("data_1") is sym2
    assert sym1.visibility is Symbol.Visibility.PRIVATE
    assert sym2.visibility is Symbol.Visibility.PRIVATE
    assert isinstance(sym1.datatype, DeferredType)
    assert sym2.datatype is INTEGER_TYPE
    assert sym2.constant_value is not None

    # Check that symbol_type only accepts symbols
    with pytest.raises(TypeError) as err:
        sym1 = symtab.new_symbol("wrong", symbol_type=str,
                                 visibility=Symbol.Visibility.PRIVATE)
    assert ("The symbol_type parameter should be a type class of Symbol or"
            " one of its sub-classes but found" in str(err.value))


def test_symbol_from_tag():
    ''' Tests the SymbolTable symbol_from_tag method '''
    # pylint: disable=unidiomatic-typecheck
    symtab = SymbolTable()
    existing_symbol = Symbol("existing")
    symtab.add(existing_symbol, tag="tag1")

    # If the given tag exists, return the symbol name
    assert symtab.symbol_from_tag("tag1") is existing_symbol

    # If the tag does not exist, create a new symbol with the tag
    tag2 = symtab.symbol_from_tag("tag2")
    assert isinstance(tag2, Symbol)
    assert symtab.lookup_with_tag("tag2") is tag2
    # By default it is a generic symbol with the same name as the tag
    assert type(tag2) == Symbol
    assert tag2.name == "tag2"

    # If the operation is repeated it returns the already created symbol
    tag2b = symtab.symbol_from_tag("tag2")
    assert tag2b is tag2

    # It can be given additional new_symbol parameters
    tag3 = symtab.symbol_from_tag("tag3",
                                  symbol_type=DataSymbol,
                                  datatype=INTEGER_TYPE,
                                  visibility=Symbol.Visibility.PRIVATE,
                                  constant_value=3)
    assert symtab.lookup_with_tag("tag3") is tag3
    assert type(tag3) == DataSymbol
    assert tag3.visibility is Symbol.Visibility.PRIVATE
    assert tag3.datatype is INTEGER_TYPE
    assert tag3.constant_value is not None

    # It can be given a different root_name
    tag4 = symtab.symbol_from_tag("tag4", root_name="var")
    assert symtab.lookup_with_tag("tag4") is tag4
    assert symtab.lookup_with_tag("tag4").name == "var"

    # If the given suggested name of an already created tag is different it
    # doesn't matter.
    tag4b = symtab.symbol_from_tag("tag4", root_name="anothername")
    assert tag4 is tag4b
    assert tag4b.name == "var"

    # Check that it fails if the Symbol type is different than expected
    with pytest.raises(SymbolError) as err:
        symtab.symbol_from_tag("tag3", symbol_type=RoutineSymbol)
    assert ("Expected symbol with tag 'tag3' to be of type 'RoutineSymbol' "
            "but found type 'DataSymbol'." in str(err.value))

    # TODO #1057: It should also fail the symbol is found but the properties
    # are different than the requested ones.


def test_rename_symbol():
    '''Test that the rename_symbol method renames a symbol and the change
    affects all its references. Also check that it fails when the arguments
    are not what the method expects.'''
    # Prepare the symbol table hierarchy for the test
    schedule_symbol_table, container_symbol_table = create_hierarchy()
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

    # Check argument conditions
    with pytest.raises(TypeError) as err:
        schedule_symbol_table.rename_symbol("not_a_symbol", "other")
    assert ("The symbol argument of rename_symbol() must be a Symbol, but "
            "found: 'str'." in str(err.value))

    with pytest.raises(TypeError) as err:
        schedule_symbol_table.rename_symbol(symbol, 3)
    assert ("The name argument of rename_symbol() must be a str, but "
            "found:" in str(err.value))

    with pytest.raises(ValueError) as err:
        container_symbol_table.rename_symbol(symbol, "somethingelse")
    assert ("The symbol argument of rename_symbol() must belong to this "
            "symbol_table instance, but " in str(err.value))

    with pytest.raises(KeyError) as err:
        schedule_symbol_table.rename_symbol(symbol, "array")
    assert ("The name argument of rename_symbol() must not already exist in "
            "this symbol_table instance, but 'array' does." in str(err.value))
