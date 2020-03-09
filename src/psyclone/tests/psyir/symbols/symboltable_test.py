# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
import pytest
from psyclone.psyir.nodes import Schedule
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ContainerSymbol, \
    LocalInterface, GlobalInterface, ArgumentInterface, UnresolvedInterface, \
    DataType, Symbol
from psyclone.errors import InternalError


def test_new_symbol_name_1():
    '''Test that the new_symbol_name method returns names that are not
    already in the symbol table.

    '''
    # Create a symbol table containing a symbol
    sym_table = SymbolTable()
    sym_table.add(ContainerSymbol("my_mod"))

    # Check we can generate a new symbol name (and add it to the symbol
    # table as this is required for further testing).
    name = sym_table.new_symbol_name()
    assert name == "psyir_tmp"
    sym_table.add(DataSymbol(name, DataType.REAL))
    # Check we return the expected symbol name when there is a
    # supplied root name.
    assert sym_table.new_symbol_name(root_name="my_name") == "my_name"
    # Check we return a new symbol by appending an integer index to
    # the root name when the names clash.
    name = sym_table.new_symbol_name(root_name="my_mod")
    assert name == "my_mod_1"
    sym_table.add(ContainerSymbol(name))
    name = sym_table.new_symbol_name(root_name="my_mod")
    assert name == "my_mod_2"
    name = sym_table.new_symbol_name(root_name="my_mod_1")
    assert name == "my_mod_1_1"
    # Check we return a new symbol by appending an integer index to
    # the default name when the names clash.
    name = sym_table.new_symbol_name()
    assert name == "psyir_tmp_1"
    sym_table.add(DataSymbol(name, DataType.REAL))
    assert sym_table.new_symbol_name() == "psyir_tmp_2"


def test_new_symbol_name_2():
    '''Test that the new_symbol_name method returns an internal name if
    the supplied root_name argument is an empty string.

    '''
    sym_table = SymbolTable()
    name = sym_table.new_symbol_name(root_name="")
    assert name == "psyir_tmp"


def test_new_symbol_name_3():
    '''Test that the new_symbol_name method returns an internal name if
    the supplied root_name argument is None.

    '''
    sym_table = SymbolTable()
    name = sym_table.new_symbol_name(root_name=None)
    assert name == "psyir_tmp"


def test_new_symbol_name_4():
    '''Test that the new_symbol_name method raises the expected exception
    if the root_name argument has the wrong type.

    '''
    sym_table = SymbolTable()
    with pytest.raises(TypeError) as excinfo:
        _ = sym_table.new_symbol_name(root_name=7)
    assert ("Argument root_name should be of type str or NoneType but found "
            "'int'." in str(excinfo.value))


def test_add():
    '''Test that the add method inserts new symbols in the symbol table,
    but raises appropriate errors when provided with wrong parameters
    or duplicate declarations.'''
    sym_table = SymbolTable()

    # Declare a symbol
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    sym_table.add(DataSymbol("var1", DataType.REAL, shape=[5, 1],
                             interface=GlobalInterface(my_mod)))
    assert sym_table._symbols["my_mod"].name == "my_mod"
    assert sym_table._symbols["var1"].name == "var1"
    assert sym_table._symbols["var1"].datatype == DataType.REAL
    assert sym_table._symbols["var1"].shape == [5, 1]
    assert sym_table._symbols["var1"].interface.container_symbol == my_mod

    # Declare a duplicate name symbol
    with pytest.raises(KeyError) as error:
        sym_table.add(DataSymbol("var1", DataType.REAL))
    assert ("Symbol table already contains a symbol with name "
            "'var1'.") in str(error.value)


def test_add_with_tags():
    '''Test that the add method with a tag inserts new symbols in the symbol
    table and raises appropiate errors.'''
    sym_table = SymbolTable()

    sym1 = Symbol("symbol_notag")
    sym2 = Symbol("symbol_tag1")
    sym3 = Symbol("symbol_tag2")
    sym_table.add(sym1)
    assert len(sym_table._tags) == 0  # No tag added if none given
    sym_table.add(sym2, tag="tag1")
    sym_table.add(sym3, tag="tag2")

    assert len(sym_table._symbols) == 3
    assert len(sym_table._tags) == 2
    assert "tag1" in sym_table._tags
    assert sym_table._tags["tag1"] == sym2
    assert "tag2" in sym_table._tags
    assert sym_table._tags["tag2"] == sym3

    with pytest.raises(KeyError) as error:
        sym_table.add(DataSymbol("var1", DataType.REAL), tag="tag1")
    assert ("Symbol table already contains the tag 'tag1' for symbol "
            "'symbol_tag1', so it can not be associated to symbol "
            "'var1'.") in str(error.value)


def test_imported_symbols():
    ''' Test the imported_symbols method. '''
    sym_table = SymbolTable()
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    assert sym_table.imported_symbols(my_mod) == []
    var1 = DataSymbol("var1", DataType.REAL, interface=LocalInterface())
    sym_table.add(var1)
    assert sym_table.imported_symbols(my_mod) == []
    var2 = DataSymbol("var2", DataType.INTEGER,
                      interface=GlobalInterface(my_mod))
    assert sym_table.imported_symbols(my_mod) == []
    sym_table.add(var2)
    assert sym_table.imported_symbols(my_mod) == [var2]
    var3 = DataSymbol("var3", DataType.INTEGER,
                      interface=GlobalInterface(my_mod))
    sym_table.add(var3)
    imported_symbols = sym_table.imported_symbols(my_mod)
    assert var3 in imported_symbols
    assert var2 in imported_symbols
    # Passing something that is not a ContainerSymbol is an error
    with pytest.raises(TypeError) as err:
        sym_table.imported_symbols(var2)
    assert "expects a ContainerSymbol but got an object of type" in \
        str(err.value)
    # Passing a ContainerSymbol that is not in the SymbolTable is an error
    with pytest.raises(KeyError) as err:
        sym_table.imported_symbols(ContainerSymbol("another_mod"))
    assert "Could not find 'another_mod' in " in str(err.value)
    # Passing a ContainerSymbol that is not in the SymbolTable but that has
    # the same name as one that is is an error
    with pytest.raises(KeyError) as err:
        sym_table.imported_symbols(ContainerSymbol("my_mod"))
    assert ("The 'my_mod' entry in this SymbolTable is not the supplied "
            "ContainerSymbol" in str(err.value))


def test_remove():
    '''Test that the remove method removes ContainerSymbols from the symbol
    table. Also checks that appropriate errors are raised when the method is
    provided with wrong parameters or if there are DataSymbols that reference
    the provided ContainerSymbol. '''
    sym_table = SymbolTable()

    # Declare a symbol
    my_mod = ContainerSymbol("my_mod")
    sym_table.add(my_mod)
    sym_table.add(DataSymbol("var1", DataType.REAL, shape=[5, 1],
                             interface=GlobalInterface(my_mod)))
    var1 = sym_table.lookup("var1")
    assert var1
    assert sym_table.imported_symbols(my_mod) == [var1]
    # We should not be able to remove a Symbol that is not a ContainerSymbol
    with pytest.raises(TypeError) as err:
        sym_table.remove(var1)
    assert "expects a ContainerSymbol object but got" in str(err.value)
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
    # Attempt to supply something that is not a Symbol
    with pytest.raises(TypeError) as err:
        sym_table.remove("broken")
    assert ("remove() expects a ContainerSymbol object but got: "
            in str(err.value))
    # Attempting to remove a Symbol that is not in the table but that has
    # the same name as an entry in the table is an error
    sym_table.add(ContainerSymbol("my_mod"))
    with pytest.raises(InternalError) as err:
        sym_table.remove(ContainerSymbol("my_mod"))
    assert ("Symbol with name 'my_mod' in this symbol table is not the "
            "same" in str(err.value))


def test_swap_symbol_properties():
    ''' Test the symboltable swap_properties method '''

    symbol1 = DataSymbol("var1", DataType.INTEGER, shape=[], constant_value=7)
    symbol2 = DataSymbol("dim1", DataType.INTEGER,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READ))
    symbol3 = DataSymbol("dim2", DataType.INTEGER,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READ))
    symbol4 = DataSymbol("var2", DataType.REAL, shape=[symbol2, symbol3],
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

    # Raise exception if both symbols have the same name
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
    assert symbol1.datatype == DataType.REAL
    assert symbol1.shape == [symbol2, symbol3]
    assert symbol1.is_argument
    assert symbol1.constant_value is None
    assert symbol1.interface.access == ArgumentInterface.Access.READWRITE

    assert symbol4.name == "var2"
    assert symbol4.datatype == DataType.INTEGER
    assert not symbol4.shape
    assert symbol4.is_local
    assert symbol4.constant_value.value == "7"
    assert symbol4.constant_value.datatype == DataType.INTEGER

    # Check symbol references are unaffected
    sym_table.swap_symbol_properties(symbol2, symbol3)
    assert symbol1.shape[0].name == "dim1"
    assert symbol1.shape[1].name == "dim2"

    # Check argument positions are updated. The original positions
    # were [dim1, dim2, var2]. They should now be [dim2, dim1, var1]
    assert sym_table.argument_list[0].name == "dim2"
    assert sym_table.argument_list[1].name == "dim1"
    assert sym_table.argument_list[2].name == "var1"


def test_lookup():
    '''Test that the lookup method retrieves symbols from the symbol table
    if the name exists, otherwise it raises an error.'''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", DataType.REAL,
                             shape=[DataSymbol.Extent.ATTRIBUTE,
                                    DataSymbol.Extent.ATTRIBUTE]))
    sym_table.add(DataSymbol("var2", DataType.INTEGER, shape=[]))
    sym_table.add(DataSymbol("var3", DataType.REAL, shape=[]))

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


def test_lookup_with_tag():
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


def test_view(capsys):
    '''Test the view method of the SymbolTable class, it should print to
    standard out a representation of the full SymbolTable.'''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", DataType.REAL))
    sym_table.add(DataSymbol("var2", DataType.INTEGER))
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
    sym_table.add(DataSymbol("var1", DataType.REAL))
    sym_table.add(DataSymbol("var2", DataType.INTEGER))
    sym_table.add(DataSymbol("var3", DataType.DEFERRED,
                             interface=GlobalInterface(my_mod)))
    sym_table_text = str(sym_table)
    assert "Symbol Table:\n" in sym_table_text
    assert "var1" in sym_table_text
    assert "var2" in sym_table_text
    assert "\nmy_mod" in sym_table_text
    assert "Global(container='my_mod')" in sym_table_text


def test_specify_argument_list():
    '''Test that the specify argument list method sets the argument_list
    with references to each DataSymbol and updates the DataSymbol attributes
    when needed.'''
    sym_table = SymbolTable()
    sym_v1 = DataSymbol("var1", DataType.REAL, [])
    sym_table.add(sym_v1)
    sym_table.add(DataSymbol("var2", DataType.REAL, []))
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
    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL, []))
    sym_v1 = sym_table.lookup("var1")
    # Attempt to say the argument list consists of "var1" which at this
    # point is just a local variable.
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err.value)
    assert "has an interface of type '" in str(err.value)
    # Now add an Interface for "var1" but of the wrong type
    sym_v1.interface = GlobalInterface(ContainerSymbol("my_mod"))
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err.value)
    assert "has an interface of type '" in str(err.value)


def test_argument_list_errors():
    ''' Tests the internal sanity checks of the SymbolTable.argument_list
    property. '''
    sym_table = SymbolTable()
    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL, []))
    sym_table.add(DataSymbol("var3", DataType.REAL,
                             interface=GlobalInterface(
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
    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL, []))
    sym_table.add(DataSymbol("var3", DataType.REAL,
                             interface=GlobalInterface(
                                 ContainerSymbol("my_mod"))))
    # Everything should be fine so far
    sym_table._validate_non_args()
    # Add an entry with an Argument interface
    sym_table.add(DataSymbol("var4", DataType.REAL,
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

    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL,
                             [DataSymbol.Extent.ATTRIBUTE]))

    assert "var1" in sym_table
    assert "var2" in sym_table
    assert "var3" not in sym_table


def test_symbols():
    '''Test that the symbols property returns a list of the symbols in the
    SymbolTable.'''
    sym_table = SymbolTable()
    assert sym_table.symbols == []
    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL,
                             [DataSymbol.Extent.ATTRIBUTE]))
    assert len(sym_table.symbols) == 2
    sym_table.add(DataSymbol("var3", DataType.REAL, [],
                             interface=GlobalInterface(
                                 ContainerSymbol("my_mod"))))
    assert len(sym_table.symbols) == 3


def test_local_datasymbols():
    '''Test that the local_datasymbols property returns a list with the
    symbols with local scope.'''
    sym_table = SymbolTable()
    assert [] == sym_table.local_datasymbols

    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL,
                             [DataSymbol.Extent.ATTRIBUTE]))
    sym_table.add(DataSymbol("var3", DataType.REAL, []))

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

    sym_table.add(DataSymbol("var4", DataType.REAL, [],
                             interface=GlobalInterface(
                                 ContainerSymbol("my_mod"))))
    assert len(sym_table.local_datasymbols) == 2
    assert sym_table.lookup("var4") not in sym_table.local_datasymbols


def test_global_datasymbols():
    '''Test that the global_datasymbols property returns those DataSymbols with
    'global' scope (i.e. that represent data that exists outside the current
    scoping unit) but are not routine arguments. '''
    sym_table = SymbolTable()
    assert sym_table.global_datasymbols == []
    # Add some local symbols
    sym_table.add(DataSymbol("var1", DataType.REAL, []))
    sym_table.add(DataSymbol("var2", DataType.REAL,
                             [DataSymbol.Extent.ATTRIBUTE]))
    assert sym_table.global_datasymbols == []
    # Add some global symbols
    sym_table.add(DataSymbol("gvar1", DataType.REAL, [],
                             interface=GlobalInterface(
                                 ContainerSymbol("my_mod"))))
    assert sym_table.lookup("gvar1") in sym_table.global_datasymbols
    sym_table.add(
        DataSymbol("gvar2", DataType.REAL, [],
                   interface=ArgumentInterface(
                       ArgumentInterface.Access.READWRITE)))
    gsymbols = sym_table.global_datasymbols
    assert len(gsymbols) == 1
    assert sym_table.lookup("gvar2") not in gsymbols


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
    sym_table.add(DataSymbol("s1", DataType.INTEGER, []))
    # Check that we get an empty list if everything is defined
    assert sym_table.get_unresolved_datasymbols() == []
    # Add a symbol with a deferred interface
    rdef = DataSymbol("r_def", DataType.INTEGER,
                      interface=UnresolvedInterface())
    sym_table.add(rdef)
    assert sym_table.get_unresolved_datasymbols() == ["r_def"]
    # Add a symbol that uses r_def for its precision
    sym_table.add(DataSymbol("s2", DataType.REAL, [], precision=rdef))
    # By default we should get this precision symbol
    assert sym_table.get_unresolved_datasymbols() == ["r_def"]
    # But not if we request that precision symbols be ignored
    assert sym_table.get_unresolved_datasymbols(ignore_precision=True) == []


def test_copy_external_global():
    ''' Tests the SymbolTable copy_external_global method. '''

    symtab = SymbolTable()

    # Test input argument type checking
    with pytest.raises(TypeError) as error:
        symtab.copy_external_global("invalid_type")
    assert "The globalvar argument of SymbolTable.copy_external_global " \
        "method should be a DataSymbol, but found " \
        in str(error.value)

    with pytest.raises(TypeError) as error:
        symtab.copy_external_global(DataSymbol("var1", DataType.REAL))
    assert "The globalvar argument of SymbolTable.copy_external_global " \
        "method should have a GlobalInterface interface, but found " \
        "'LocalInterface'." \
        in str(error.value)

    # Copy a globalvar
    container = ContainerSymbol("my_mod")
    var = DataSymbol("a", DataType.DEFERRED,
                     interface=GlobalInterface(container))
    symtab.copy_external_global(var)
    assert "a" in symtab
    assert "my_mod" in symtab
    assert var.interface.container_symbol.name == "my_mod"
    # The symtab items should be new copies not connected to the original
    assert symtab.lookup("a") != var
    assert symtab.lookup("my_mod") != container
    assert symtab.lookup("a").interface.container_symbol != container

    # Copy a second globalvar with a reference to the same external Container
    container2 = ContainerSymbol("my_mod")
    var2 = DataSymbol("b", DataType.DEFERRED,
                      interface=GlobalInterface(container2))
    symtab.copy_external_global(var2)
    assert "b" in symtab
    assert "my_mod" in symtab
    assert var2.interface.container_symbol.name == "my_mod"
    assert symtab.lookup("b") != var2
    assert symtab.lookup("my_mod") != container2
    assert symtab.lookup("b").interface.container_symbol != container2

    # The new globalvar should reuse the available container reference
    assert symtab.lookup("a").interface.container_symbol == \
        symtab.lookup("b").interface.container_symbol

    # The copy of globalvars that already exist is supported
    var3 = DataSymbol("b", DataType.DEFERRED,
                      interface=GlobalInterface(container2))
    symtab.copy_external_global(var3)

    # But if the symbol is different (e.g. points to a different container),
    # it should fail
    container3 = ContainerSymbol("my_other_mod")
    var4 = DataSymbol("b", DataType.DEFERRED,
                      interface=GlobalInterface(container3))
    with pytest.raises(KeyError) as error:
        symtab.copy_external_global(var4)
    assert "Couldn't copy 'b: <DataType.DEFERRED, Scalar, Global(container=" \
           "'my_other_mod')>' into the SymbolTable. The name 'b' is already" \
           " used by another symbol." in str(error.value)

    # If the symbol is the same but the given tag in not in the symbol table,
    # the new tag should reference the existing symbol
    symtab.copy_external_global(var3, tag="anothertag")
    assert symtab.lookup_with_tag("anothertag").name == "b"

    # If a tag is given but this is already used, it should fail
    symtab.add(Symbol("symbol"), tag="tag")
    var5 = DataSymbol("c", DataType.DEFERRED,
                      interface=GlobalInterface(container3))
    with pytest.raises(KeyError) as error:
        symtab.copy_external_global(var5, "tag")
    assert "Symbol table already contains the tag 'tag' for symbol 'symbol'," \
           " so it can not be associated to symbol 'c'." in str(error.value)

    # It should also fail if the symbol exist and the tag is given to another
    # symbol
    with pytest.raises(KeyError) as error:
        symtab.copy_external_global(var3, "tag")
    assert " into the SymbolTable. The tag 'tag' is already used by another" \
        " symbol." in str(error.value)

    # If the tag does not already exist, the tag is associated with the new
    # symbol
    var6 = DataSymbol("d", DataType.DEFERRED,
                      interface=GlobalInterface(container3))
    symtab.copy_external_global(var6, "newtag")
    assert symtab.lookup_with_tag("newtag").name == "d"


def test_normalization():
    ''' Tests the SymbolTable normalize method lower cases the strings '''
    assert SymbolTable._normalize("aAbB") == "aabb"


def test_shallow_copy():
    ''' Tests the SymbolTable shallow copy generated new top-level containers
    but keeps the same objects in the symbol table'''

    # Create an initial SymbolTable
    dummy = Schedule()
    symtab = SymbolTable(schedule=dummy)
    sym1 = DataSymbol("symbol1", DataType.INTEGER,
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
    assert symtab2._schedule == dummy
    assert sym1 in symtab2.argument_list

    # Add new symbols in both symbols tables and check they are not added
    # to the other symbol table
    symtab.add(Symbol("st1"))
    symtab2.add(Symbol("st2"))
    assert "st1" in symtab
    assert "st2" in symtab2
    assert "st2" not in symtab
    assert "st1" not in symtab2


def test_name_from_tag():
    ''' Tests the SymbolTable name_from_tag method '''
    symtab = SymbolTable()
    symtab.add(Symbol("symbol1"), tag="tag1")

    # If the give tag exist, return the symbol name
    assert symtab.name_from_tag("tag1") == "symbol1"

    # If the tag does not exist, create a symbol with the tag as name
    assert symtab.name_from_tag("tag2") == "tag2"
    assert symtab.lookup("tag2").name == "tag2"
    assert symtab.lookup_with_tag("tag2").name == "tag2"

    # If the tag does not exist and a root name is givem create a new symbol
    # with the given name as root
    assert symtab.name_from_tag("tag3", root="newsymbol") == "newsymbol"
    assert symtab.lookup("newsymbol").name == "newsymbol"
    assert symtab.lookup_with_tag("tag3").name == "newsymbol"
    assert symtab.name_from_tag("tag4", root="newsymbol") == "newsymbol_1"
