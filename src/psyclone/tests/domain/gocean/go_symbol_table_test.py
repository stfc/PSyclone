# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''pytest tests for the GOSymbolTable class.'''

import pytest

from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOSymbolTable
from psyclone.psyir.nodes import Schedule
from psyclone.psyir.symbols import (
    DataSymbol, ScalarType, UnresolvedType,
    ArgumentInterface, SymbolTable, Symbol)


def test_gosymboltable_conformity_check():
    '''Test that the expected exception is raised in method
    _check_gocean_conformity within GOSymbolTable when one or both of
    the first two kernel arguments are not scalar integers.

    '''
    symbol_table = GOSymbolTable()
    i_var = DataSymbol("i", ScalarType.integer_type(),
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    j_var = DataSymbol("j", ScalarType.integer_type(),
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    symbol_table.specify_argument_list([i_var, j_var])
    # Set the datatype of the first datasymbol to have an invalid type
    # in order to raise the required exception.
    symbol_table._argument_list[0].datatype = UnresolvedType()
    with pytest.raises(GenerationError) as excinfo:
        symbol_table._check_gocean_conformity()
    assert ("GOcean API kernels first argument should be a scalar integer "
            "but got 'UnresolvedType'." in str(excinfo.value))


def test_gosymboltable_properties():
    '''Test the various properties of a GOSymbolTable.'''
    table = GOSymbolTable()
    i_var = DataSymbol("i", ScalarType.integer_type(),
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    table.specify_argument_list([i_var])
    # Incomplete argument list.
    with pytest.raises(GenerationError) as err:
        _ = table.iteration_indices
    assert ("GOcean API kernels should always have at least two arguments "
            "representing the iteration indices but the Symbol Table has "
            "only 1 argument" in str(err.value))
    j_var = DataSymbol("j", ScalarType.integer_type(),
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    table.specify_argument_list([i_var, j_var])
    assert table.iteration_indices == [i_var, j_var]
    assert table.data_arguments == []
    fld_var = DataSymbol("fld1", ScalarType.integer_type(),
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.WRITE))
    table.specify_argument_list([i_var, j_var, fld_var])
    assert table.data_arguments == [fld_var]


def test_gosymboltable_create_from_table():
    '''Test the create_from_table() method of GOSymbolTable.'''
    with pytest.raises(TypeError) as err:
        GOSymbolTable.create_from_table("hello")
    assert ("expected an instance of SymbolTable but got a 'str'" in
            str(err.value))
    table = SymbolTable(default_visibility=Symbol.Visibility.PRIVATE)
    my_sched = Schedule(symbol_table=table)
    tsym = table.new_symbol("tom", symbol_type=DataSymbol,
                            datatype=ScalarType.integer_type())
    psym = table.new_symbol("port", symbol_type=DataSymbol,
                            datatype=ScalarType.real_type(),
                            interface=ArgumentInterface())
    starb_sym = table.new_symbol("starboad", tag="right",
                                 symbol_type=DataSymbol,
                                 datatype=ScalarType.real_type())
    table.specify_argument_list([psym])
    gotable = GOSymbolTable.create_from_table(table)
    assert isinstance(gotable, GOSymbolTable)
    assert gotable.lookup("tom") is tsym
    assert gotable.lookup("port") is psym
    assert gotable.lookup_with_tag("right") is starb_sym
    assert gotable.argument_list == [psym]
    assert gotable.node is my_sched
    assert gotable.default_visibility == Symbol.Visibility.PRIVATE
