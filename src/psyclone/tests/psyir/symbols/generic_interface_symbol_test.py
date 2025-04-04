# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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

''' This module contains pytest tests for GenericInterfaceSymbol.'''

import pytest

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.symbols import (
    ContainerSymbol, GenericInterfaceSymbol, ImportInterface, INTEGER_TYPE,
    RoutineSymbol, SymbolTable, Symbol, UnresolvedInterface)


def test_gis_constructor():
    '''
    Test that a GenericInterfaceSymbol can be constructed and that the
    expected errors are raised if an incorrect argument is provided.

    '''
    with pytest.raises(ValueError) as err:
        _ = GenericInterfaceSymbol("beech", [])
    assert ("A GenericInterfaceSymbol requires a list of RoutineSymbols but "
            "none were provided." in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = GenericInterfaceSymbol("oak", "sycamore")
    assert ("requires a list of tuples describing its member routines but "
            "got: 'sycamore'" in str(err.value))
    acorn = RoutineSymbol("acorn")
    with pytest.raises(TypeError) as err:
        _ = GenericInterfaceSymbol("oak", [acorn, "sycamore"])
    assert ("GenericInterfaceSymbol ('oak') requires a list of tuples but "
            "got: 'RoutineSymbol'" in str(err.value))
    with pytest.raises(TypeError) as err:
        _ = GenericInterfaceSymbol("oak", [(acorn, False),
                                           (acorn, False, False)])
    assert ("Each tuple used to define a routine within the "
            "GenericInterfaceSymbol 'oak' must consist of a RoutineSymbol and "
            "a bool but got: ['RoutineSymbol', 'bool', 'bool']" in
            str(err.value))
    oak = GenericInterfaceSymbol("oak", [(acorn, False)])
    assert len(oak.routines) == 1
    assert oak.routines[0].symbol is acorn
    assert oak.routines[0].from_container is False
    assert oak.container_routines == []
    assert oak.external_routines == [acorn]
    nut = RoutineSymbol("nut")
    oak.routines.append(GenericInterfaceSymbol.RoutineInfo(nut, True))
    assert oak.container_routines == [nut]


def test_gis_specialise():
    '''
    Specialise a generic symbol into a GenericInterfaceSymbol.

    '''
    # Specialise symbols without routines
    symbol = Symbol("no_routines")
    symbol.specialise(GenericInterfaceSymbol)
    assert symbol.routines == []  # It now has a routines attribute

    symbol = Symbol("has_routines")
    impl1 = RoutineSymbol("impl1")
    impl2 = RoutineSymbol("impl2")
    symbol.specialise(GenericInterfaceSymbol,
                      routines=[(impl1, True), (impl2, False)])
    assert symbol.routines[0].symbol is impl1
    assert symbol.routines[0].from_container is True
    assert symbol.routines[1].symbol is impl2
    assert symbol.routines[1].from_container is False


def test_gis_typedsymbol_keywords():
    '''
    Test that keyword arguments to the constructor are passed through to the
    TypedSymbol constructor.
    '''
    walnut = GenericInterfaceSymbol("walnut", [(RoutineSymbol("nut"), True)],
                                    datatype=INTEGER_TYPE)
    assert walnut.datatype == INTEGER_TYPE


def test_gis_str():
    '''
    Test the __str__ method of GenericInterfaceSymbol.
    '''
    ash = RoutineSymbol("ash")
    holly = RoutineSymbol("holly")
    coppice = GenericInterfaceSymbol("coppice", [(ash, True), (holly, False)])
    assert str(coppice) == ("coppice: GenericInterfaceSymbol<NoType, "
                            "routines=['ash', 'holly']>")


def test_gis_copy():
    '''
    Test the copy() method of GenericInterfaceSymbol.
    '''
    ash = RoutineSymbol("ash")
    holly = RoutineSymbol("holly")
    coppice = GenericInterfaceSymbol("coppice", [(ash, True), (holly, False)])
    spinney = coppice.copy()
    assert isinstance(spinney, GenericInterfaceSymbol)
    assert spinney is not coppice
    assert len(spinney.routines) == 2
    # The list of routines should be a copy.
    assert spinney.routines is not coppice.routines
    # The Routine objects themselves should be unchanged.
    rsyms = [item.symbol for item in spinney.routines]
    assert ash in rsyms
    assert holly in rsyms


def test_gis_copy_properties():
    '''
    Test the copy_properties() method of GenericInterfaceSymbol.
    '''
    ash = RoutineSymbol("ash")
    holly = RoutineSymbol("holly")
    coppice = GenericInterfaceSymbol("coppice", [(ash, True), (holly, False)])
    oak = RoutineSymbol("oak")
    new_sym = GenericInterfaceSymbol("spinney", [(oak, True)],
                                     interface=UnresolvedInterface())
    new_sym.copy_properties(coppice)
    assert ([info.symbol.name for info in new_sym.routines] ==
            [info.symbol.name for info in coppice.routines])
    # Check that if the interface symbol has an import interface then so do
    # its constituent RoutineSymbols.
    csym = ContainerSymbol("woodland")
    coppice2 = GenericInterfaceSymbol("coppice2", [(ash, True)],
                                      interface=ImportInterface(csym))
    new_sym.copy_properties(coppice2)
    for info in new_sym.routines:
        assert info.symbol.interface.container_symbol is csym


@pytest.mark.parametrize("table", [None, SymbolTable()])
def test_gis_replace_symbols_using(table):
    '''Test that replace_symbols_using() correctly updates symbols referred
    to by a GenericInterfaceSymbol.

    '''
    ash = RoutineSymbol("ash")
    holly = RoutineSymbol("holly")
    birch = RoutineSymbol("birch")
    coppice = GenericInterfaceSymbol("coppice", [(ash, True), (holly, False),
                                                 (birch, True)])
    for rinfo in coppice.routines:
        assert rinfo.symbol in [ash, holly, birch]
    ashling = ash.copy()

    # Test when the replacement symbol is currently just a Symbol. It should
    # be converted into a RoutineSymbol (in place).
    birch = Symbol("birch")
    if table is not None:
        table.add(ashling)
        table.add(birch)
        coppice.replace_symbols_using(table)
    else:
        coppice.replace_symbols_using(ashling)
        coppice.replace_symbols_using(birch)
    for rinfo in coppice.routines:
        assert rinfo.symbol in [ashling, holly, birch]
        assert isinstance(rinfo.symbol, RoutineSymbol)
    newholly = holly.copy()
    if table is not None:
        table.add(newholly)
        coppice.replace_symbols_using(table)
    else:
        coppice.replace_symbols_using(newholly)
    for rinfo in coppice.routines:
        assert rinfo.symbol in [ashling, newholly, birch]


def test_gis_reference_accesses():
    '''Tests for the reference_accesses() method.'''
    ash = RoutineSymbol("ash")
    holly = RoutineSymbol("holly")
    birch = RoutineSymbol("birch")
    coppice = GenericInterfaceSymbol("coppice", [(ash, True), (holly, False),
                                                 (birch, True)])
    vai = VariablesAccessInfo()
    coppice.reference_accesses(vai)
    all_names = [sig.var_name for sig in vai.all_signatures]
    assert len(all_names) == 3
    assert "ash" in all_names
    assert "holly" in all_names
    assert "birch" in all_names
