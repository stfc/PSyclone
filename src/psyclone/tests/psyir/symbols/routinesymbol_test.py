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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.routinesymbol file '''

import pytest
from psyclone.psyir.symbols import (
    RoutineSymbol, Symbol, UnresolvedInterface,
    NoType, INTEGER_TYPE, UnresolvedType, DataTypeSymbol)


def test_routinesymbol_init():
    '''Test that a RoutineSymbol instance can be created.'''
    # A RoutineSymbol should be of type NoType by default.
    jo_sym = RoutineSymbol('jo')
    assert isinstance(jo_sym, RoutineSymbol)
    assert isinstance(jo_sym.datatype, NoType)
    # By default we don't know whether a symbol is pure or elemental.
    assert jo_sym.is_pure is None
    assert jo_sym.is_elemental is None
    ellie_sym = RoutineSymbol('ellie', INTEGER_TYPE,
                              visibility=Symbol.Visibility.PRIVATE)
    assert isinstance(ellie_sym, RoutineSymbol)
    assert ellie_sym.datatype == INTEGER_TYPE
    isaac_sym = RoutineSymbol('isaac', UnresolvedType(),
                              interface=UnresolvedInterface())
    assert isinstance(isaac_sym, RoutineSymbol)
    assert isinstance(isaac_sym.datatype, UnresolvedType)

    tam_type = DataTypeSymbol('tam_type', UnresolvedType())
    tam_sym = RoutineSymbol('tam', tam_type)
    assert isinstance(tam_sym, RoutineSymbol)
    assert tam_sym.datatype is tam_type
    # Check that is_pure and is_elemental can be specified.
    marvin_sym = RoutineSymbol('marvin', UnresolvedType(), is_pure=True)
    assert marvin_sym.is_pure is True
    paranoid_sym = RoutineSymbol('paranoid', UnresolvedType(),
                                 is_elemental=False)
    assert paranoid_sym.is_elemental is False


def test_routinesymbol_init_error():
    '''Test that the RoutineSymbol raises an error (via the Symbol parent
    class) if there is an invalid argument.

    '''
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol(None)
    assert ("RoutineSymbol 'name' attribute should be of type 'str' but "
            "'NoneType' found." in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("isaac", "integer")
    assert ("datatype of a RoutineSymbol must be specified using either a "
            "DataType or a DataTypeSymbol but got: 'str'" in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("android", UnresolvedType(), is_pure="maybe")
    assert ("is_pure for a RoutineSymbol must be a bool or None but got "
            "'str'" in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("android", UnresolvedType(), is_elemental="maybe")
    assert ("is_elemental for a RoutineSymbol must be a bool or None but got "
            "'str'" in str(error.value))


def test_routinesymbol_specialise_and_process_arguments():
    ''' Tests that a RoutineSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''

    # Try to make a RoutineSymbol without a datatype
    sym1 = Symbol("symbol1")
    sym1.specialise(RoutineSymbol)
    # pylint gets confused because it doesn't know about specialise()
    # pylint: disable=no-member
    assert isinstance(sym1.datatype, NoType)

    # Include a datatype
    sym2 = Symbol("symbol2")
    sym2.specialise(RoutineSymbol, datatype=INTEGER_TYPE)
    assert sym2.datatype is INTEGER_TYPE

    # Include is_pure
    sym3 = Symbol("sym3")
    sym3.specialise(RoutineSymbol, is_pure=False)
    assert sym3.is_pure is False

    # Include is_elemental
    sym4 = Symbol("sym4")
    sym4.specialise(RoutineSymbol, is_elemental=True)
    assert sym4.is_elemental is True


def test_routinesymbol_str():
    '''Test that the __str__ method in routinesymbol behaves as expected.'''
    routine_symbol = RoutineSymbol("roo")
    assert (str(routine_symbol) == "roo: RoutineSymbol<NoType, "
            "pure=unknown, elemental=unknown>")
    routine_symbol = RoutineSymbol("roo", INTEGER_TYPE)
    assert (str(routine_symbol) ==
            "roo: RoutineSymbol<Scalar<INTEGER, UNDEFINED>, pure=unknown, "
            "elemental=unknown>")
    type_sym = DataTypeSymbol("some_type", UnresolvedType())
    routine_symbol = RoutineSymbol("roo", type_sym, is_elemental=True,
                                   is_pure=True)
    assert (str(routine_symbol) ==
            "roo: RoutineSymbol<some_type: DataTypeSymbol, pure=True, "
            "elemental=True>")
    routine_symbol = RoutineSymbol("eyore", type_sym, is_elemental=False,
                                   is_pure=True)
    assert (str(routine_symbol) ==
            "eyore: RoutineSymbol<some_type: DataTypeSymbol, pure=True, "
            "elemental=False>")
