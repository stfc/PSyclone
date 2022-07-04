# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.routinesymbol file '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import RoutineSymbol, Symbol, UnresolvedInterface,\
    NoType, INTEGER_TYPE, DeferredType, DataTypeSymbol


def test_routinesymbol_init():
    '''Test that a RoutineSymbol instance can be created.'''
    # A RoutineSymbol should be of type NoType by default.
    jo_sym = RoutineSymbol('jo')
    assert isinstance(jo_sym, RoutineSymbol)
    assert isinstance(jo_sym.datatype, NoType)
    ellie_sym = RoutineSymbol('ellie', INTEGER_TYPE,
                              visibility=Symbol.Visibility.PRIVATE)
    assert isinstance(ellie_sym, RoutineSymbol)
    assert ellie_sym.datatype == INTEGER_TYPE
    isaac_sym = RoutineSymbol('isaac', DeferredType(),
                              interface=UnresolvedInterface())
    assert isinstance(isaac_sym, RoutineSymbol)
    assert isinstance(isaac_sym.datatype, DeferredType)

    tam_type = DataTypeSymbol('tam_type', DeferredType())
    tam_sym = RoutineSymbol('tam', tam_type)
    assert isinstance(tam_sym, RoutineSymbol)
    assert tam_sym.datatype is tam_type


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


def test_routinesymbol_specialise_and_process_arguments():
    ''' Tests that a RoutineSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''

    # Try to make a RoutineSymbol without a datatype
    sym1 = Symbol("symbol1")
    sym1.specialise(RoutineSymbol)
    assert isinstance(sym1.datatype, NoType)

    # Include a datatype
    sym2 = Symbol("symbol2")
    sym2.specialise(RoutineSymbol, datatype=INTEGER_TYPE)
    assert sym2.datatype is INTEGER_TYPE


def test_routinesymbol_str():
    '''Test that the __str__ method in routinesymbol behaves as expected.'''
    routine_symbol = RoutineSymbol("roo")
    assert routine_symbol.__str__() == "roo: RoutineSymbol<NoType>"
    routine_symbol = RoutineSymbol("roo", INTEGER_TYPE)
    assert (routine_symbol.__str__() ==
            "roo: RoutineSymbol<Scalar<INTEGER, UNDEFINED>>")
    type_sym = DataTypeSymbol("some_type", DeferredType())
    routine_symbol = RoutineSymbol("roo", type_sym)
    assert (routine_symbol.__str__() ==
            "roo: RoutineSymbol<some_type: DataTypeSymbol>")
