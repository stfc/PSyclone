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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the DataTypeSymbol class. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import DataTypeSymbol, UnresolvedType, Symbol, \
    UnresolvedInterface, ArrayType, REAL_SINGLE_TYPE


def test_create_datatypesymbol():
    ''' Check that a basic DataTypeSymbol can be created with the expected
    properties. '''
    sym = DataTypeSymbol("my_type", UnresolvedType())
    assert sym.name == "my_type"
    assert isinstance(sym.datatype, UnresolvedType)
    assert str(sym) == "my_type: DataTypeSymbol"


def test_create_datatypesymbol_wrong_datatype():
    ''' Check that attempting to specify the type of a DataTypeSymbol with an
    invalid type results in the expected error. '''
    sym = DataTypeSymbol("my_type", UnresolvedType())
    with pytest.raises(TypeError) as err:
        sym.datatype = "integer"
    assert ("datatype of a DataTypeSymbol must be specified using a "
            "DataType but got: 'str'" in str(err.value))


def test_datatypesymbol_copy():
    ''' Check that a DataTypeSymbol can be copied. '''
    symbol = DataTypeSymbol("my_type", UnresolvedType(),
                            visibility=Symbol.Visibility.PRIVATE,
                            interface=UnresolvedInterface())
    new_symbol = symbol.copy()
    assert new_symbol is not symbol
    assert new_symbol.name == "my_type"
    assert isinstance(new_symbol.datatype, UnresolvedType)
    assert new_symbol.visibility == Symbol.Visibility.PRIVATE
    assert isinstance(new_symbol.interface, UnresolvedInterface)


def test_data_type_symbol_copy_properties():
    ''' Check that the copy_properties() method works as expected. '''
    symbol = DataTypeSymbol("origin", ArrayType(REAL_SINGLE_TYPE, [1, 2]))
    new_sym = DataTypeSymbol("new_name", UnresolvedType())

    new_sym.copy_properties(symbol)

    # new_sym name should be unchanged, but its datatype should be updated
    assert new_sym.name == "new_name"
    assert new_sym.datatype == symbol.datatype
    assert isinstance(new_sym.datatype, ArrayType)
    assert new_sym.datatype.intrinsic.name == "REAL"
    assert new_sym.datatype.shape[1] == symbol.datatype.shape[1]

    with pytest.raises(TypeError) as err:
        new_sym.copy_properties(REAL_SINGLE_TYPE)
    assert ("Argument should be of type 'DataTypeSymbol' but found "
            "'ScalarType'" in str(err.value))
