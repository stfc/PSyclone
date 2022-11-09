# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

''' This module tests the LFric-specific SymbolTable.'''

import pytest

from psyclone.domain.lfric import psyir, LFRicSymbolTable
from psyclone.psyir.symbols import (DataSymbol, REAL_DOUBLE_TYPE,
                                    RoutineSymbol, Symbol)

TEST_API = "dynamo0.3"


def test_get_or_create_integer():
    '''Checks the get_or_create_integer convenience function. '''
    symbol_table = LFRicSymbolTable()
    sym_i = symbol_table.get_or_create_integer_symbol("i")
    assert isinstance(sym_i, Symbol)
    assert sym_i.name == "i"
    # pylint: disable=no-member
    assert sym_i.datatype == psyir.LfricIntegerScalarDataType()

    # Make sure the symbol exists without a tag:
    sym_i2 = symbol_table.lookup("i")
    assert sym_i2 is sym_i
    with pytest.raises(KeyError) as err:
        symbol_table.lookup_with_tag("i")
    assert "Could not find the tag 'i' in the Symbol Table" in str(err.value)

    # If we call the same function again, we must get the same symbol.
    sym_i2 = symbol_table.get_or_create_integer_symbol("i")
    assert sym_i2 is sym_i

    # Now create a variable with the same name, but a tag. In this case the
    # name of the created symbol must be different:
    sym_i_tag = symbol_table.get_or_create_integer_symbol("i", tag="i")
    assert sym_i_tag is not sym_i
    assert sym_i_tag.name != sym_i.name

    sym_i_tag2 = symbol_table.lookup_with_tag("i")
    assert sym_i_tag2 is sym_i_tag


def test_get_or_create_integer_errors():
    '''Tests various error conditions of get_or_create_integer_symbol.'''

    symbol_table = LFRicSymbolTable()
    routine = RoutineSymbol('routine')
    symbol_table.add(routine)

    with pytest.raises(TypeError) as err:
        symbol_table.get_or_create_integer_symbol("routine")
    assert ("Symbol routine already exists, but is not a DataTypeSymbol"
            in str(err.value))

    symbol_table.new_symbol("real", symbol_type=DataSymbol,
                            datatype=REAL_DOUBLE_TYPE)
    with pytest.raises(TypeError) as err:
        symbol_table.get_or_create_integer_symbol("real")
    assert ("Symbol real already exists, but is not an integer"
            in str(err.value))
