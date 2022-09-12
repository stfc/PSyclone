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
# Author S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests of the ArrayMixin PSyIR nodes trait. '''

import pytest
from psyclone.psyir.nodes import ArrayReference, ArrayOfStructuresReference, \
    Range, Literal
from psyclone.psyir.symbols import DataSymbol, DeferredType, ArrayType, \
    INTEGER_TYPE


def test_get_outer_range_index():
    '''Check that the get_outer_range_index method returns the outermost index
    of the children list that is a range. Use ArrayReference and
    ArrayOfStructuresReference as concrete implementations of ArrayMixins.
    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10, 10, 10]))
    array = ArrayReference.create(symbol, [Range(), Range(), Range()])
    assert array.get_outer_range_index() == 2

    symbol = DataSymbol("my_symbol", DeferredType())
    aos = ArrayOfStructuresReference.create(
        symbol, [Range(), Range(), Range()], ["nx"])
    assert aos.get_outer_range_index() == 3  # +1 for the member child


def test_get_outer_range_index_error():
    '''Check that the get_outer_range_index method raises an IndexError if
    no range exist as child of the given array. Use ArrayReference as concrete
    implementation of ArrayMixin.
    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array = ArrayReference.create(symbol, [Literal("2", INTEGER_TYPE)])
    with pytest.raises(IndexError):
        _ = array.get_outer_range_index()


def test_is_upper_lower_bound(fortran_reader):
    '''Test the is_lower_bound() and is_upper_bound() methods return the
    expected values if an array reference has literal bounds. Create
    and use an ArrayReference node to test the abstract ArrayMixin
    class.

    '''
    code = (
        "subroutine test()\n"
        "real a(10)\n"
        "a(1:10) = 0.0\n"
        "end subroutine\n")

    # Return True as the literal values or the declaration and array
    # reference match.
    psyir = fortran_reader.psyir_from_source(code)
    array_ref = psyir.children[0].children[0].lhs
    assert array_ref.is_lower_bound(0)
    assert array_ref.is_upper_bound(0)

    # Remove the symbol from the symbol table to force the returning
    # of False.
    symbol = array_ref.symbol
    symbol_table = array_ref.scope.symbol_table
    norm_name = symbol_table._normalize(symbol.name)
    symbol_table._symbols.pop(norm_name)
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)

    # Return False as the literal values of the array declaration and
    # array reference do not match.
    psyir = fortran_reader.psyir_from_source(code.replace("1:10", "2:9"))
    array_ref = psyir.children[0].children[0].lhs
    assert not array_ref.is_lower_bound(0)
    assert not array_ref.is_upper_bound(0)
