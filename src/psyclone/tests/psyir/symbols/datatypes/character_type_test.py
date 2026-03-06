# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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

'''
py.test tests for the psyclone.psyir.symbols.datatypes.CharacterType
class.

'''

import pytest

from psyclone.psyir.nodes import Literal, Reference
from psyclone.psyir.symbols import DataSymbol
from psyclone.psyir.symbols.datatypes import (
    CharacterType, INTEGER_TYPE, IntrinsicType)


def test_constructor():
    '''
    Test that a CharacterType can be constructed and that its properties
    are set correctly.
    '''
    ctype = CharacterType(IntrinsicType.Precision.UNDEFINED, 1)
    assert ctype.length == Literal("1", INTEGER_TYPE)
    assert ctype.precision == IntrinsicType.Precision.UNDEFINED


def test_str():
    '''
    Test the __str__ method.
    '''
    ctype = CharacterType(IntrinsicType.Precision.UNDEFINED,
                          Literal("10", INTEGER_TYPE))
    assert "Character<UNDEFINED, len:Literal[value:'10'" in str(ctype)
    my_len = DataSymbol("my_len", INTEGER_TYPE)
    ctype2 = CharacterType(4, Reference(my_len))
    assert "Character<4, len:Reference[name:'my_len']>" == str(ctype2)


def test_length_setter():
    '''
    Tests for the setter of the 'length' property.
    '''
    ctype = CharacterType(IntrinsicType.Precision.UNDEFINED, "*")
    assert ctype.length == Literal(
        "*", CharacterType(IntrinsicType.Precision.UNDEFINED, 1))
    ctype.length = ":"
    assert ctype.length == Literal(
        ":", CharacterType(IntrinsicType.Precision.UNDEFINED, 1))
    ctype.length = 6
    assert ctype.length == Literal("6", INTEGER_TYPE)
    # Invalid values
    with pytest.raises(ValueError) as err:
        ctype.length = -1
    assert ("length of a CharacterType is specified using an int then it "
            "must be >= 0 but got: -1" in str(err.value))
    with pytest.raises(ValueError) as err:
        ctype.length = "yellow"
    assert ("length of a CharacterType is specified as a str then it must "
            "contain only ':' or '*' but got: 'yellow'" in str(err.value))
    # Wrong type of value
    with pytest.raises(TypeError) as err:
        ctype.length = True
    assert ("The length property of a CharacterType must be an int, str or "
            "DataNode but got 'bool'" in str(err.value))


def test_copy():
    '''Test the copy() method.'''
    my_len = DataSymbol("my_len", INTEGER_TYPE)
    ctype = CharacterType(4, Reference(my_len))
    ctype2 = ctype.copy()
    assert ctype2 is not ctype
    # Reference should be shallow copied
    assert ctype2.length is not ctype.length
    assert ctype2.length.symbol is ctype.length.symbol
    assert ctype2.precision == ctype.precision


def test_get_all_accessed_symbols():
    '''Test the get_all_accessed_symbols() captures Symbols referenced
    in both the precision and length properties.'''
    my_len = DataSymbol("my_len", INTEGER_TYPE)
    my_kind = DataSymbol("char_kind", INTEGER_TYPE)
    ctype = CharacterType(Reference(my_kind), Reference(my_len))
    all_syms = ctype.get_all_accessed_symbols()
    assert my_len in all_syms
    assert my_kind in all_syms
