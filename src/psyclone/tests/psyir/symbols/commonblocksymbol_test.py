# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Tests for the CommonBlockSymbol class.'''

import pytest

from psyclone.psyir.symbols import (
    CommonBlockSymbol, DataSymbol, ScalarType)
from psyclone.psyir.symbols.interfaces import CommonBlockInterface

REAL_TYPE = ScalarType.real_type()


# ---------------------------------------------------------------------------
# __init__
# ---------------------------------------------------------------------------

def test_commonblocksymbol_init_named():
    '''A CommonBlockSymbol can be created with a non-empty name.'''
    sym = CommonBlockSymbol("myblock")
    assert sym.name == "myblock"
    assert sym.variables == []


def test_commonblocksymbol_init_blank():
    '''The blank common block is represented by the empty string.'''
    sym = CommonBlockSymbol("")
    assert sym.name == ""
    assert sym.variables == []


def test_commonblocksymbol_init_type_error():
    '''A non-str name raises TypeError.'''
    with pytest.raises(TypeError) as err:
        CommonBlockSymbol(42)
    assert "CommonBlockSymbol 'name' attribute should be of type 'str'" in \
        str(err.value)
    assert "'int' found" in str(err.value)


# ---------------------------------------------------------------------------
# variables property
# ---------------------------------------------------------------------------

def test_commonblocksymbol_variables_returns_copy():
    '''variables returns a *copy* of the internal list, so mutating it does
    not affect the symbol.'''
    cb = CommonBlockSymbol("blk")
    var = DataSymbol("x", REAL_TYPE)
    cb.add_variable(var)
    lst = cb.variables
    lst.clear()
    # Original must be unchanged.
    assert cb.variables == [var]


# ---------------------------------------------------------------------------
# add_variable
# ---------------------------------------------------------------------------

def test_commonblocksymbol_add_variable():
    '''add_variable appends DataSymbols in insertion order.'''
    cb = CommonBlockSymbol("blk")
    x = DataSymbol("x", REAL_TYPE)
    y = DataSymbol("y", REAL_TYPE)
    cb.add_variable(x)
    cb.add_variable(y)
    assert cb.variables == [x, y]


def test_commonblocksymbol_add_variable_type_error():
    '''add_variable rejects non-DataSymbol values.'''
    cb = CommonBlockSymbol("blk")
    with pytest.raises(TypeError) as err:
        cb.add_variable("not_a_datasymbol")
    assert "expected a DataSymbol" in str(err.value)
    assert "'str'" in str(err.value)


def test_commonblocksymbol_add_variable_duplicate_error():
    '''add_variable raises ValueError when the same DataSymbol is added
    twice.'''
    cb = CommonBlockSymbol("blk")
    var = DataSymbol("x", REAL_TYPE)
    cb.add_variable(var)
    with pytest.raises(ValueError) as err:
        cb.add_variable(var)
    assert "already in the variable list" in str(err.value)


# ---------------------------------------------------------------------------
# replace_variable
# ---------------------------------------------------------------------------

def test_commonblocksymbol_replace_variable():
    '''replace_variable substitutes the old symbol at the same position.'''
    cb = CommonBlockSymbol("blk")
    x = DataSymbol("x", REAL_TYPE)
    y = DataSymbol("y", REAL_TYPE)
    x2 = DataSymbol("x", REAL_TYPE)
    cb.add_variable(x)
    cb.add_variable(y)
    cb.replace_variable(x, x2)
    assert cb.variables == [x2, y]


def test_commonblocksymbol_replace_variable_not_found():
    '''replace_variable raises ValueError when old_sym is not present.'''
    cb = CommonBlockSymbol("blk")
    x = DataSymbol("x", REAL_TYPE)
    other = DataSymbol("z", REAL_TYPE)
    cb.add_variable(x)
    with pytest.raises(ValueError) as err:
        cb.replace_variable(other, DataSymbol("z2", REAL_TYPE))
    assert "not in the variable list" in str(err.value)


# ---------------------------------------------------------------------------
# copy
# ---------------------------------------------------------------------------

def test_commonblocksymbol_copy():
    '''copy produces a new CommonBlockSymbol with the same name but an
    empty variable list (to be repopulated by replace_symbols_using).'''
    cb = CommonBlockSymbol("blk")
    var = DataSymbol("x", REAL_TYPE)
    cb.add_variable(var)

    copy = cb.copy()
    assert isinstance(copy, CommonBlockSymbol)
    assert copy is not cb
    assert copy.name == cb.name
    # The copy starts with an empty variable list.
    assert copy.variables == []


def test_commonblocksymbol_copy_blank():
    '''copy also works for the blank common block (empty name).'''
    cb = CommonBlockSymbol("")
    copy = cb.copy()
    assert copy.name == ""
    assert copy.variables == []


# ---------------------------------------------------------------------------
# __str__
# ---------------------------------------------------------------------------

def test_commonblocksymbol_str_named():
    '''__str__ wraps named blocks in /.../.'''
    cb = CommonBlockSymbol("myblock")
    assert str(cb) == "/myblock/: CommonBlockSymbol"


def test_commonblocksymbol_str_blank():
    '''__str__ uses // for the blank common block.'''
    cb = CommonBlockSymbol("")
    assert str(cb) == "//: CommonBlockSymbol"


# ---------------------------------------------------------------------------
# Integration: CommonBlockInterface linkage
# ---------------------------------------------------------------------------

def test_commonblocksymbol_with_commonblockinterface():
    '''DataSymbols can reference a CommonBlockSymbol via CommonBlockInterface,
    and the back-link is consistent.'''
    cb = CommonBlockSymbol("myblock")
    x = DataSymbol("x", REAL_TYPE)
    x.interface = CommonBlockInterface(cb)
    cb.add_variable(x)

    assert x.interface.common_block_symbol is cb
    assert x in cb.variables
    assert str(x.interface) == "CommonBlock(/myblock/)"
