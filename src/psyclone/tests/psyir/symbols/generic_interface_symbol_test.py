# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
from psyclone.psyir.symbols import (GenericInterfaceSymbol, INTEGER_TYPE,
                                    RoutineSymbol)


def test_gis_constructor():
    '''
    Test that a GenericInterfaceSymbol can be constructed and that the
    expected errors are raised if an incorrect argument is provided.

    '''
    with pytest.raises(ValueError) as err:
        _ = GenericInterfaceSymbol("beech", [])
    assert ("A GenericInterfaceSymbol requires a list of Routine names but "
            "none were provided." in str(err.value))
    acorn = RoutineSymbol("acorn")
    with pytest.raises(TypeError) as err:
        _ = GenericInterfaceSymbol("oak", [acorn, "sycamore"])
    assert ("of Routine names as strings but got: ['RoutineSymbol', 'str']" in
            str(err.value))
    oak = GenericInterfaceSymbol("oak", ["acorn"])
    assert oak.routines == ["acorn"]


def test_gis_typedsymbol_keywords():
    '''
    Test that keyword arguments to the constructor are passed through to the
    TypedSymbol constructor.
    '''
    walnut = GenericInterfaceSymbol("walnut", ["nut"], datatype=INTEGER_TYPE)
    assert walnut.datatype == INTEGER_TYPE


def test_gis_str():
    '''
    Test the __str__ method of GenericInterfaceSymbol.
    '''
    coppice = GenericInterfaceSymbol("coppice", ["ash", "holly"])
    assert str(coppice) == ("coppice: GenericInterfaceSymbol<NoType, "
                            "pure=unknown, elemental=unknown, "
                            "routines=['ash', 'holly']>")


def test_gis_copy():
    '''
    Test the copy() method of GenericInterfaceSymbol.
    '''
    coppice = GenericInterfaceSymbol("coppice", ["ash", "holly"])
    spinney = coppice.copy()
    assert isinstance(spinney, GenericInterfaceSymbol)
    assert spinney is not coppice
    assert len(spinney.routines) == 2
    assert "ash" in spinney.routines
    assert "holly" in spinney.routines
