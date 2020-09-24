# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
from psyclone.psyir.symbols import RoutineSymbol, Symbol, UnresolvedInterface


def test_routinesymbol_init():
    '''Test that a RoutineSymbol instance can be created.'''

    assert isinstance(RoutineSymbol('jo'), RoutineSymbol)
    assert isinstance(
        RoutineSymbol('ellie', visibility=Symbol.Visibility.PRIVATE),
        RoutineSymbol)
    assert isinstance(
        RoutineSymbol('isaac', interface=UnresolvedInterface()),
        RoutineSymbol)


def test_routinesymbol_init_error():
    '''Test that the RoutineSymbol raises an error (via the Symbol parent
    class) if there is an invalid argument.

    '''
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol(None)
    assert ("RoutineSymbol 'name' attribute should be of type 'str' but "
            "'NoneType' found." in str(error.value))


def test_routinesymbol_str():
    '''Test that the __str__ method in routinesymbol behaves as expected.'''
    routine_symbol = RoutineSymbol("roo")
    assert routine_symbol.__str__() == "roo : RoutineSymbol"
