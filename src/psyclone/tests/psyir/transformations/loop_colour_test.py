# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab

''' pytest tests for the ColourTrans transformation. '''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Loop, Literal
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.transformations import ColourTrans


def test_colour_trans_str():
    ''' Test the __str__ property of the class. '''
    ctrans = ColourTrans()
    assert str(ctrans) == "Split a loop into colours"


def test_colour_trans_create_loop():
    '''
    Test that the '_create_colours_loop()' method raises an
    InternalError.

    '''
    ctrans = ColourTrans()
    with pytest.raises(InternalError) as err:
        ctrans._create_colours_loop(None)
    assert ("_create_colours_loop() must be overridden in an API-specific "
            "sub-class" in str(err.value))
    # Check that apply() also calls _create_colours_loop().
    with pytest.raises(InternalError) as err:
        ctrans.apply(Loop.create(DataSymbol("ji", INTEGER_TYPE),
                                 Literal("1", INTEGER_TYPE),
                                 Literal("10", INTEGER_TYPE),
                                 Literal("1", INTEGER_TYPE),
                                 []))
    assert ("_create_colours_loop() must be overridden in an API-specific "
            "sub-class" in str(err.value))
