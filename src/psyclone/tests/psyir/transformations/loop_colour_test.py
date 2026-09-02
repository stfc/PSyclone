# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' pytest tests for the ColourTrans transformation. '''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Loop, Literal
from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.transformations import ColourTrans


def test_colour_trans_str():
    ''' Test the __str__ property of the class. '''
    ctrans = ColourTrans()
    assert str(ctrans) == "Split a loop into colours"


def test_colour_trans_create_loop_not_implemented():
    '''
    Test that the '_create_colours_loop()' and _create_tiled_colours_loops
    methods raises an InternalError in the base class.

    '''
    ctrans = ColourTrans()
    with pytest.raises(InternalError) as err:
        ctrans._create_colours_loop(None)
    assert ("_create_colours_loop() must be overridden in an API-specific "
            "sub-class" in str(err.value))
    with pytest.raises(InternalError) as err:
        ctrans._create_tiled_colours_loops(None)
    assert ("_create_tiled_colours_loops() must be overridden in an "
            "API-specific sub-class" in str(err.value))
    # Check that apply() also calls _create_colours_loop().
    with pytest.raises(InternalError) as err:
        ctrans.apply(Loop.create(DataSymbol("ji", ScalarType.integer_type()),
                                 Literal("1", ScalarType.integer_type()),
                                 Literal("10", ScalarType.integer_type()),
                                 Literal("1", ScalarType.integer_type()),
                                 []))
    assert ("_create_colours_loop() must be overridden in an API-specific "
            "sub-class" in str(err.value))
