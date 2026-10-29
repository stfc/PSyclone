# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Module to test the psyad adjoint base class transformation.'''

import pytest

from psyclone.psyir.symbols import DataSymbol, ScalarType
from psyclone.psyad.transformations.adjoint_trans import AdjointTransformation


def test_abstract():
    '''Check that the adjoint base class transformation can't be created
    as it is abstract.

    '''
    with pytest.raises(TypeError) as info:
        _ = AdjointTransformation([])
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accommodate for this.
    assert ("Can't instantiate abstract class AdjointTransformation with"
            in str(info.value))
    assert ("abstract method" in str(info.value))
    assert ("apply" in str(info.value))


def test_args():
    '''Check that the adjoint base class transformation stores its
    arguments in the expected variables with the expected values.'''

    class DummyTrans(AdjointTransformation):
        '''A dummy transformation used to test the adjoint base class.'''

        def apply(self, _):
            '''Dummy apply method to make a concrete transformation.'''

        @property
        def name(self):
            '''Dummy name method to make a concrete transformation.'''

    with pytest.raises(TypeError) as info:
        DummyTrans(None)
    assert ("The active variables argument should be a list, but found "
            "'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        DummyTrans([])
    assert "There should be at least one active variable." in str(info.value)

    with pytest.raises(TypeError) as info:
        DummyTrans([None])
    assert ("Active variables should be of type DataSymbol, but found "
            "'NoneType'." in str(info.value))

    active_vars = [DataSymbol("x", ScalarType.real_type())]
    dummy_trans = DummyTrans(active_vars)
    assert dummy_trans._active_variables is active_vars
