# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#
'''Module to test the psyad adjoint base class transformation.'''

from __future__ import absolute_import
import pytest

from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
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
    # We split the check to accomodate for this.
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

    active_vars = [DataSymbol("x", REAL_TYPE)]
    dummy_trans = DummyTrans(active_vars)
    assert dummy_trans._active_variables is active_vars
