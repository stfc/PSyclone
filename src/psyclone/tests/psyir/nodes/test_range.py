# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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


''' pytest tests for the various Range sub-classes of Node. '''

import pytest
from psyclone.psyir.nodes.range import ExplicitRange
from psyclone.psyGen import InternalError


def test_explicit_range_errors():
    ''' Basic tests for incorrect creation of an ExplicitRange node. '''

    # Too few children
    with pytest.raises(InternalError) as err:
        ExplicitRange(ast=None, children=[], parent=None)
    assert ("must have exactly 2 or 3 children (for start, stop and "
            "optional step) but got 0" in str(err.value))

    # Too many children
    with pytest.raises(InternalError) as err:
        ExplicitRange(ast=None, children=[None, None, None, None], parent=None)
    assert ("must have exactly 2 or 3 children (for start, stop and "
            "optional step) but got 4" in str(err.value))

    # Correct number of children but wrong type
    with pytest.raises(TypeError) as err:
        ExplicitRange(ast=None, children=[None, None, None], parent=None)
    assert ("must be sub-classes of Node but got: ['NoneType', "
            in str(err.value))

    with pytest.raises(TypeError) as err:
        ExplicitRange(ast=None, children=[1, 2], parent=None)
    assert ("must be sub-classes of Node but got: ['int', "
            in str(err.value))
