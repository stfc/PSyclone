# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk and S. Siso, STFC Daresbury Lab.
# -----------------------------------------------------------------------------

''' This module contains the test for the UnknownDirective node.'''

import pytest
from psyclone.psyir.nodes import UnknownDirective


def test_psydirective_constructor_and_getters():
    '''Tests the functionality of the UnknownDirective.'''

    # Check TypeErrors
    with pytest.raises(TypeError) as err:
        direc = UnknownDirective(3)
    assert "'directive_string' must be a 'str' but found" in str(err.value)
    with pytest.raises(TypeError) as err:
        direc = UnknownDirective("hello", 3)
    assert ("'sentinel_infix_string' must be a 'str' but found"
            in str(err.value))

    direc = UnknownDirective("hello", "there")
    assert direc._directive_string == "hello"
    assert direc.directive_string == "hello"
    assert direc._sentinel_infix_string == "there"
    assert direc.sentinel_infix_string == "there"

    assert not UnknownDirective._validate_child(None, None)
