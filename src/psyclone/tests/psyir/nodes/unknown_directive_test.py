# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
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
