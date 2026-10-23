# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Module to test the PSyAD TangentLinearError class.'''

import pytest

from psyclone.psyad.transformations import TangentLinearError


def test_error():
    '''Test that the TangentLinearError exception behaves in the expected
    way.

    '''
    message = "It's all gone a bit Pete Tong."
    with pytest.raises(TangentLinearError) as info:
        raise TangentLinearError(message)
    assert str(info.value) == "TangentLinearError: "+message
