# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''pytest tests for the transformation_errors module.'''

from psyclone.errors import LazyString
from psyclone.psyir.transformations import TransformationError


# TransformationError class

def test_transformationerror():
    '''Test that the TransformationError class behaves as expected.'''
    error = TransformationError("hello")
    assert isinstance(error, TransformationError)
    assert isinstance(error.value, LazyString)
    assert repr(error) == "TransformationError()"
    assert str(error) == "Transformation Error: hello"
