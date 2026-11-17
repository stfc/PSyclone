# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the ColumnwiseOperatorArgMetadata class.

'''
from psyclone.domain.lfric.kernel import ColumnwiseOperatorArgMetadata


def test_create():
    '''Test that an instance of ColumnwiseOperatorArgMetadata can be
    created successfully.

    '''
    operator_arg = ColumnwiseOperatorArgMetadata(
        "GH_REAL", "GH_READ", "W0", "W1")
    assert isinstance(operator_arg, ColumnwiseOperatorArgMetadata)
    assert operator_arg.form == "gh_columnwise_operator"
    assert operator_arg._datatype == "gh_real"
    assert operator_arg._access == "gh_read"
    assert operator_arg._function_space_to == "w0"
    assert operator_arg._function_space_from == "w1"
