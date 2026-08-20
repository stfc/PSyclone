# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for ValueRangeCheckTrans.
'''

import pytest


from psyclone.errors import InternalError
from psyclone.psyir.nodes import Node, ValueRangeCheckNode
from psyclone.psyir.transformations import (ValueRangeCheckTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans


# ---------------------------------------------------------------------------
def test_value_range_check_trans():
    '''Tests basic functions in ValueRangeCheckTrans.'''
    value_range = ValueRangeCheckTrans()
    assert str(value_range) == ("Create a sub-tree of the PSyIR that has a "
                                "node of type ValueRangeCheckNode at its "
                                "root.")
    assert value_range.name == "ValueRangeCheckTrans"


# -----------------------------------------------------------------------------
def test_malformed_value_range_check_node(monkeypatch):
    ''' Check that we raise the expected error if a ValueRangeCheckNode does
    not have a single Schedule node as its child. '''
    value_range_check_node = ValueRangeCheckNode()
    monkeypatch.setattr(value_range_check_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = value_range_check_node.value_range_check_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(value_range_check_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = value_range_check_node.value_range_check_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_value_range_check_options():
    '''Check that options are passed to the ValueRangeCheckNode and trigger
    the use of the newly defined names.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0, dist_mem=False)
    value_range_check = ValueRangeCheckTrans()
    value_range_check.apply(invoke.schedule[0],
                            options={"region_name": ("a", "b")})
    assert invoke.schedule[0].module_name == "a"
    assert invoke.schedule[0].region_name == "b"


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by ValueRangeCheckTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0)
    value_range_check = ValueRangeCheckTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        value_range_check.apply(invoke.schedule[0].dir_body[0],
                                options={"region_name": ("a", "b")})

    assert ("Error in ValueRangeCheckTrans: Application to a Loop without "
            "its parent Directive is not allowed." in str(err.value))

    with pytest.raises(TransformationError) as err:
        value_range_check.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                                options={"region_name": ("a", "b")})

    assert ("Error in ValueRangeCheckTrans: Application to Nodes enclosed "
            "within a thread-parallel region is not allowed."
            in str(err.value))
