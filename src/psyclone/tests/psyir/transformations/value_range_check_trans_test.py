# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified by: R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
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
