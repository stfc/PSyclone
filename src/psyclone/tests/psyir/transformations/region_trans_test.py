# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

''' Module containing tests for testing RegionTrans'''

from __future__ import absolute_import

import pytest

from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Node, Schedule
from psyclone.psyir.transformations import RegionTrans
from psyclone.tests.utilities import get_invoke
from psyclone.gocean1p0 import GOLoop


class MyRegionTrans(RegionTrans):
    '''We can't create an instance of RegionTrans since it is
    abstract, so create a simple class that can be instantiated
    by adding dummy implementations of the missing methods.
    '''
    excluded_node_types = ()

    def apply(self, node, options=None):
        '''Dummy only to make this not abstract.'''

    @property
    def name(self):
        '''Dummy only to make this not abstract.'''


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("node_list", [5, [1], [1, Node()],
                                       [Node(), 1], [Node(), Node(), 1],
                                       [Node(), 1, Node()]])
def test_get_node_list_errors(node_list):
    '''Test incorrect parameters to get_node_list.
    '''
    my_rt = MyRegionTrans()
    with pytest.raises(TransformationError) as err:
        my_rt.get_node_list(node_list)
    assert "Argument must be a single Node in a Schedule, a Schedule or a "\
           "list of Nodes in a Schedule" in str(err.value)

    # Test for more specific error message for the first test case:
    # node_list = 5
    if isinstance(node_list, int):
        # Python 3 reports 'class', python 2 'type' - so just check for both
        assert ("<type 'int'>" in str(err.value) or "<class 'int'>"
                in str(err.value))


# -----------------------------------------------------------------------------
def test_get_node_list():
    '''Test for valid parameters to get_node_list.'''

    my_rt = MyRegionTrans()
    # 1) Provide a schedule
    # ---------------------
    sched = Schedule()
    # get_node_list returns a copy of the list, so it must be a list
    # with the same content, but NOT the same list:
    node_list = my_rt.get_node_list(sched)
    assert sched.children == node_list
    assert node_list is not sched.children

    # 2) Provide a single node
    # ------------------------
    node = Node()
    node_list = my_rt.get_node_list(node)
    assert node_list == [node]

    # 3) Provide a node list
    # ----------------------
    # We use the previously returned node list, and make sure
    # that we get a copy of that list.
    node_list2 = my_rt.get_node_list(node_list)
    assert node_list2 == node_list
    assert node_list2 is not node_list


# -----------------------------------------------------------------------------
def test_validate_errors():
    '''Tests error handling of the region transformation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)

    schedule = invoke.schedule
    my_rt = MyRegionTrans()

    node_list = my_rt.get_node_list([schedule.children[0].children[0],
                                     schedule.children[1]])
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert "supplied nodes are not children of the same parent." \
        in str(err.value)

    # Test that it will only allow correctly ordered nodes:
    node_list = [schedule.children[1], schedule.children[0]]
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert "Children are not consecutive children of one parent:" \
        in str(err.value)

    node_list = [schedule.children[0], schedule.children[2]]
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert "Children are not consecutive children of one parent:" \
           in str(err.value)

    # Test 3 element lists: first various incorrect ordering:
    node_list = [schedule.children[0], schedule.children[2],
                 schedule.children[1]]
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert "Children are not consecutive children of one parent:" \
           in str(err.value)

    node_list = [schedule.children[1], schedule.children[0],
                 schedule.children[2]]
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert "Children are not consecutive children of one parent:" \
           in str(err.value)

    # Supply incorrect type to options parameter:
    with pytest.raises(TransformationError) as err:
        my_rt.validate([], options=1)
    assert "Transformation apply method options argument must be a " \
        "dictionary" in str(err.value)

    # Check that the tuple of excluded node types is used correctly (by
    # resetting it here to a Loop which is within the region)
    my_rt.excluded_node_types = (GOLoop,)
    node_list = [schedule.children[0], schedule.children[1],
                 schedule.children[2]]
    with pytest.raises(TransformationError) as err:
        my_rt.validate(node_list)
    assert ("Transformation Error: Nodes of type 'GOLoop' cannot be enclosed"
            in str(err.value))


# -----------------------------------------------------------------------------
def test_validate_ok():
    '''Test parameters that validate should accept.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule
    my_rt = MyRegionTrans()

    # Check that correct ordering works:
    node_list = [schedule.children[0], schedule.children[1],
                 schedule.children[2]]
    my_rt.validate(node_list)

    # Check that a single Node is accepted
    my_rt.validate(schedule.children[0])

    # Check that a single Schedule is accepted
    my_rt.validate(schedule)
