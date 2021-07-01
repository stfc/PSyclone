# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: Joerg Henrichs, Bureau of Meteorology

'''This module tests the ComponentIndices class in psyclone/core.'''

from __future__ import absolute_import
import pytest

from psyclone.core import ComponentIndices
from psyclone.errors import InternalError


def test_component_indices():
    '''Test the ComponentIndices class.
    '''

    component_indices = ComponentIndices()
    assert component_indices.get() == [[]]
    assert str(component_indices) == "[[]]"
    assert len(component_indices) == 1

    component_indices = ComponentIndices(["a"])
    assert component_indices.get() == [["a"]]
    assert str(component_indices) == "[['a']]"
    assert component_indices[0] == ['a']

    component_indices = ComponentIndices([["a", "b"], ["c"]]).get()
    assert component_indices == [["a", "b"], ["c"]]
    assert component_indices[0] == ["a", "b"]
    assert component_indices[1] == ["c"]
    assert len(component_indices) == 2


# -----------------------------------------------------------------------------
def test_is_array():
    '''Test if arrays are correctly detected.
    '''
    component_indices = ComponentIndices([["a", "b"], ["c"]])

    assert component_indices.is_array()

    component_indices = ComponentIndices([[], []])
    assert not component_indices.is_array()


# -----------------------------------------------------------------------------
def test_component_indices_exceptions():
    '''Test that the right exceptions are raised.
    '''
    with pytest.raises(InternalError) as err:
        _ = ComponentIndices(123)
    assert "Index object in ComponentIndices constructor must be None, a " \
           "list or list of lists, got '123'" in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = ComponentIndices([[], 123])
    assert "ComponentIndices: Invalid list parameter '[[], 123]' - some " \
           "elements but not all are lists" in str(err.value)


# -----------------------------------------------------------------------------
def test_iterating():
    '''Tests that iterating works, and that the returned values from the
    iterator can be used in dictionary-like accesses.
    '''

    component_indices = ComponentIndices([["a", "b"], ["c"]])
    correct_index_pairs = [(0, 0), (0, 1), (1, 0)]
    correct = ["a", "b", "c"]

    for count, indx in enumerate(component_indices.iterate()):
        assert correct_index_pairs[count] == indx
        assert correct[count] == component_indices[indx]
