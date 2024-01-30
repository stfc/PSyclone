# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

from psyclone.core import ComponentIndices, VariablesAccessInfo
from psyclone.errors import InternalError


def test_component_indices():
    '''Test the ComponentIndices class.
    '''

    component_indices = ComponentIndices()
    assert component_indices.indices_lists == [[]]
    assert str(component_indices) == "[[]]"
    assert len(component_indices) == 1

    component_indices = ComponentIndices(["a"])
    assert component_indices.indices_lists == [["a"]]
    assert str(component_indices) == "[['a']]"
    assert component_indices[0] == ['a']

    component_indices = ComponentIndices([["a", "b"], ["c"]]).indices_lists
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


# -----------------------------------------------------------------------------
def test_component_indices_getitem_exceptions():
    '''Tests useful error messages are provided if a tuple is provided that
    is out of bounds.
    '''
    component_indices = ComponentIndices([["a", "b"], ["c"]])
    with pytest.raises(IndexError) as err:
        _ = component_indices[(2, 0)]
    assert "First index (2) of (2, 0) is out of range." in str(err.value)

    with pytest.raises(IndexError) as err:
        _ = component_indices[(1, 2)]
    assert "Second index (2) of (1, 2) is out of range." in str(err.value)

    with pytest.raises(IndexError) as err:
        _ = component_indices[(-1, 0)]
    assert "First index (-1) of (-1, 0) is out of range." in str(err.value)

    with pytest.raises(IndexError) as err:
        _ = component_indices[(1, -1)]
    assert "Second index (-1) of (1, -1) is out of range." in str(err.value)


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("expression, correct",
                         # We look for i, j and k; l will be ignored
                         [("a1(i+i+j+l)", [set(("i", "j"))]),
                          ("a1(1)", [set()]),
                          ("a2(i+j,2*j+k+1)", [set(("i", "j")),
                                               set(("j", "k"))]),
                          ("a3(i,j,i)", [set("i"), set("j"), set("i")]),
                          ("dv(i)%a(j)%b(k)", [set("i"), set("j"),
                                               set("k")])])
def test_get_subscripts_of(expression, correct, fortran_reader):
    '''Tests that getting the indices of an array expressions
    works as expected.
    '''
    source = f'''program test
                 use my_mod, only: my_type
                 type(my_type) :: dv(10)
                 integer i, j, k, l
                 integer, parameter :: n=10
                 real, dimension(n) :: a1
                 real, dimension(n,n) :: a2
                 real, dimension(n,n,n) :: a3
                 {expression} = 1
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.children[0].children[0]

    # Get all access info for the expression
    access_info = VariablesAccessInfo(assign)

    # Find the access that is not to i,j, or k --> this must be
    # the 'main' array variable we need to check for:
    sig = None
    loop_vars = set(["i", "j", "k"])
    for sig in access_info:
        if str(sig) not in loop_vars:
            break
    # Get all accesses to the array variable. It has only one
    # access
    access = access_info[sig][0]
    result = access.component_indices.get_subscripts_of(loop_vars)
    assert result == correct
