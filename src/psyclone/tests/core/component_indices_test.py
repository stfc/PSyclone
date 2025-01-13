# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

import pytest

from psyclone.core import ComponentIndices, Signature, VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Assignment


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


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("index_1, index_2, result", [("i", "i", True),
                                                      ("i", "i-1", False),
                                                      ("i", "2*i+1-i-1", True),
                                                      ("i", "2*i+1-i", False),
                                                      ("i,j", "i,j", True),
                                                      ("i,2*j-1",
                                                       "j+2*i+1-j-1-i, 2*j-1",
                                                       True),
                                                      ("i,j", "j,i", False),
                                                      ("i,j", "i", False),
                                                      ])
def test_component_indices_equal(index_1, index_2, result, fortran_reader):
    '''Tests that comparing component indices works as expected, taking
    symbolic equality into account. Also test if some accesses should
    have different number of indices, e.g.: a(i,j) vs b(i), since the
    component indices are independent of the symbol. In this test,
    we use the same variable name (e.g. a(i,j) and a(i)), which is not
    semantically correct, but it creates the right component indices
    for this test.
    '''
    # Check if we need 1D or 2D arrays:
    if "," in index_1:
        declaration = "n,n"
    else:
        declaration = "n"
    source = f'''program test
                 use my_mod, only: my_type
                 type(my_type) :: dv(10)
                 integer i, j, k, l
                 integer, parameter :: n=10
                 real, dimension({declaration}) :: a1, a2
                 a1({index_1}) = a2({index_2})
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assign = psyir.walk(Assignment)

    # Get all access info for the expression
    access_info = VariablesAccessInfo(assign)
    comp_index_1 = access_info[Signature("a1")][0].component_indices
    comp_index_2 = access_info[Signature("a2")][0].component_indices
    assert comp_index_1.equal(comp_index_2) == result


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("var1, var2, result", [("a(i)%b(j)",
                                                 "a(2*i-i)%b(1+j-2+1)", True),
                                                ("a(i)%b",
                                                 "a(i)%b(j)", False),
                                                ("a(i)%b",
                                                 "a(i)%b(i, j)", False),
                                                ("a(i)%b(i)",
                                                 "a(i)%b(i, j)", False),
                                                ("a(i)%b(i, j, k)",
                                                 "a(i)%b(i, j)", False),
                                                ("a(i)%b(i)%c",
                                                 "a(i)%b%c(i)", False),
                                                ("a(i)%b",
                                                 "a(i)%b(j)%c(k)", False),
                                                ])
def test_component_indices_equal_derived(var1, var2, result, fortran_reader):
    '''Tests that comparing component indices of derived types work as
    expected. Besides using symbolic maths, it tests if different level of
    members (a%b vs a%b%c) are used.
    '''
    source = f'''program test
                 use my_mod, only: my_type
                 type(my_type), dimension(10) :: a, b, c
                 {var1} = {var2}
                 end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    assignment = psyir.walk(Assignment)[0]

    # Get all access info for the expression
    access_info_1 = VariablesAccessInfo(assignment.lhs)
    access_info_2 = VariablesAccessInfo(assignment.rhs)
    if var1.count("%") == 1:
        comp_index_1 = access_info_1[Signature("a%b")][0].component_indices
    else:
        comp_index_1 = access_info_1[Signature("a%b%c")][0].component_indices
    if var2.count("%") == 1:
        comp_index_2 = access_info_2[Signature("a%b")][0].component_indices
    else:
        # Support the tests with a%b%c
        comp_index_2 = access_info_2[Signature("a%b%c")][0].component_indices

    assert comp_index_1.equal(comp_index_2) == result
    assert comp_index_2.equal(comp_index_1) == result
