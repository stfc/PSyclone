# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Assignment PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Assignment, Reference, Literal, \
    ArrayReference, Range
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, \
    INTEGER_SINGLE_TYPE, REAL_TYPE, ArrayType, INTEGER_TYPE
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links
from psyclone.psyir.nodes.node import colored


def test_assignment_node_str():
    ''' Check the node_str method of the Assignment class.'''
    assignment = Assignment()
    coloredtext = colored("Assignment", Assignment._colour)
    assert coloredtext+"[]" in assignment.node_str()


def test_assignment_can_be_printed():
    '''Test that an Assignment instance can always be printed (i.e. is
    initialised fully)'''
    assignment = Assignment()
    assert "Assignment[]\n" in str(assignment)


def test_assignment_semantic_navigation():
    '''Test that the Assignment navigation properties reference the expected
    children'''
    assignment = Assignment()

    # lhs should fail if first child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.lhs
    assert "' malformed or incomplete. It needs at least 1 child to have " \
        "a lhs." in str(err.value)

    ref = Reference(DataSymbol("a", REAL_SINGLE_TYPE), assignment)
    assignment.addchild(ref)

    # rhs should fail if second child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.rhs
    assert " malformed or incomplete. It needs at least 2 children to have " \
        "a rhs." in str(err.value)

    lit = Literal("1", INTEGER_SINGLE_TYPE, assignment)
    assignment.addchild(lit)
    assert assignment.lhs is assignment._children[0]
    assert assignment.rhs is assignment._children[1]


def test_assignment_create():
    '''Test that the create method in the Assignment class correctly
    creates an Assignment instance.

    '''
    lhs = Reference(DataSymbol("tmp", REAL_SINGLE_TYPE))
    rhs = Literal("0.0", REAL_SINGLE_TYPE)
    assignment = Assignment.create(lhs, rhs)
    check_links(assignment, [lhs, rhs])
    result = FortranWriter().assignment_node(assignment)
    assert result == "tmp = 0.0\n"


def test_assignment_children_validation():
    '''Test that children added to assignment are validated. Assignment
    accepts just 2 children and both are DataNodes.

    '''
    # Create method with lhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Assignment.create("invalid", Literal("0.0", REAL_SINGLE_TYPE))
    assert ("Item 'str' can't be child 0 of 'Assignment'. The valid format "
            "is: 'DataNode, DataNode'.") in str(excinfo.value)

    # Create method with rhs not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Assignment.create(Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
                              "invalid")
    assert ("Item 'str' can't be child 1 of 'Assignment'. The valid format "
            "is: 'DataNode, DataNode'.") in str(excinfo.value)

    # Direct assignment of a 3rd children
    assignment = Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Literal("0.0", REAL_SINGLE_TYPE))
    with pytest.raises(GenerationError) as excinfo:
        assignment.children.append(Literal("0.0", REAL_SINGLE_TYPE))
    assert ("Item 'Literal' can't be child 2 of 'Assignment'. The valid format"
            " is: 'DataNode, DataNode'.") in str(excinfo.value)


def test_is_array_range():
    '''test that the is_array_range method behaves as expected, returning
    true if the LHS of the assignment is an array range access and
    false otherwise.

    '''
    one = Literal("1.0", REAL_TYPE)
    int_one = Literal("1", INTEGER_TYPE)
    var = DataSymbol("x", REAL_TYPE)
    reference = Reference(var)

    # lhs is not an array
    assignment = Assignment.create(reference, one)
    assert not assignment.is_array_range

    # lhs is an array reference but has no range
    array_type = ArrayType(REAL_TYPE, [10, 10])
    symbol = DataSymbol("x", array_type)
    array_ref = Reference(symbol)
    assignment = Assignment.create(array_ref, one.copy())
    assert not assignment.is_array_range

    # lhs is an array reference with a range
    my_range = Range.create(int_one, int_one.copy(), int_one.copy())
    array_ref = ArrayReference.create(symbol, [my_range, int_one.copy()])
    assignment = Assignment.create(array_ref, one.copy())
    assert assignment.is_array_range
