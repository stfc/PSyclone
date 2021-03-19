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

''' Performs py.test tests on the Loop PSyIR node. '''

from __future__ import absolute_import
import os
import pytest
from psyclone.psyir.nodes import Loop, Literal, Schedule, Return, Assignment, \
    Reference
from psyclone.psyir.symbols import DataSymbol, REAL_SINGLE_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER_TYPE, ArrayType, REAL_TYPE
from psyclone.psyGen import PSyFactory
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke, check_links
from psyclone.parse.algorithm import parse


def test_loop_init():
    '''Test that a loop instance is created as expected and that it raises
    the expected exceptions where appropriate.

    '''
    loop = Loop()
    assert loop.parent is None
    assert loop._valid_loop_types == []
    assert loop.annotations == []
    assert loop._loop_type is None
    assert loop._field is None
    assert loop._field_name is None
    assert loop._field_space is None
    assert loop._iteration_space is None
    assert loop._kern is None
    assert loop._iterates_over == "unknown"
    assert loop._variable is None
    assert loop._id == ""

    # valid variable
    loop = Loop(variable=DataSymbol("var", INTEGER_TYPE))
    assert loop.variable.name == "var"

    # invalid variable (test_check_variable tests check all ways a
    # variable could be invalid. Here we just check that the
    # _check_variable() method is called correctly)
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop(variable="hello")
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'str'.") in str(excinfo.value)

    # valid_loop_types. Note, there is no error checking for this
    # variable in the Loop class.
    loop = Loop(valid_loop_types=["a"])
    assert loop._valid_loop_types == ["a"]

    parent = Schedule()
    loop = Loop(parent=parent)
    assert loop.parent is parent

    annotations = ["was_where"]
    loop = Loop(annotations=annotations)
    assert loop.annotations == annotations


def test_loop_navigation_properties():
    # pylint: disable=too-many-statements
    ''' Tests the start_expr, stop_expr, step_expr and loop_body
    setter and getter properties.

    '''
    loop = Loop()

    # Properties return an error if the node is incomplete
    error_str = ("Loop is incomplete. It should have exactly 4 "
                 "children, but found")
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)

    loop.addchild(Literal("start", INTEGER_SINGLE_TYPE, parent=loop))
    loop.addchild(Literal("stop", INTEGER_SINGLE_TYPE, parent=loop))
    loop.addchild(Literal("step", INTEGER_SINGLE_TYPE, parent=loop))

    # If it's not fully complete, it still returns an error
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.stop_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.step_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.start_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.stop_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.step_expr = Literal("invalid", INTEGER_SINGLE_TYPE, parent=loop)
    assert error_str in str(err.value)

    # Check that Getters properties work
    loop.addchild(Schedule(parent=loop))
    loop.loop_body.addchild(Return(parent=loop.loop_body))

    assert loop.start_expr.value == "start"
    assert loop.stop_expr.value == "stop"
    assert loop.step_expr.value == "step"
    assert isinstance(loop.loop_body[0], Return)

    # Test Setters
    loop.start_expr = Literal("newstart", INTEGER_SINGLE_TYPE, parent=loop)
    loop.stop_expr = Literal("newstop", INTEGER_SINGLE_TYPE, parent=loop)
    loop.step_expr = Literal("newstep", INTEGER_SINGLE_TYPE, parent=loop)

    assert loop.start_expr.value == "newstart"
    assert loop.stop_expr.value == "newstop"
    assert loop.step_expr.value == "newstep"


def test_loop_invalid_type():
    ''' Tests assigning an invalid type to a Loop object. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    assert isinstance(loop, Loop)
    with pytest.raises(GenerationError) as err:
        loop.loop_type = "not_a_valid_type"
    assert ("loop_type value (not_a_valid_type) is invalid. Must be one of "
            "['inner', 'outer']" in str(err.value))


def test_loop_gen_code():
    ''' Check that the Loop gen_code method prints the proper loop '''
    base_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
    _, invoke_info = parse(os.path.join(base_path,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # By default DynLoop has step = 1 and it is not printed in the Fortran DO
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen

    # Change step to 2
    loop = psy.invokes.get('invoke_important_invoke').schedule[4]
    loop.step_expr = Literal("2", INTEGER_SINGLE_TYPE, parent=loop)

    # Now it is printed in the Fortran DO with the expression  ",2" at the end
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1),2" in gen


def test_invalid_loop_annotations():
    ''' Check that the Loop constructor validates any supplied annotations. '''
    # Check that we can have 'was_where' on its own
    test_loop = Loop(annotations=['was_where'])
    assert test_loop.annotations == ['was_where']
    # Check that 'was_single_stmt' on its own raises an error
    with pytest.raises(InternalError) as err:
        Loop(annotations=['was_single_stmt'])
    assert ("Loop with the 'was_single_stmt' annotation must also have the "
            "'was_where'" in str(err.value))
    # Check that it's accepted in combination with 'was_where'
    test_loop = Loop(annotations=['was_single_stmt', 'was_where'])
    assert test_loop.annotations == ['was_single_stmt', 'was_where']


def test_loop_create():
    '''Test that the create method in the Loop class correctly
    creates a Loop instance.

    '''
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(DataSymbol("tmp", REAL_SINGLE_TYPE)),
        Reference(DataSymbol("i", REAL_SINGLE_TYPE)))
    loop = Loop.create(DataSymbol("i", INTEGER_SINGLE_TYPE),
                       start, stop, step, [child_node])
    schedule = loop.children[3]
    assert isinstance(schedule, Schedule)
    check_links(loop, [start, stop, step, schedule])
    check_links(schedule, [child_node])
    result = FortranWriter().loop_node(loop)
    assert result == "do i = 0, 1, 1\n  tmp = i\nenddo\n"


def test_loop_create_invalid():
    '''Test that the create method in a Loop class raises the expected
    exception if the provided input is invalid.

    '''
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    children = [Assignment.create(
        Reference(DataSymbol("x", INTEGER_SINGLE_TYPE)),
        one.copy())]

    # invalid variable (test_check_variable tests check all ways a
    # variable could be invalid. Here we just check that the
    # _check_variable() method is called correctly)
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(1, zero, one, one, children)
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'int'.") in str(excinfo.value)

    variable = DataSymbol("i", INTEGER_TYPE)

    # start not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, "invalid", one, one, children)
    assert ("Item 'str' can't be child 0 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'.") in str(excinfo.value)

    # stop not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, "invalid", one, children)
    assert ("Item 'str' can't be child 1 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'.") in str(excinfo.value)

    # step not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, one, "invalid", children)
    assert ("Item 'str' can't be child 2 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'.") in str(excinfo.value)

    # children not a list
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, one, one, "invalid")
    assert ("children argument in create method of Loop class should "
            "be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, one, one, ["invalid"])
    assert ("Item 'str' can't be child 0 of 'Schedule'. The valid format is: "
            "'[Statement]*'." in str(excinfo.value))


def test_loop_children_validation():
    '''Test that children added to Loop are validated. Loop accepts
    3 DataNodes and a Schedule.

    '''
    loop = Loop()
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE, parent=loop)
    datanode2 = Literal("2", INTEGER_SINGLE_TYPE, parent=loop)
    datanode3 = Literal("3", INTEGER_SINGLE_TYPE, parent=loop)
    schedule = Schedule(parent=loop)

    # First child
    with pytest.raises(GenerationError) as excinfo:
        loop.addchild(schedule)
    assert ("Item 'Schedule' can't be child 0 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'." in str(excinfo.value))
    loop.addchild(datanode1)

    # Second child
    with pytest.raises(GenerationError) as excinfo:
        loop.addchild(schedule)
    assert ("Item 'Schedule' can't be child 1 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'." in str(excinfo.value))
    loop.addchild(datanode2)

    # Third child
    with pytest.raises(GenerationError) as excinfo:
        loop.addchild(schedule)
    assert ("Item 'Schedule' can't be child 2 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'." in str(excinfo.value))
    loop.addchild(datanode3)

    # Fourth child
    with pytest.raises(GenerationError) as excinfo:
        loop.addchild(datanode1)
    assert ("Item 'Literal' can't be child 3 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'." in str(excinfo.value))
    loop.addchild(schedule)

    # Additional children
    with pytest.raises(GenerationError) as excinfo:
        loop.addchild(schedule)
    assert ("Item 'Schedule' can't be child 4 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'." in str(excinfo.value))


def test_check_variable():
    '''Test the _check_variable utility method behaves as expected'''

    with pytest.raises(GenerationError) as info:
        Loop._check_variable(None)
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'NoneType'." in str(info.value))

    with pytest.raises(GenerationError) as info:
        Loop._check_variable("hello")
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'str'." in str(info.value))

    array_type = ArrayType(INTEGER_TYPE, shape=[10, 20])
    array_symbol = DataSymbol("my_array", array_type)
    with pytest.raises(GenerationError) as info:
        Loop._check_variable(array_symbol)
    assert ("variable property in Loop class should be a ScalarType but "
            "found 'ArrayType'." in str(info.value))

    scalar_symbol = DataSymbol("my_array", REAL_TYPE)
    with pytest.raises(GenerationError) as info:
        Loop._check_variable(scalar_symbol)
    assert ("variable property in Loop class should be a scalar integer but "
            "found 'REAL'." in str(info.value))

    scalar_symbol = DataSymbol("my_array", INTEGER_TYPE)
    assert Loop._check_variable(scalar_symbol) is None


def test_variable_setter():
    '''Check that we can set the _variable property using the variable
    setter method and that it raises an exception if an invalid value
    is provided.

    '''
    loop = Loop()
    assert loop._variable is None

    # valid variable
    loop.variable = DataSymbol("var", INTEGER_TYPE)
    assert loop.variable.name == "var"

    # invalid variable (test_check_variable tests check all ways a
    # variable could be invalid. Here we just check that the
    # _check_variable() method is called correctly)
    with pytest.raises(GenerationError) as excinfo:
        loop.variable = None
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'NoneType'.") in str(excinfo.value)


def test_variable_getter():
    '''Check that the variable property raises an exception if it is
    accessed and its value has not been set (is still None).

    '''
    loop = Loop()
    # invalid variable (test_check_variable tests check all ways a
    # variable could be invalid. Here we just check that the
    # _check_variable() method is called correctly). The particular
    # case we want to catch in the code is when the variable has not
    # been set, so is None.
    with pytest.raises(GenerationError) as excinfo:
        _ = loop.variable
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'NoneType'.") in str(excinfo.value)


def test_halo_read_access_is_abstract():
    '''Check that the generic _halo_read_access method is abstract'''
    loop = Loop()
    with pytest.raises(NotImplementedError) as excinfo:
        _ = loop._halo_read_access(None)
    assert ("This method needs to be implemented by the APIs that support "
            "distributed memory.") in str(excinfo.value)
