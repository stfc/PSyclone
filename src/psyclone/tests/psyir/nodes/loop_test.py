# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Modified by L. Turner, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Loop PSyIR node. '''

import os
import pytest
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import (
    Assignment, Loop, Literal, Schedule, Return, Reference, Routine)
from psyclone.psyir.symbols import (
    DataSymbol, REAL_SINGLE_TYPE, INTEGER_SINGLE_TYPE, INTEGER_TYPE, ArrayType,
    REAL_TYPE, SymbolTable)
from psyclone.psyir.tools import DependencyTools
from psyclone.tests.utilities import check_links


def make_loop():
    '''
    Utility to create a simple PSyIR loop for testing. The loop is contained
    within a Routine as it must have a parent scope for its Symbols.

    :returns: a PSyIR Loop instance.
    :rtype: :py:class:`psyclone.psyir.nodes.Loop`

    '''
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    sched = Routine.create("loop_test_sub")
    tmp = sched.symbol_table.new_symbol("tmp", symbol_type=DataSymbol,
                                        datatype=REAL_SINGLE_TYPE)
    isym = sched.symbol_table.new_symbol("i", symbol_type=DataSymbol,
                                         datatype=INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(Reference(tmp), Reference(isym))
    loop = Loop.create(isym, start, stop, step, [child_node])
    sched.addchild(loop)
    return loop


def test_loop_init():
    '''Test that a loop instance is created as expected and that it raises
    the expected exceptions where appropriate.

    '''
    loop = Loop()
    assert loop.parent is None
    assert loop.annotations == []
    assert loop._variable is None

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

    parent = Schedule()
    loop = Loop(parent=parent)
    assert loop.parent is parent

    annotations = ["was_where"]
    loop = Loop(annotations=annotations)
    assert loop.annotations == annotations


def test_loop_navigation_properties():
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

    loop.addchild(Literal("0", INTEGER_SINGLE_TYPE))
    loop.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    loop.addchild(Literal("1", INTEGER_SINGLE_TYPE))

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
        loop.start_expr = Literal("NOT_INITIALISED", INTEGER_SINGLE_TYPE)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.stop_expr = Literal("NOT_INITIALISED", INTEGER_SINGLE_TYPE)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.step_expr = Literal("NOT_INITIALISED", INTEGER_SINGLE_TYPE)
    assert error_str in str(err.value)

    # Check that Getters properties work
    loop.addchild(Schedule(parent=loop))
    loop.loop_body.addchild(Return(parent=loop.loop_body))

    assert loop.start_expr.value == "0"
    assert loop.stop_expr.value == "2"
    assert loop.step_expr.value == "1"
    assert isinstance(loop.loop_body[0], Return)

    # Test Setters
    loop.start_expr = Literal("1", INTEGER_SINGLE_TYPE)
    loop.stop_expr = Literal("3", INTEGER_SINGLE_TYPE)
    loop.step_expr = Literal("2", INTEGER_SINGLE_TYPE)

    assert loop.start_expr.value == "1"
    assert loop.stop_expr.value == "3"
    assert loop.step_expr.value == "2"


def test_loop_dag_name():
    '''Test the dag_name method of Loop.'''
    loop = make_loop()
    assert loop.dag_name == "loop_1"
    # Make the Loop an orphan to test the error handling.
    loop.detach()
    with pytest.raises(InternalError) as err:
        _ = loop.dag_name
    assert ("Cannot generate DAG name for loop node 'Loop[variable:'i']"
            in str(err.value))


def test_loop_node_str(monkeypatch):
    '''Test the node_str method of Loop.'''
    loop = make_loop()
    assert loop.node_str(colour=False) == "Loop[variable='i']"

    # Rather than mess about with colour codes, it's simpler if we
    # monkeypatch the 'coloured_name' method (inherited from Node).
    def fake_coloured_name(colour=True):
        if colour:
            return "yes"
        return "no"
    monkeypatch.setattr(loop, "coloured_name", fake_coloured_name)
    assert loop.node_str(colour=True) == "yes[variable='i']"
    assert loop.node_str(colour=False) == "no[variable='i']"

    # And with loop_type rules
    Loop.set_loop_type_inference_rules({"i-loop": {"variable": "i"}})
    out = loop.node_str()
    assert "yes[variable='i', loop_type='i-loop']" in out
    Loop.set_loop_type_inference_rules({})


def test_loop_replace_symbols_using():
    '''Test the replace_symbols_using() method of Loop.'''
    loop = make_loop()
    assert loop.variable.name == "i"
    # Create a symbol table containing a replacement symbol.
    table = SymbolTable()
    new_i = table.new_symbol("i", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE)
    assert loop.variable is not new_i
    loop.replace_symbols_using(table)
    # Loop variable should have been updated.
    assert loop.variable is new_i
    # Check that the method has recursed to the children too.
    assert loop.loop_body[0].rhs.symbol is new_i
    # Test when the Loop doesn't have the _variable property set.
    loop = Loop()
    loop.addchild(Literal("0", INTEGER_SINGLE_TYPE))
    loop.addchild(Literal("2", INTEGER_SINGLE_TYPE))
    loop.addchild(Literal("1", INTEGER_SINGLE_TYPE))
    loop.addchild(Schedule(parent=loop))
    assert not loop._variable
    loop.replace_symbols_using(table)


def test_loop_str():
    '''Test the __str__ property of Loop.'''
    loop = make_loop()
    out = str(loop)
    assert "Loop[variable:'i']\n" in out
    assert "End Loop" in out

    # And with loop_type rules
    Loop.set_loop_type_inference_rules({"i-loop": {"variable": "i"}})
    out = str(loop)
    assert "Loop[variable:'i', loop_type:'i-loop']\n" in out
    Loop.set_loop_type_inference_rules({})


def test_loop_independent_iterations():
    '''Test the independent_iterations method of Loop.'''
    loop = make_loop()
    assert not loop.independent_iterations()
    # Test that we can supply our own instance of DependencyTools and use
    # it to query any messages.
    dtools = DependencyTools()
    loop.independent_iterations(dep_tools=dtools)
    msgs = dtools.get_all_messages()
    assert len(msgs) == 1
    assert "variable 'tmp' is only written once" in str(msgs[0])


def test_loop_gen_code():
    ''' Check that the Loop gen_code method prints the proper loop '''
    base_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
    _, invoke_info = parse(os.path.join(base_path,
                                        "1.0.1_single_named_invoke.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)

    # By default LFRicLoop has step = 1 and it is not printed in the Fortran DO
    gen = str(psy.gen)
    assert "loop0_start = 1" in gen
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in gen
    assert "DO cell = loop0_start, loop0_stop" in gen

    # Change step to 2
    loop = psy.invokes.get('invoke_important_invoke').schedule[4]
    loop.step_expr = Literal("2", INTEGER_SINGLE_TYPE)

    # Now it is printed in the Fortran DO with the expression  ",2" at the end
    gen = str(psy.gen)
    assert "DO cell = loop0_start, loop0_stop, 2" in gen


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


def test_loop_create(fortran_writer):
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
    result = fortran_writer.loop_node(loop)
    assert result == "do i = 0, 1, 1\n  tmp = i\nenddo\n"


def test_loop_create_invalid():
    '''Test that the create method in a Loop class raises the expected
    exception if the provided input is invalid.

    '''
    zero = Literal("0", INTEGER_SINGLE_TYPE)
    one = Literal("1", INTEGER_SINGLE_TYPE)
    children = Assignment.create(
        Reference(DataSymbol("x", INTEGER_SINGLE_TYPE)),
        one.copy())

    # invalid variable (test_check_variable tests check all ways a
    # variable could be invalid. Here we just check that the
    # _check_variable() method is called correctly)
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(1, zero, one, one, [children.copy()])
    assert ("variable property in Loop class should be a DataSymbol but "
            "found 'int'.") in str(excinfo.value)

    variable = DataSymbol("i", INTEGER_TYPE)

    # start not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, "invalid", one, one, [children.copy()])
    assert ("Item 'str' can't be child 0 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'.") in str(excinfo.value)

    # stop not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, "invalid", one, [children.copy()])
    assert ("Item 'str' can't be child 1 of 'Loop'. The valid format is: "
            "'DataNode, DataNode, DataNode, Schedule'.") in str(excinfo.value)

    # step not a Node.
    with pytest.raises(GenerationError) as excinfo:
        _ = Loop.create(variable, zero, one, "invalid", [children.copy()])
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
    datanode1 = Literal("1", INTEGER_SINGLE_TYPE)
    datanode2 = Literal("2", INTEGER_SINGLE_TYPE)
    datanode3 = Literal("3", INTEGER_SINGLE_TYPE)
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
    assert ("variable 'my_array' in Loop class should be a ScalarType but "
            "found 'ArrayType'." in str(info.value))

    scalar_symbol = DataSymbol("my_array", REAL_TYPE)
    with pytest.raises(GenerationError) as info:
        Loop._check_variable(scalar_symbol)
    assert ("variable 'my_array' in Loop class should be a scalar integer but "
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


def test_loop_equality():
    '''Test the __eq__ method of Loop'''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    # Set up the symbols
    tmp = DataSymbol("tmp", REAL_SINGLE_TYPE)
    i_sym = DataSymbol("i", REAL_SINGLE_TYPE)

    # Create two equal loops
    loop_sym = DataSymbol("i", INTEGER_SINGLE_TYPE)
    sched1 = Schedule(symbol_table=symboltable)
    start = Literal("0", INTEGER_SINGLE_TYPE)
    stop = Literal("1", INTEGER_SINGLE_TYPE)
    step = Literal("1", INTEGER_SINGLE_TYPE)
    child_node = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched1.addchild(child_node)
    loop1 = Loop.create(loop_sym,
                        start, stop, step, [])
    loop1.children[3].detach()
    loop1.addchild(sched1, 3)
    start2 = start.copy()
    stop2 = stop.copy()
    step2 = step.copy()
    sched2 = Schedule()
    # Make sure it has the same ST instance, providing it as a constructor
    # parameter would create a copy and not use the same instance.
    sched2._symbol_table = symboltable
    child_node2 = Assignment.create(
        Reference(tmp),
        Reference(i_sym))
    sched2.addchild(child_node2)
    loop2 = Loop.create(loop_sym,
                        start2, stop2, step2, [])
    loop2.children[3].detach()
    loop2.addchild(sched2, 3)
    assert loop1 == loop2

    # Set different variables
    loop2.variable = DataSymbol("k", INTEGER_SINGLE_TYPE)
    assert loop1 != loop2


def test_set_loop_type_inference_rules():
    ''' Check that the set_loop_type_inference_rules populates the class
    attribute with the appropriate values, or fails if the rules do not
    follow the expected format.
    '''
    #
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules("a")
    assert ("The rules argument must be of type 'dict' but found"
            in str(err.value))
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules({1: "a"})
    assert "The rules keys must be of type 'str' but found" in str(err.value)
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules({"a": 1})
    assert "values must be of type 'dict' but found" in str(err.value)
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules({"a": {"invalid": 3}})
    assert ("All the values of the rule definition must be of type 'str' "
            "but found" in str(err.value))
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules({"a": {"invalid": "name"}})
    assert ("Currently only the 'variable' rule key is accepted, but found:"
            " 'invalid'." in str(err.value))
    with pytest.raises(TypeError) as err:
        Loop.set_loop_type_inference_rules({"a": {}})
    assert ("A rule must at least have a 'variable' field to specify the loop "
            "variable name that defines this loop_type, but the rule for "
            "'a' does not have it." in str(err.value))

    Loop.set_loop_type_inference_rules({"a": {"variable": "name"}})
    assert "name" in Loop._loop_type_inference_rules
    assert Loop._loop_type_inference_rules["name"] == "a"


def test_loop_type(fortran_reader):
    ''' Check that the loop_type method works as expeced '''
    code = '''
    subroutine basic_loop()
      integer, parameter :: jpi=16, jpj=16
      integer :: ji, jj
      real :: a(jpi, jpj), fconst
      do jj = 1, jpj
        do ji = 1, jpi
          a(ji) = fconst
        end do
      end do
    end subroutine basic_loop
    '''
    psyir = fortran_reader.psyir_from_source(code)

    # We can set inference rules, which will provide the right behaviour for
    # the generic loop.loop_type property
    Loop.set_loop_type_inference_rules({
            "lon": {"variable": "ji"},
            "lat": {"variable": "jj"}
    })
    outer_loop = psyir.walk(Loop)[0]
    assert outer_loop.loop_type == "lat"
    inner_loop = psyir.walk(Loop)[1]
    assert inner_loop.loop_type == "lon"

    # The rules can also be unset, which will mean that no loop has a loop_type
    Loop.set_loop_type_inference_rules(None)
    assert outer_loop.loop_type is None
    assert inner_loop.loop_type is None


def test_explicitly_private_symbols(fortran_reader):
    ''' Check that the explicitly_private_symbols functionality works '''
    code = '''
    subroutine basic_loop()
      integer, parameter :: jpi=16, jpj=16
      integer :: ji, jj
      real :: a(jpi, jpj), fconst
      do jj = 1, jpj
        do ji = 1, jpi
          a(ji) = b(ji, jj)
        end do
      end do
    end subroutine basic_loop
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    a_ref = psyir.walk(Assignment)[0].lhs
    b_ref = psyir.walk(Assignment)[0].rhs

    # By default no loop has explict local symbols
    assert len(loops[0].explicitly_private_symbols) == 0
    assert len(loops[1].explicitly_private_symbols) == 0

    # Add A as explicitly local to the first loop
    loops[0].explicitly_private_symbols.add(a_ref.symbol)
    assert len(loops[0].explicitly_private_symbols) == 1
    assert a_ref.symbol in loops[0].explicitly_private_symbols
    assert b_ref.symbol not in loops[0].explicitly_private_symbols
    assert len(loops[1].explicitly_private_symbols) == 0

    # Check that the copy method appropriately updates the symbol references
    new_psyir = psyir.copy()
    new_loops = new_psyir.walk(Loop)
    new_a_ref = new_psyir.walk(Assignment)[0].lhs
    assert new_a_ref.symbol is not a_ref.symbol
    assert a_ref.symbol not in new_loops[0].explicitly_private_symbols
    assert new_a_ref.symbol in new_loops[0].explicitly_private_symbols
