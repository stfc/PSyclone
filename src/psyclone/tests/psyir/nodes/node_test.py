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

''' Performs py.test tests on the Node PSyIR node. '''

from __future__ import absolute_import
import sys
import os
import re
import pytest
from psyclone.psyir.nodes.node import (ChildrenList, Node,
                                       _graphviz_digraph_class)
from psyclone.psyir.nodes import Schedule, Reference, Container, Routine, \
    Assignment, Return, Loop, Literal, Statement, node, KernelSchedule, \
    BinaryOperation

from psyclone.psyir.symbols import DataSymbol, SymbolError, \
    INTEGER_TYPE, REAL_TYPE, SymbolTable
from psyclone.psyGen import PSyFactory, Kern
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.tests.utilities import get_invoke
# pylint: disable=redefined-outer-name
from psyclone.psyir.nodes.node import colored

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


def test_node_coloured_name():
    ''' Tests for the coloured_name method of the Node class. '''
    tnode = Node()
    # Node is an abstract class
    with pytest.raises(NotImplementedError) as err:
        tnode.node_str()
    assert ("_text_name is an abstract attribute which needs to be given a "
            "string value in the concrete class 'Node'." in str(err.value))
    # Exception as _colour has not been set
    tnode._text_name = "ATest"
    with pytest.raises(NotImplementedError) as err:
        _ = tnode.coloured_name()
    assert ("The _colour attribute is abstract so needs to be given a string "
            "value in the concrete class 'Node'." in str(err.value))
    # Valid values
    tnode._colour = "white"
    assert tnode.coloured_name(False) == "ATest"
    assert tnode.coloured_name(True) == colored("ATest", "white")


def test_node_coloured_name_exception(monkeypatch):
    '''Test that the expected exception is raised if the colour provided
    to the colored function is invalid. Note, an exception is only
    raised if the termcolor package is installed. Therefore we
    monkeypatch the function to force the exception whether termcolor
    is installed or not.

    '''
    def dummy(_1, _2):
        '''Utility used to raise the required exception.'''
        raise KeyError()

    monkeypatch.setattr(node, "colored", dummy)

    tnode = Node()
    tnode._text_name = "ATest"
    tnode._colour = "invalid"
    with pytest.raises(InternalError) as err:
        _ = tnode.coloured_name()
    assert ("The _colour attribute in class 'Node' has been set to a "
            "colour ('invalid') that is not supported by the termcolor "
            "package." in str(err.value))


def test_node_str():
    ''' Tests for the Node.node_str method. '''
    tnode = Node()
    # Node is an abstract class
    with pytest.raises(NotImplementedError) as err:
        tnode.node_str()
    assert ("_text_name is an abstract attribute which needs to be given a "
            "string value in the concrete class 'Node'." in str(err.value))

    # Manually set the _text_name and _colour for this node to
    # something that will result in coloured output (if requested
    # *and* termcolor is installed).
    tnode._text_name = "FakeName"
    tnode._colour = "green"
    assert tnode.node_str(False) == "FakeName[]"
    assert tnode.node_str(True) == colored("FakeName", "green") + "[]"


def test_node_depth():
    '''
    Test that the Node class depth method returns the correct value for a
    Node in a tree. The start depth to determine a Node's depth is set to
    0. Depth of a Schedule is 1 and increases for its descendants.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule.detach()
    # Assert that start_depth of any Node (including Schedule) is 0
    assert schedule.START_DEPTH == 0
    # Assert that Schedule depth is 1
    assert schedule.depth == 1
    # Depth increases by 1 for descendants at each level
    for child in schedule.children:
        assert child.depth == 2
    for child in schedule.children[3].children:
        assert child.depth == 3


def test_node_position():
    '''
    Test that the Node class position and abs_position methods return
    the correct value for a Node in a tree. The start position is
    set to 0. Relative position starts from 0 and absolute from 1.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.7_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule.detach()
    child = schedule.children[6]
    # Assert that position of a Schedule (no parent Node) is 0
    assert schedule.position == 0
    assert schedule.abs_position == 0
    # Assert that start_position of any Node is 0
    assert child.START_POSITION == 0
    # Assert that relative and absolute positions return correct values
    assert child.position == 6
    assert child.abs_position == 7

    # Insert two more levels of nodes in top of the root
    previous_root = child.root
    container1 = Container("test1")
    container2 = Container("test2")
    container2.addchild(previous_root)
    container1.addchild(container2)
    # The relative position should still be the same but the absolute position
    # should increase by 2.
    assert child.position == 6
    assert child.abs_position == 9

    # Check that the _find_position returns the correct distance between itself
    # and the provided ancestor.
    _, position = child._find_position(child.ancestor(Routine), 0)
    assert position == 7

    # If no starting position is provided it starts with START_POSITION=0
    _, same_position = child._find_position(child.ancestor(Routine))
    assert same_position == position

    # Test InternalError for _find_position with an incorrect position
    with pytest.raises(InternalError) as excinfo:
        _, _ = child._find_position(child.root.children, -2)
    assert "started from -2 instead of 0" in str(excinfo.value)


def test_node_abs_position_error():
    ''' Check that the abs_position method produces and internal error when
    a node can be found as one of the children of its parent (this just
    happens with inconsistent parent-child connections). '''

    parent = Schedule()
    node1 = Statement()
    # Manually connect the _parent attribute which won't make a consistent
    # two-way relationship
    node1._parent = parent

    with pytest.raises(InternalError) as err:
        _ = node1.abs_position
    assert "Error in search for Node position in the tree" in str(err.value)


def test_node_root():
    '''
    Test that the Node class root method returns the correct instance
    for a Node in a tree.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.7_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    ru_schedule = invoke.schedule
    # Select a loop and the kernel inside
    ru_loop = ru_schedule.children[1]
    ru_kern = ru_loop.children[0]
    # Assert that the absolute root is a Container
    assert isinstance(ru_kern.root, Container)


def test_node_annotations():
    '''Test that an instance of the Node class raises an exception if an
    annotation is invalid. Note, any annotation will be invalid here
    as Node does not set a list of valid annotations (this is the job
    of the subclass).

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Node(annotations=["invalid"])
    assert (
        "Node with unrecognised annotation 'invalid', valid annotations are: "
        "()." in str(excinfo.value))


def test_node_args():
    '''Test that the Node class args method returns the correct arguments
    for Nodes that do not have arguments themselves'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    kern1 = loop1.loop_body[0]
    loop2 = schedule.children[1]
    kern2 = loop2.loop_body[0]
    # 1) Schedule (not that this is useful)
    all_args = kern1.arguments.args
    all_args.extend(kern2.arguments.args)
    schedule_args = schedule.args
    for idx, arg in enumerate(all_args):
        assert arg == schedule_args[idx]
    # 2) Loop1
    loop1_args = loop1.args
    for idx, arg in enumerate(kern1.arguments.args):
        assert arg == loop1_args[idx]
    # 3) Loop2
    loop2_args = loop2.args
    for idx, arg in enumerate(kern2.arguments.args):
        assert arg == loop2_args[idx]
    # 4) Loop fuse
    ftrans = LFRicLoopFuseTrans()
    ftrans.apply(schedule.children[0], schedule.children[1])
    loop = schedule.children[0]
    kern1 = loop.loop_body[0]
    kern2 = loop.loop_body[1]
    loop_args = loop.args
    kern_args = kern1.arguments.args
    kern_args.extend(kern2.arguments.args)
    for idx, arg in enumerate(kern_args):
        assert arg == loop_args[idx]


def test_node_forward_dependence():
    '''Test that the Node class forward_dependence method returns the
    closest dependent Node after the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for a call
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0]
    assert first_loop.forward_dependence() == writer
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[7]
    halo_field = schedule.children[8]
    next_loop = schedule.children[9]
    # a) previous loop depends on halo exchange
    assert prev_loop.forward_dependence() == halo_field
    # b) halo exchange depends on following loop
    assert halo_field.forward_dependence() == next_loop

    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[0]
    sum_loop = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_loop = schedule.children[3]
    # a) prev loop depends on sum loop
    assert prev_loop.forward_dependence() == sum_loop
    # b) sum loop depends on global sum loop
    assert sum_loop.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next loop
    assert global_sum_loop.forward_dependence() == next_loop


def test_node_backward_dependence():
    '''Test that the Node class backward_dependence method returns the
    closest dependent Node before the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: loop no backwards dependence
    loop3 = schedule.children[2]
    assert not loop3.backward_dependence()
    # 2: loop to loop backward dependence
    # a) many steps
    last_loop_node = schedule.children[6]
    prev_dep_loop_node = schedule.children[3]
    assert last_loop_node.backward_dependence() == prev_dep_loop_node
    # b) previous
    assert prev_dep_loop_node.backward_dependence() == loop3
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop2 = schedule.children[7]
    halo_exchange = schedule.children[8]
    loop3 = schedule.children[9]
    # a) following loop node depends on halo exchange node
    result = loop3.backward_dependence()
    assert result == halo_exchange
    # b) halo exchange node depends on previous loop node
    result = halo_exchange.backward_dependence()
    assert result == loop2
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    global_sum = schedule.children[2]
    loop3 = schedule.children[3]
    # a) loop3 depends on global sum
    assert loop3.backward_dependence() == global_sum
    # b) global sum depends on loop2
    assert global_sum.backward_dependence() == loop2
    # c) loop2 (sum) depends on loop1
    assert loop2.backward_dependence() == loop1


def test_node_is_valid_location():
    ''' Test that the Node class is_valid_location method returns True if
    the new location does not break any data dependencies, otherwise it
    returns False.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: new node argument is invalid
    anode = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location("invalid_node_argument")
    assert "argument is not a Node, it is a 'str'." in str(excinfo.value)
    # 2: optional position argument is invalid
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(anode, position="invalid_node_argument")
    assert "The position argument in the psyGen" in str(excinfo.value)
    assert "method must be one of" in str(excinfo.value)
    # 3: parents of node and new_node are not the same
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(schedule.children[4].children[0])
    assert ("the node and the location do not have the same "
            "parent") in str(excinfo.value)
    # 4: positions are the same
    prev_node = schedule.children[0]
    anode = schedule.children[1]
    next_node = schedule.children[2]
    # a) before this node
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(anode, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # b) after this node
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(anode, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # c) after previous node
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(prev_node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # d) before next node
    with pytest.raises(GenerationError) as excinfo:
        anode.is_valid_location(next_node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # 5: valid no previous dependency
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 6: valid no prev dep
    anode = schedule.children[2]
    assert anode.is_valid_location(schedule.children[0])
    # 7: valid prev dep (after)
    anode = schedule.children[6]
    assert anode.is_valid_location(schedule.children[3], position="after")
    # 8: invalid prev dep (before)
    assert not anode.is_valid_location(schedule.children[3], position="before")
    # 9: valid no following dep
    anode = schedule.children[4]
    assert anode.is_valid_location(schedule.children[6], position="after")
    # 10: valid following dep (before)
    anode = schedule.children[0]
    assert anode.is_valid_location(schedule.children[3], position="before")
    # 11: invalid following dep (after)
    anode = schedule.children[0]
    assert not anode.is_valid_location(schedule.children[3], position="after")


def test_node_ancestor():
    ''' Test the Node.ancestor() method. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0,
                           dist_mem=False)
    sched = invoke.schedule
    kern = sched[0].loop_body[0].loop_body[0]
    anode = kern.ancestor(Node)
    assert isinstance(anode, Schedule)
    # If 'excluding' is supplied then it can only be a single type or a
    # tuple of types
    anode = kern.ancestor(Node, excluding=Schedule)
    assert anode is sched[0].loop_body[0]
    anode = kern.ancestor(Node, excluding=(Schedule,))
    assert anode is sched[0].loop_body[0]
    with pytest.raises(TypeError) as err:
        kern.ancestor(Node, excluding=[Schedule])
    assert ("argument to ancestor() must be a type or a tuple of types but "
            "got: 'list'" in str(err.value))
    # Check that the include_self argument behaves as expected
    anode = kern.ancestor(Kern, excluding=(Schedule,), include_self=True)
    assert anode is kern


def test_dag_names():
    ''' Test that the dag_name method returns the correct value for the
    node class and its specialisations. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    assert super(Schedule, schedule).dag_name == "node_1"
    assert schedule.dag_name == "routine_invoke_0_testkern_type_0"
    assert schedule.children[0].dag_name == "checkHaloExchange(f1)_0"
    assert schedule.children[4].dag_name == "loop_5"
    schedule.children[4].loop_type = "colour"
    assert schedule.children[4].dag_name == "loop_[colour]_5"
    schedule.children[4].loop_type = ""
    assert (schedule.children[4].loop_body[0].dag_name ==
            "kernel_testkern_code_10")
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert global_sum.dag_name == "globalsum(asum)_2"
    builtin = schedule.children[1].loop_body[0]
    assert builtin.dag_name == "builtin_sum_x_12"


def test_node_digraph_no_graphviz(monkeypatch):
    ''' Test that the function to get the graphviz Digraph class type returns
    None if graphviz is not installed. We monkeypatch sys.modules to ensure
    that it always appears that graphviz is not installed on this system. '''
    monkeypatch.setitem(sys.modules, 'graphviz', None)
    dag_class = _graphviz_digraph_class()
    assert dag_class is None


def test_node_dag_no_graphviz(tmpdir, monkeypatch):
    ''' Test that the dag generation returns None (and that no file is created)
    when graphviz is not installed. We make this test independent of whether or
    not graphviz is installed by monkeypatching sys.modules. '''
    monkeypatch.setitem(sys.modules, 'graphviz', None)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    my_file = tmpdir.join('test')
    dag = invoke.schedule.dag(file_name=my_file.strpath)
    assert dag is None
    assert not os.path.exists(my_file.strpath)


def test_node_dag_returns_digraph(monkeypatch):
    ''' Test that the dag generation returns the expected Digraph object. We
    make this test independent of whether or not graphviz is installed by
    monkeypatching the psyir.nodes.node._graphviz_digraph_class function to
    return a fake digraph class type. '''
    class FakeDigraph(object):
        ''' Fake version of graphviz.Digraph class with key methods
        implemented as noops. '''
        # pylint: disable=redefined-builtin
        def __init__(self, format=None):
            ''' Fake constructor. '''

        def node(self, _name):
            ''' Fake node method. '''

        def edge(self, _name1, _name2, color="red"):
            ''' Fake edge method. '''

        def render(self, filename):
            ''' Fake render method. '''

    monkeypatch.setattr(node, "_graphviz_digraph_class", lambda: FakeDigraph)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    dag = schedule.dag()
    assert isinstance(dag, FakeDigraph)


def test_node_dag_wrong_file_format(monkeypatch):
    ''' Test the handling of the error raised by graphviz when it is passed
    an invalid file format. We make this test independent of whether or not
    graphviz is actually available by monkeypatching the
    psyir.nodes.node._graphviz_digraph_class function to return a fake digraph
    class type that mimics the error. '''
    class FakeDigraph(object):
        ''' Fake version of graphviz.Digraph class that raises a ValueError
        when instantiated. '''
        # pylint: disable=redefined-builtin
        def __init__(self, format=None):
            raise ValueError(format)

    monkeypatch.setattr(node, "_graphviz_digraph_class", lambda: FakeDigraph)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    with pytest.raises(GenerationError) as err:
        invoke.schedule.dag()
    assert "unsupported graphviz file format 'svg' provided" in str(err.value)


# Use a regex to allow for whitespace differences between graphviz
# versions. Need a raw-string (r"") to get new-lines handled nicely.
EXPECTED2 = re.compile(
    r"digraph {\n"
    r"\s*routine_invoke_0_0_start\n"
    r"\s*routine_invoke_0_0_end\n"
    r"\s*loop_1_start\n"
    r"\s*loop_1_end\n"
    r"\s*loop_1_end -> loop_7_start \[color=green\]\n"
    r"\s*routine_invoke_0_0_start -> loop_1_start \[color=blue\]\n"
    r"\s*schedule_5_start\n"
    r"\s*schedule_5_end\n"
    r"\s*schedule_5_end -> loop_1_end \[color=blue\]\n"
    r"\s*loop_1_start -> schedule_5_start \[color=blue\]\n"
    r"\s*kernel_testkern_qr_code_6\n"
    r"\s*kernel_testkern_qr_code_6 -> schedule_5_end \[color=blue\]\n"
    r"\s*schedule_5_start -> kernel_testkern_qr_code_6 \[color=blue\]\n"
    r"\s*loop_7_start\n"
    r"\s*loop_7_end\n"
    r"\s*loop_7_end -> routine_invoke_0_0_end \[color=blue\]\n"
    r"\s*loop_1_end -> loop_7_start \[color=red\]\n"
    r"\s*schedule_11_start\n"
    r"\s*schedule_11_end\n"
    r"\s*schedule_11_end -> loop_7_end \[color=blue\]\n"
    r"\s*loop_7_start -> schedule_11_start \[color=blue\]\n"
    r"\s*kernel_testkern_qr_code_12\n"
    r"\s*kernel_testkern_qr_code_12 -> schedule_11_end \[color=blue\]\n"
    r"\s*schedule_11_start -> kernel_testkern_qr_code_12 \[color=blue\]\n"
    r"}")
# pylint: enable=anomalous-backslash-in-string


def test_node_dag(tmpdir, have_graphviz):
    ''' Test that dag generation works correctly. Skip the test if
    graphviz is not installed. '''
    if not have_graphviz:
        return
    # We may not have graphviz installed so disable pylint error
    # pylint: disable=import-outside-toplevel
    import graphviz
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.1_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    dag = schedule.dag(file_name=my_file.strpath)
    assert isinstance(dag, graphviz.Digraph)

    result = my_file.read()
    assert EXPECTED2.match(result)
    my_file = tmpdir.join('test.svg')
    result = my_file.read()
    for name in ["<title>routine_invoke_0_0_start</title>",
                 "<title>routine_invoke_0_0_end</title>",
                 "<title>loop_1_start</title>",
                 "<title>loop_1_end</title>",
                 "<title>kernel_testkern_qr_code_6</title>",
                 "<title>kernel_testkern_qr_code_12</title>",
                 "<svg", "</svg>", ]:
        assert name in result
    for colour_name, colour_code in [("blue", "#0000ff"),
                                     ("green", "#00ff00"),
                                     ("red", "#ff0000")]:
        assert colour_name in result or colour_code in result

    with pytest.raises(GenerationError) as excinfo:
        schedule.dag(file_name=my_file.strpath, file_format="rubbish")
    assert "unsupported graphviz file format" in str(excinfo.value)


def test_scope():
    '''Test that the scope method in a Node instance returns the closest
    ancestor Schedule or Container Node (including itself) or raises
    an exception if one does not exist.

    '''
    kernel_symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    kernel_symbol_table.add(symbol)
    ref = Reference(symbol)
    assign = Assignment.create(ref, Literal("0.0", REAL_TYPE))
    kernel_schedule = KernelSchedule.create("my_kernel", kernel_symbol_table,
                                            [assign])
    container = Container.create("my_container", SymbolTable(),
                                 [kernel_schedule])
    assert ref.scope is kernel_schedule
    assert assign.scope is kernel_schedule
    assert kernel_schedule.scope is kernel_schedule
    assert container.scope is container

    anode = Literal("x", INTEGER_TYPE)
    with pytest.raises(SymbolError) as excinfo:
        _ = anode.scope
    assert ("Unable to find the scope of node "
            "'Literal[value:'x', Scalar<INTEGER, UNDEFINED>]' as "
            "none of its ancestors are Container or Schedule nodes."
            in str(excinfo.value))


def test_children_validation():
    ''' Test that nodes are validated when inserted as children of other
    nodes. For simplicity we use Node subclasses to test this functionality
    across a range of possible operations.

    The specific logic of each validate method will be tested individually
    inside each Node test file.
    '''
    assignment = Assignment()
    return_stmt = Return()
    reference = Reference(DataSymbol("a", INTEGER_TYPE))

    assert isinstance(assignment.children, (ChildrenList, list))

    # Try adding a invalid child (e.g. a return_stmt into an assignment)
    with pytest.raises(GenerationError) as error:
        assignment.addchild(return_stmt)
    assert "Item 'Return' can't be child 0 of 'Assignment'. The valid format" \
        " is: 'DataNode, DataNode'." in str(error.value)

    # The same behaviour occurs when list insertion operations are used.
    with pytest.raises(GenerationError):
        assignment.children.append(return_stmt)

    with pytest.raises(GenerationError):
        assignment.children[0] = return_stmt

    with pytest.raises(GenerationError):
        assignment.children.insert(0, return_stmt)

    with pytest.raises(GenerationError):
        assignment.children.extend([return_stmt])

    with pytest.raises(GenerationError):
        assignment.children = assignment.children + [return_stmt]

    # Valid nodes are accepted
    assignment.addchild(reference)

    # Check displaced items are also be checked when needed
    start = Literal("0", INTEGER_TYPE)
    stop = Literal("1", INTEGER_TYPE)
    step = Literal("2", INTEGER_TYPE)
    child_node = Assignment.create(Reference(DataSymbol("tmp", REAL_TYPE)),
                                   Reference(DataSymbol("i", REAL_TYPE)))
    loop_variable = DataSymbol("idx", INTEGER_TYPE)
    loop = Loop.create(loop_variable, start, stop, step, [child_node])
    with pytest.raises(GenerationError):
        loop.children.insert(1, Literal("0", INTEGER_TYPE))

    with pytest.raises(GenerationError):
        loop.children.remove(stop)

    with pytest.raises(GenerationError):
        del loop.children[2]

    with pytest.raises(GenerationError):
        loop.children.pop(2)

    with pytest.raises(GenerationError):
        loop.children.reverse()

    # But the in the right circumstances they work fine
    assert isinstance(loop.children.pop(), Schedule)
    loop.children.reverse()
    assert loop.children[0].value == "2"


def test_children_is_orphan_validation():
    ''' Test that all kinds of children addition operations make sure that a
    child is orphan before accepting it as its own child. '''
    # Create 2 schedules and add a child in the first Schedule
    schedule1 = Schedule()
    schedule2 = Schedule()
    statement = Return(parent=schedule1)
    schedule1.addchild(statement)

    # Try to move the child without removing the parent connection first
    errmsg = ("Item 'Return' can't be added as child of 'Schedule' because "
              "it is not an orphan. It already has a 'Schedule' as a parent.")
    with pytest.raises(GenerationError) as error:
        schedule2.addchild(statement)
    assert errmsg in str(error.value)

    with pytest.raises(GenerationError) as error:
        schedule2.children.append(statement)
    assert errmsg in str(error.value)

    with pytest.raises(GenerationError) as error:
        schedule2.children.insert(0, statement)
    assert errmsg in str(error.value)

    with pytest.raises(GenerationError) as error:
        schedule2.children.extend([statement])
    assert errmsg in str(error.value)

    with pytest.raises(GenerationError) as error:
        schedule2.children = [statement]
    assert errmsg in str(error.value)

    schedule2.addchild(Return())
    with pytest.raises(GenerationError) as error:
        schedule2.children[0] = statement
    assert errmsg in str(error.value)

    # It can be added when it has been detached from its previous parent
    schedule2.addchild(statement.detach())


def test_children_is_orphan_same_parent():
    ''' Test children addition operations with a node that is not an orphan
    and already belongs to the parent to which it is being added.'''
    # Create 2 schedules and add a child in the first Schedule
    schedule1 = Schedule()
    statement = Return(parent=schedule1)
    schedule1.addchild(statement)

    with pytest.raises(GenerationError) as error:
        schedule1.addchild(statement)
    assert ("Item 'Return' can't be added as child of 'Schedule' because "
            "it is not an orphan. It already has a 'Schedule' as a parent."
            in str(error.value))


def test_children_setter():
    ''' Test that the children setter sets-up accepts lists or raises
    the appropriate issue. '''
    testnode = Schedule()

    # children is initialised as a ChildrenList
    assert isinstance(testnode.children, ChildrenList)

    # When is set up with a list, this becomes a ChildrenList
    statement1 = Statement()
    statement2 = Statement()
    testnode.children = [statement1, statement2]
    assert isinstance(testnode.children, ChildrenList)
    assert statement1.parent is testnode
    assert statement2.parent is testnode

    # Other types are not accepted
    with pytest.raises(TypeError) as error:
        testnode.children = Node()
    assert "The 'my_children' parameter of the node.children setter must be" \
           " a list." in str(error.value)

    # If a children list is overwritten, it properly disconnects the previous
    # children
    testnode.children = []
    assert statement1.parent is None
    assert statement2.parent is None


def test_lower_to_language_level(monkeypatch):
    ''' Test that Node has a lower_to_language_level() method that \
    recurses to the same method of its children. '''

    # Monkeypatch the lower_to_language_level to just mark a flag
    def visited(self):
        self._visited_flag = True
    monkeypatch.setattr(Statement, "lower_to_language_level", visited)

    testnode = Schedule()
    node1 = Statement()
    node2 = Statement()
    testnode.children = [node1, node2]

    # Execute method
    testnode.lower_to_language_level()

    # Check all children have been visited
    for child in testnode.children:
        # This member only exists in the monkeypatched version
        # pylint:disable=no-member
        assert child._visited_flag


def test_replace_with():
    '''Check that the replace_with method behaves as expected.'''

    parent_node = Schedule()
    node1 = Statement()
    node2 = Statement()
    node3 = Statement()
    parent_node.children = [node1, node2, node3]
    new_node = Assignment()

    node2.replace_with(new_node)

    assert parent_node.children[1] is new_node
    assert new_node.parent is parent_node
    assert node2.parent is None


def test_replace_with_error1():
    '''Check that the replace_with method raises the expected exception if
    the type of node is invalid for the location it is being added
    to.

    '''
    iterator = DataSymbol("i", INTEGER_TYPE)
    start = Literal("0", INTEGER_TYPE)
    stop = Literal("1", INTEGER_TYPE)
    step = Literal("1", INTEGER_TYPE)
    loop = Loop.create(iterator, start, stop, step, [])
    new_node = Assignment()
    # The first child of a loop is the loop start value which should
    # be a DataNode.
    with pytest.raises(GenerationError) as info:
        loop.children[0].replace_with(new_node)
    assert("Item 'Assignment' can't be child 0 of 'Loop'. The valid "
           "format is: 'DataNode, DataNode, DataNode, Schedule'"
           in str(info.value))


def test_replace_with_error2():
    '''Check that the replace_with method raises the expected exceptions
    if either node is invalid.

    '''
    parent = Schedule()
    node1 = Statement()
    node2 = Statement()

    with pytest.raises(TypeError) as info:
        node1.replace_with("hello")
    assert("The argument node in method replace_with in the Node class "
           "should be a Node but found 'str'." in str(info.value))

    with pytest.raises(GenerationError) as info:
        node1.replace_with(node2)
    assert("This node should have a parent if its replace_with method "
           "is called." in str(info.value))

    parent.children = [node1, node2]
    with pytest.raises(GenerationError) as info:
        node1.replace_with(node2)
    assert("The parent of argument node in method replace_with in the Node "
           "class should be None but found 'Schedule'." in str(info.value))

    node3 = Container("hello")
    with pytest.raises(GenerationError) as info:
        node1.replace_with(node3)
        assert (
            "Generation Error: Item 'Container' can't be child 0 of "
            "'Schedule'. The valid format is: '[Statement]*'." in
            str(info.value))


def test_copy():
    ''' Check that the copy method of a generic Node creates a duplicate of
    the original node'''
    tnode = Node()
    duplicated_instance = tnode.copy()
    assert isinstance(duplicated_instance, type(tnode))
    assert duplicated_instance is not tnode
    assert duplicated_instance.parent is None


def test_pop_all_children():
    ''' Check that the pop_all_children method removes the children nodes
    from the children list and return them all in a list. '''

    # Create a PSyIR tree
    parent = Schedule()
    node1 = Statement()
    parent.addchild(node1)
    node2 = Statement()
    parent.addchild(node2)

    # Execute pop_all_children method
    result = parent.pop_all_children()

    # Check the resulting nodes and connections are as expected
    assert isinstance(result, list)
    assert len(parent.children) == 0
    assert node1.parent is None
    assert node2.parent is None
    assert result[0] is node1 and result[1] is node2


def test_detach():
    ''' Check that the detach method removes a node from its parent node. '''

    # Create a PSyIR tree
    parent = Schedule()
    node1 = Statement()
    parent.addchild(node1)
    node2 = Statement()
    parent.addchild(node2)

    # Execute the detach method on node 1, it should return itself
    assert node1.detach() is node1

    # Check that the resulting nodes and connections are correct
    assert node1.parent is None
    assert len(parent.children) == 1
    assert parent.children[0] is node2

    # Executing it again still succeeds
    assert node1.detach() is node1


def test_parent_references_coherency():
    ''' Check that the parent references keep updated with the children
    node operations. '''
    parent = Schedule()

    # Children addition methods
    node1 = Statement()
    parent.addchild(node1)
    assert node1.parent is parent

    node2 = Statement()
    parent.children.append(node2)
    assert node2.parent is parent

    node3 = Statement()
    parent.children.extend([node3])
    assert node3.parent is parent

    node4 = Statement()
    parent.children.insert(0, node4)
    assert node4.parent is parent

    # Node deletion
    node = parent.children.pop()
    assert node.parent is None
    assert node is node3

    del parent.children[0]
    assert node4.parent is None

    parent.children = []
    assert node2.parent is None

    # The insertion has deletions and additions
    parent.addchild(node1)
    parent.children[0] = node2
    assert node1.parent is None
    assert node2.parent is parent

    # The assignment also deletes and adds nodes
    parent.addchild(node1)
    parent.children = parent.children + [node3]
    assert node1.parent is parent
    assert node2.parent is parent
    assert node3.parent is parent


def test_node_constructor_with_parent():
    ''' Check that the node constructor parent parameter works as expected. '''
    parent = Schedule()
    wrong_parent = Schedule()

    # By default no parent reference is given
    node = Statement()
    assert node.parent is None
    assert node.has_constructor_parent is False

    # The parent argument can predefine the parent reference
    node = Return(parent=parent)
    assert node.parent is parent
    assert node.has_constructor_parent is True

    # Then only an addition to this predefined parent is accepted
    with pytest.raises(GenerationError) as err:
        wrong_parent.addchild(node)
    assert ("'Schedule' cannot be set as parent of 'Return' because its "
            "constructor predefined the parent reference to a different "
            "'Schedule' node." in str(err.value))

    # Once given the proper parent, it can act as a regular node
    parent.addchild(node)
    assert node.parent is parent
    assert node.has_constructor_parent is False
    wrong_parent.addchild(node.detach())
    assert node.parent is wrong_parent


def test_following_preceding():
    '''Test that the preceding and following() methods in the Node class
    behave as expected.

    '''
    # 1: There is no Routine ancestor node.
    a_ref = Reference(DataSymbol("a", REAL_TYPE))
    b_ref = Reference(DataSymbol("b", REAL_TYPE))
    c_ref = Reference(DataSymbol("c", REAL_TYPE))
    d_ref = Reference(DataSymbol("d", REAL_TYPE))
    multiply1 = BinaryOperation.create(
        BinaryOperation.Operator.MUL, c_ref, d_ref)
    multiply2 = BinaryOperation.create(
        BinaryOperation.Operator.MUL, b_ref, multiply1)
    assign1 = Assignment.create(a_ref, multiply2)

    # 1a: First node.
    assert assign1.following() == [
        a_ref, multiply2, b_ref, multiply1, c_ref, d_ref]
    assert not assign1.preceding()

    # 1b: Last node.
    assert not d_ref.following()
    assert d_ref.preceding() == [
        assign1, a_ref, multiply2, b_ref, multiply1, c_ref]
    assert d_ref.preceding(reverse=True) == [
        c_ref, multiply1, b_ref, multiply2, a_ref, assign1]

    # 1c: Middle node.
    assert multiply1.following() == [c_ref, d_ref]
    assert multiply1.preceding() == [assign1, a_ref, multiply2, b_ref]

    # 2: Routine is an ancestor node, but is not a root
    # node.
    routine1 = Routine.create("routine1", SymbolTable(), [assign1])
    e_ref = Reference(DataSymbol("e", REAL_TYPE))
    zero = Literal("0.0", REAL_TYPE)
    assign2 = Assignment.create(e_ref, zero)
    routine2 = Routine.create("routine2", SymbolTable(), [assign2])
    container = Container.create(
        "container", SymbolTable(), [routine1, routine2])

    # 2a: Middle node. Additional container and routine2 nodes are not
    # returned by default ('routine' argument defaults to True).
    assert multiply1.following() == [c_ref, d_ref]
    assert (multiply1.preceding() ==
            [routine1, assign1, a_ref, multiply2, b_ref])

    # 2b: Middle node. 'routine' argument is set to False. Additional
    # container and routine2 nodes are returned.
    assert (multiply1.following(routine=False) ==
            [c_ref, d_ref, routine2, assign2, e_ref, zero])
    assert (multiply1.preceding(routine=False) ==
            [container, routine1, assign1, a_ref, multiply2, b_ref])
