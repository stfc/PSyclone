# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
from psyclone.psyir.nodes import Node, Schedule, Reference, Container, \
    Assignment, Literal
from psyclone.psyir.symbols import DataSymbol, SymbolError, SymbolTable, \
    REAL_TYPE
from psyclone.psyGen import PSyFactory, OMPDoDirective, Kern, KernelSchedule
from psyclone.errors import InternalError, GenerationError
from psyclone.parse.algorithm import parse
from psyclone.transformations import DynamoLoopFuseTrans
from psyclone.tests.utilities import get_invoke

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


def test_node_abstract_methods():
    ''' Tests that the abstract methods of the Node class raise appropriate
    errors. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    with pytest.raises(NotImplementedError) as err:
        Node.gen_code(loop, parent=None)
    assert "Please implement me" in str(err.value)


def test_node_coloured_name():
    ''' Tests for the coloured_name method of the Node class. '''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    tnode = Node()
    assert tnode.coloured_name(False) == "Node"
    # Check that we can change the name of the Node and the colour associated
    # with it
    tnode._text_name = "ATest"
    tnode._colour_key = "Schedule"
    assert tnode.coloured_name(False) == "ATest"
    assert tnode.coloured_name(True) == colored(
        "ATest", SCHEDULE_COLOUR_MAP["Schedule"])
    # Check that an unrecognised colour-map entry gives us un-coloured text
    tnode._colour_key = "not-recognised"
    assert tnode.coloured_name(True) == "ATest"


def test_node_str():
    ''' Tests for the Node.node_str method. '''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    tnode = Node()
    # Manually set the colour key for this node to something that will result
    # in coloured output (if requested *and* termcolor is installed).
    tnode._colour_key = "Loop"
    assert tnode.node_str(False) == "Node[]"
    assert tnode.node_str(True) == colored("Node",
                                           SCHEDULE_COLOUR_MAP["Loop"]) + "[]"


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
    schedule = invoke.schedule
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
    schedule = invoke.schedule
    child = schedule.children[6]
    # Assert that position of a Schedule (no parent Node) is 0
    assert schedule.position == 0
    # Assert that start_position of any Node is 0
    assert child.START_POSITION == 0
    # Assert that relative and absolute positions return correct values
    assert child.position == 6
    assert child.abs_position == 7
    # Test InternalError for _find_position with an incorrect position
    with pytest.raises(InternalError) as excinfo:
        _, _ = child._find_position(child.root.children, -2)
    assert "started from -2 instead of 0" in str(excinfo.value)
    # Test InternalError for abs_position with a Node that does
    # not belong to the Schedule
    ompdir = OMPDoDirective()
    with pytest.raises(InternalError) as excinfo:
        _ = ompdir.abs_position
    assert ("PSyclone internal error: Error in search for Node position "
            "in the tree") in str(excinfo.value)


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
    # Assert that the absolute root is a Schedule
    assert isinstance(ru_kern.root, Schedule)


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
    ftrans = DynamoLoopFuseTrans()
    ftrans.same_space = True
    schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1])
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
    '''Test that the Node class is_valid_location method returns True if
    the new location does not break any data dependencies, otherwise it
    returns False'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: new node argument is invalid
    node = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location("invalid_node_argument")
    assert "argument is not a Node, it is a 'str'." in str(excinfo.value)
    # 2: optional position argument is invalid
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="invalid_node_argument")
    assert "The position argument in the psyGen" in str(excinfo.value)
    assert "method must be one of" in str(excinfo.value)
    # 3: parents of node and new_node are not the same
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(schedule.children[3].children[0])
    assert ("the node and the location do not have the same "
            "parent") in str(excinfo.value)
    # 4: positions are the same
    prev_node = schedule.children[0]
    node = schedule.children[1]
    next_node = schedule.children[2]
    # a) before this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # b) after this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # c) after previous node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(prev_node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # d) before next node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(next_node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # 5: valid no previous dependency
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 6: valid no prev dep
    node = schedule.children[2]
    assert node.is_valid_location(schedule.children[0])
    # 7: valid prev dep (after)
    node = schedule.children[6]
    assert node.is_valid_location(schedule.children[3], position="after")
    # 8: invalid prev dep (before)
    assert not node.is_valid_location(schedule.children[3], position="before")
    # 9: valid no following dep
    node = schedule.children[4]
    assert node.is_valid_location(schedule.children[6], position="after")
    # 10: valid following dep (before)
    node = schedule.children[0]
    assert node.is_valid_location(schedule.children[3], position="before")
    # 11: invalid following dep (after)
    node = schedule.children[0]
    assert not node.is_valid_location(schedule.children[3], position="after")


def test_node_ancestor():
    ''' Test the Node.ancestor() method. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched[0].loop_body[0].loop_body[0]
    node = kern.ancestor(Node)
    assert isinstance(node, Schedule)
    # If 'excluding' is supplied then it can only be a single type or a
    # tuple of types
    node = kern.ancestor(Node, excluding=Schedule)
    assert node is sched[0].loop_body[0]
    node = kern.ancestor(Node, excluding=(Schedule,))
    assert node is sched[0].loop_body[0]
    with pytest.raises(TypeError) as err:
        kern.ancestor(Node, excluding=[Schedule])
    assert ("argument to ancestor() must be a type or a tuple of types but "
            "got: 'list'" in str(err.value))
    # Check that the include_self argument behaves as expected
    node = kern.ancestor(Kern, excluding=(Schedule,), include_self=True)
    assert node is kern


def test_dag_names():
    '''test that the dag_name method returns the correct value for the
    node class and its specialisations'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    assert super(Schedule, schedule).dag_name == "node_0"
    assert schedule.dag_name == "schedule_0"
    assert schedule.children[0].dag_name == "checkHaloExchange(f2)_0"
    assert schedule.children[3].dag_name == "loop_4"
    schedule.children[3].loop_type = "colour"
    assert schedule.children[3].dag_name == "loop_[colour]_4"
    schedule.children[3].loop_type = ""
    assert (schedule.children[3].loop_body[0].dag_name ==
            "kernel_testkern_code_9")
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


def test_node_dag_no_graphviz(tmpdir, monkeypatch):
    '''test that dag generation does nothing if graphviz is not
    installed. We monkeypatch sys.modules to ensure that it always
    appears that graphviz is not installed on this system. '''
    monkeypatch.setitem(sys.modules, 'graphviz', None)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    assert not os.path.exists(my_file.strpath)


# Use a regex to allow for whitespace differences between graphviz
# versions. Need a raw-string (r"") to get new-lines handled nicely.
EXPECTED2 = re.compile(
    r"digraph {\n"
    r"\s*schedule_0_start\n"
    r"\s*schedule_0_end\n"
    r"\s*loop_1_start\n"
    r"\s*loop_1_end\n"
    r"\s*loop_1_end -> loop_7_start \[color=green\]\n"
    r"\s*schedule_0_start -> loop_1_start \[color=blue\]\n"
    r"\s*schedule_5_start\n"
    r"\s*schedule_5_end\n"
    r"\s*schedule_5_end -> loop_1_end \[color=blue\]\n"
    r"\s*loop_1_start -> schedule_5_start \[color=blue\]\n"
    r"\s*kernel_testkern_qr_code_6\n"
    r"\s*kernel_testkern_qr_code_6 -> schedule_5_end \[color=blue\]\n"
    r"\s*schedule_5_start -> kernel_testkern_qr_code_6 \[color=blue\]\n"
    r"\s*loop_7_start\n"
    r"\s*loop_7_end\n"
    r"\s*loop_7_end -> schedule_0_end \[color=blue\]\n"
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
    '''test that dag generation works correctly. Skip the test if
    graphviz is not installed'''
    if not have_graphviz:
        return
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.1_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    result = my_file.read()
    assert EXPECTED2.match(result)
    my_file = tmpdir.join('test.svg')
    result = my_file.read()
    for name in ["<title>schedule_0_start</title>",
                 "<title>schedule_0_end</title>",
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


def test_find_symbol_table():
    '''Test that the find_symbol_table method in a Node instance returns
    the nearest symbol table if there is one and raises an exception if
    not.
    '''
    kernel_symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    kernel_symbol_table.add(symbol)
    ref = Reference(symbol)
    assign = Assignment.create(ref, Literal("0.0", REAL_TYPE))
    kernel_schedule = KernelSchedule.create("my_kernel", kernel_symbol_table,
                                            [assign])
    container_symbol_table = SymbolTable()
    container = Container.create("my_container", container_symbol_table,
                                 [kernel_schedule])
    assert ref.find_symbol_table() is kernel_symbol_table
    assert assign.find_symbol_table() is kernel_symbol_table
    assert kernel_schedule.find_symbol_table() is kernel_symbol_table
    assert container.find_symbol_table() is container_symbol_table

    node = Node()
    with pytest.raises(InternalError) as excinfo:
        node.find_symbol_table()
    assert ("PSyclone internal error: Symbol table not found in any "
            "ancestor nodes." in str(excinfo.value))


def test_find_or_create_symbol():
    '''Test that the find_or_create_symbol method in a Node instance
    returns the associated symbol if there is one and raises an
    exception if not. Also test for an incorrect scope argument.

    '''
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel_schedule = kernels[0].get_kernel_schedule()
    references = kernel_schedule.walk(Reference)

    # Symbol in KernelSchedule SymbolTable
    field_old = references[0]
    assert field_old.name == "field_old"
    assert isinstance(field_old.symbol, DataSymbol)
    assert field_old.symbol in kernel_schedule.symbol_table.symbols

    # Symbol in KernelSchedule SymbolTable with KernelSchedule scope
    assert isinstance(field_old.find_or_create_symbol(
        field_old.name, scope_limit=kernel_schedule), DataSymbol)
    assert field_old.symbol.name == field_old.name

    # Symbol in KernelSchedule SymbolTable with parent scope, so
    # the symbol should not be found as we limit the scope to the
    # immediate parent of the reference
    with pytest.raises(SymbolError) as excinfo:
        _ = field_old.find_or_create_symbol(field_old.name,
                                            scope_limit=field_old.parent)
    assert "No Symbol found for name 'field_old'." in str(excinfo.value)

    # Symbol in Container SymbolTable
    alpha = references[6]
    assert alpha.name == "alpha"
    assert isinstance(alpha.find_or_create_symbol(alpha.name), DataSymbol)
    container = kernel_schedule.root
    assert isinstance(container, Container)
    assert (alpha.find_or_create_symbol(alpha.name) in
            container.symbol_table.symbols)

    # Symbol in Container SymbolTable with KernelSchedule scope, so
    # the symbol should not be found as we limit the scope to the
    # kernel so do not search the container symbol table.
    with pytest.raises(SymbolError) as excinfo:
        _ = alpha.find_or_create_symbol(alpha.name,
                                        scope_limit=kernel_schedule)
    assert "No Symbol found for name 'alpha'." in str(excinfo.value)

    # Symbol in Container SymbolTable with Container scope
    assert (alpha.find_or_create_symbol(
        alpha.name, scope_limit=container).name == alpha.name)

    # find_or_create_symbol method with invalid scope type
    with pytest.raises(TypeError) as excinfo:
        _ = alpha.find_or_create_symbol(alpha.name, scope_limit="hello")
    assert ("The scope_limit argument 'hello' provided to the "
            "find_or_create_symbol method, is not of type `Node`."
            in str(excinfo.value))

    # find_or_create_symbol method with invalid scope location
    with pytest.raises(ValueError) as excinfo:
        _ = alpha.find_or_create_symbol(alpha.name, scope_limit=alpha)
    assert ("The scope_limit node 'Reference[name:'alpha']' provided to the "
            "find_or_create_symbol method, is not an ancestor of this node "
            "'Reference[name:'alpha']'." in str(excinfo.value))


def test_find_or_create_new_symbol():
    ''' Check that the Node.find_or_create_symbol() method creates new
    symbols when appropriate. '''
    from psyclone.psyir.symbols import SymbolTable, REAL_TYPE, \
        ContainerSymbol, UnresolvedInterface, ScalarType, DeferredType
    from psyclone.psyir.nodes import Assignment, Literal
    from psyclone.psyGen import KernelSchedule
    # Create some suitable PSyIR from scratch
    symbol_table = SymbolTable()
    symbol_table.add(DataSymbol("tmp", REAL_TYPE))
    kernel1 = KernelSchedule.create("mod_1", SymbolTable(), [])
    container = Container.create("container_name", symbol_table,
                                 [kernel1])
    xvar = DataSymbol("x", REAL_TYPE)
    xref = Reference(xvar)
    assign = Assignment.create(xref, Literal("1.0", REAL_TYPE))
    kernel1.addchild(assign)
    assign.parent = kernel1
    # We have no wildcard imports so there can be no symbol named 'undefined'
    with pytest.raises(SymbolError) as err:
        _ = assign.find_or_create_symbol("undefined")
    assert "No Symbol found for name 'undefined'" in str(err.value)
    # We should be able to find the 'tmp' symbol in the parent Container
    sym = assign.find_or_create_symbol("tmp")
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.REAL
    # Add a wildcard import to the SymbolTable of the KernelSchedule
    new_container = ContainerSymbol("some_mod")
    new_container.wildcard_import = True
    kernel1.symbol_table.add(new_container)
    # Symbol not in any container but we do have wildcard imports so we
    # get a new symbol back
    new_symbol = assign.find_or_create_symbol("undefined")
    assert new_symbol.name == "undefined"
    assert isinstance(new_symbol.interface, UnresolvedInterface)
    assert isinstance(new_symbol.datatype, DeferredType)
    assert "undefined" not in container.symbol_table
    assert kernel1.symbol_table.lookup("undefined") is new_symbol


def test_nemo_find_container_symbol(parser):
    ''' Check that find_or_create_symbol() works for the NEMO API when the
    searched-for symbol is declared in the parent module. '''
    from fparser.common.readfortran import FortranFileReader
    from psyclone.psyir.nodes import BinaryOperation
    from psyclone.psyir.symbols import ScalarType
    reader = FortranFileReader(
        os.path.join(
            os.path.dirname(os.path.dirname(os.path.dirname(
                os.path.abspath(__file__)))),
            "test_files", "gocean1p0", "kernel_with_global_mod.f90"))
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    # Get a node from the schedule
    bops = psy._invokes.invoke_list[0].schedule.walk(BinaryOperation)
    # Use it as the starting point for the search
    symbol = bops[0].find_or_create_symbol("alpha")
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL
