# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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

''' Module containing tests for generating PSyData hooks'''

from __future__ import absolute_import

import re
import pytest

from psyclone.psyir.nodes import Node, PSyDataNode, Schedule
from psyclone.psyir.transformations import PSyDataTrans
from psyclone.psyGen import Loop, NameSpace
from psyclone.tests.utilities import get_invoke


@pytest.fixture(scope="function", autouse=True)
def clear_psydata_namespace():
    '''This function is called before any test function. It
    creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order
    or number of tests run is changed, otherwise the created region
    names will change.'''
    PSyDataNode._namespace = NameSpace()


# -----------------------------------------------------------------------------
def test_psy_data_basic(capsys):
    # pylint: disable=too-many-locals
    '''Check basic functionality: node names, schedule view.
    '''
    from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    assert "Insert a PSyData node" in str(data_trans)
    assert data_trans.name == "PSyDataTrans"
    data_trans.apply(schedule)

    assert isinstance(invoke.schedule[0], PSyDataNode)

    schedule.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])
    sched = colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])
    loop = Loop().coloured_name(True)
    data = invoke.schedule[0].coloured_name(True)

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    expected = (
        gsched + "[invoke='invoke_0', Constant loop bounds=True]\n"
        "    0: " + data + "[]\n"
        "        " + sched + "[]\n"
        "            0: " + loop + "[type='outer', field_space='go_cv', "
        "it_space='go_internal_pts']\n")
    assert expected in out

    # Insert a DataTrans call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    new_sched, _ = data_trans.apply(invoke.schedule[0].psy_data_body[0]
                                    .loop_body[0])

    new_sched_str = str(new_sched)
    correct = ("""GOInvokeSchedule[invoke='invoke_0', \
Constant loop bounds=True]:
PSyDataStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', DataType.INTEGER]
Literal[value:'jstop-1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
PSyDataStart[var=psy_data_1]
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', DataType.INTEGER]
Literal[value:'istop', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
kern call: compute_cv_code
End Schedule
End GOLoop
PSyDataEnd[var=psy_data_1]
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', DataType.INTEGER]
Literal[value:'jstop+1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', DataType.INTEGER]
Literal[value:'istop+1', DataType.INTEGER]
Literal[value:'1', DataType.INTEGER]
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
PSyDataEnd[var=psy_data]
End Schedule""")

    assert correct in new_sched_str


# -----------------------------------------------------------------------------
def test_tree_correct():
    '''Test that adding children and parents will result in the correct
    relationship with the inserted node.
    '''

    # 1. No parent and no children:
    # =============================
    psy_node = PSyDataNode()

    # We must have a single profile node with a schedule which has
    # no children:
    assert psy_node.parent is None
    assert len(psy_node.children) == 1   # This is the Schedule
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent == psy_node
    assert not psy_node.psy_data_body.children

    # 2. Parent, but no children:
    # ===========================
    parent = Node()
    psy_node = PSyDataNode(parent=parent)

    # We must have a single node connected to the parent, and an
    # empty schedule for the ExtractNode:
    assert psy_node.parent == parent
    assert parent.children[0] == psy_node
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent is psy_node
    assert not psy_node.psy_data_body.children

    # 3. No parent, but children:
    # ===========================
    children = [Node(), Node()]
    psy_node = PSyDataNode(children=children)

    # The children must be connected to the schedule, which is
    # connected to the ExtractNode:
    assert psy_node.parent is None
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    assert psy_node.psy_data_body.parent is psy_node
    assert len(psy_node.psy_data_body.children) == 2
    assert psy_node.psy_data_body.children[0] is children[0]
    assert psy_node.psy_data_body.children[1] is children[1]
    assert children[0].parent == psy_node.psy_data_body
    assert children[1].parent == psy_node.psy_data_body

    # 4. Parent and children:
    # =======================
    parent = Node()
    # The children must be added to the parent before creating the ExtractNode
    parent.addchild(Node())
    parent.addchild(Node())
    # Add another child that must stay with the parent node
    third_child = Node(parent=parent)
    parent.addchild(third_child)
    assert parent.children[2] is third_child
    # Only move the first two children, leave the third where it is
    children = [parent.children[0], parent.children[1]]
    psy_node = PSyDataNode(parent=parent, children=children)

    # Check all connections
    assert psy_node.parent is parent
    assert parent.children[0] is psy_node
    assert len(psy_node.children) == 1
    assert isinstance(psy_node.psy_data_body, Schedule)
    schedule = psy_node.psy_data_body
    assert schedule.parent is psy_node
    assert len(schedule.children) == 2
    for i in range(2):
        assert schedule.children[i] is children[i]
        assert children[i].parent is schedule
    # The third child of the original parent is now the
    # second child, next to the inserted ExtractNode
    assert parent.children[1] is third_child
    assert third_child.parent is parent


# -----------------------------------------------------------------------------
def test_c_code_creation():
    '''Tests the handling when trying to create C code, which is not supported
    at this stage.
    '''

    data_node = PSyDataNode()
    with pytest.raises(NotImplementedError) as excinfo:
        data_node.gen_c_code()
    assert "Generation of C code is not supported for PSyDataNode" \
        in str(excinfo)


# -----------------------------------------------------------------------------
def test_psy_data_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0])

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")
    # First a simple test that the nesting is correct - the
    # PSyData regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    # Since this is only PSyData, which by default does not supply
    # variable information, the parameters to PreStart are both 0.
    correct_re = ("subroutine invoke.*"
                  "use psy_data_mod, only: PSyDataType.*"
                  r"TYPE\(PSyDataType\), save :: psy_data.*"
                  r"call psy_data%PreStart\(\"psy_single_invoke_different"
                  r"_iterates_over\", \"invoke_0:compute_cv_code:r0\","
                  r" 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call psy_data%PostEnd")

    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same PSyDataNode
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again


# -----------------------------------------------------------------------------
def test_psy_data_options():
    '''Check that the options for PSyData work as expected.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule
    data_trans = PSyDataTrans()

    data_trans.apply(schedule[0].loop_body)
    data_node = schedule[0].loop_body[0]
    assert isinstance(data_node, PSyDataNode)

    from psyclone.f2pygen import ModuleGen
    # 1) Test that the listed variables will appear in the list
    # ---------------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={"pre_var_list": ["a"],
                                     "post_var_list": ["b"]})

    out = "\n".join([str(i.root) for i in mod.children])
    expected = ['CALL psy_data%PreDeclareVariable("a", a)',
                'CALL psy_data%PreDeclareVariable("b", b)',
                'CALL psy_data%ProvideVariable("a", a)',
                'CALL psy_data%PostStart',
                'CALL psy_data%ProvideVariable("b", b)']
    for line in expected:
        assert line in out

    # 2) Test that variables suffixes are added as expected
    # -----------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={"pre_var_list": ["a"],
                                     "post_var_list": ["b"],
                                     "pre_var_postfix": "_pre",
                                     "post_var_postfix": "_post"})

    out = "\n".join([str(i.root) for i in mod.children])
    expected = ['CALL psy_data%PreDeclareVariable("a_pre", a)',
                'CALL psy_data%PreDeclareVariable("b_post", b)',
                'CALL psy_data%ProvideVariable("a_pre", a)',
                'CALL psy_data%PostStart',
                'CALL psy_data%ProvideVariable("b_post", b)']
    for line in expected:
        assert line in out

    # 3) Check that we don't get any declaration if there are no variables:
    # ---------------------------------------------------------------------
    mod = ModuleGen(None, "test")
    data_node.gen_code(mod, options={})

    out = "\n".join([str(i.root) for i in mod.children])
    # Only PreStart and PostEnd should appear
    assert "PreStart" in out
    assert "PreDeclareVariable" not in out
    assert "ProvideVariable" not in out
    assert "PreEnd" not in out
    assert "PostStart" not in out
    assert "PostEnd" in out
