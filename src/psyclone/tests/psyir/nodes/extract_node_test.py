# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author A. B. G. Chalk, STFC Daresbury Lab
# Modified S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs pytest tests on the ExtractNode PSyIR node. '''

from psyclone.psyir.nodes import ExtractNode, Schedule
from psyclone.psyir.symbols import SymbolTable
from psyclone.tests.utilities import get_invoke


def test_extract_node_constructor():
    '''Tests the constructor of the ExtractNode. '''
    # The constructor must have a read_write_info key
    en = ExtractNode()
    assert en.post_name == "_post"
    assert en._read_write_info is None

    en = ExtractNode(options={'read_write_info': 1})
    assert en._post_name == "_post"
    assert en._read_write_info == 1

    # Add a schedule and test that we get the same object as extract body:
    schedule = Schedule()
    en.children.append(schedule)
    assert en.extract_body is schedule


def test_extract_node_gen_code():
    '''Test the ExtractNode's gen_code function if there is no ReadWriteInfo
    object specified in the options. Since the transformations will always
    do that, we need to manually insert the ExtractNode into a schedule:

    '''
    _, invoke = get_invoke("1.0.1_single_named_invoke.f90",
                           "lfric", idx=0, dist_mem=False)
    loop = invoke.schedule.children[0]
    loop.detach()
    en = ExtractNode()
    en._var_name = "psydata"
    en.addchild(Schedule(children=[loop]))
    invoke.schedule.addchild(en)

    code = str(invoke.gen())
    expected = [
        'CALL psydata%PreStart("single_invoke_psy", '
        '"invoke_important_invoke-testkern_code-r0", 17, 2)',
        'CALL psydata%PreDeclareVariable("a", a)',
        'CALL psydata%PreDeclareVariable("f1_data", f1_data)',
        'CALL psydata%PreDeclareVariable("f2_data", f2_data)',
        'CALL psydata%PreDeclareVariable("loop0_start", loop0_start)',
        'CALL psydata%PreDeclareVariable("loop0_stop", loop0_stop)',
        'CALL psydata%PreDeclareVariable("m1_data", m1_data)',
        'CALL psydata%PreDeclareVariable("m2_data", m2_data)',
        'CALL psydata%PreDeclareVariable("map_w1", map_w1)',
        'CALL psydata%PreDeclareVariable("map_w2", map_w2)',
        'CALL psydata%PreDeclareVariable("map_w3", map_w3)',
        'CALL psydata%PreDeclareVariable("ndf_w1", ndf_w1)',
        'CALL psydata%PreDeclareVariable("ndf_w2", ndf_w2)',
        'CALL psydata%PreDeclareVariable("ndf_w3", ndf_w3)',
        'CALL psydata%PreDeclareVariable("nlayers", nlayers)',
        'CALL psydata%PreDeclareVariable("undf_w1", undf_w1)',
        'CALL psydata%PreDeclareVariable("undf_w2", undf_w2)',
        'CALL psydata%PreDeclareVariable("undf_w3", undf_w3)',
        'CALL psydata%PreDeclareVariable("cell_post", cell)',
        'CALL psydata%PreDeclareVariable("f1_data_post", f1_data)']
    for line in expected:
        assert line in code


def test_extract_node_equality():
    ''' Test the __eq__ method of ExtractNode. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    symboltable2 = SymbolTable()
    # Make sure they have the same ST instance, providing them as constructor
    # parameters would create a copy and not use the same instance.
    sched1 = Schedule()
    sched1._symbol_table = symboltable
    sched2 = Schedule()
    sched2._symbol_table = symboltable
    sched3 = Schedule(symbol_table=symboltable2)
    node1 = ExtractNode(children=[sched1])
    node2 = ExtractNode(children=[sched2])
    node3 = ExtractNode(children=[sched3])

    assert node1 == node2
    assert node1 != node3

    node1._post_name = "testa"
    node2._post_name = "testb"
    assert node1 != node2
