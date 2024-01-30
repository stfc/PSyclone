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
# -----------------------------------------------------------------------------

''' Performs pytest tests on the ExtractNode PSyIR node. '''

from psyclone.psyir.nodes import ExtractNode, Schedule
from psyclone.psyir.symbols import SymbolTable


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
