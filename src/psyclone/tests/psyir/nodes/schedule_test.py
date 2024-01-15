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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Schedule PSyIR node. '''

from __future__ import absolute_import
import os
import pytest
from psyclone.psyir.nodes import Schedule, Assignment, Range, Statement
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import SymbolTable
from psyclone.psyGen import PSyFactory
from psyclone.parse.algorithm import parse
from psyclone.errors import GenerationError


BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


def test_sched_init():
    ''' Check the Schedule class is initialised as expected.'''

    # By default Schedule sets parent to None, children to an empty list and
    # initialises its own symbol table.
    sched = Schedule()
    assert isinstance(sched, Schedule)
    assert not sched.parent
    assert not sched.children
    assert isinstance(sched.symbol_table, SymbolTable)

    # A custom symbol table and parent and children nodes can be given as
    # arguments of Schedule.
    symtab = SymbolTable()
    sched2 = Schedule(parent=sched, children=[Statement(), Statement()],
                      symbol_table=symtab)
    assert isinstance(sched2, Schedule)
    assert sched2.parent is sched
    assert len(sched2.children) == 2
    assert sched2.symbol_table is symtab


def test_sched_node_str():
    ''' Check the node_str method of the Schedule class'''
    sched = Schedule()
    assert colored("Schedule", Schedule._colour) in sched.node_str()


def test_sched_getitem():
    '''Test that Schedule has the [int] operator overloaded to return the
    given index child'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    sched = psy.invokes.invoke_list[0].schedule
    for indx in range(len(sched._children)):
        assert sched[indx] is sched._children[indx]

    # Test range indexing
    children = sched[:]
    assert len(children) == 2
    assert children[0] is sched._children[0]
    assert children[1] is sched._children[1]

    # Test index out-of-bounds Error
    with pytest.raises(IndexError) as err:
        _ = sched[len(sched._children)]
    assert "list index out of range" in str(err.value)


def test_sched_can_be_printed():
    ''' Check the schedule class can always be printed'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = Schedule
    output = str(psy.invokes.invoke_list[0].schedule)

    assert "Schedule:\n" in output


def test_sched_children_validation():
    '''Test that children added to Schedule are validated. Schedule accepts
    Statements as children.

    '''
    schedule = Schedule()
    statement = Assignment()
    nonstatement = Range()

    # Invalid child
    with pytest.raises(GenerationError) as excinfo:
        schedule.addchild(nonstatement)
    assert ("Item 'Range' can't be child 0 of 'Schedule'. The valid"
            " format is: '[Statement]*'." in str(excinfo.value))

    # Valid children
    schedule.addchild(statement)
