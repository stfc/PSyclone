# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the ProfileNode. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import (Node, ProfileNode, Literal, Assignment,
                                  Reference, Return, KernelSchedule, Loop,
                                  CodeBlock)
from psyclone.psyir.symbols import SymbolTable, DataSymbol, REAL_TYPE
from psyclone.profiler import Profiler
from psyclone.errors import InternalError


def test_malformed_profile_node(monkeypatch):
    ''' Check that we raise the expected error if a ProfileNode does not have
    a single Schedule node as its child. '''
    pnode = ProfileNode()
    monkeypatch.setattr(pnode, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = pnode.profile_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(pnode, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = pnode.profile_body
    assert "malformed or incomplete. It should have a " in str(err.value)


@pytest.mark.parametrize("value", [["a", "b"], ("a"), ("a", "b", "c"),
                                   ("a", []), ([], "a")])
def test_profile_node_invalid_name(value):
    '''Test that the expected exception is raised when an invalid profile
    name is provided to a ProfileNode.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = ProfileNode(options={"region_name": value})
    assert ("Error in PSyDataNode. The name must be a tuple containing "
            "two non-empty strings." in str(excinfo.value))


def test_lower_to_lang_level(parser):
    ''' Test the lower_to_language_level() method. '''
    Profiler.set_options([Profiler.INVOKES])
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE)
    zero = Literal("0.0", REAL_TYPE)
    assign1 = Assignment.create(Reference(arg1), zero)
    assign2 = Assignment.create(Reference(arg1), zero.copy())

    kschedule = KernelSchedule.create(
        "work1", symbol_table, [assign1, assign2, Return()])
    Profiler.add_profile_nodes(kschedule, Loop)
    kschedule.view()
    assert isinstance(kschedule.children[0], ProfileNode)
    kschedule.lower_to_language_level()
    assert isinstance(kschedule.children[0], CodeBlock)
    kschedule.view()
    assert 0
