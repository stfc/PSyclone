# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: L. Turner and O. Brunt, Met Office

''' This module contains pytest tests for the LFRicLoopBounds collection
class. '''

import os

from psyclone.domain.lfric import LFRicLoopBounds
from psyclone.f2pygen import SubroutineGen, ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import symbols


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_lbounds_construction():
    ''' Check that we can create an LFRicLoopBounds object. '''
    # We need a valid LFRicInvoke node and the easiest way to get one of
    # those is to create one from code.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    lbounds = LFRicLoopBounds(invoke)
    assert isinstance(lbounds, LFRicLoopBounds)


def test_lbounds_initialise(monkeypatch):
    ''' Test the initialise method of LFRicLoopBounds. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    mod = ModuleGen()
    fake_parent = SubroutineGen(mod)
    lbounds = LFRicLoopBounds(invoke)

    table = invoke.schedule.symbol_table
    assert "loop0_start" not in table
    assert "loop0_stop" not in table

    lbounds.initialise(fake_parent)

    # Check that new symbols have been added.
    start_sym = table.lookup("loop0_start")
    assert start_sym.datatype.intrinsic == symbols.ScalarType.Intrinsic.INTEGER
    stop_sym = table.lookup("loop0_stop")
    assert stop_sym.datatype.intrinsic == symbols.ScalarType.Intrinsic.INTEGER

    assert "Set-up all of the loop bounds" in str(fake_parent.children[1].root)
    # Monkeypatch the schedule so that it appears to have no loops.
    monkeypatch.setattr(invoke.schedule, "loops", lambda: [])
    lbounds = LFRicLoopBounds(invoke)
    fake_parent = SubroutineGen(mod)
    # The initialise() should not raise an error but nothing should be
    # added to the f2pygen tree.
    lbounds.initialise(fake_parent)
    assert fake_parent.children == []
    # Symbols representing loop bounds should be unaffected.
    assert table.lookup("loop0_start") is start_sym
    assert table.lookup("loop0_stop") is stop_sym
