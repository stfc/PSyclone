# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

''' This module contains pytest tests for the LFRicInvokeSchedule class. '''

import os
from psyclone.domain.lfric import LFRicSymbolTable, LFRicInvokeSchedule
from psyclone.parse.algorithm import parse
from psyclone.psyir.nodes import Container, colored
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.psyGen import PSyFactory


BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
TEST_API = "lfric"


def test_lfricinvsched_parent():
    ''' Check the setting of the parent of a LFRicInvokeSchedule. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api=TEST_API)
    kcalls = invoke_info.calls[0].kcalls
    symbol = RoutineSymbol("my_sched")
    # With no parent specified
    dsched = LFRicInvokeSchedule(symbol, kcalls)
    assert dsched.parent is None
    # With a parent
    fake_parent = Container("my_mod", symbol_table=LFRicSymbolTable())
    dsched2 = LFRicInvokeSchedule(symbol, kcalls, parent=fake_parent)
    assert dsched2.parent is fake_parent


def test_lfricinvsched_node_str_coloured():
    '''
    Check the node_str method of the LFRicInvokeSchedule class. We need an
    Invoke object for this which we get using the LFRic API.

    This test checks that `dm` is printed equal to `True` when dist_mem is
    `True` in the config and that `InvokeSchedule` is coloured when requested
    by the `node_str` method.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="lfric")

    # distributed_memory set to True
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)
    # Create a plain LFRicInvokeSchedule
    sched = LFRicInvokeSchedule.create('name')
    # Manually supply it with an Invoke object created with the LFRic API.
    sched.invoke = psy.invokes.invoke_list[0]
    output = sched.node_str()

    assert colored("InvokeSchedule", LFRicInvokeSchedule._colour) in output
    assert str("[invoke='" + sched.invoke.name + "', dm=True]") in output


def test_lfricinvsched_node_str_colourless():
    '''
    Check the node_str method of the LFRicInvokeSchedule class. We need an
    Invoke object for this which we get using the LFRic API.

    This test checks that `dm` is printed equal to `False` when dist_mem is
    `False` in the config and that `InvokeSchedule` is uncoloured when
    requested by the `node_str` method.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=False).create(invoke_info)
    # Create a plain LFRicInvokeSchedule
    sched = LFRicInvokeSchedule.create('name')
    # Manually supply it with an Invoke object created with the LFRic API.
    sched.invoke = psy.invokes.invoke_list[0]

    # colour set to False
    output = sched.node_str(colour=False)

    # expected output
    expected = str("InvokeSchedule[invoke='" + sched.invoke.name +
                   "', dm=False]")

    assert expected in output
