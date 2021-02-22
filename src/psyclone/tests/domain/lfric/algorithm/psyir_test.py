# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the translation of PSyIR to LFRic
Algorithm PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Reference, Literal, Node
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.domain.lfric.algorithm import \
    psyir_to_algpsyir, LfricAlgorithmInvokeCall, LfricCodedCall, \
    LfricBuiltinCall
from psyclone.errors import GenerationError, InternalError


def test_lfricalgorithminvokecall():
    '''Check that an instance of LfricAlgorithmInvokeCall can be
    created.

    '''
    routine = RoutineSymbol("hello")
    call = LfricAlgorithmInvokeCall(routine)
    assert call._description is None
    assert call.parent is None
    assert call.routine is routine
    assert call._text_name == "LfricAlgorithmInvokeCall"


def test_lfricalgorithminvokecall_options():
    '''Check that an instance of LfricAlgorithmInvokeCall can be created
    with optional arguments and that these optional arguments are
    stored as expected.

    '''
    node = Node()
    routine = RoutineSymbol("hello")
    call = LfricAlgorithmInvokeCall(
        routine, description="describing an invoke", parent=node)
    assert call._description == "describing an invoke"
    assert call.parent is node


class DummySubClass(LfricAlgorithmInvokeCall):
    '''A dummy subclass of LfricAlgorithmInvokeCall used for testing the
    behaviour of the create method in LfricAlgorithmInvokeCall.

    '''


@pytest.mark.parametrize("cls", [LfricAlgorithmInvokeCall, DummySubClass])
def test_lfricalgorithminvokecall_create(cls):
    '''Check that the LfricAlgorithmInvokeCall create method creates the
    expected object.

    '''
    routine = RoutineSymbol("hello")
    klc = LfricCodedCall.create(RoutineSymbol("arg"), [])
    call = cls.create(routine, [klc], description="describing an invoke")
    assert call._description == "describing an invoke"
    assert call.routine is routine
    assert type(call) is cls
    assert len(call.children) == 1
    assert call.children[0] == klc


def test_lfricalgorithminvokecall_create_nodescription():
    '''Check that the LfricAlgorithmInvokeCall create method sets
    description to None if it is not provided.

    '''
    routine = RoutineSymbol("hello")
    call = LfricAlgorithmInvokeCall.create(routine, [])
    assert call._description is None


def test_lfricalgorithminvokecall_node_str():
    '''Check that the LfricAlgorithmInvokeCall node_str  method creates the
    expected object.

    '''
    routine = RoutineSymbol("hello")
    call = LfricAlgorithmInvokeCall.create(
        routine, [], description="describing an invoke")
    assert ("LfricAlgorithmInvokeCall[description=\"describing an invoke\"]"
            in call.node_str(colour=False))


def test_lfricbuiltincall():
    '''test that an instance of LfricBuiltinCall class can be created.

    '''
    routine = RoutineSymbol("hello")
    lbc = LfricBuiltinCall(routine)
    assert type(lbc) == LfricBuiltinCall
    assert lbc._text_name == "LfricBuiltinCall"


def test_lfricCodedcall():
    '''test that an instance of LfricCodedCall class can be created.

    '''
    routine = RoutineSymbol("hello")
    lbc = LfricCodedCall(routine)
    assert type(lbc) == LfricCodedCall
    assert lbc._text_name == "LfricCodedCall"
