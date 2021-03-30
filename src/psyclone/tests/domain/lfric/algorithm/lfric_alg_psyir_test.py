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

from psyclone.psyir.nodes import Node
from psyclone.psyir.symbols import RoutineSymbol, TypeSymbol, \
    StructureType
from psyclone.domain.lfric.algorithm import \
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor, \
    LFRicBuiltinFunctor


def test_lfricalgorithminvokecall():
    '''Check that an instance of LFRicAlgorithmInvokeCall can be
    created.

    '''
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall(routine)
    assert call._description is None
    assert call.parent is None
    assert call.routine is routine
    assert call._text_name == "LFRicAlgorithmInvokeCall"


def test_lfricalgorithminvokecall_options():
    '''Check that an instance of LFRicAlgorithmInvokeCall can be created
    with optional arguments and that these optional arguments are
    stored as expected.

    '''
    node = Node()
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall(
        routine, description="describing an invoke", parent=node)
    assert call._description == "describing an invoke"
    assert call.parent is node


class DummySubClass(LFRicAlgorithmInvokeCall):
    '''A dummy subclass of LFRicAlgorithmInvokeCall used for testing the
    behaviour of the create method in LFRicAlgorithmInvokeCall.

    '''


@pytest.mark.parametrize("cls", [LFRicAlgorithmInvokeCall, DummySubClass])
def test_lfricalgorithminvokecall_create(cls):
    '''Check that the LFRicAlgorithmInvokeCall create method creates the
    expected object.

    '''
    routine = RoutineSymbol("hello")
    klc = LFRicKernelFunctor.create(TypeSymbol("arg", StructureType()), [])
    call = cls.create(routine, [klc], description="describing an invoke")
    assert call._description == "describing an invoke"
    assert call.routine is routine
    assert type(call) is cls
    assert len(call.children) == 1
    assert call.children[0] == klc


def test_lfricalgorithminvokecall_create_nodescription():
    '''Check that the LFRicAlgorithmInvokeCall create method sets
    description to None if it is not provided.

    '''
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall.create(routine, [])
    assert call._description is None


def test_lfricalgorithminvokecall_node_str():
    '''Check that the LFRicAlgorithmInvokeCall node_str  method creates the
    expected object.

    '''
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall.create(
        routine, [], description="describing an invoke")
    assert ("LFRicAlgorithmInvokeCall[description=\"describing an invoke\"]"
            in call.node_str(colour=False))


def test_lfricbuiltinfunctor():
    '''test that an instance of LFRicBuiltinFunctor class can be created.

    '''
    routine = TypeSymbol("hello", StructureType())
    lbc = LFRicBuiltinFunctor(routine)
    assert type(lbc) == LFRicBuiltinFunctor
    assert lbc._text_name == "LFRicBuiltinFunctor"


def test_lfrickernelfunctor():
    '''test that an instance of LFRicKernelFunctor class can be created.

    '''
    routine = TypeSymbol("hello", StructureType())
    lbc = LFRicKernelFunctor(routine)
    assert type(lbc) == LFRicKernelFunctor
    assert lbc._text_name == "LFRicKernelFunctor"
