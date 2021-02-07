# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Call PSyIR node. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Call, Reference, ArrayReference, Schedule
from psyclone.psyir.nodes.node import colored, SCHEDULE_COLOUR_MAP
from psyclone.psyir.symbols import ArrayType, INTEGER_TYPE, DataSymbol, \
    RoutineSymbol
from psyclone.errors import GenerationError


class SpecialCall(Call):
    '''Test Class specialising the Call class'''


def test_call_init():
    '''Test that a Call can be created as expected. Also test the routine
    property.

    '''
    # routine argument
    routine = RoutineSymbol("jo")
    call = Call(routine)
    assert call._routine is routine
    assert call.routine is call._routine
    assert call.parent is None
    assert call.children == []

    # optional parent argument
    parent = Schedule()
    call = Call(routine, parent=parent)
    assert call.routine is routine
    assert call.parent is parent
    assert call.children == []


def test_call_init_error():
    '''Test that the appropriate exception is raised if the routine
    argument is not a RoutineSymbol.

    '''
    with pytest.raises(TypeError) as info:
        _ = Call(None)
    assert ("Call routine argument should be a RoutineSymbol but found "
            "'NoneType'." in str(info.value))


@pytest.mark.parametrize("cls", [Call, SpecialCall])
def test_call_create(cls):
    '''Test that the create method creates a valid call with arguments'''

    routine = RoutineSymbol("ellie")
    array_type = ArrayType(INTEGER_TYPE, shape=[10, 20])
    arguments = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                 ArrayReference(DataSymbol("arg2", array_type))]
    call = cls.create(routine, arguments)
    # pylint: disable=unidiomatic-typecheck
    assert type(call) is cls
    assert call.routine is routine
    for idx, child, in enumerate(call.children):
        assert child is arguments[idx]
        assert child.parent is call


def test_call_create_error1():
    '''Test that the appropriate exception is raised if the routine
    argument to the create method is not a RoutineSymbol.

    '''
    with pytest.raises(GenerationError) as info:
        _ = Call.create(None, [])
    assert ("Call create routine argument should be a RoutineSymbol but "
            "found 'NoneType'." in str(info.value))


def test_call_error2():
    '''Test that the appropriate exception is raised if the arguments
    argument to the create method is not a list'''
    routine = RoutineSymbol("isaac")
    with pytest.raises(GenerationError) as info:
        _ = Call.create(routine, None)
    assert ("Call create arguments argument should be a list but found "
            "'NoneType'." in str(info.value))


def test_call_error3():
    '''Test that the appropriate exception is raised if one or more of the
    arguments argument list entries to the create method is not a
    DataNode.

    '''
    routine = RoutineSymbol("roo")
    with pytest.raises(GenerationError) as info:
        _ = Call.create(
            routine, [Reference(DataSymbol("arg1", INTEGER_TYPE)), None])
    assert ("Item 'NoneType' can't be child 1 of 'Call'. The valid format "
            "is: '[DataNode]*'." in str(info.value))


def test_call_node_str():
    ''' Test that the node_str method behaves as expected '''
    routine = RoutineSymbol("isaac")
    call = Call(routine)
    colouredtext = colored("Call", SCHEDULE_COLOUR_MAP["Call"])
    assert call.node_str() == colouredtext+"[name='isaac']"


def test_call_str():
    ''' Test that the str method behaves as expected '''
    routine = RoutineSymbol("roo")
    call = Call(routine)
    assert str(call) == "Call[name='roo']"
