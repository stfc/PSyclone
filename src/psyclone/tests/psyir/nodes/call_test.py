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
from psyclone.psyir.nodes import Call, Reference, Array, Schedule
from psyclone.psyir.symbols import ArrayType, INTEGER_TYPE, DataSymbol, \
    RoutineSymbol


def test_call_init():
    '''Test that a Call can be created as expected'''

    # name and arguments
    array_type = ArrayType(INTEGER_TYPE, shape=[10,20])
    arg_list = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                Array(DataSymbol("arg2", array_type))]
    routine = RoutineSymbol("jo")
    call = Call(routine, arg_list)
    assert call._routine.name == "jo"
    assert call.routine.name is call._routine.name
    assert isinstance(call._arguments, list)
    assert len(call._arguments) == 2
    assert call._arguments[0].name == "arg1"
    assert call._arguments[1].name == "arg2"
    assert call.arguments is call._arguments
    assert call.parent is None

    # Change original list and make sure that the list in Call
    # instance does not change, i.e. make sure it copies the list.
    arg_list.append(Array(DataSymbol("arg2", array_type)))
    assert len(arg_list) == 3
    assert len(call._arguments) == 2
    
    # optional parent
    call = Call(routine, [], parent=Schedule())
    assert call._routine.name == "jo"
    assert isinstance(call.parent, Schedule)


def test_call_node_str():
    ''' Test that the node_str method behaves as expected '''
    routine = RoutineSymbol("ellie")
    call = Call(routine, [])
    assert "Call[name='ellie', args=[]]" in call.node_str()
    array_type = ArrayType(INTEGER_TYPE, shape=[10,20])
    arg_list = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                Array(DataSymbol("arg2", array_type))]
    call = Call(routine, arg_list)
    assert ("Call[name='ellie', args=['arg1', 'arg2']]"
            in call.node_str())


def test_call_error1():
    '''Test that the appropriate exception is raised if the name argument
    is not a string'''
    with pytest.raises(TypeError) as info:
        _ = Call(None, [])
    assert ("Call routine argument should be a RoutineSymbol but found "
            "'NoneType'." in str(info.value))


def test_call_error2():
    '''Test that the appropriate exception is raised if the arguments
    argument is not a list'''
    routine = RoutineSymbol("isaac")
    with pytest.raises(TypeError) as info:
        _ = Call(routine, None)
    assert ("Call arguments argument should be a list but found 'NoneType'."
            in str(info.value))


def test_call_error3():
    '''Test that the appropriate exception is raised if one or more of the
    arguments argument list entries is not a Reference.

    '''
    routine = RoutineSymbol("roo")
    with pytest.raises(TypeError) as info:
        _ = Call(routine, [Reference(DataSymbol("arg1",INTEGER_TYPE)), None])
    assert ("Call arguments argument list entries should all be references "
            "but at least one is not." in str(info.value))
