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
from psyclone.psyir.symbols import ArrayType, INTEGER_TYPE, DataSymbol


def test_call_init():
    '''Test that a Call can be created as expected'''

    # name and arguments
    array_type = ArrayType(INTEGER_TYPE, shape=[10,20])
    arg_list = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                Array(DataSymbol("arg2", array_type))]
    call = Call("jo", arg_list)
    assert call._name == "jo"
    assert call.name is call._name
    assert isinstance(call._arguments, list)
    assert len(call._arguments) == 2
    assert call._arguments[0].name == "arg1"
    assert call._arguments[1].name == "arg2"
    assert call.arguments is call._arguments
    assert isinstance(call._returns, list)
    assert not call._returns
    assert call.returns is call._returns
    assert call._calls is None
    assert call.calls is None
    assert call.parent is None

    # returns
    call = Call("ellie", [], arg_list)
    assert call._name == "ellie"
    assert isinstance(call._arguments, list)
    assert not call._arguments
    assert isinstance(call._returns, list)    
    assert call._returns[0].name == "arg1"
    assert call._returns[1].name == "arg2"
    assert call.returns is call._returns

    # calls
    call = Call("isaac", [], calls=Schedule())
    assert call._name == "isaac"
    assert isinstance(call._calls, Schedule)
    assert call.calls is call._calls

    # parent
    call = Call("roo", [], parent=Schedule())
    assert call._name == "roo"
    assert isinstance(call.parent, Schedule)

# TODO Test that lists are separate copies

def test_call_node_str():
    ''' Test that the node_str method behaves as expected '''
    call = Call("test", [])
    assert ("Call[name='test', args=[], returns=[], calls=NoneType]"
            in call.node_str())
    array_type = ArrayType(INTEGER_TYPE, shape=[10,20])
    arg_list = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                Array(DataSymbol("arg2", array_type))]
    call = Call("test", arg_list, returns=arg_list, calls=Schedule())
    assert ("Call[name='test', args=['arg1', 'arg2'], returns=['arg1', "
            "'arg2'], calls=Schedule]" in call.node_str())


def test_call_error1():
    '''Test that the appropriate exception is raised if the name argument
    is not a string'''
    with pytest.raises(TypeError) as info:
        _ = Call(None, [])
    assert ("Call name argument should be a string but found 'NoneType'."
            in str(info.value))


def test_call_error2():
    '''Test that the appropriate exception is raised if an empty name is
    provided '''
    with pytest.raises(TypeError) as info:    
        _ = Call("", [])
    assert "Call name argument must not be empty." in str(info.value)


def test_call_error3():
    '''Test that the appropriate exception is raised if the arguments
    argument is not a list'''
    with pytest.raises(TypeError) as info:
        _ = Call("roo", None)
    assert ("Call arguments argument should be a list but found 'NoneType'."
            in str(info.value))


def test_call_error4():
    '''Test that the appropriate exception is raised if one or more of the
    arguments argument list entries is not a Reference.

    '''
    with pytest.raises(TypeError) as info:
        _ = Call("roo", [Reference(DataSymbol("arg1",INTEGER_TYPE)), None])
    assert ("Call arguments argument list entries should all be references "
            "but at least one is not." in str(info.value))



def test_call_error5():
    '''Test that the appropriate exception is raised if the returns
    argument is not a list'''
    with pytest.raises(TypeError) as info:
        _ = Call("roo", [], returns=None)
    assert ("Call returns argument should be a list but found 'NoneType'."
            in str(info.value))


def test_call_error6():
    '''Test that the appropriate exception is raised if one or more of the
    returns argument list entries is not a Reference.

    '''
    with pytest.raises(TypeError) as info:
        _ = Call("roo", [],
                 returns=[Reference(DataSymbol("arg1",INTEGER_TYPE)), None])
    assert ("Call returns argument list entries should all be references "
            "but at least one is not." in str(info.value))

# TBD calls argument errors
