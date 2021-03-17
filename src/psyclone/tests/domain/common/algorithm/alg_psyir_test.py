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

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
from __future__ import absolute_import
import pytest

from psyclone.psyir.nodes import Reference, Node
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import RoutineSymbol, TypeSymbol, \
    StructureType, Symbol
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelLayerRef
from psyclone.errors import GenerationError


def test_algorithminvokecall():
    '''Check that an instance of AlgorithmInvokeCall can be
    created.

    '''
    routine = RoutineSymbol("hello")
    call = AlgorithmInvokeCall(routine)
    assert call._text_name == "AlgorithmInvokeCall"
    assert call._colour == "green"


def test_algorithminvokecall_invalid_arg():
    '''Check that the create method of an AlgorithmInvokeCall raises the
    expected exception if its supplied children are not the expected
    type (tests _validate_child method and _children_valid_format
    variable).

    '''
    routine = RoutineSymbol("symbol")
    with pytest.raises(GenerationError) as info:
        _ = AlgorithmInvokeCall.create(routine, ["hello"])
    assert("Item 'str' can't be child 0 of 'AlgorithmInvokeCall'. The valid "
           "format is: '[KernelLayerRef]*'." in str(info.value))


def test_kernellayerref():
    '''Check that an instance of KernelLayerRef class can be created. Also
    check that the symbol method works as expected.

    '''
    symbol = TypeSymbol("hello", StructureType())
    klr = KernelLayerRef(symbol)
    assert klr._symbol == symbol
    assert klr.symbol == symbol
    assert klr._colour == "yellow"
    assert klr._text_name == "KernelLayerRef"
    assert klr.parent is None


def test_kernellayerref_parent():
    '''Check that the optional parent argument to a KernelLayerRef class
    constructor is stored correctly.

    '''
    parent = Node()
    symbol = TypeSymbol("hello", StructureType())
    klr = KernelLayerRef(symbol, parent=parent)
    assert klr.parent == parent


def test_kernellayerref_invalid_symbol():
    '''Check KernelLayerRef raises the expected exception if the type of
    the symbol argument is invalid.

    '''
    with pytest.raises(TypeError) as info:
        _ = KernelLayerRef(Symbol("hello"))
    assert ("KernelLayerRef symbol argument should be a TypeSymbol but "
            "found 'Symbol'." in str(info.value))


class SubClass(KernelLayerRef):
    '''Utility subclass of KernelLayerRef to test that the create method
    in KernelLayerRef behaves as expected for subclasses.'''


@pytest.mark.parametrize("cls", [KernelLayerRef, SubClass])
def test_kernellayerref_create(cls):
    '''Check that the create method of KernelLayerRef works as expected.

    '''
    symbol = TypeSymbol("hello", StructureType())
    klr = cls.create(symbol, [])
    assert type(klr) is cls
    assert klr._symbol == symbol
    assert len(klr.children) == 0

    arg = Reference(Symbol("dummy"))
    klr = KernelLayerRef.create(symbol, [arg])
    assert len(klr.children) == 1
    assert klr.children[0] == arg
    assert arg.parent == klr


def test_kernellayerref_create_invalid_symbol():
    '''Check that the create method of KernelLayerRef raises the expected
    exception if the provided symbol argument is not the correct type.

    '''
    symbol = Symbol("hello")
    with pytest.raises(GenerationError) as info:
        _ = KernelLayerRef.create(symbol, [])
    assert ("Call create symbol argument should be a TypeSymbol but found "
            "'Symbol'." in str(info.value))


def test_kernellayerref_create_invalid_args1():
    '''Check that the create method of KernelLayerRef raises the expected
    exception if the provided 'arguments' argument is not a list.

    '''
    symbol = TypeSymbol("hello", StructureType())
    with pytest.raises(GenerationError) as info:
        _ = KernelLayerRef.create(symbol, "Not a list")
    assert ("Call create arguments argument should be a list but found "
            "'str'." in str(info.value))


def test_kernellayerref_invalid_args2():
    '''Check that the create method of KernelLayerRef raises the expected
    exception if its supplied list of children are not the expected
    type (tests _validate_child method and _children_valid_format
    variable)

    '''
    symbol = TypeSymbol("hello", StructureType())
    with pytest.raises(GenerationError) as info:
        _ = KernelLayerRef.create(symbol, ["hello"])
    assert("Item 'str' can't be child 0 of 'KernelLayerRef'. The valid "
           "format is: '[DataNode]*'." in str(info.value))


def test_kernellayerref_node_str():
    '''Check the node_str method of the KernelLayerRef class.'''

    symbol = TypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelLayerRef.create(symbol, [arg])
    coloredtext = colored("KernelLayerRef", KernelLayerRef._colour)
    assert klr.node_str() == coloredtext+"[name='hello']"


def test_kernellayerref_str():
    '''Check the str method of the KernelLayerRef class.'''

    symbol = TypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelLayerRef.create(symbol, [arg])
    assert klr.__str__() == "KernelLayerRef[name='hello']"
