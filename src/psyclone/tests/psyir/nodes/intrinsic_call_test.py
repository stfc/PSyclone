# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

'''This module contains pytest tests for the IntrinsicCall node.'''

import pytest

from psyclone.psyir.nodes import (
    ArrayReference, Literal, IntrinsicCall, Reference)
from psyclone.psyir.symbols import (ArrayType, DataSymbol, INTEGER_TYPE,
                                    IntrinsicSymbol, REAL_TYPE)


def test_intrinsiccall_alloc_create():
    '''
    Tests the create() method supports various forms of 'allocate'.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    bsym = DataSymbol("my_array2", ArrayType(INTEGER_TYPE,
                                             [ArrayType.Extent.DEFERRED]))
    # Straightforward allocation of an array.
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])])
    assert isinstance(alloc, IntrinsicCall)
    assert isinstance(alloc.routine, IntrinsicSymbol)
    assert alloc.routine.name == "ALLOCATE"
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(sym), ("Mold", Reference(bsym))])
    assert isinstance(alloc, IntrinsicCall)
    assert alloc.argument_names == [None, "Mold"]


def test_intrinsiccall_dealloc_create():
    '''
    Tests for the creation of a 'deallocate' call.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    ierr = DataSymbol("ierr", INTEGER_TYPE)
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym)])
    assert isinstance(dealloc, IntrinsicCall)
    assert isinstance(dealloc, IntrinsicCall)
    assert isinstance(dealloc.routine, IntrinsicSymbol)
    assert dealloc.routine.name == "DEALLOCATE"
    assert dealloc.children[0].symbol is sym
    # With 'stat' optional argument.
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym),
                                             ("Stat", Reference(ierr))])
    assert dealloc.argument_names == [None, "Stat"]


def test_intrinsiccall_random_create():
    '''
    Tests for the creation of a 'random' call.

    '''
    sym = DataSymbol("my_array", ArrayType(REAL_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    rand = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.RANDOM, [Reference(sym)])
    assert isinstance(rand, IntrinsicCall)
    assert isinstance(rand, IntrinsicCall)
    assert isinstance(rand.routine, IntrinsicSymbol)
    assert rand.routine.name == "RANDOM"
    assert rand.children[0].symbol is sym


def test_intrinsiccall_create_errors():
    '''
    Checks for the validation/type checking in the create() method.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    aref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create("ALLOCATE", [Reference(sym)])
    assert ("'intrinsic' argument should be an instance of "
            "IntrinsicCall.Intrinsic but found 'str'" in str(err.value))
    # Supplied arguments must be a list.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, aref)
    assert ("IntrinsicCall.create() 'arguments' argument should be a list "
            "but found 'ArrayReference'" in str(err.value))
    # An allocate must have one or more References as argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [])
    assert ("The 'ALLOCATE' intrinsic requires at least 1 arguments but "
            "got 0" in str(err.value))
    # The random intrinsic only accepts one argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM, [aref,
                                                              aref.copy()])
    assert ("The 'RANDOM' intrinsic requires between 1 and 1 arguments but "
            "got 2" in str(err.value))
    # Wrong type for a positional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [sym])
    assert ("The 'ALLOCATE' intrinsic requires that positional arguments be "
            "of type " in str(err.value))
    assert "but got a 'DataSymbol'" in str(err.value)
    # Positional argument after named argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.DEALLOCATE,
                             [Reference(sym), ("stat", aref), aref])
    assert ("Found a positional argument *after* a named argument ('stat'). "
            "This is invalid." in str(err.value))
    # 'random' does not have any optional arguments
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM,
                             [aref, ("willow", sym)])
    assert ("The 'RANDOM' intrinsic does not support any optional arguments "
            "but got 'willow'" in str(err.value))
    # An allocate only supports the 'stat' and 'mold' arguments.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [aref, ("yacht", Reference(sym))])
    assert ("The 'ALLOCATE' intrinsic supports the optional arguments "
            "['mold', 'stat'] but got 'yacht'" in str(err.value))
    # Wrong type for an optional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [aref, ("stat", sym)])
    assert ("The optional argument 'stat' to intrinsic 'ALLOCATE' must be "
            "of type 'Reference' but got 'DataSymbol'" in str(err.value))
