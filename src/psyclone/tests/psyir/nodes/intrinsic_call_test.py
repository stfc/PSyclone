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

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ArrayReference, Literal, IntrinsicCall, Reference)
from psyclone.psyir.symbols import (ArrayType, DataSymbol, INTEGER_TYPE,
                                    IntrinsicSymbol)


def test_intrinsiccall_create():
    '''
    Tests for the create() method.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])])
    assert isinstance(alloc, IntrinsicCall)


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
    # An allocate must have one or more ArrayReferences as argument.
    with pytest.raises(GenerationError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [])
    assert ("The 'ALLOCATE' intrinsic requires 1 arguments but got 0" in
            str(err.value))
    with pytest.raises(GenerationError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [Reference(sym)])
    assert ("The 'ALLOCATE' intrinsic requires an argument of type "
            "'ArrayReference' at position 0 but got a 'Reference'" in
            str(err.value))
    # An allocate only supports the 'stat' and 'mold' arguments.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [aref, ("yacht", Reference(sym))])
    assert "hello" in str(err.value)
