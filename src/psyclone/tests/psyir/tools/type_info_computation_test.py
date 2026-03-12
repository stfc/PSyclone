# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab

''' This module contains the tests for the type info computation functions.'''

import pytest

from enum import Enum

from psyclone.errors import InternalError
from psyclone.psyir.tools.type_info_computation import (
    compute_precision,
    compute_scalar_type
)
from psyclone.psyir.nodes import Reference, BinaryOperation
from psyclone.psyir.symbols import ScalarType, UnresolvedType


def test_compute_precision(fortran_reader, monkeypatch):
    '''Test the compute_precision helper routine.'''
    code = """subroutine test
        integer, parameter :: wp = 4
        integer(kind=wp) :: a, b
        integer*8 :: i
        integer*4 :: k
        double precision :: l, m
        real :: n

        a = a + b
        i = i + k
        l = l + m
        l = m + n
        a = a + i

    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    ops = psyir.walk(BinaryOperation)

    # First operation is two Reference kinds, result should be the same
    # reference.
    op = ops[0]
    res = compute_precision([x.datatype.precision for x in op.operands])
    assert isinstance(res, Reference)
    assert res.symbol.name == "wp"

    # Second operation is two integer precisions, so result is the max of them
    op = ops[1]
    res = compute_precision([x.datatype.precision for x in op.operands])
    assert isinstance(res, int)
    assert res == 8

    # Third operation is two double ScalarType.Precisions so should result in
    # a double ScalarType.Precision
    op = ops[2]
    res = compute_precision([x.datatype.precision for x in op.operands])
    assert isinstance(res, ScalarType.Precision)
    assert res == ScalarType.Precision.DOUBLE

    # Fourth operation is one double and one undefined so should result in an
    # undefined.
    op = ops[3]
    res = compute_precision([x.datatype.precision for x in op.operands])
    assert isinstance(res, ScalarType.Precision)
    assert res == ScalarType.Precision.UNDEFINED

    # Last operation is one wp and one int precision so we should get an
    # undefined
    op = ops[4]
    res = compute_precision([x.datatype.precision for x in op.operands])
    assert isinstance(res, ScalarType.Precision)
    assert res == ScalarType.Precision.UNDEFINED

    # Have two arguments with unknown precisions to cause an error.
    class FakePrecision(Enum):
        '''Fake version of Precision enum for testing.'''
        SINGLE = 1
        DOUBLE = 2
        UNDEFINED = 3
        OTHER = 4

    monkeypatch.setattr(ScalarType, "Precision", FakePrecision)
    with pytest.raises(InternalError) as err:
        res = compute_precision([ScalarType.Precision.SINGLE,
                                 FakePrecision.OTHER])
    assert ("PSyclone internal error: Could not compute precision for inputs "
            "'[<FakePrecision.SINGLE: 1>, <FakePrecision.OTHER: 4>]' due to "
            "unknown Precisions being supplied." in str(err.value))

    # If one is a DOUBLE and the other is SINGLE we should get a DOUBLE
    res = compute_precision([ScalarType.Precision.SINGLE,
                             ScalarType.Precision.DOUBLE])
    assert res == ScalarType.Precision.DOUBLE


def test_compute_scalar_type(fortran_reader):
    '''Tests the compute_scalar_type helper function.'''
    code = """subroutine test
        use some_mod
        integer, parameter :: wp = 4
        integer(kind=wp) :: a, b
        integer*8 :: i
        integer*4 :: k
        double precision :: l, m
        real :: n
        logical :: x

        a = a + b
        a = unknown + c
        m = l + i
        m = i + n
        a = i + k
        n = x + k

    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    ops = psyir.walk(BinaryOperation)

    # First operation is all integers with the same precision so get the
    # same datatype back.
    op = ops[0]
    res = compute_scalar_type([x.datatype for x in op.operands])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision.symbol.name == "wp"

    # Second operation contains an UnresolvedType so we get back an
    # UnresolvedType
    op = ops[1]
    res = compute_scalar_type([x.datatype for x in op.operands])
    assert isinstance(res, UnresolvedType)

    # Third operation has the first as a real and the second not, we get
    # back a real
    op = ops[2]
    res = compute_scalar_type([x.datatype for x in op.operands])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.REAL
    assert res.precision == ScalarType.Precision.DOUBLE

    # Fourth operation has the second as a real and first not, we get
    # back a real
    op = ops[3]
    res = compute_scalar_type([x.datatype for x in op.operands])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.REAL
    assert res.precision == ScalarType.Precision.UNDEFINED

    # Fifth operation has two ints with difference precisions,
    # should get the correctly promoted precision.
    op = ops[4]
    res = compute_scalar_type([x.datatype for x in op.operands])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == 8

    # Last operation has a logical, should fail.
    op = ops[5]
    with pytest.raises(TypeError) as excinfo:
        _ = compute_scalar_type([x.datatype for x in op.operands])
    assert ("Couldn't compute the type of an operation as the types of the "
            "arguments differ and one is non-numeric. Provided "
            "arguments were 'Scalar<BOOLEAN, UNDEFINED>' and "
            "'Scalar<INTEGER, 4>'." in str(excinfo.value))

    # Too many argument failure
    with pytest.raises(InternalError) as excinfo:
        _ = compute_scalar_type([1, 2, 3])
    assert ("Can't compute the scalar type of more than 2 inputs but "
            "3 were provided." in str(excinfo.value))
