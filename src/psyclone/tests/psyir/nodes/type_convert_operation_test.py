# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

''' Performs pytest tests specific to the REAL/INT type-conversion
    BinaryOperations. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import BinaryOperation, Literal, Reference
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE, \
    REAL_SINGLE_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import check_links


@pytest.mark.parametrize("operation, op_str",
                         [(BinaryOperation.Operator.REAL, "real"),
                          (BinaryOperation.Operator.INT, "int")])
def test_type_convert_binaryop_create(operation, op_str):
    '''Test that the create method in the BinaryOperation class correctly
    creates a BinaryOperation instance for the REAL and INT type-conversion
    operations..

    '''
    sym = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    lhs = Reference(sym)
    wp_sym = DataSymbol("wp", INTEGER_SINGLE_TYPE)
    # Reference to a kind parameter
    rhs = Reference(wp_sym)
    binaryoperation = BinaryOperation.create(operation, lhs, rhs)
    assert binaryoperation._operator is operation
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert op_str + "(tmp1, wp)" in result.lower()
    # Kind specified with an integer literal
    rhs = Literal("4", INTEGER_SINGLE_TYPE)
    binaryoperation = BinaryOperation.create(operation, lhs.detach(), rhs)
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert op_str + "(tmp1, 4)" in result.lower()
    # Kind specified as an arithmetic expression
    rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Reference(wp_sym),
                                 Literal("2", INTEGER_SINGLE_TYPE))
    binaryoperation = BinaryOperation.create(operation, lhs.detach(), rhs)
    check_links(binaryoperation, [lhs, rhs])
    result = FortranWriter().binaryoperation_node(binaryoperation)
    assert op_str + "(tmp1, wp + 2)" in result.lower()


@pytest.mark.xfail(reason="Only limited checking is performed on the "
                   "arguments supplied to the BinaryOperation.create() "
                   "method - TODO #658.")
def test_real_binaryop_invalid():
    ''' Test that the create method rejects invalid precisions. '''
    sym = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    oper = BinaryOperation.Operator.REAL
    with pytest.raises(TypeError) as err:
        _ = BinaryOperation.create(oper, Reference(sym),
                                   Literal("1.0", REAL_SINGLE_TYPE))
    assert ("Precision argument to REAL operation must be specified using a "
            "DataSymbol, ScalarType.PRECISION or integer Literal but got "
            "xxxx" in str(err.value))
    # A Symbol of REAL type cannot be used to specify a precision
    wrong_kind = DataSymbol("not_wp", REAL_SINGLE_TYPE)
    with pytest.raises(TypeError) as err:
        _ = BinaryOperation.create(oper, Reference(sym), Reference(wrong_kind))
    assert ("If the precision argument to a REAL operation is a Reference "
            "then it must be to a symbol of integer type but got: 'yyyy'" in
            str(err.value))
