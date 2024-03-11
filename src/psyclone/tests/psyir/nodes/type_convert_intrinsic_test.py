# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs pytest tests specific to the REAL/INT type-conversion
    intrinsics. '''

import pytest
from psyclone.psyir.nodes import BinaryOperation, Literal, Reference, \
    IntrinsicCall
from psyclone.psyir.symbols import DataSymbol, INTEGER_SINGLE_TYPE, \
    REAL_SINGLE_TYPE
from psyclone.tests.utilities import check_links


@pytest.mark.parametrize("intrinsic, intr_str",
                         [(IntrinsicCall.Intrinsic.REAL, "real"),
                          (IntrinsicCall.Intrinsic.INT, "int")])
def test_type_convert_intrinsic_create(intrinsic, intr_str, fortran_writer):
    '''Test that the create method in the IntrinsicCall class correctly
    creates a IntrinsicCall instance for the REAL and INT type-conversion
    operations..

    '''
    sym = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    lhs = Reference(sym)
    wp_sym = DataSymbol("wp", INTEGER_SINGLE_TYPE)
    # Reference to a kind parameter
    rhs = Reference(wp_sym)
    intr_call = IntrinsicCall.create(intrinsic, [lhs, ("kind", rhs)])
    assert intr_call.intrinsic is intrinsic
    check_links(intr_call, [lhs, rhs])
    result = fortran_writer(intr_call)
    assert intr_str + "(tmp1, kind=wp)" in result.lower()
    # Kind specified with an integer literal
    rhs = Literal("4", INTEGER_SINGLE_TYPE)
    intr_call = IntrinsicCall.create(intrinsic, [lhs.detach(), ("kind", rhs)])
    check_links(intr_call, [lhs, rhs])
    result = fortran_writer(intr_call)
    assert intr_str + "(tmp1, kind=4)" in result.lower()
    # Kind specified as an arithmetic expression
    rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                 Reference(wp_sym),
                                 Literal("2", INTEGER_SINGLE_TYPE))
    intr_call = IntrinsicCall.create(intrinsic, [lhs.detach(), ("kind", rhs)])
    check_links(intr_call, [lhs, rhs])
    result = fortran_writer(intr_call)
    assert intr_str + "(tmp1, kind=wp + 2)" in result.lower()


@pytest.mark.xfail(reason="No PSyIR symbol type checking is performed on the "
                   "arguments supplied to the IntrinsicCall.create() "
                   "method - TODO #658.")
def test_real_intrinsic_invalid():
    ''' Test that the create method rejects invalid precisions. '''
    sym = DataSymbol("tmp1", REAL_SINGLE_TYPE)
    intrinsic = IntrinsicCall.Intrinsic.REAL
    with pytest.raises(TypeError) as err:
        _ = IntrinsicCall.create(
            intrinsic,
            [Reference(sym), ("kind", Literal("1.0", REAL_SINGLE_TYPE))])
    assert ("Precision argument to REAL operation must be specified using a "
            "DataSymbol, ScalarType.PRECISION or integer Literal but got "
            "xxxx" in str(err.value))
    # A Symbol of REAL type cannot be used to specify a precision
    wrong_kind = DataSymbol("not_wp", REAL_SINGLE_TYPE)
    with pytest.raises(TypeError) as err:
        _ = IntrinsicCall.create(
            intrinsic,
            [Reference(sym), ("kind", Reference(wrong_kind))])
    assert ("If the precision argument to a REAL operation is a Reference "
            "then it must be to a symbol of integer type but got: 'yyyy'" in
            str(err.value))
