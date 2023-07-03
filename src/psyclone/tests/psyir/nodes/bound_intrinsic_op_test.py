# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

''' pytest module for the U/LBOUND intrinsic PSyIR operators. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import BinaryOperation, Literal, ArrayReference
from psyclone.psyir.symbols import (REAL_TYPE, INTEGER_TYPE, DataSymbol,
                                    DeferredType)


@pytest.mark.xfail(reason="#677 the create() method does not check that the "
                   "types of the Nodes it is passed are correct for the "
                   "provided operator.")
@pytest.mark.parametrize("bound", [BinaryOperation.Operator.LBOUND,
                                   BinaryOperation.Operator.UBOUND])
def test_bound_intrinsic_wrong_type(bound):
    ''' Check that attempting to create an L/UBOUND intrinsic operator
    with the wrong type of arguments raises the expected error. '''
    int_one = Literal("1", INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        # First argument must be an Array
        _ = BinaryOperation.create(bound, int_one.copy(), int_one.copy())
    assert "must be an Array but got: 'Literal" in str(err.value)
    sym = DataSymbol("array", DeferredType())
    with pytest.raises(TypeError) as err:
        # Second argument cannot be a real literal
        _ = BinaryOperation.create(
            bound, ArrayReference.create(sym, [int_one.copy()]),
            Literal("1.0", REAL_TYPE))
    assert ("must be an integer but got a Literal of type REAL" in
            str(err.value))
