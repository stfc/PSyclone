# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Laboratory
# Modified: A. R. Porter, STFC Daresbury Laboratory

'''Module providing a transformation from a PSyIR MIN operator to
PSyIR code. This could be useful if the MIN operator is not supported
by the back-end or if the performance of the inline code is better
than the intrinsic.

'''
from __future__ import absolute_import

from psyclone.psyir.nodes import BinaryOperation, NaryOperation
from psyclone.psyir.transformations.intrinsics.minormax2code_trans import \
        MinOrMax2CodeTrans


class Min2CodeTrans(MinOrMax2CodeTrans):
    '''Provides a transformation from a PSyIR MIN Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed (by a parent class).

    The transformation replaces

    .. code-block:: python

        R = MIN(A, B, C ...)

    with the following logic:

    .. code-block:: python

        R = A
        if B < R:
            R = B
        if C < R:
            R = C
        ...

    '''
    def __init__(self):
        super(Min2CodeTrans, self).__init__()
        self._operator_name = "MIN"
        self._operators = (BinaryOperation.Operator.MIN,
                           NaryOperation.Operator.MIN)
        self._compare_operator = BinaryOperation.Operator.LT
