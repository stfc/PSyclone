# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing a PSyAD kernel transformation script that applies
any required tranformations to the tangent linear PSyIR before it is
translated to adjoint PSyIR.

'''
from psyclone.psyir.nodes import BinaryOperation
from psyclone.psyir.transformations import DotProduct2CodeTrans


def preprocess_trans(kernel_psyir):
    '''PSyclone kernel transformation script which replaces dotproduct
    intrinsics with equivalent code and returns the modified
    psyir. This is called internally by the PSyAD script before
    transforming the code to its adjoint form.

    :param kernel_psyir: PSyIR representation of the tangent linear \
        kernel code.
    :type kernel_psyir: :py:class:`psyclone.psyir.nodes.Node`

    '''
    dot_product_trans = DotProduct2CodeTrans()

    for oper in kernel_psyir.walk(BinaryOperation):
        if oper.operator == BinaryOperation.Operator.DOT_PRODUCT:
            # Apply DOT_PRODUCT transformation
            dot_product_trans.apply(oper)
