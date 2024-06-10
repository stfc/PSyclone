# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab

'''Module containing a PSyAD kernel transformation script that applies
any required transformations to the tangent linear PSyIR before it is
translated to adjoint PSyIR.

'''
from psyclone.core import SymbolicMaths
from psyclone.psyad.utils import node_is_passive
from psyclone.psyir.nodes import (Assignment, IntrinsicCall, Reference)
from psyclone.psyir.transformations import (DotProduct2CodeTrans,
                                            Matmul2CodeTrans,
                                            ArrayRange2LoopTrans,
                                            TransformationError,
                                            Reference2ArrayRangeTrans)


def preprocess_trans(kernel_psyir, active_variable_names):
    '''PSyclone kernel transformation script which modifies the supplied
    kernel psyir by replacing array-ranges with explicit loops,
    dotproduct and matmul intrinsics with equivalent code and
    performing symbolic expansion (e.g. x(a+b) => x*a+x*b). This is
    called internally by the PSyAD script before transforming the code
    to its adjoint form.

    :param kernel_psyir: PSyIR representation of the tangent linear \
        kernel code.
    :type kernel_psyir: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variable_names: list of active variable names.
    :type active_variable_names: list of str

    '''
    dot_product_trans = DotProduct2CodeTrans()
    matmul_trans = Matmul2CodeTrans()
    arrayrange2loop_trans = ArrayRange2LoopTrans()
    reference2arrayrange_trans = Reference2ArrayRangeTrans()

    # Replace references to arrays (array notation) with array-ranges
    for reference in kernel_psyir.walk(Reference):
        try:
            reference2arrayrange_trans.apply(reference)
        except TransformationError:
            pass

    # Replace array-ranges with explicit loops
    for assignment in kernel_psyir.walk(Assignment):
        if node_is_passive(assignment, active_variable_names):
            # No need to modify passive assignments
            continue
        # Repeatedly apply the transformation until there are no more
        # array ranges in this assignment.
        while True:
            try:
                arrayrange2loop_trans.apply(assignment)
            except TransformationError:
                break

    for call in kernel_psyir.walk(IntrinsicCall):
        if call.intrinsic == IntrinsicCall.Intrinsic.DOT_PRODUCT:
            # Apply DOT_PRODUCT transformation
            dot_product_trans.apply(call)
        elif call.intrinsic == IntrinsicCall.Intrinsic.MATMUL:
            # Apply MATMUL transformation
            matmul_trans.apply(call)

    # Deal with any associativity issues here as AssignmentTrans
    # is not able to.
    for assignment in kernel_psyir.walk(Assignment):
        sym_maths = SymbolicMaths.get()
        sym_maths.expand(assignment.rhs)
