# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author: R. W. Ford STFC Daresbury Lab


'''An example PSyclone transformation script to demonstrate
optimisations to the matrix vector kernel to improve its performance
on CPUs. It replaces the matmul Fortran intrinsic with inline matrix
vector code.

This script can be applied via the -s option in the psyclone
command, it is not designed to be directly run from python.

'''
from __future__ import print_function
import sys
from psyclone.psyir.nodes import BinaryOperation
from psyclone.psyir.transformations import Matmul2CodeTrans
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psy):
    '''PSyclone transformation script for the Dynamo0.3 API to optimise
    the matvec kernel for many-core CPUs. This is currently limited to
    running on the scaled_matrix_vector_code kernel but should work
    more generally. Any matmul calls are replaced with inline matric
    vector code.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    matmul2code_trans = Matmul2CodeTrans()

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for kernel in schedule.coded_kernels():
            if kernel.name.lower() == "scaled_matrix_vector_code":
                kernel.modified = True
                kernel_schedule = kernel.get_kernel_schedule()
                # Replace matmul with inline code
                for bin_op in kernel_schedule.walk(BinaryOperation):
                    if bin_op.operator is BinaryOperation.Operator.MATMUL:
                        matmul2code_trans.apply(bin_op)
                print(kernel_schedule.view())
    return psy
