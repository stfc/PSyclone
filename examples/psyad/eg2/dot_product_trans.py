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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR) and

1) modifies any PSyIR min, max, abs and sign intrinsics to PSyIR code
beforehand using transformations, as SIR does not support intrinsics.

2) transforms implicit loops to explicit loops as the SIR does not
have the concept of implicit loops.

Translation to the SIR is limited to the NEMO API. The NEMO API has no
algorithm layer so all of the original code is captured in the invoke
objects. Therefore by translating all of the invoke objects, all of
the original code is translated.

'''
from __future__ import print_function
from psyclone.nemo import NemoKern
from psyclone.psyir.nodes import (UnaryOperation, BinaryOperation,
                                  NaryOperation, Operation, Assignment)
from psyclone.psyir.transformations import DotProduct2CodeTrans


def trans(psy):
    '''xxx'''
    dot_product_trans = DotProduct2CodeTrans()

    # the invokes represent all of the original code.
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        for kernel in schedule.walk(NemoKern):

            kernel_schedule = kernel.get_kernel_schedule()
            for oper in kernel_schedule.walk(Operation):
                if oper.operator == BinaryOperation.Operator.DOT_PRODUCT:
                    # Apply DOT_PRODUCT transformation
                    dot_product_trans.apply(oper)

    return psy
