# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council
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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab

''' Module providing a transformation script that converts the Schedule of
    the first Invoke to use OpenCL. '''

from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.transformations import (
    FoldConditionalReturnExpressionsTrans)
from psyclone.domain.gocean.transformations import (
    GOOpenCLTrans, GOMoveIterationBoundariesInsideKernelTrans)


def trans(psyir):
    '''
    Applies OpenCL to the given PSy-layer.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ocl_trans = GOOpenCLTrans()
    fold_trans = FoldConditionalReturnExpressionsTrans()
    move_boundaries_trans = GOMoveIterationBoundariesInsideKernelTrans()

    # Provide kernel-specific OpenCL optimization options
    for idx, kern in enumerate(psyir.kernels()):
        # Move the PSy-layer loop boundaries inside the kernel as a kernel
        # mask, this allows to iterate through the whole domain
        move_boundaries_trans.apply(kern)
        # Change the syntax to remove the return statements introduced by the
        # previous transformation
        kschedules = kern.get_callees()
        # NOTE: we assume the kernel is not polymorphic and thus there is
        # only one schedule associated with it.
        fold_trans.apply(kschedules[0])
        # Specify the OpenCL queue and workgroup size of the kernel
        # In this case we dispatch each kernel in a different queue to check
        # that the output code has the necessary barriers to guarantee the
        # kernel execution order.
        kern.set_opencl_options({"queue_number": idx+1, 'local_size': 4})

    # Transform the Schedule
    for schedule in psyir.walk(InvokeSchedule):
        ocl_trans.apply(schedule, options={"end_barrier": True})
