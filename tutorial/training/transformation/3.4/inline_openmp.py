# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology

'''
This example inlines all kernels, fuses loops together, applies OpenMP
parallelisiation, and then tiles the fused loops.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.transformations import MoveTrans, TransformationError
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
from psyclone.psyir.transformations import (InlineTrans, LoopFuseTrans,
                                            LoopTiling2DTrans)
from psyclone.psyir.nodes import Assignment, Call, Loop, Reference


def trans(psyir):
    '''A complex program that inline all loops, moves the scalar assignment to
    the top so that all loops are next to each other

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    # First inline all kernels. We first need to 'module inline' each
    # subroutine, i.e. copy the subroutine into the current module using
    # the KernelModuleInlineTrans. Once this is done, we can use the
    # inlining transformation:
    kmit = KernelModuleInlineTrans()
    inline = InlineTrans()

    for call in psyir.walk(Call):
        if call.routine.name != #TODO: not for output_field
            print("Inlining", call.routine)
            TODO appy inlining, first kmit, then inline

    # Study the output code - and find a way to add openmp - ideally
    # by using `openmp parallel` only once around all loops.
    # There is an easy solution, but a more complicated one will
    # allow you to fuse loop.
    # Alternatively/additionally, try to apply LoopTiling
