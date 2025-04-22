# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel extraction code to
all invokes.
'''

from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                      ACCLoopTrans, ACCRoutineTrans)
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import Loop
from psyclone.gocean1p0 import GOKern

from fuse_loops import trans as fuse_trans

from psyclone.psyir.nodes import Directive, Loop, Routine
from psyclone.psyir.transformations import (
    ACCKernelsTrans, TransformationError, OMPTargetTrans)
from psyclone.transformations import (
    Dynamo0p3ColourTrans, Dynamo0p3OMPLoopTrans,
    Dynamo0p3RedundantComputationTrans, OMPParallelTrans,
    ACCParallelTrans, ACCLoopTrans, ACCRoutineTrans,
    OMPDeclareTargetTrans, OMPLoopTrans)


def trans(psyir):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psyir: the PSyIR layer to transform.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`


    '''
    loop_offloading_trans = OMPLoopTrans(
        omp_directive="teamsdistributeparalleldo",
        omp_schedule="none")
    gpu_region_trans = OMPTargetTrans()
    gpu_annotation_trans = OMPDeclareTargetTrans()

    inline = KernelModuleInlineTrans()


    #fuse_trans(psy)

    # Inline all kernels to help gfortran with inlining.
    for kern in psyir.walk(GOKern):
        inline.apply(kern)

    for subroutine in psyir.walk(Routine):
        for loop in subroutine.loops():
            for kern in loop.kernels():
                try:
                    gpu_annotation_trans.apply(kern)
                except TransformationError as err:
                    print(f"Failed to annotate '{kern.name}' with "
                          f"GPU-enabled directive due to:\n"
                          f"{err.value}")
        for loop in subroutine.walk(Loop):
            if loop.loop_type == "outer":
                loop_offloading_trans.apply(
                    loop, options={"independent": True})
                gpu_region_trans.apply(loop.ancestor(Directive))
