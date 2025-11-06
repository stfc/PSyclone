# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
function via the -s option. It adds OpenMP offload directives
to all kernels.
'''

from psyclone.psyir.nodes import Directive, Loop, Routine, Call
from psyclone.psyir.transformations import TransformationError, OMPTargetTrans
from psyclone.transformations import OMPLoopTrans
from psyclone.psyir.transformations import OMPDeclareTargetTrans
from fuse_loops import trans as fuse_trans


def trans(psyir):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psyir: the PSyIR layer to transform.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`


    '''

    declare_target = OMPDeclareTargetTrans()

    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    for kern in psyir.walk(Call):
        # Put a ``declare target`` directive inside each kernel
        try:
            declare_target.apply(kern)
        except TransformationError as err:
            print(f"Failed to annotate '{kern.name}' with "
                  f"GPU-enabled directive due to:\n"
                  f"{err.value}")

    loop_offloading = OMPLoopTrans(
        omp_directive="teamsdistributeparalleldo",
        omp_schedule="none")
    target_trans = OMPTargetTrans()

    for subroutine in psyir.walk(Routine):
        for loop in subroutine.walk(Loop):
            if loop.loop_type == "outer":
                loop_offloading.apply(loop)
                target_trans.apply(loop.ancestor(Directive))
