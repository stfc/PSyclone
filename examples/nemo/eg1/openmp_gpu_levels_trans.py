#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council
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
# Authors: S. Siso, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

from psyclone.psyir.nodes import Loop, Assignment
from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans
from psyclone.psyir.transformations import OMPTargetTrans, OMPLoopTrans
from psyclone.transformations import TransformationError

USE_GPU = True  # Enable for generating OpenMP target directives

Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})


def trans(psyir):
    ''' Add OpenMP Target and Loop directives to all loops.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_target_trans = OMPTargetTrans()
    omp_loop_trans = OMPLoopTrans()
    omp_loop_trans.omp_directive = "loop"

    # Convert all array implicit loops to explicit loops
    explicit_loops = ArrayAssignment2LoopsTrans()
    for assignment in psyir.walk(Assignment):
        try:
            explicit_loops.apply(assignment)
        except TransformationError:
            pass

    for loop in psyir.walk(Loop):
        if loop.loop_type == "levels":
            try:
                if USE_GPU:
                    omp_target_trans.apply(loop)
                omp_loop_trans.apply(loop)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass
