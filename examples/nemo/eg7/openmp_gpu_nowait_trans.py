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
# Authors: A. B. G. Chalk, STFC Daresbury Lab

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

from psyclone.psyir.nodes import Loop, Assignment, Routine
from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans
from psyclone.psyir.transformations import OMPTargetTrans, OMPLoopTrans
from psyclone.psyir.transformations import OMPMinimiseSyncTrans
from psyclone.transformations import TransformationError

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
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
    opts = {"nowait": True}

    # First convert assignments to loops whenever possible unless
    # they have an ancestor levels loop
    for assignment in psyir.walk(Assignment):
        ancestor = assignment.ancestor(Loop)
        has_levels_ancestor = False
        while ancestor:
            if ancestor.loop_type == "levels":
                has_levels_ancestor = True
                break
            ancestor = ancestor.ancestor(Loop)
        if has_levels_ancestor:
            continue
        try:
            parent = assignment.parent
            pos = assignment.position
            ArrayAssignment2LoopsTrans().apply(assignment)
            omp_target_trans.apply(parent[pos], options=opts)
            omp_loop_trans.apply(parent[pos].dir_body.children[0])
        except TransformationError:
            pass

    for loop in psyir.walk(Loop):
        if loop.loop_type == "levels":
            try:
                omp_target_trans.apply(loop, options=opts)
                omp_loop_trans.apply(loop, nowait=True)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass

    for routine in psyir.walk(Routine):
        OMPMinimiseSyncTrans().apply(routine)
