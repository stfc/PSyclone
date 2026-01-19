# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2026, Science and Technology Facilities Council
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
parallelisation, and then tiles the fused loops.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.transformations import MoveTrans, TransformationError
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
from psyclone.psyir.transformations import (InlineTrans, LoopFuseTrans,
                                            LoopTiling2DTrans)
from psyclone.psyir.nodes import (Assignment, Call, FileContainer,
                                  IntrinsicCall, Loop, Reference, Routine)


def trans(psyir: FileContainer) -> None:
    '''A complex program that inline all loops, moves the scalar assignment to
    the top so that all loops are next to each other. This allows loops to
    be fused then. After fusion, OpenMP parallelisation is added. Once
    parallelisation has been added, apply loop tiling.

    :param psyir: the PSyIR of the provided file.

    '''

    # First inline all kernels. We first need to 'module inline' each
    # subroutine, i.e. copy the subroutine into the current module using
    # the KernelModuleInlineTrans. Once this is done, we can use the
    # inlining transformation:
    module_inline = KernelModuleInlineTrans()
    inline = InlineTrans()
    for call in psyir.walk(Call):
        if (not isinstance(call, IntrinsicCall) and
                call.routine.name != "output_field"):
            module_inline.apply(call)
            inline.apply(call)

    # Collect all outer (latitude) loops. We only collect the loops
    # in the `time_step` subroutine (due to kernel module inlining above
    # the other subroutines would otherwise be found as well, but we
    # do not want to modify them). Outer loops have a loop as loop body:
    lat_loops = []
    time_step_routine = None
    for subroutine in psyir.walk(Routine):
        if subroutine.name == "time_step":
            time_step_routine = subroutine
            break
    else:
        raise RuntimeError("Cannot find subroutine 'time_step'.")

    for loop in time_step_routine.walk(Loop):
        # We can't rely on variable names/loop types, since inlining will
        # create new, unique variable names. So identify outer loops by
        # checking if the body of the loop is a Loop:
        if isinstance(loop.loop_body.children[0], Loop):
            lat_loops.append(loop)

    if len(lat_loops) == 0:
        # Inlining didn't work? To support testing without inlining,
        # just return here:
        return

    parent = lat_loops[0].parent
    # Keep track of all assignments (see below)
    assignments = [assignment for assignment
                   in parent.children
                   if isinstance(assignment, Assignment)]
    # We need to move all statements that define the loop boundaries
    #    xstart = current%internal%xstart
    #    ...
    #    xstart_1 = current%internal%xstart etc
    # to before the first loop (to allow loop fusion). We can't move the
    # assignments before the first loop (they would be moved to the same
    # location, which the move transformation does not allow), so we
    # start with all direct children of the parent after the first loop.
    # Moving only assignments will also make sure that the output step
    # remains at the end of the loop body:
    move = MoveTrans()
    for child in parent.children[lat_loops[0].position+1:]:
        if isinstance(child, Assignment):
            try:
                move.apply(child, lat_loops[0], options={"position": "before"})
            except TransformationError as err:
                print(f"Cannot move code {child}: {err.value}")

    const_mapping = {}
    for assignment in assignments:
        const_mapping[assignment.lhs.name] = assignment.rhs

    for lat_loop in lat_loops:
        for ref in lat_loop.walk(Reference):
            if ref.name in const_mapping:
                value = const_mapping[ref.name].copy()
                ref.replace_with(value)

    fuse = LoopFuseTrans()

    fuse.apply(lat_loops[0], lat_loops[1])
    fuse.apply(lat_loops[0].loop_body[0], lat_loops[0].loop_body[1])
    # Remove fused loop from list of loops previously collected
    del lat_loops[1]

    fuse.apply(lat_loops[0], lat_loops[1])
    fuse.apply(lat_loops[0].loop_body[0], lat_loops[0].loop_body[1])
    # Remove fused loop from list of loops previously collected
    del lat_loops[1]

    # Now add an omp parallel around all loops ...
    ompp = OMPParallelTrans()
    ompp.apply(lat_loops)

    # ... and then omp do around the outer loops
    ompl = OMPLoopTrans()
    for loop in lat_loops:
        try:
            ompl.apply(loop)
        except TransformationError as err:
            print(f"Cannot apply omp loop {loop}: {err.value}")

    # Then apply tiling for the big fused loop
    # Applying this last avoids the problem that lat_loops[0]
    # is suddenly not an outer loop anymore (and ompl.apply()
    # then fails because the loops don't have a common parent).
    # Plus, the 4-times nested loop is too much for the dependency
    # analysis and would require an option to ignore (incorrectly)
    # detected dependencies for some variables.
    tiling = LoopTiling2DTrans()
    try:
        tiling.apply(lat_loops[0])
    except TransformationError as err:
        print(f"Cannot tile loop {lat_loops[0]}: {err.value}")
