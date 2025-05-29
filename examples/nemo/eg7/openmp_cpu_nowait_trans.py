#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab

''' PSyclone transformation script showing the introduction of
asynchronous OpenMP directives into Nemo code. '''

from psyclone.psyir.transformations import (
        ArrayAssignment2LoopsTrans,
        OMPLoopTrans,
        OMPMinimiseSyncTrans,
        TransformationError
)
from psyclone.transformations import OMPParallelTrans
from psyclone.psyir.nodes import (
        Assignment,
        Directive,
        IfBlock,
        Loop,
        OMPBarrierDirective,
        OMPDoDirective,
        Routine,
)


def add_parallel_region_to_contiguous_directives(schedule):
    '''Adds OMPParallelDirective nodes around areas of the schedule with
    contiguous OpenMP directives.

    :param schedule: The Schedule to add OpenMPParallelDirectives to.
    :type schedule: :py:class:`psyclone.nodes.Schedule`
    '''
    par_trans = OMPParallelTrans()
    start = -1
    end = -1
    sets = []
    # Loop through the children, if its an OpenMP directive add it
    # to the current set
    for child in schedule:
        if isinstance(child, (OMPDoDirective, OMPBarrierDirective)):
            if start < 0:
                start = child.position
            end = child.position + 1
        else:
            # If we have a non OMPDodirective/OMPBarrierDirective then add
            # an OMPParallelDirective if needed.
            if start >= 0:
                sets.append((start, end))
                start = -1
                end = -1
            # Recurse appropriately to sub schedules:
            if isinstance(child, Loop):
                add_parallel_region_to_contiguous_directives(child.loop_body)
            elif isinstance(child, IfBlock):
                add_parallel_region_to_contiguous_directives(child.if_body)
                if child.else_body:
                    add_parallel_region_to_contiguous_directives(
                        child.else_body
                    )
    # If we get to the end and need to enclose some nodes in a parallel
    # directive we do it now
    if start >= 0:
        sets.append((start, end))

    for subset in sets[::-1]:
        par_trans.apply(schedule[subset[0]:subset[1]])


def trans(psyir):
    ''' Adds OpenMP Loop directives with nowait to Nemo loops over levels.
    This is followed by applying OpenMP parallel directives as required,
    before removing barriers where possible.


    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    loop_trans = OMPLoopTrans()
    minsync_trans = OMPMinimiseSyncTrans()

    # First convert assignments to loops whenever possible
    for assignment in psyir.walk(Assignment):
        try:
            ArrayAssignment2LoopsTrans().apply(assignment)
        except TransformationError:
            pass

    # Apply loop_trans to all the loops possible.
    for loop in psyir.walk(Loop):
        if not loop.ancestor(Directive):
            try:
                loop_trans.apply(loop, nowait=True)
            except TransformationError:
                # Not all of the loops in the example can be parallelised.
                pass

    # Apply the largest possible parallel regions and remove any barriers that
    # can be removed.
    for routine in psyir.walk(Routine):
        add_parallel_region_to_contiguous_directives(routine)
        minsync_trans.apply(routine)
