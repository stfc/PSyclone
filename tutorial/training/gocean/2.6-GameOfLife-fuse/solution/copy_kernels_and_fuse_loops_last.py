# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies kernel inlining and then fuses the last three loops of
the first invoke.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied FileContainer object, apply module inlining and then
    fuse the last three loops of the first invoke.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    module_inline = KernelModuleInlineTrans()

    # Inline all kernels to help gfortran with inlining.
    for kern in psyir.kernels():
        module_inline.apply(kern)

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # This schedule has four loops, corresponding to
    # count_neighbours, compute_born, compute_die, combine kernels

    fuse = GOceanLoopFuseTrans()
    # First merge 2nd and 3rd loops
    fuse.apply(schedule[1], schedule[2])
    # Then merge the (previous fourth, now third) loop to the
    # fused loop
    fuse.apply(schedule[1], schedule[2])
    # Now we have:
    # do j count_neighbours
    # do j
    #   do i
    #   do i
    #   do i
    # Fuse the three inner loops:
    fuse.apply(schedule[1].loop_body[0], schedule[1].loop_body[1])
    fuse.apply(schedule[1].loop_body[0], schedule[1].loop_body[1])
    print(schedule.view())
