# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Python script intended to be passed to PSyclone via the -s option.
It applies module inlining, then fuses the first three loops of the
first invoke.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.gocean1p0 import GOKern
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply module inlining, and then
    fuse all loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    fuse = GOceanLoopFuseTrans()
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # schedule.view()
    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    # This schedule has four loops, corresponding to
    # count_neighbours, compute_born, compute_die, combine kernels

    # First merge the first two loops
    fuse.apply(schedule[0], schedule[1])

    # Then merge the (previous third, now second) loop to the
    # fused loop
    fuse.apply(schedule[0], schedule[1])
    # Now we have:
    # do j
    #   do i
    #   do i
    #   do i
    # do j combine
    # Fuse the three inner loops: first the first two
    fuse.apply(schedule[0].loop_body[0], schedule[0].loop_body[1])
    # Then merge in the previous third, now second) loop
    fuse.apply(schedule[0].loop_body[0], schedule[0].loop_body[1])
    schedule.view()
