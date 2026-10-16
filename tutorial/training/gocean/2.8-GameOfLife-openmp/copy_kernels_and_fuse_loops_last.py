# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies module inlining and then fuses the last three loops in
the first invoke.
'''


from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply kernel module inlining
    and fuse the last three loops.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    fuse = GOceanLoopFuseTrans()
    module_inline = KernelModuleInlineTrans()

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    for kern in schedule.kernels():
        module_inline.apply(kern)

    # This schedule has four loops, corresponding to
    # count_neighbours, compute_born, compute_die, combine kernels

    # First merge 2nd and 3rd loops
    fuse.apply(...)
    # Then merge the (previous fourth, now third) loop to the
    # fused loop
    fuse.apply(...)
    # Now we have:
    # do j count_neighbours
    # do j
    #   do i
    #   do i
    #   do i
    # Fuse the three inner loops:
    fuse.apply(...)
    fuse.apply(...)
    print(schedule.view())
