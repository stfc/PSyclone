# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Python script intended to be passed to PSyclone via the -s option.
It applies kernel inlining and then fuses the first three loops of
the first invoke.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import FileContainer


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply kernel inlining and then
    fuse the first three loops

    :param psyir: the PSyIR of the PSy-layer.

    '''
    module_inline = KernelModuleInlineTrans()

    # Inline all kernels to help gfortran with inlining.
    for kern in psyir.kernels():
        module_inline.apply(kern)

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    print(schedule.view())

    fuse = GOceanLoopFuseTrans()
    # do j do i count
    # do j do i born
    # do j do i die
    # do j do i combine

    # First merge the first two j loops
    fuse.apply(schedule[0], schedule[1])
    # do j do i count
    #      do i born
    # do j do i die
    # do j do i combine

    # Then merge the (previous third, now second) loop to the
    # fused loop
    fuse.apply(schedule[0], schedule[1])
    # do j do i count
    #      do i born
    #      do i die
    # do j do i combine

    # You cannot fuse the two remaining outer loops!

    # Fuse the three inner loops: first the first two
    fuse.apply(schedule[0].loop_body[0], schedule[0].loop_body[1])
    # do j do i count born
    #      do i die
    # do j do i combine

    # Then merge in the previous third, now second) loop
    fuse.apply(schedule[0].loop_body[0], schedule[0].loop_body[1])
    # do j do i count born die
    # do j do i combine
    print(schedule.view())
