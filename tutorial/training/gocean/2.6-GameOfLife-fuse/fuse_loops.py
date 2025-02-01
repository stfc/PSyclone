# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
function via the -s option. It adds kernel fusion code to
all invokes.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule


def trans(psy):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    fuse = GOceanLoopFuseTrans()
    inline = KernelModuleInlineTrans()

    # Inline all kernels to help gfortran with inlining.
    for kern in psyir.kernels():
        inline.apply(kern)

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]
    print(schedule.view())

    # do j do i count
    # do j do i born
    # do j do i die
    # do j do i combine

    # TODO: First merge the first two j loops. You can address them
    # using `schedule[index]`
    fuse.apply ...

    # do j do i count
    #      do i born
    # do j do i die
    # do j do i combine

    # TODO: Then merge the (previous third, now second) loop to the
    # fused loop
    fuse.apply ...
    # do j do i count
    #      do i born
    #      do i die
    # do j do i combine

    # You cannot fuse the two remaining outer loops!

    # TODO: Fuse the three inner loops: first the first two
    # To access an inner loop you need to get the loop_body of the
    # outer loop, e.g.: schedule[0].loop_body[0]
    fuse.apply ...
    # do j do i count born
    #      do i die
    # do j do i combine

    # TODO: Then merge in the previous third, now second) inner loop
    fuse.apply ...
    # do j do i count born die
    # do j do i combine
    print(schedule.view())
