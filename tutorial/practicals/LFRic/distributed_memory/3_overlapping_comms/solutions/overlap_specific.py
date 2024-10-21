# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''A PSyclone transformation script that transforms a specific
synchronous halo exchange into an asynchronous halo exchange and
moves the halo exchange start part as early as possible in the
schedule in order to maximise the overlap of communication and
computation. Also outputs a textual view of the transformed PSyIR
representing the PSy-layer.

This is a kernel-specific implementation that will only work for
schedules with a halo exchange in a hard-coded location and a
resultant halo exchange start that can be moved to a hard-coded
location.

It is designed to work with the helmholtz example with the annexed
dofs option set to false.

This PSyclone transformation script is designed to be passed to
PSyclone, it is not designed to be run directly from python.

'''
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans, MoveTrans


def trans(psy):
    '''Transforms a specific synchronous halo exchange into an
    asynchronous halo exchange and moves the halo exchange start part
    as early as possible in the schedule in order to maximise the
    overlap of communication and computation. Also outputs a textual
    view of the transformed PSyIR representing the PSy-layer.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    # Create the required transformations
    async_hex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    # Get a specific invoke
    invoke = psy.invokes.invoke_list[0]
    # Get the schedule (the PSyIR representation of the PSy-layer)
    schedule = invoke.schedule
    # Reference a specific node that we can move
    hex_node = schedule[2]
    async_hex_trans.apply(hex_node)

    # Move the (specific) halo exchange start node to the start of the
    # schedule
    move_trans.apply(schedule[2], schedule[0])

    # Take a look at the modified PSy-layer PSyIR
    print(schedule.view())
