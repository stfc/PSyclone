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
# Modified: O. Brunt, Met Office

'''A PSyclone transformation script that transforms all synchronous
halo exchanges into asynchronous halo exchanges and moves the halo
exchange start part of each asynchronous halo exchange as early as
possible in the schedule in order to maximise the overlap of
communication and computation. Also outputs a textual view of the
transformed PSyIR representing the PSy-layer.

This is a generic implementation that will work for all LFRic
schedules and for algorithms containing multiple invoke calls.

This PSyclone transformation script is designed to be passed to
PSyclone, it is not designed to be run directly from python.

'''
from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans, \
    MoveTrans, TransformationError
from psyclone.dynamo0p3 import LFRicHaloExchange, LFRicHaloExchangeStart


def trans(psy):
    '''Transforms all synchronous halo exchanges into asynchronous halo
    exchanges and moves the halo exchange start part of each
    asynchronous halo exchange as early as possible in the schedule in
    order to maximise the overlap of communication and
    computation. Also outputs a textual view of the transformed PSyIR
    representing the PSy-layer.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    # Create the required transformations
    async_hex = Dynamo0p3AsyncHaloExchangeTrans()
    move_trans = MoveTrans()

    # Iterate over the invokes in this algorithm file
    invokes = psy.invokes.invoke_list
    for invoke in invokes:

        # Get the schedule (the PSyIR representation of the PSy-layer)
        schedule = invoke.schedule
        # Split any synchronous halo exchanges into asynchronous halo exchanges
        for hex_node in schedule.walk(LFRicHaloExchange):
            async_hex.apply(hex_node)

        # Move any halo exchange starts as early as possible in the
        # schedule to maximise overlap of compute and comms within the
        # invoke.
        for hex_start_node in reversed(schedule.walk(LFRicHaloExchangeStart)):
            idx = hex_start_node.position
            parent = hex_start_node.parent
            # Move halo exchange start node up one node at a time
            # until there is an exception (which indicates the move is
            # invalid). No need to check for idx == 0 as a negative
            # index wraps to the end of the list which will be
            # invalid.
            try:
                while True:
                    move_trans.apply(parent[idx], parent[idx-1])
                    idx -= 1
            except TransformationError:
                pass

        # Take a look at the modified PSy-layer PSyIR
        print(schedule.view())
