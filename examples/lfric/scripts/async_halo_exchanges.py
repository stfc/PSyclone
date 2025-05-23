# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council
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
# Authors: R. Ford, A. R. Porter and S. Siso, STFC Daresbury Laboratory
# Modified: O. Brunt, Met Office

'''File containing a PSyclone transformation script for the LFRic
API to make asynchronous halo exchanges and overlap their
communication with computation. This can be applied via the -s option
in the generator.py script.

'''

from psyclone.lfric import LFRicHaloExchange, LFRicHaloExchangeStart
from psyclone.psyGen import InvokeSchedule
from psyclone.transformations import LFRicAsyncHaloExchangeTrans, \
    MoveTrans, TransformationError


def trans(psyir):
    '''A transformation script to use asynchronous halo exchanges with
    overlapping compute and communication for the LFRic model.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    for subroutine in psyir.walk(InvokeSchedule):
        # This transformation splits the three synchronous halo exchanges
        ahex_trans = LFRicAsyncHaloExchangeTrans()
        for h_ex in subroutine.walk(LFRicHaloExchange):
            ahex_trans.apply(h_ex)

        # This transformation moves the start of the halo exchanges as far
        # as possible offering the potential for overlap between communication
        # and computation.
        mtrans = MoveTrans()
        location_cursor = 0
        for ahex in subroutine.walk(LFRicHaloExchangeStart):
            if ahex.position <= location_cursor:
                continue
            try:
                mtrans.apply(ahex, subroutine.children[location_cursor])
                location_cursor += 1
            except TransformationError:
                pass

        print(f"{location_cursor} AsyncHaloExchanges have been rearranged"
              f" in {subroutine.name}")
