# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council
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
# Authors: R. Ford, A. R. Porter and S. Siso STFC Daresbury Laboratory
# Modified: I. Kavcic, Met Office
# Modified by J. Henrichs, Bureau of Meteorology


''' File containing a PSyclone transformation script for the LFRic
API to apply colouring and OpenMP generically. This can be applied via
the -s option in the "psyclone" script. '''
from psyclone.transformations import LFRicColourTrans, \
    LFRicOMPParallelLoopTrans
from psyclone.psyir.nodes import Loop
from psyclone.psyGen import InvokeSchedule
from psyclone.domain.lfric import LFRicConstants


def trans(psyir):
    ''' PSyclone transformation script for the LFRic api to apply
    colouring and OpenMP generically.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPParallelLoopTrans()
    const = LFRicConstants()

    # Loop over all of the Invokes
    for subroutine in psyir.walk(InvokeSchedule):
        print(f"Transforming invoke '{subroutine.name}'...")

        # Colour all of the loops over cells unless they are on
        # discontinuous spaces
        for child in subroutine.children:
            if isinstance(child, Loop) \
               and child.field_space.orig_name \
               not in const.VALID_DISCONTINUOUS_NAMES \
               and child.iteration_space.endswith("cell_column"):
                ctrans.apply(child)
        # Then apply OpenMP to each of the colour loops
        for child in subroutine.children:
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    otrans.apply(child.loop_body[0])
                else:
                    otrans.apply(child)
