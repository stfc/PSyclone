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
# Authors: S. Siso, STFC Daresbury Lab

''' File containing a PSyclone transformation script for the LFRic
API to apply tiled-colouring and OpenMP threading.'''

from psyclone.transformations import LFRicColourTrans, \
    LFRicOMPParallelLoopTrans
from psyclone.psyir.nodes import Loop, Routine, FileContainer
from psyclone.domain.lfric import LFRicConstants


def trans(psyir: FileContainer):
    ''' PSyclone transformation script to apply tiled-colouring and OpenMP
    threading.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    ctrans = LFRicColourTrans()
    otrans = LFRicOMPParallelLoopTrans()
    const = LFRicConstants()

    for subroutine in psyir.walk(Routine):

        print(f"Transforming invoke '{subroutine.name}'...")

        # Colour all of the loops over cells (with the tiling option)
        # unless they are on discontinuous spaces
        for child in subroutine.children:
            if isinstance(child, Loop) \
               and child.field_space.orig_name \
               not in const.VALID_DISCONTINUOUS_NAMES \
               and child.iteration_space == "cell_column":
                ctrans.apply(child, tiling=True)

        # Then apply OpenMP to each of the colour loop
        for child in subroutine.children:
            if isinstance(child, Loop):
                if child.loop_type == "colours":
                    otrans.apply(child.loop_body[0])
                else:
                    otrans.apply(child)
