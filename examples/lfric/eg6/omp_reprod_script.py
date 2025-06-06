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
# Authors: R. W. Ford, A. R. Porter, S. Siso, STFC Daresbury Laboratory
# Modified: I. Kavcic, Met Office
# Modified: J. Henrichs, Bureau of Meteorology

'''File containing a PSyclone transformation script for the LFRic
API to apply loop fusion and then OpenMP parallelisation to an invoke
with two Kernels. This can be applied via the -s option in the
generator.py script.'''

from psyclone.configuration import Config
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.transformations import OMPParallelTrans, LFRicOMPLoopTrans


def trans(psyir):
    ''' PSyclone transformation script for the LFRic API to apply
    loop fusion and OpenMP for a particular example.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    otrans = OMPParallelTrans()
    ltrans = LFRicOMPLoopTrans()
    ftrans = LFRicLoopFuseTrans()

    # Get first invoke subroutine
    schedule = psyir.children[0].children[0]

    config = Config.get()
    if config.api_conf("lfric").compute_annexed_dofs and \
       config.distributed_memory:
        # We can't loop fuse as the loop bounds differ so add
        # OpenMP parallel do directives to the loops
        otrans.apply(schedule.children[0])
        otrans.apply(schedule.children[1])
    else:
        # Loop fuse the two built-in kernels. The 'same_space' flag needs to
        # be set as built-ins are over ANY_SPACE.
        ftrans.apply(schedule[0], schedule[1], {"same_space": True})

        # Add an OpenMP do directive to the resultant loop-fused loop,
        # specifying that we want reproducible reductions
        ltrans.apply(schedule.children[0], {"reprod": True})

        # Add an OpenMP parallel directive around the OpenMP do directive
        otrans.apply(schedule.children[0])

    # take a look at what we've done
    print(schedule.view())
    schedule.dag()
