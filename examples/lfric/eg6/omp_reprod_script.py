# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''File containing a PSyclone transformation script for the LFRic
API to apply loop fusion and then OpenMP parallelisation to an invoke
with two Kernels. This can be applied via the -s option in the
generator.py script.'''

from psyclone.configuration import Config
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.transformations import LFRicOMPLoopTrans
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.domain.lfric import LFRicLoop


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
    loops = schedule.walk(LFRicLoop)

    config = Config.get()
    if config.api_conf("lfric").compute_annexed_dofs and \
       config.distributed_memory:
        # We can't loop fuse as the loop bounds differ so add
        # OpenMP parallel do directives to the loops
        otrans.apply(loops[0])
        otrans.apply(loops[1])
    else:
        # Loop fuse the two built-in kernels. The 'same_space' flag needs to
        # be set as built-ins are over ANY_SPACE.
        ftrans.apply(loops[0:2], same_space=True)

        # Add an OpenMP parallel directive around the loop, then place the
        # reproducible OpenMP do directive inside it.
        loop = schedule.walk(LFRicLoop)[0]
        otrans.apply(loop)
        ltrans.apply(loop, {"reprod": True})

    # take a look at what we've done
    print(schedule.view())
    schedule.dag()
