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
from psyclone.transformations import LFRicOMPParallelLoopTrans


def trans(psyir):
    ''' PSyclone transformation script for the LFRic API to apply
    loop fusion and OpenMP for a particular example.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    otrans = LFRicOMPParallelLoopTrans()
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

        # Add an OpenMP parallel do directive to the resultant loop-fused loop
        otrans.apply(schedule.children[0])

    # take a look at what we've done
    print(schedule.view())
    schedule.dag(file_format="png")
