# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the OpenMPCPURoutineTrans metatransformation.'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Directive, Loop, Routine
)
from psyclone.psyir.transformations.maximal_omp_parallel_region_trans import (
    MaximalOMPParallelRegionTrans)
from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.psyir.transformations.omp_minimise_sync_trans import (
    OMPMinimiseSyncTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class OMPCPURoutineTrans(Transformation):
    '''This metatransformation applies the OMPLoopTrans, the
    MaximalOMPParallelRegionTrans, and (optionally) the
    OMPMinimiseSyncTrans to the supplied Routine.

    This metatransformation can be used as an all-in-one method
    to parallelise routines with OpenMP CPU parallelism, and will
    attempt to maximise the amount of parallelism available. If the
    nowait option is set to True, it will also attempt to minimise the
    number of synchronisation points added into the parallel region.
    '''
    _SUB_TRANSFORMATIONS = [OMPLoopTrans, MaximalOMPParallelRegionTrans,
                            OMPMinimiseSyncTrans]

    def validate(self, node: Routine, **kwargs):
        '''
        Validates the input options of the OpenMPCPURoutineTrans.

        :param node: The Routine node to validate.
        '''
        # Validate the provided options are allowed and typed correctly.
        self.validate_options(**kwargs)
        # Sub transformation validation is done in the apply function
        # as the inputs are not known until computation is done
        # in the apply.

    def apply(self, node: Routine, **kwargs):
        '''
        Applies the OMPLoopTrans, MaximalOMPParallelRegionTrans and
        OMPMinimiseSyncTrans to the relevant parts of the input
        node.

        :param node: The Routine node to transform
        '''

        # Split the options for the subtransformations. The options are
        # returned in the order of the _SUB_TRANSFORMATIONS list.
        local_kwargs, loop_kwargs, maxpar_kwargs, minsync_kwargs = \
            self.split_kwargs(
                **kwargs
            )

        self.validate(node, **local_kwargs)

        # Find all of the loops.
        loops = node.walk(Loop)
        ltrans = OMPLoopTrans()
        for loop in loops:
            if loop.ancestor(Directive):
                continue  # Skip if an outer loop is already parallelised
            try:
                # Try to apply the OMPLoopTrans.
                ltrans.apply(loop, **loop_kwargs)
            except TransformationError:
                # If we fail to parallelise a loop we just skip it.
                continue

        # Apply the maximal openMP parallel region transformation to the
        # routine.
        momprtrans = MaximalOMPParallelRegionTrans()
        momprtrans.apply(node.children[:], **maxpar_kwargs)

        nowait = self.get_option("nowait", **kwargs)
        # If the asynchronous option was specified, then we need to apply the
        # OMPMinimiseSyncTrans as well.
        if nowait:
            minstrans = OMPMinimiseSyncTrans()
            minstrans.apply(node, **minsync_kwargs)


# For Sphinx AutoAPI documentation generation
__all__ = ["OMPCPURoutineTrans"]
