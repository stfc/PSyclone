# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab

'''This module contains the OpenMPCPURoutineTrans metatransformation.'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Directive, Loop, Routine
)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.psyir.transformations.maximal_omp_parallel_region_trans import (
    MaximalOMPParallelRegionTrans)
from psyclone.psyir.transformations.omp_minimise_sync_trans import (
    OMPMinimiseSyncTrans)
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

    def apply(self, node: Routine, **kwargs):
        '''
        Applies the OMPLoopTrans, MaximalOMPParallelRegionTrans and
        OMPMinimiseSyncTrans to the relevant parts of the input
        node.

        :param node: The Routine node to transform
        '''

        # Split the options for the subtransformations.
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
                # Validate that this loop can be parallelised.
                ltrans.validate(loop, **loop_kwargs)
                # If it is, then apply the OMPLoopTrans
                ltrans.apply(loop, **loop_kwargs)
            except TransformationError:
                # If we fail to parallelise a loop we just skip it.
                continue

        # Apply the maximal openMP parallel region transformation to the
        # routine.
        momprtrans = MaximalOMPParallelRegionTrans()
        momprtrans.validate(node.children[:], **maxpar_kwargs)
        momprtrans.apply(node.children[:], **maxpar_kwargs)

        nowait = self.get_option("nowait", **kwargs)
        # If the asynchronous option was specified, then we need to apply the
        # OMPMinimiseSyncTrans as well.
        if nowait:
            minstrans = OMPMinimiseSyncTrans()
            minstrans.validate(node, **minsync_kwargs)
            minstrans.apply(node, **minsync_kwargs)


# For Sphinx AutoAPI documentation generation
__all__ = ["OMPCPURoutineTrans"]
