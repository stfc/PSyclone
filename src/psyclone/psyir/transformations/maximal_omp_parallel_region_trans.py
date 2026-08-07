# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the MaximalOMPParallelRegionTrans.'''

from typing import Union

from psyclone.psyir.nodes import (
        OMPTaskwaitDirective,
        OMPBarrierDirective,
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
        Node,
        Schedule
)
from psyclone.psyir.transformations.maximal_region_trans import (
        MaximalRegionTrans)
from psyclone.psyir.transformations.omp_parallel_trans import OMPParallelTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class MaximalOMPParallelRegionTrans(MaximalRegionTrans):
    '''Applies OpenMP Parallel directives around the largest possible sections
    of the input.

    At current, this will never place OpenMP parallel sections around
    Assignments that are outside of another OpenMP directive. See #3157 and
    the discussion on #3205 for more detail.'''
    # The type of parallel transformation to be applied to the input region.
    _transformation = OMPParallelTrans
    _SUB_TRANSFORMATIONS = [OMPParallelTrans]
    # Tuple of statement nodes allowed inside the _transformation
    _allowed_contiguous_statements = (
        OMPTaskwaitDirective,
        OMPBarrierDirective,
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
    )
    _required_nodes = (
        OMPSerialDirective,
        OMPTaskloopDirective,
        OMPDoDirective,
        OMPLoopDirective,
        OMPTaskDirective,
        DynamicOMPTaskDirective,
    )

    def apply(self, nodes: Union[Node, Schedule, list[Node]], **kwargs):
        '''Applies the transformation to the nodes provided.

        :param nodes: can be a single node, a schedule or a list of nodes.
        '''
        super().apply(nodes, **kwargs)
