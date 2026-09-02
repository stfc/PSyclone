#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyclone transformation script showing the introduction of OpenMP
directives into generic code and using transformation options to resolve
reported dependencies. '''

from psyclone.psyir.transformations import TransformationError, OMPLoopTrans
from psyclone.psyir.nodes import Loop


def trans(psyir):
    ''' Add OpenMP Parallel Loop directives by using available transformation
    options.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_trans = OMPLoopTrans()
    omp_trans.omp_directive = "paralleldo"

    # Select all outer loops
    loops = psyir.walk(Loop, stop_type=Loop)

    for loop in loops:
        # Only the first loop can be parallelised without options (the
        # 'verbose=True' transformation option would add a comments
        # explaining the reason why other loops are not parallelised
        # and providing suggestions)
        try:
            omp_trans.apply(loop)
        except TransformationError:
            pass

    # In this example we will use the available options to parallelise the
    # remaining loops:

    # The second loop has a CodeBlock, we can parallelise it by disabling
    # node_type_check
    omp_trans.apply(loops[1], node_type_check=False)

    # The third loop has a reduction, we can parallelise it by enabling
    # reductions
    omp_trans.apply(loops[2], enable_reductions=True)

    # The forth loop has a temporary array, we can parallelise it by providing
    # the array names that could be privatised
    omp_trans.apply(loops[3], privatise_arrays=True)

    # The fifth loop has a false dependency, we can parallelise it by providing
    # the false dependency to 'ignore_dependencies_for'
    omp_trans.apply(loops[4], ignore_dependencies_for=["a"])

    # The sixth loop has a dependency that would be resolved if it was private,
    # we can parallelise it by providing 'ignore_dependencies_for' and
    # 'force_private'
    omp_trans.apply(loops[5], ignore_dependencies_for=["a"],
                    force_private=["a"])
