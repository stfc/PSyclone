# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A simple transformation script for the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone. See README.md in the
top-level psyclone directory.

Once you have PSyclone installed, this script may be used by doing:

 >>> psyclone -s ./omp_trans.py my_file.F90

This should produce a lot of output, ending with generated
Fortran.

'''
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPParallelLoopTrans, OMPLoopTrans, \
    TransformationError

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})

# Get the transformation we will apply
OMP_TRANS = OMPParallelLoopTrans()
OMP_LOOP_TRANS = OMPLoopTrans()
OMP_PARALLEL_TRANS = OMPParallelTrans()


def trans(psyir):
    ''' Transform a specific Schedule by making all loops
    over vertical levels OpenMP parallel. Encloses children 6-9 of the
    outer iteration loop within a single OpenMP parallel region.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    for loop in psyir.walk(Loop, stop_type=Loop):
        if loop.loop_type == "levels":
            try:
                OMP_TRANS.apply(loop)
            except TransformationError as err:
                print(f"Could not parallelise:\n{loop.debug_string()}"
                      f"because:\n{err.value}")

    # Find body of the iteration loop (identified as a 'tracer' loop)
    it_loop_body = None
    for loop in psyir.walk(Loop):
        if loop.loop_type == "tracers":
            it_loop_body = loop.loop_body
            break

    # Put an OMP parallel do around all suitable loops except 6-9
    for child in it_loop_body.children[0:6] + it_loop_body.children[10:]:
        if isinstance(child, Loop) and child.loop_type == "levels":
            OMP_TRANS.apply(child)

    # Put an OMP loop around each of loops 6-9
    for child in it_loop_body.children[6:10]:
        OMP_LOOP_TRANS.apply(child)

    # Enclose loops 6-9 within a single OMP parallel region
    OMP_PARALLEL_TRANS.apply(it_loop_body.children[6:10])
