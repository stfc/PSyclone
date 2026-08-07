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
from psyclone.psyir.nodes import Directive
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.transformations import OMPParallelLoopTrans, OMPLoopTrans, \
    TransformationError

# Get the transformation we will apply
OMP_TRANS = OMPParallelLoopTrans()
OMP_LOOP_TRANS = OMPLoopTrans()
OMP_PARALLEL_TRANS = OMPParallelTrans()

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})


def trans(psyir):
    ''' Transform a specific Schedule by making all loops
    over vertical levels OpenMP parallel.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    loops = [loop for loop in psyir.walk(Loop) if loop.loop_type == "levels"]
    idx = 0
    # Loop over each of these loops over levels to see which neighbour each
    # other in the Schedule and thus can be put in a single parallel region.
    while idx < len(loops):
        child = loops[idx]
        posn = child.parent.children.index(child)
        loop_list = [child]
        current = idx + 1
        # Look at the children of the parent of the current node, starting
        # from the immediate sibling of the current node
        for sibling in child.parent.children[posn+1:]:
            # Is this immediate sibling also in our list of loops?
            if current < len(loops) and sibling is loops[current]:
                # It is so add it to the list and move on to the next sibling
                loop_list.append(sibling)
                current += 1
            else:
                # It's not so that's the end of the list of nodes that we
                # can enclose in a single parallel region
                break
        idx = current

        try:
            OMP_PARALLEL_TRANS.apply(loop_list)
            for loop in loop_list:
                OMP_LOOP_TRANS.apply(loop)
        except TransformationError:
            pass

    directives = psyir.walk(Directive)
    print(f"Added {len(directives)} Directives")

    # Display the transformed PSyIR
    print(psyir.view())
