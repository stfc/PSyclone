# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

'''A simple transformation script for the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone. See README.md in the
top-level psyclone directory.

Once you have PSyclone installed, this script may be used by doing:

 >>> psyclone -api "nemo" -s ./omp_trans.py my_file.F90

This should produce a lot of output, ending with generated
Fortran.

'''
from psyclone.psyir.nodes import Loop
from psyclone.psyir.nodes import Directive
from psyclone.transformations import OMPParallelLoopTrans, OMPLoopTrans, \
    OMPParallelTrans, TransformationError

# Get the transformation we will apply
OMP_TRANS = OMPParallelLoopTrans()
OMP_LOOP_TRANS = OMPLoopTrans()
OMP_PARALLEL_TRANS = OMPParallelTrans()


def trans(psy):
    ''' Transform a specific Schedule by making all loops
    over vertical levels OpenMP parallel.

    :param psy: the object holding all information on the PSy layer \
                to be modified.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object
    :rtype:  :py:class:`psyclone.psyGen.PSy`

    '''
    # Get the Schedule of the target routine
    sched = psy.invokes.get('tra_adv').schedule

    loops = [loop for loop in sched.walk(Loop) if loop.loop_type == "levels"]
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

    directives = sched.walk(Directive)
    print(f"Added {len(directives)} Directives")

    # Display the transformed PSyIR
    print(sched.view())

    # Return the modified psy object
    return psy
