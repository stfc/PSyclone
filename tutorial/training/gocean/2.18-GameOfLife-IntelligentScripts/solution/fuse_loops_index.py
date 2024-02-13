# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel fuseion code to
all invokes.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyir.transformations import TransformationError
from psyclone.gocean1p0 import GOKern, GOLoop


def trans(psy):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    invoke = psy.invokes.get("invoke_compute")
    schedule = invoke.schedule

    # Inline all kernels to help gfortran with inlining.
    inline = KernelModuleInlineTrans()
    for kern in schedule.walk(GOKern):
        inline.apply(kern)

    # Collect all outer loops
    outer_loops = []
    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            outer_loops.append(loop)

    fuse = GOceanLoopFuseTrans()
    # Then try to combine consecutive nodes as much as possible
    while outer_loops:
        # Get and remove the first kernel
        current = outer_loops.pop(0)

        # Now check all 'next' nodes to see  if they can be fused:
        ind = current.position
        while ind+1 < len(current.parent.children):
            next = current.parent.children[ind+1]
            # If the next node is NOT an outer loop, don't try to fuse
            if next not in outer_loops:
                break
            # Create a string for user feedback, containing the names of all
            # fused kernels:
            current_name =  "+".join(i.name for i in current.walk(GOKern))
            try:
                print(f"Fusing '{current_name}' and "
                      f"'{next.walk(GOKern)[0].name}'.")
                fuse.apply(current, next)
            except TransformationError as err:
                print("Cannot fuse because", str(err.value))
                break

            # Remove the fused sibling, then keep on fusing
            outer_loops.remove(next)
            # Note that we don't need to increase ind: the previous 'ind'
            # loop has been removed from the parent, so 'ind' is now already
            # the next loop
