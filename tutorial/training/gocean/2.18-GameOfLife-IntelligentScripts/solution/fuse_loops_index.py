# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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

'''
Python script intended to be passed to PSyclone via the -s option.
It applies kernel inlining and then fuses the first three loops of
the first invoke.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.psyir.transformations import TransformationError
from psyclone.psyGen import InvokeSchedule


def apply_all(node_list, transform):
    '''This subroutine applies the specified transformation, which takes
    two consecutive nodes (e.g. loop fusion), and applies it to a block
    as large as possible. For example given six loops, of which the first
    three and the last two can be fused, it would result in:
    loop
       1, 2, 3
    loop
       4
    loop 5, 6

    :param node_list: list of all candidate nodes.
    :type node_list: list[:py:class:`psyclone.psyir.nodes.Node`]
    :param transform: the transformation to apply.
    :type transform: :py:class:`psyclone.psyGen.Transformation`
    '''

    # Create a copy in case that the caller needs the original list
    node_list = node_list[:]

    # Then try to combine consecutive nodes as much as possible
    while node_list:
        # Get and remove the first kernel
        current = node_list.pop(0)

        # Now check all 'next_node' nodes to see  if they can be transformed:
        ind = current.position
        while ind+1 < len(current.parent.children):
            next_node = current.parent.children[ind+1]
            # If next_node is NOT in the node list, don't even try
            # apply the transformation, it must be a wrong type
            if next_node not in node_list:
                break

            # Create a string for user feedback, containing the names of all
            # transformed kernels so far:
            current_name = "+".join(i.name for i in current.walk(GOKern))
            try:
                print(f"Applying {transform.name} on '{current_name}' and "
                      f"'{next_node.walk(GOKern)[0].name}'.")
                transform.apply(current, next_node)
            except TransformationError as err:
                print(f"Cannot apply {transform.name}:", str(err.value))
                break

            # Remove the transformed sibling, then keep on transforming
            node_list.remove(next_node)
            # Note that we don't need to increase `ind`: the previous `ind`
            # loop has been removed from the parent, so `ind` is now already
            # the next loop


# -----------------------------------------------------------------------------
def trans(psyir):
    '''
    Take the supplied psy object, apply module inlining and fuse loops as
    much as possible.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    module_inline = KernelModuleInlineTrans()
    for kern in schedule.walk(GOKern):
        module_inline.apply(kern)

    # Collect all outer loops
    outer_loops = []
    for loop in schedule.walk(GOLoop):
        if loop.loop_type == "outer":
            outer_loops.append(loop)

    fuse = GOceanLoopFuseTrans()
    apply_all(outer_loops, fuse)

    for outer in outer_loops:
        # Note that some of the loops in outer_loops are not part of
        # the tree anymore, since their loop body has been fused with
        # the previous loop. Additionally, if a loop has only one
        # child, no need to try fusing one child.
        if len(outer.loop_body.children) > 1:
            apply_all(outer.loop_body.children, fuse)
