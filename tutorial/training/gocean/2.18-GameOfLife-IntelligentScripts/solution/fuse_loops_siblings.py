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
It applies kernel inlining and then loop fusion to all kernel calls.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.gocean1p0 import GOKern, GOLoop
from psyclone.psyir.transformations import TransformationError
from psyclone.psyGen import InvokeSchedule


def apply_all(node_list, transform):
    '''This subroutine takes a transformation that merges two consecutive
    nodes in the tree, and applies it repeatedly to merge as many
    loops as possible.

    This is mostly intended to use the LoopFusion transformation, but shows
    how to write meta-transformations to repeatedly apply an existing
    transformation to blocks of code.

    As an example: given six loops, of which the first three and the last
    two can be fused, calling this function with LoopFusion would result in:
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
        # Important to make a copy, otherwise as we delete nodes we
        # remove them from the PSyIR tree!!
        siblings = current.siblings[:]

        # The siblings list contains all siblings including the current node.
        # Delete all previous siblings:
        while siblings[0] is not current:
            del siblings[0]

        # Remove the current node, so we have only all following nodes:
        del siblings[0]

        # Now see if current and following sibling can be combined:
        while siblings:
            # If the next node is NOT an outer loop, don't even try to apply
            # the transformation. We could also use isinstance(GOLoop) and
            # loop_type, but using the existing list of all outer_loops is
            # shorter:
            if siblings[0] not in node_list:
                break
            # Create a string for user feedback, containing the names of all
            # transformed kernels:
            current_name = "+".join(i.name for i in current.walk(GOKern))
            try:
                print(f"Applying {transform.name} on '{current_name}' and "
                      f"'{siblings[0].walk(GOKern)[0].name}'.")
                transform.apply(current, siblings[0])
            except TransformationError as err:
                print("Cannot apply {transform.name} because", str(err.value))
                break

            # Remove the transformed sibling - first from the list of outer
            # loops (we don't need to test transformed loops in the outer loopq
            # again), then from the list of siblings, so the while loop
            # will now try to transform the next sibling
            node_list.remove(siblings[0])
            del siblings[0]


def trans(psyir):
    '''
    Take the supplied psyir object, apply module inlining, and fuse all loops
    as much as possible.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    # Inline all kernels to help gfortran with inlining.
    km_inline = KernelModuleInlineTrans()
    for kern in schedule.walk(GOKern):
        km_inline.apply(kern)

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
