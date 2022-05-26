# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

'''This module provides the generic loop fusion class, which is the base
class for all API-specific loop fusion transformations.
'''

from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class LoopFuseTrans(LoopTrans):
    ''' Provides a generic loop-fuse transformation to two Nodes in the
    PSyIR of a Schedule after performing validity checks for the supplied
    Nodes. Examples are given in the descriptions of any children classes.

    '''
    def __str__(self):
        return "Fuse two adjacent loops together"

    def validate(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
        ''' Performs various checks to ensure that it is valid to apply
        the LoopFuseTrans transformation to the supplied Nodes.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if one or both of the Nodes is/are not \
                                     a :py:class:`psyclone.psyir.nodes.Loop`.
        :raises TransformationError: if one or both Nodes are not fully-formed.
        :raises TransformationError: if the Nodes do not have the same parent.
        :raises TransformationError: if the Nodes are not next to each \
                                     other in the tree.
        :raises TransformationError: if the two Loops do not have the same \
                                     iteration space.
        '''
        # Check that the supplied Nodes are Loops
        super(LoopFuseTrans, self).validate(node1, options=options)
        super(LoopFuseTrans, self).validate(node2, options=options)

        # Check loop1 and loop2 have the same parent
        if not node1.sameParent(node2):
            raise TransformationError(
                f"Error in {self.name} transformation. Loops do not have "
                f"the same parent.")

        # Check node1 and node2 are next to each other
        if abs(node1.position-node2.position) != 1:
            raise TransformationError(
                f"Error in {self.name} transformation. Nodes are not siblings "
                f"who are next to each other.")
        # Check that the iteration space is the same
        if node1.iteration_space != node2.iteration_space:
            raise TransformationError(
                f"Error in {self.name} transformation. Loops do not have the "
                f"same iteration space.")

    def apply(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
        ''' Fuses two loops represented by `psyclone.psyir.nodes.Node` objects
        after performing validity checks.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        # Validity checks for the supplied nodes
        self.validate(node1, node2, options=options)

        schedule = node1.root

        # Remove node2 from the parent
        node2.detach()

        # Add loop contents of node2 to node1
        node1.loop_body.children.extend(node2.loop_body.pop_all_children())


# For automatic documentation generation
__all__ = ["LoopFuseTrans"]
