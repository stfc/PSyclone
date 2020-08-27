# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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


'''This module contains the base class for verifying read-only access in
   a region of code."
'''

from __future__ import absolute_import
from psyclone.psyir.nodes import ReadOnlyVerifyNode, Schedule
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class ReadOnlyVerifyTrans(PSyDataTrans):
    '''This transformation inserts a ReadOnlyVerifyNode or a node derived
    from ReadOnlyVerifyNode into the PSyIR of a schedule. At code creation
    time this node will use the PSyData API to create code that will
    verify that read-only quantities are not modified.

    After applying the transformation the Nodes marked for verification are
    children of the ReadOnlyVerifyNode.
    Nodes to verify can be individual constructs within an Invoke (e.g.
    Loops containing a Kernel or BuiltIn call) or entire Invokes.

    :param node_class: The class of Node which will be inserted \
        into the tree (defaults to ReadOnlyVerifyNode), but can be any \
        derived class.
    :type node_class: :py:class:`psyclone.psyir.nodes.ReadOnlyVerifyNode` or \
        derived class

    '''
    from psyclone.psyir import nodes
    from psyclone import psyGen
    # The types of node that this transformation can enclose
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive, nodes.Literal, nodes.Reference)

    def __init__(self, node_class=ReadOnlyVerifyNode):
        super(ReadOnlyVerifyTrans, self).__init__(node_class=node_class)

    def __str__(self):
        return ("Create a sub-tree of the PSyIR that has a "
                "ReadOnlyVerifyNode at its root.")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "ReadOnlyVerifyTrans"

    def validate(self, node_list, options=None):
        '''Performs validation checks specific to read-only-based
        transformations.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or NoneType

        :raises TransformationError: if distributed memory is configured.
        :raises TransformationError: if transformation is applied to a \
                                     Kernel or a BuiltIn call without its \
                                     parent Loop.
        :raises TransformationError: if transformation is applied to a Loop \
                                     without its parent Directive when \
                                     optimisations are applied.
        :raises TransformationError: if transformation is applied to an \
                                     orphaned Directive without its parent \
                                     Directive.

        '''
        # Check ReadOnlyVerifyTrans specific constraints.
        # Check constraints not covered by valid_node_types for
        # individual Nodes in node_list.
        from psyclone.psyir.nodes import Loop
        from psyclone.psyGen import Directive, \
            OMPParallelDirective, ACCParallelDirective

        for node in node_list:

            # Check that a ReadOnlyVerifyNode is not inserted between a Loop
            # and its parent Directive when optimisations are applied, as
            # this may result in including the end Directive for verification
            # but not the beginning.
            if isinstance(node, Loop) and isinstance(node.parent, Schedule) \
               and isinstance(node.parent.parent, Directive):
                raise TransformationError(
                    "Error in {0}: Extraction of a Loop without its parent "
                    "Directive is not allowed.".format(str(self.name)))

            # Check that the ReadOnlyVerifyNode is not inserted within a
            # thread parallel region when optimisations are applied.
            if node.ancestor((OMPParallelDirective, ACCParallelDirective)):
                raise TransformationError(
                    "Error in {0}: Extraction of Nodes enclosed within "
                    "a thread-parallel region is not allowed."
                    .format(str(self.name)))

        # Performs validation checks specific to PSyData-based
        # transformations.
        super(ReadOnlyVerifyTrans, self).validate(node_list, options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ReadOnlyVerifyTrans"]
