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


'''This module contains the base class for verifying that input and output
   parameters of a region of code is not NAN and not infinite. It is basically
   identical to ReadOnlyVerifyTrans (which provides input parameter before
   and output parameter after a kernel), from which it inherits all
   actual code.
'''

from __future__ import absolute_import

from psyclone import psyGen
from psyclone.psyir.nodes import NanTestNode
from psyclone.psyir import nodes
from psyclone.psyir.transformations.read_only_verify_trans \
    import ReadOnlyVerifyTrans


class NanTestTrans(ReadOnlyVerifyTrans):
    '''This transformation inserts a NanTestNode into the PSyIR of a
    schedule. At code creation time this node will use the PSyData API
    to create code that will verify all input parameters are not NANs
    and not infinite, and the same for all output parameters.

    After applying the transformation the Nodes marked for verification are
    children of the NanTestNode.
    Nodes to verify can be individual constructs within an Invoke (e.g.
    Loops containing a Kernel or BuiltIn call) or entire Invokes.

    :param node_class: The class of Node which will be inserted \
        into the tree (defaults to NanTestNode), but can be any \
        derived class.
    :type node_class: :py:class:`psyclone.psyir.nodes.NanTestNode` or \
        derived class

    '''
    # The types of node that this transformation can enclose
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive, nodes.Literal, nodes.Reference)

    def __init__(self, node_class=NanTestNode):
        # This function is only here to change the default node type
        super(NanTestTrans, self).__init__(node_class=node_class)

    def validate(self, node_list, options=None):
        '''Performs validation checks specific to nan-test
        transformations. This function is only here so that it
        is documented.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or NoneType

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
        # pylint: disable=useless-super-delegation

        super(NanTestTrans, self).validate(node_list, options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["NanTestTrans"]
