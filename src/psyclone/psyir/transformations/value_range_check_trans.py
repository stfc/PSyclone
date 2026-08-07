# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''This module contains the base class for verifying that input and output
   parameters of a region of code is not NAN and not infinite. It is basically
   identical to ReadOnlyVerifyTrans (which provides input parameter before
   and output parameter after a kernel), from which it inherits all
   actual code.
'''

from psyclone import psyGen
from psyclone.psyir.nodes import ValueRangeCheckNode
from psyclone.psyir import nodes
from psyclone.psyir.transformations.read_only_verify_trans \
    import ReadOnlyVerifyTrans


class ValueRangeCheckTrans(ReadOnlyVerifyTrans):
    '''This transformation inserts a ValueRangeCheckNode into the PSyIR of a
    schedule. At code creation time this node will use the PSyData API
    to create code that will verify all input parameters are not NANs
    and not infinite, and the same for all output parameters.

    After applying the transformation the Nodes marked for verification are
    children of the ValueRangeCheckNode.
    Nodes to verify can be individual constructs within an Invoke (e.g.
    Loops containing a Kernel or BuiltIn call) or entire Invokes.

    :param node_class: The class of Node which will be inserted
        into the tree (defaults to ValueRangeCheckNode), but can be any
        derived class.
    :type node_class: :py:class:`psyclone.psyir.nodes.ValueRangeCheckNode` or
        derived class

    '''
    # The types of node that this transformation can enclose
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        nodes.Directive, nodes.Literal, nodes.Reference)

    def __init__(self, node_class=ValueRangeCheckNode):
        # This function is only here to change the default node type
        super().__init__(node_class=node_class)

    def validate(self, node_list, options=None):
        '''Performs validation checks specific to nan-test
        transformations. This function is only here so that it
        is documented.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

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

        super().validate(node_list, options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ValueRangeCheckTrans"]
