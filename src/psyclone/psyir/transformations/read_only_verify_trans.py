# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''This module contains the base class for verifying read-only access in
   a region of code."
'''

from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import (Literal, Loop, ReadOnlyVerifyNode, Directive,
                                  Reference, Schedule, OMPParallelDirective,
                                  ACCParallelDirective)
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
    # The types of node that this transformation can enclose
    valid_node_types = (Loop, Kern, BuiltIn, Directive, Literal, Reference)

    def __init__(self, node_class=ReadOnlyVerifyNode):
        super().__init__(node_class=node_class)

    # -------------------------------------------------------------------------
    def validate(self, node_list, options=None):
        # pylint: disable=arguments-renamed
        '''Performs validation checks specific to read-only-based
        transformations.

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
        # Check ReadOnlyVerifyTrans specific constraints.
        # Check constraints not covered by valid_node_types for
        # individual Nodes in node_list.
        for node in node_list:

            # Check that a ReadOnlyVerifyNode is not inserted between a Loop
            # and its parent Directive when optimisations are applied, as
            # this may result in including the end Directive for verification
            # but not the beginning.
            if isinstance(node, Loop) and isinstance(node.parent, Schedule) \
               and isinstance(node.parent.parent, Directive):
                raise TransformationError(
                    f"Error in {self.name}: Application to a Loop without its "
                    f"parent Directive is not allowed.")

            # Check that the ReadOnlyVerifyNode is not inserted within a
            # thread parallel region when optimisations are applied.
            if node.ancestor((OMPParallelDirective, ACCParallelDirective)):
                raise TransformationError(
                    f"Error in {self.name}: Application to Nodes enclosed "
                    f"within a thread-parallel region is not allowed.")

        # Performs validation checks specific to PSyData-based
        # transformations.
        super().validate(node_list, options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ReadOnlyVerifyTrans"]
