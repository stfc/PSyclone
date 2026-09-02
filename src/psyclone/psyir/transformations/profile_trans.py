# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides the Profile transformation.
'''

from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import CodeBlock, ProfileNode, Return, Routine
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans


class ProfileTrans(PSyDataTrans):
    ''' Create a profile region around a list of statements. For
    example:

    >>> from psyclone.tests.utilities import get_psylayer_schedule
    >>> filename = "nemolite2d_alg_mod.f90"
    >>> schedule = get_psylayer_schedule(filename, api="gocean")
    >>>
    >>> from psyclone.psyir.transformations import ProfileTrans
    >>> p_trans = ProfileTrans()
    >>>
    >>> # Enclose all children within a single profile region
    >>> p_trans.apply(schedule.children)

    This implementation relies completely on the base class PSyDataTrans
    for the actual work, it only adjusts the name etc, and the list
    of valid nodes.

    '''
    # Unlike other transformations we can be fairly relaxed about the nodes
    # that a region can contain as we don't have to understand them.
    excluded_node_types = (Return,)

    def __init__(self):
        super().__init__(ProfileNode)

    def validate(self, nodes, options=None):
        '''
        Checks that the supplied list of nodes is valid for profiling
        callipers.

        :param nodes: a node or list of nodes to be instrumented with
                      profiling.
        :type nodes: :py:class:`psyclone.psyir.nodes.Node` or
                     list[:py:class:`psyclone.psyir.nodes.Node`]
        :param bool options["force"]: whether to ignore potential control
                                      flow jumps when applying this
                                      transformation. Default is False.

        :raises TransformationError: if the supplied region contains a
                                     potential control flow jump that could
                                     result in skipping the end of profiling
                                     caliper, e.g. EXIT or GOTO.
        '''
        if not options:
            options = {}
        forced = options.get("force", False)
        super().validate(nodes, options)
        if forced:
            return
        node_list = self.get_node_list(nodes)
        # If the node_list is the same as a whole routine then we skip the
        # checks for internal control flow jumps.
        parent = node_list[0].parent
        if (isinstance(parent, Routine) and
                len(parent.children) == len(node_list)):
            # If the node_list is the same size and the parent of the first
            # is the routine then this is the full Routine (see
            # RegionDirective.validate for the validation).
            return

        # Find all the codeblocks and check if they contain a control
        # flow jump.
        for node in node_list:
            codeblocks = node.walk(CodeBlock)
            for block in codeblocks:
                if block.has_potential_control_flow_jump():
                    raise TransformationError(
                        f"Cannot apply the ProfileTrans to a code region "
                        f"containing a potential control flow jump, as these "
                        f"could skip the end of profiling caliper. "
                        f"Found:\n'{block.debug_string()}'")
