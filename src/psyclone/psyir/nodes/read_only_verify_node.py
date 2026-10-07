# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module provides support for verification that read-only variables are
indeed not modified (especially accidentally overwritten). The code to
be verified may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke.

There is currently only one class in this module: ReadOnlyVerifyNode.

'''

from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ReadOnlyVerifyNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    read-only-verification. By applying the ReadOnlyVerifyTrans
    transformation, the Nodes marked for extraction become
    children of (the Schedule of) a ReadOnlyVerifyNode.

    '''
    _text_name = "ReadOnlyVerify"
    _colour = "green"
    # The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = "read_only_verify"

    @property
    def read_only_verify_body(self):
        '''
        :returns: the Schedule associated with this ExtractNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super().psy_data_body

    def lower_to_language_level(self):
        # pylint: disable=arguments-differ
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.call_tree_utils import CallTreeUtils
        # Determine the variables to write:
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools import ReadWriteInfo
        ctu = CallTreeUtils()
        read_write_info = ReadWriteInfo()
        ctu.get_input_parameters(read_write_info, [self],
                                 include_non_data_accesses=True)
        options = {'pre_var_list': read_write_info.read_list,
                   'post_var_list': read_write_info.read_list}

        return super().lower_to_language_level(options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ReadOnlyVerifyNode"]
