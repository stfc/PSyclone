# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
This module provides support for verifying that the real inputs and outputs
of a kernel are valid numbers (i.e. neither NAN nor infinite).
'''

from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ValueRangeCheckNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    NAN-checking using the NanTestTrans transformation. The Nodes
    marked for checking become children of (the Schedule of) a
    ValueRangeCheckNode.

    '''
    # Textual description of the node.
    _text_name = "ValueRangeCheck"
    _colour = "green"
    # The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = "value_range_check"

    @property
    def value_range_check_body(self):
        '''
        :returns: the Schedule associated with this ValueRangeCheckNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super().psy_data_body

    def _get_var_lists(self):
        '''This method uses the CallTreeUtils to get all input-
        and output-variables. They are added to a dictionary, which
        will be provided to the code creation method in the base class.

        :returns: dictionary with key/values for pre_var_list and
            post_var_list.
        :rtype: Dict[str, List[Tuple[str,:py:class:`psyclone.core.Signature`]]]

        '''
        # This cannot be moved to the top, it would cause a circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.call_tree_utils import CallTreeUtils

        ctu = CallTreeUtils()
        read_write_info = ctu.get_in_out_parameters(self)
        return {'pre_var_list': read_write_info.read_list,
                'post_var_list': read_write_info.write_list}

    def lower_to_language_level(self):
        # pylint: disable=arguments-differ
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        options = self._get_var_lists()
        return super().lower_to_language_level(options)


# For AutoAPI documentation generation
__all__ = ['ValueRangeCheckNode']
