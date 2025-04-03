# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by: R. W. Ford, STFC Daresbury Lab
#              S. Siso, STFC Daresbury Lab
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
