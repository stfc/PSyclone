# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council
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
# Modified by: R. W. Ford, STFC Daresbury Lab
#              S. Siso, STFC Daresbury Lab

'''
This module provides support for verification that read-only variables are
indeed not modified (especially accidentally overwritten). The code to
be verified may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke.

There is currently only one class in this module: ReadOnlyVerifyNode.
'''

from __future__ import absolute_import, print_function

from psyclone.core import VariablesAccessInfo
from psyclone.f2pygen import CommentGen
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
        return super(ReadOnlyVerifyNode, self).psy_data_body

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''
        Generates the code required for read-only verification of one or
        more Nodes. It uses the PSyData API (via the base class PSyDataNode)
        to create the required callbacks that will allow a library to
        validate that read-only data is not modified.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.
        '''

        # Determine the variables to validate:
        variables_info = VariablesAccessInfo(self)
        read_only = []
        for var_name in variables_info:
            if variables_info[var_name].is_read_only():
                read_only.append(var_name)

        options = {'pre_var_list': read_only,
                   'post_var_list': read_only}

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ReadOnlyVerifyStart"))
        parent.add(CommentGen(parent, ""))
        super(ReadOnlyVerifyNode, self).gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ReadOnlyVerifyEnd"))
        parent.add(CommentGen(parent, ""))

    def lower_to_language_level(self):
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.
        '''
        # Determine the variables to validate:
        variables_info = VariablesAccessInfo(self)
        read_only = []
        for var_name in variables_info:
            if variables_info[var_name].is_read_only():
                read_only.append(var_name)

        options = {'pre_var_list': read_only,
                   'post_var_list': read_only}

        super(ReadOnlyVerifyNode, self).lower_to_language_level(options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["ReadOnlyVerifyNode"]
