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

'''
This module provides support for verification that read-only variables are
indeed not modified (especially accidentally overwritten). The code to
be verified may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke.

There is currently only one class in this module: ReadOnlyVerifyNode.
'''

from __future__ import absolute_import, print_function
from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ReadOnlyVerifyNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    read-only-verification. By applying the ReadOnlyVerifyTrans
    transformation, the Nodes marked for extraction become
    children of (the Schedule of) an ReadOnlyVerifyNode.

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options provided via transformations.
    :type options: dict of string:values or NoneType

    '''
    def __init__(self, ast=None, children=None, parent=None, options=None):
        if options:
            my_options = options.copy()
        else:
            my_options = {}

        # If there is no value passed to the constructor, default
        # to the "read_only_verify" prefix.
        my_options["prefix"] = my_options.get("prefix", "read_only_verify")
        super(ReadOnlyVerifyNode, self).__init__(ast=ast, children=children,
                                                 parent=parent,
                                                 options=my_options)
        self._text_name = "ReadOnlyVerify"
        self._colour = "green"

    @property
    def read_only_verify_body(self):
        '''
        :returns: the Schedule associated with this ExtractNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super(ReadOnlyVerifyNode, self).psy_data_body

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "read_only_verify_" + str(self.position)

    def update_vars_and_postname(self):
        '''
        This function is called after the variables to be verified
        have been stored in self._input_list and self._output_list.
        This default function does not do anything for read-only
        verification.
        '''

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
        from psyclone.core.access_info import VariablesAccessInfo
        variables_info = VariablesAccessInfo(self)
        read_only = []
        for var_name in variables_info:
            if variables_info[var_name].is_read_only():
                read_only.append(var_name)

        # Add a callback here so that derived classes can adjust the list
        # of variables to provide, or the suffix used (which might
        # depend on the variable name which could create clashes).
        self.update_vars_and_postname()

        options = {'pre_var_list': read_only,
                   'post_var_list': read_only}

        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ReadOnlyVerifyStart"))
        parent.add(CommentGen(parent, ""))
        super(ReadOnlyVerifyNode, self).gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ReadOnlyVerifyEnd"))
        parent.add(CommentGen(parent, ""))


# ============================================================================
# For automatic documentation creation:
__all__ = ["ReadOnlyVerifyNode"]
