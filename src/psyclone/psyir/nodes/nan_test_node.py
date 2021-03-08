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
# Author J. Henrichs, Bureau of Meteorology
# Modified by: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''
This module provides support for verifying that the real inputs and outputs
of a kernel are valid numbers (i.e. neither NAN nor infinite).
'''

from __future__ import absolute_import, print_function

from psyclone.f2pygen import CommentGen
from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class NanTestNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    NAN-checking using the NanTestTrans transformation. By
    applying the transformation the Nodes marked for checking become
    children of (the Schedule of) a NanTestNode.

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options provided via transformations.
    :type options: dictionary of string:values or None
    :param str options["prefix"]: a prefix to use for the PSyData module name \
        (``prefix_psy_data_mod``) and the PSyDataType (``prefix_PSyDataType``)\
        - a "_" will be added automatically. It defaults to "``nan_test_``", \
        which means the module name used will be ``nan_test_psy_data_mode``, \
        and the data type ``nan_test_PSyDataType``.

    '''
    # Textual description of the node.
    _text_name = "NanTest"
    _colour = "green"

    def __init__(self, ast=None, children=None, parent=None, options=None):
        if options:
            my_options = options.copy()
        else:
            my_options = {}
        # If there is no value specified in the constructor, default
        # to the "nan_test" class.
        my_options["prefix"] = my_options.get("prefix", "nan_test")
        super(NanTestNode, self).__init__(ast=ast, children=children,
                                          parent=parent, options=my_options)

    @property
    def nan_test_body(self):
        '''
        :returns: the Schedule associated with this NanTestNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super(NanTestNode, self).psy_data_body

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of NanTestNode.
        :rtype: str
        '''
        return "nan_test_" + str(self.position)

    def update_vars_and_postname(self):
        '''
        This function is called after the variables to be checked
        have been stored in self._input_list and self._output_list.
        It can be used to e.g. remove unnecessary variables (e.g. loop
        counter), or adjust the postfix to assure that no duplicated
        variable name is created. This default function does not
        do anything atm.
        '''

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''
        Generates the code required for NAN/infinite verification of the
        parameters of one or more Nodes. It uses the PSyData API (via
        the base class PSyDataNode) to create the required callbacks
        that will allow a library to do checks on parameters.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.

        '''

        # pylint: disable=import-outside-toplevel
        # This cannot be moved to the top, it would cause a circular import
        from psyclone.psyir.tools.dependency_tools import DependencyTools
        # Determine the variables to check:
        dep = DependencyTools()
        input_list, output_list = dep.get_in_out_parameters(self)

        options = {'pre_var_list': input_list,
                   'post_var_list': output_list}

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " NanTestStart"))
        parent.add(CommentGen(parent, ""))
        super(NanTestNode, self).gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " NanTestEnd"))
        parent.add(CommentGen(parent, ""))
