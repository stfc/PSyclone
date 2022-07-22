# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''
This module provides support for array access tracing. The code to
be verified may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke.

There is currently only one class in this module: TracingNode.
'''

from psyclone.f2pygen import CommentGen
from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class TracingNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    array access tracing. By applying the TracingTrans
    transformation, the Nodes marked for tracing become
    children of (the Schedule of) a TracingNode.

    '''
    _text_name = "Tracing"
    _colour = "green"
    # The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = "tracing"

    @property
    def tracing_body(self):
        '''
        :returns: the Schedule associated with this TracingNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super().psy_data_body

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''
        Generates the code required for tracing array access of one or
        more Nodes. It uses the PSyData API (via the base class PSyDataNode)
        to create the required callbacks that will allow a library to
        trace array accesses.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.
        '''

        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.dependency_tools import DependencyTools

        # Determine the variables to write:
        dep = DependencyTools()
        # Determine the variables to validate:
        input_list, output_list = dep.get_in_out_parameters(self)
        # Variables read will be provided before the kernel call, any
        # variable modified will be provided at the end ()
        options = {'pre_var_list': input_list,
                   'post_var_list': output_list}

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " TracingStart"))
        parent.add(CommentGen(parent, ""))
        super().gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " TracingEnd"))
        parent.add(CommentGen(parent, ""))

    def lower_to_language_level(self):
        # pylint: disable=arguments-differ
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.
        '''

        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.dependency_tools import DependencyTools
        # Determine the variables to write:
        dep = DependencyTools()
        # Determine the variables to validate:
        input_list, output_list = dep.get_in_out_parameters(self)
        options = {"pre_var_list":  input_list,
                   "post_var_list": output_list}

        super().lower_to_language_level(options)


# ============================================================================
# For automatic documentation creation:
__all__ = ["TracingNode"]
