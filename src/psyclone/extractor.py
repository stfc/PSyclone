# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# -----------------------------------------------------------------------------

'''
This module provides support for extraction of code within a specified
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).

There is currently only one class in this module: ExtractNode (see below).

Another class which contains helper functions for code extraction, such as
wrapping up settings for generating driver for the extracted code, will
be added in Issue #298.
'''

from __future__ import absolute_import, print_function
from psyclone.psyGen import Node


class ExtractNode(Node):
    '''
    This class can be inserted into a Schedule to mark Nodes for \
    code extraction using the ExtractRegionTrans transformation. By \
    applying the transformation the Nodes marked for extraction become \
    children of an ExtractNode.

    :param ast: reference into the fparser2 parse tree corresponding to \
                this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, ast=None, children=None, parent=None):
        super(ExtractNode, self).__init__(ast, children, parent)
        self._text_name = "Extract"
        self._colour_key = "Extract"

    def __str__(self):
        '''
        Returns a string representation of the subtree starting at the \
        Extract Node.

        :returns: the Extract Node with all its children.
        :rtype: str
        '''
        result = "ExtractStart\n"
        for child in self.children:
            result += str(child) + "\n"
        return result + "ExtractEnd"

    @property
    def dag_name(self):
        '''
        Returns the name to use in a DAG for this Node

        :returns: the dag name of ExtractNode.
        :rtype: str
        '''
        return "extract_" + str(self.position)

    def gen_code(self, parent):
        '''
        Generates the code required for extraction of one or more Nodes. \
        For now it inserts comments before and after the code belonging \
        to all the children of this ExtractNode. These comments will be \
        replaced by calls to write out arguments of extracted Node(s) or \
        Kernel(s) in Issue #234.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(
            parent, " CALL write_extract_arguments(argument_list)"))
        parent.add(CommentGen(parent, ""))
        for child in self.children:
            child.gen_code(parent)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))
