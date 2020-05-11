# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the CodeBlock node implementation.'''

from enum import Enum
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode


class CodeBlock(Statement, DataNode):
    '''Node representing some generic Fortran code that PSyclone does not
    attempt to manipulate. As such it is a leaf in the PSyIR and therefore
    has no children.

    :param fp2_nodes: list of fparser2 AST nodes representing the Fortran \
                      code constituting the code block.
    :type fp2_nodes: list of :py:class:`fparser.two.utils.Base`
    :param structure: argument indicating whether this code block is a \
    statement or an expression.
    :type structure: :py:class:`psyclone.psyir.nodes.CodeBlock.Structure`
    :param parent: the parent node of this code block in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "CodeBlock"
    _colour_key = "CodeBlock"

    class Structure(Enum):
        '''
        Enumeration that captures the structure of the code block which
        may be required when processing.

        '''
        # The Code Block comprises one or more Fortran statements
        # (which themselves may contain expressions).
        STATEMENT = 1
        # The Code Block comprises one or more Fortran expressions.
        EXPRESSION = 2

    def __init__(self, fp2_nodes, structure, parent=None):
        super(CodeBlock, self).__init__(parent=parent)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the contents of the list because
        # the list itself is a temporary product of the process of converting
        # from the fparser2 AST to the PSyIR.
        self._fp2_nodes = fp2_nodes[:]
        # Store references back into the fparser2 AST
        if fp2_nodes:
            self.ast = self._fp2_nodes[0]
            self.ast_end = self._fp2_nodes[-1]
        else:
            self.ast = None
            self.ast_end = None
        # Store the structure of the code block.
        self._structure = structure

    @property
    def structure(self):
        '''
        :returns: whether this code block is a statement or an expression.
        :rtype: :py:class:`psyclone.psyir.nodes.CodeBlock.Structure`

        '''
        return self._structure

    @property
    def get_ast_nodes(self):
        '''
        :returns: the list of nodes associated with this code block in \
        the original AST.
        :rtype: list of subclass of \
        `:py:classfparser.two.Fortran2003.Base`

        '''
        return self._fp2_nodes

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include control codes for colour.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + \
            "[" + str(list(map(type, self._fp2_nodes))) + "]"

    def __str__(self):
        return "CodeBlock[{0} nodes]".format(len(self._fp2_nodes))
