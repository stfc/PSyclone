# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
from fparser.two import Fortran2003
from fparser.two.utils import walk
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode


class CodeBlock(Statement, DataNode):
    '''Node representing some generic Fortran code that PSyclone does not
    attempt to manipulate. As such it is a leaf in the PSyIR and therefore
    has no children.

    :param fp2_nodes: the fparser2 parse-tree nodes representing the
        Fortran code constituting the code block.
    :type fp2_nodes: list[:py:class:`fparser.two.utils.Base`]
    :param structure: argument indicating whether this code block is a
        statement or an expression.
    :type structure: :py:class:`psyclone.psyir.nodes.CodeBlock.Structure`
    :param parent: the parent node of this code block in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param annotations: tags that provide additional information about
        the node. The node should still be functionally correct when
        ignoring these tags.
    :type annotations: list[str | NoneType]

    '''
    #: Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "CodeBlock"
    _colour = "red"
    #: The annotations that are supported by this node.
    #: psy-data-start - this node has replaced a PSyDataNode during the
    #: lowering of the PSyIR to language level.
    valid_annotations = ("psy-data-start")

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

    def __init__(self, fp2_nodes, structure, parent=None, annotations=None):
        super(CodeBlock, self).__init__(parent=parent, annotations=annotations)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the contents of the list because
        # the list itself is a temporary product of the process of converting
        # from the fparser2 parse tree to the PSyIR.
        self._fp2_nodes = fp2_nodes[:]
        # Store references back into the fparser2 parse tree.
        if fp2_nodes:
            self.ast = self._fp2_nodes[0]
            self.ast_end = self._fp2_nodes[-1]
        else:
            self.ast = None
            self.ast_end = None
        # Store the structure of the code block.
        self._structure = structure

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two CodeBlock nodes are equal
        if they are the same type, their ast_nodes lists are equal (which
        means the same instance) and have the same structure.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.get_ast_nodes == other.get_ast_nodes
        is_eq = is_eq and self.structure == other.structure

        return is_eq

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
        :returns: the nodes associated with this code block in
                  the original fparser2 parse tree.
        :rtype: list[:py:class:`fparser.two.Fortran2003.Base`]

        '''
        return self._fp2_nodes

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include control codes for colour.

        :return: text description of this node.
        :rtype: str
        '''
        return (f"{self.coloured_name(colour)}["
                f"{list(map(type, self._fp2_nodes))}]")

    def get_symbol_names(self):
        '''
        :returns: the list of symbol names used inside the CodeBock.
        :rtype: list[str]
        '''
        parse_tree = self.get_ast_nodes
        result = []
        for node in walk(parse_tree, Fortran2003.Name):
            if isinstance(node.parent, Fortran2003.Else_If_Stmt):
                # Need to make sure we include any Symbol in the conditional
                # part but not a label (which would be the second child in the
                # parse tree). We cannot simply do
                # `node.parent.children.index(node)` because of fparser #174.
                if (len(node.parent.children) == 1 or
                        node is node.parent.children[0]):
                    result.append(node.string)
            elif not isinstance(node.parent,
                                (Fortran2003.Cycle_Stmt,
                                 Fortran2003.End_Do_Stmt,
                                 Fortran2003.Exit_Stmt,
                                 Fortran2003.Else_Stmt,
                                 Fortran2003.End_If_Stmt)):
                # We don't want labels associated with loop or branch control.
                result.append(node.string)

        return result

    def __str__(self):
        return f"CodeBlock[{len(self._fp2_nodes)} nodes]"
