# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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

import re
from enum import Enum
from typing import List

from fparser.two import Fortran2003, pattern_tools
from fparser.two.utils import walk
from psyclone.core import AccessType, Signature, VariablesAccessMap
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode


class CodeBlock(Statement, DataNode):
    '''Node representing any generic Fortran code that PSyclone does not
    attempt to manipulate. As such it is a leaf in the PSyIR. A CodeBlock
    can still answer answer limited questions about the encosed code. For
    this reason it keeps reference to the underlaying parse_tree, and each
    frontend parser needs to subclass CodeBlock with the concrete
    implementation.

    :param parse_tree: the fparser2 parse-tree nodes representing the
        Fortran code constituting the code block.
    :type parse_tree: list[:py:class:`fparser.two.utils.Base`]
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
    valid_annotations = ("psy-data-start", )

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

    def __init__(self, parse_tree, structure, parent=None, annotations=None):
        super().__init__(parent=parent, annotations=annotations)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the list container because
        # the list itself is often a temporary product of the process of
        # converting from the the parse tree to the PSyIR.
        self._parse_tree = parse_tree[:]
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
        is_eq = is_eq and self.get_ast_nodes() == other.get_ast_nodes()
        is_eq = is_eq and self.structure == other.structure

        return is_eq

    @property
    def structure(self):
        '''
        :returns: whether this code block is a statement or an expression.
        :rtype: :py:class:`psyclone.psyir.nodes.CodeBlock.Structure`

        '''
        return self._structure

    def get_ast_nodes(self):
        '''
        :returns: the nodes associated with this code block in
                  the original fparser2 parse tree.
        :rtype: list[:py:class:`fparser.two.Fortran2003.Base`]

        '''
        return self._parse_tree

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include control codes for colour.

        :return: text description of this node.
        :rtype: str
        '''
        return (f"{self.coloured_name(colour)}["
                f"{list(map(type, self._parse_tree))}]")

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        Get the symbol access map. Since this is a CodeBlock we
        only know the names of symbols accessed within it but not how they
        are accessed. Therefore we err on the side of caution and mark
        them all as READWRITE, unfortunately, this will include the names of
        any routines that are called.

        TODO #2863 - it would be better to use AccessType.UNKNOWN here but
        currently VariablesAccessMap does not consider that type of access.

        This method makes use of
        :py:meth:`~psyclone.psyir.nodes.CodeBlock.get_symbol_names` and is
        therefore subject to the same limitations as that method.

        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()
        for name in self.get_symbol_names():
            var_accesses.add_access(Signature(name), AccessType.READWRITE,
                                    self)
        return var_accesses

    def __str__(self):
        return f"CodeBlock[{len(self._parse_tree)} nodes]"

    def get_symbol_names(self) -> List[str]:
        '''
        :returns: the name of all symbols accessed in the CodeBlock.
        '''
        if not self._parse_tree:
            return []
        raise NotImplementedError("Use appropriate CodeBlock subclass")

    def has_potential_control_flow_jump(self) -> bool:
        '''
        :returns: whether the Codeblock might have control flow jumps.
        '''
        if not self._parse_tree:
            return False
        raise NotImplementedError("Use appropriate CodeBlock subclass")

    def get_fortran_lines(self) -> list[str]:
        '''
        :returns: a list of each line of fortran represented by this node.
        '''
        if not self._parse_tree:
            return []
        raise NotImplementedError("Use appropriate CodeBlock subclass")


class Fparser2CodeBlock(CodeBlock):
    ''' The fparser2 implementation of CodeBlock. '''

    def get_symbol_names(self) -> List[str]:
        '''
        Analyses the fparser2 parse tree associated with this CodeBlock and
        returns the names of all symbols accessed within it. Since, by
        definition, we do not understand the contents of a CodeBlock, we do not
        attempt to analyse how these symbols are accessed - they are all marked
        as being READWRITE (this includes the names of any routines that might
        be called).

        Note that the names of any Fortran intrinsics are *not* included in the
        result. If the original code has unwisely overridden a Fortran
        intrinsic then fparser *may* incorrectly identify the use of such a
        variable/routine as still being an intrinsic call and, as such, it will
        be omitted from the names returned by this method.

        TODO #2863 - these limitations (blanket use of READWRITE and the
        ignoring of Fortran intrinsics) need to be re-visited.

        :returns: the symbol names used inside the CodeBock.
        '''
        parse_tree = self.get_ast_nodes()
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
        # Precision on literals requires special attention since they are just
        # stored in the tree as str (fparser/#456).
        for node in walk(parse_tree, (Fortran2003.Int_Literal_Constant,
                                      Fortran2003.Real_Literal_Constant,
                                      Fortran2003.Logical_Literal_Constant,
                                      Fortran2003.Char_Literal_Constant)):
            if node.items[1]:
                result.append(node.items[1])
        # Complex literals require even more special attention.
        for node in walk(parse_tree, Fortran2003.Complex_Literal_Constant):
            # A complex literal constant has a real part and an imaginary part.
            # Each of these can have a kind.
            for part in node.items:
                if part.items[1]:
                    result.append(part.items[1])
        # For directives, we need to analyse all alphanumeric* parts of the
        # comment string and return any names that match a symbol in the
        # symbol table.
        for node in walk(parse_tree, Fortran2003.Directive):
            string_rep = node.tostr()
            string_rep = string_rep[string_rep.index("$"):]
            pattern = pattern_tools.name.get_compiled()
            matches = re.findall(pattern, string_rep)
            scope = self.scope
            for match in matches:
                sym = scope.symbol_table.lookup(match, otherwise=None)
                if sym:
                    result.append(sym.name)

        return result

    def has_potential_control_flow_jump(self) -> bool:
        '''
        :returns: whether this CodeBlock contains a potential control flow
                  jump, e.g. GOTO, EXIT or a labeled statement.
        '''
        # Loop over the fp2_nodes and check if any are GOTO, EXIT or
        # labelled statements
        for node in self._parse_tree:
            for child in walk(node, (Fortran2003.Goto_Stmt,
                                     Fortran2003.Exit_Stmt,
                                     Fortran2003.Cycle_Stmt,
                                     Fortran2003.StmtBase)):
                if isinstance(child,
                              (Fortran2003.Goto_Stmt,
                               Fortran2003.Exit_Stmt,
                               Fortran2003.Cycle_Stmt)):
                    return True
                # Also can't support Labelled statements.
                if isinstance(child, Fortran2003.StmtBase):
                    if child.item and child.item.label:
                        return True
        return False

    def get_fortran_lines(self) -> list[str]:
        '''
        :returns: a list of each line of fortran represented by this node.
        '''
        output = []
        for node in self._parse_tree:
            output.extend(node.tofortran().split("\n"))
        return output


class TreeSitterCodeBlock(CodeBlock):
    ''' The treesitter implementation of CodeBlock. '''

    def get_fortran_lines(self):
        '''
        :returns: a list of each line of fortran represented by this node.
        '''
        return [str(ast_node.text, encoding="utf8") for ast_node
                in self.get_ast_nodes()]
