# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Modified: N. Nobre, STFC Daresbury Lab


'''PSyIR visitor layer that provides convenient functions that can be reused
for different language-specific visitors.
'''

import abc

from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.nodes import Member


class LanguageWriter(PSyIRVisitor):
    '''A convenience PSyIR visitor intermediate class. It provides
    configuration options and functions that can be shared between different
    language-specific visitors.

    :param array_parenthesis: a tuple of two strings that contain the \
        opening and closing parenthesis used for array accesses - e.g.:
        ("(", ")").
    :type array_parenthesis: 2-tuple of str
    :param str structure_symbol: the symbol to be used to address a
        member of a structure, e.g. "%".
    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param str indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree. Defaults to True.

    :raises TypeError: if any of the supplied parameters are of the wrong type.

    '''
    # pylint: disable=too-many-arguments
    def __init__(self, array_parenthesis, structure_character,
                 skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        super(LanguageWriter, self).__init__(skip_nodes, indent_string,
                                             initial_indent_depth,
                                             check_global_constraints)
        if not isinstance(array_parenthesis, tuple) or \
                len(array_parenthesis) != 2:
            raise TypeError(f"Invalid array-parenthesis parameter, must be "
                            f"a tuple of two strings, got "
                            f"'{array_parenthesis}'.")
        if not isinstance(structure_character, str):
            raise TypeError(f"Invalid structure_character parameter, must be "
                            f"a string, got '{array_parenthesis}'.")

        self._array_parenthesis = array_parenthesis
        self._structure_character = structure_character

    # ------------------------------------------------------------------------
    @property
    def array_parenthesis(self):
        ''':returns: the array parenthesis to be used in this language.
        :rtype: two element list of str
        '''

        return self._array_parenthesis

    # ------------------------------------------------------------------------
    @property
    def structure_character(self):
        ''':returns: the character use to access a member of a structure in \
            this language.
        :rtype: str
        '''
        return self._structure_character

    # ------------------------------------------------------------------------
    @abc.abstractmethod
    def gen_indices(self, indices, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.

        :param indices: list of PSyIR nodes.
        :type indices: list of :py:class:`psyclone.psyir.symbols.Node`
        :param str var_name: name of the variable for which the dimensions \
            are created. Only used in the C implementation.

        :returns: the code representation of the dimensions.
        :rtype: list of str

        '''

    # ------------------------------------------------------------------------
    def arrayreference_node(self, node):
        '''This method is called when an ArrayReference instance is found
        in the PSyIR tree.

        :param node: an ArrayNode PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayNode`

        :returns: the code as a string.
        :rtype: str

        :raises VisitorError: if the node does not have any children.

        '''
        if not node.children:
            raise VisitorError(
                f"Incomplete ArrayReference node (for symbol '{node.name}') "
                f"found: must have one or more children.")
        args = self.gen_indices(node.children, node.name)
        result = f"{node.name}{self._array_parenthesis[0]}"\
                 f"{','.join(args)}{self._array_parenthesis[1]}"
        return result

    # ------------------------------------------------------------------------
    def structurereference_node(self, node):
        '''
        Creates the code for an access to a member of a structure type.

        :param node: a StructureReference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StructureReference`

        :returns: the code as string.
        :rtype: str

        :raises VisitorError: if this node does not have an instance of Member\
                              as its only child.

        '''
        if len(node.children) != 1:
            raise VisitorError(
                f"A StructureReference must have a single child but the "
                f"reference to symbol '{node.name}' has {len(node.children)}.")
        if not isinstance(node.children[0], Member):
            raise VisitorError(
                f"A StructureReference must have a single child which is a "
                f"sub-class of Member but the reference to symbol "
                f"'{node.name}' has a child of type "
                f"'{type(node.children[0]).__name__}'")
        result = node.symbol.name + self._structure_character + \
            self._visit(node.children[0])
        return result

    # ------------------------------------------------------------------------
    def member_node(self, node):
        '''
        Creates the code for an access to a member of a derived type.

        :param node: a Member PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Member`

        :returns: the code as string
        :rtype: str

        '''
        result = node.name
        if not node.children:
            # A simple member access that does not access any further
            # structures, so just return the name itself
            return result

        if isinstance(node.children[0], Member):
            # If the first child is a member, we are accessing a structure:
            if len(node.children) > 1:
                # If the node has more children, any additional children are
                # array indices. Add the indices to the output string:
                indices = self.gen_indices(node.children[1:], node.name)
                result += f"{self._array_parenthesis[0]}{','.join(indices)}"\
                          f"{self._array_parenthesis[1]}"
            # Now add the first child, which is the member that is being
            # accessed, to the output string
            result += self._structure_character + self._visit(node.children[0])
        else:
            # There is no access of a structure element, add the children
            # (which exist since this was tested above) as indices to the
            # output string.
            args = self.gen_indices(node.children, node.name)
            result += f"{self._array_parenthesis[0]}{','.join(args)}"\
                      f"{self._array_parenthesis[1]}"
        return result

    # ------------------------------------------------------------------------

    def arrayofstructuresreference_node(self, node):
        '''
        Creates the code for a reference to one or more elements of an
        array of derived types.

        :param node: an ArrayOfStructuresReference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayOfStructuresReference`

        :returns: the code as string.
        :rtype: str

        :raises VisitorError: if the supplied node does not have the correct \
                              number and type of children.
        '''
        if len(node.children) < 2:
            raise VisitorError(
                f"An ArrayOfStructuresReference must have at least two "
                f"children but found {len(node.children)}")

        if not isinstance(node.children[0], Member):
            raise VisitorError(
                f"An ArrayOfStructuresReference must have a Member as its "
                f"first child but found '{type(node.children[0]).__name__}'")

        # Generate the array reference. We need to skip over the first child
        # (as that refers to the member of the derived type being accessed).
        args = self.gen_indices(node.children[1:])

        result = (node.symbol.name + self._array_parenthesis[0] +
                  ",".join(args) + self._array_parenthesis[1] +
                  self._structure_character + self._visit(node.children[0]))
        return result


# For AutoAPI documentation generation
__all__ = ['LanguageWriter']
