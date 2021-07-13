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


'''PSyIR visitor layer that provides convenient functions that can be reused
for different language-specific visitors.
'''

import abc

from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError


class LanguageWriter(PSyIRVisitor):
    '''A convenience PSyIR visitor base class. It provides configuration
    options and functions that can be shared between different
    language-specific visitors.

    :param array_parenthesis: a list of two strings that contain the \
        opening and closing parenthesis used for array accesses - e.g.:
        ["(", ")"].
    :type array_parenthesis: list of str of len 2
    :param str structure_symbol: the symbol to be used to address a
        member of a structure, e.g. "%".
    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :type indent_string: str or NoneType
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree.

    :raises TypeError: if any of the supplied parameters are of the wrong type.

    '''
    # pylint: disable=too-many-arguments
    def __init__(self, array_parenthesis, structure_character,
                 skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        super(LanguageWriter, self).__init__(skip_nodes, indent_string,
                                             initial_indent_depth,
                                             check_global_constraints)
        if not isinstance(array_parenthesis, list) or \
                len(array_parenthesis) != 2:
            raise TypeError("Invalid array-parenthesis parameter, must be "
                            "a list of two strings, got '{0}'."
                            .format(array_parenthesis))
        if not isinstance(structure_character, str):
            raise TypeError("Invalid structure_character parameter, must be "
                            "a string of length 2, got '{0}'."
                            .format(array_parenthesis))

        self._array_parenthesis = array_parenthesis
        self._structure_character = structure_character

    @abc.abstractmethod
    def gen_dims(self, shape, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.

        :param shape: list of PSyIR nodes.
        :type shape: list of :py:class:`psyclone.psyir.symbols.Node`
        :param str var_name: name of the variable for which the dimensions \
            are created. Only used in the C implementation.

        :returns: the Fortran representation of the dimensions.
        :rtype: list of str

        '''
        raise NotImplementedError("gen_dims() is abstract")

    def arrayreference_node(self, node):
        '''This method is called when an ArrayReference instance is found
        in the PSyIR tree.

        :param node: an ArrayNode PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayNode`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the node does not have any children.

        '''
        if not node.children:
            raise VisitorError(
                "Incomplete ArrayReference node (for symbol '{0}') found: "
                "must have one or more children.".format(node.name))
        args = self.gen_dims(node.children, node.name)
        result = "{0}{2}{1}{3}".format(node.name,
                                       ",".join(args),
                                       self._array_parenthesis[0],
                                       self._array_parenthesis[1])
        return result


# For AutoAPI documentation generation
__all__ = ['LanguageWriter']
