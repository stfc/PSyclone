# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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

''' This module contains the Container node implementation.'''

from psyclone.psyir.nodes.scoping_node import ScopingNode
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.symbols import SymbolTable
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.commentable_mixin import CommentableMixin


class Container(ScopingNode, CommentableMixin):
    '''Node representing a set of Routine and/or Container nodes, as well
    as a name and a SymbolTable. This construct can be used to scope
    symbols of variables, Routine names and Container names. In
    Fortran a container would naturally represent a module or a
    submodule.

    :param str name: the name of the container.
    :param parent: optional parent node of this Container in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param symbol_table: initialise the node with a given symbol table.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable` or \
            NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "[Container | Routine | CodeBlock]*"
    _text_name = "Container"
    _colour = "green"

    def __init__(self, name, **kwargs):
        super().__init__(**kwargs)
        self._name = name

    def __eq__(self, other):
        '''Checks the equality of this Container with other. Containers are
        equal if they are the same type, and have the same name.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.name == other.name
        return is_eq

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        return isinstance(child, (Container, Routine, CodeBlock))

    @classmethod
    def create(cls, name, symbol_table, children):
        '''Create a Container instance given a name, a symbol table and a
        list of child nodes.

        :param str name: the name of the Container.
        :param symbol_table: the symbol table associated with this \
            Container.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            Container. These must be Containers or Routines.
        :type children: list of :py:class:`psyclone.psyir.nodes.Container` \
            or :py:class:`psyclone.psyir.nodes.Routine`

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` or subclass
            thereof

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise GenerationError(
                f"name argument in create method of Container class "
                f"should be a string but found '{type(name).__name__}'.")
        if not isinstance(symbol_table, SymbolTable):
            raise GenerationError(
                f"symbol_table argument in create method of Container class "
                f"should be a SymbolTable but found "
                f"'{type(symbol_table).__name__}'.")
        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of Container class "
                f"should be a list but found '{type(children).__name__}'.")

        container = cls(name, symbol_table=symbol_table)
        container.children = children
        return container

    @property
    def name(self):
        '''
        :returns: name of the container.
        :rtype: str

        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''Sets a new name for the container.

        :param str new_name: new name for the container.

        '''
        self._name = new_name

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + f"[{self.name}]"

    def __str__(self):
        return f"Container[{self.name}]\n"


# For AutoAPI documentation generation
__all__ = ['Container']
