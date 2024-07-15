# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the ScopingNode implementation.'''

from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import SymbolTable


class ScopingNode(Node):
    ''' Abstract node that has an associated Symbol Table to keep track of
    symbols declared in its scope (symbols that can be accessed by this node
    and any of its descendants). If a pre-existing symbol table is provided,
    it will be attached to the node (raising an error if the symbol table
    is already attached to another scope), otherwise a new empty Symbol Table
    will be created.

    :param children: the PSyIR nodes that are children of this node.
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the parent node of this node in the PSyIR.
    :type parent: Optional[:py:class:`psyclone.psyir.nodes.Node`]
    :param symbol_table: attach the given symbol table to the new node.
    :type symbol_table: \
            Optional[:py:class:`psyclone.psyir.symbols.SymbolTable`]

    '''
    # Polymorphic parameter to initialize the Symbol Table of the ScopingNode
    _symbol_table_class = SymbolTable

    def __init__(self, children=None, parent=None, symbol_table=None):
        super().__init__(children=children, parent=parent)
        self._symbol_table = None

        if symbol_table is not None:
            # Attach the provided symbol table to this scope
            symbol_table.attach(self)
        else:
            # Create a new symbol table attached to this scope
            self._symbol_table = self._symbol_table_class(self)

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Scoping nodes are equal if their
        symbol tables are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.symbol_table == other.symbol_table
        return is_eq

    def _refine_copy(self, other):
        ''' Refine the object attributes when a shallow copy is not the most
        appropriate operation during a call to the copy() method.

        :param other: object we are copying from.
        :type other: :py:class:`psyclone.psyir.node.Node`

        '''
        super(ScopingNode, self)._refine_copy(other)
        self._symbol_table = other.symbol_table.deep_copy()
        # pylint: disable-next=protected-access
        self._symbol_table._node = self  # Associate to self

        # Update of children to point to the equivalent symbols in
        # the new symbol table attached to self.
        self.replace_symbols_using(self.symbol_table)

    @property
    def symbol_table(self):
        '''
        :returns: table containing symbol information for this scope.
        :rtype: :py:class:`psyclone.psyGen.SymbolTable`
        '''
        return self._symbol_table


# For AutoAPI documentation generation
__all__ = ['ScopingNode']
