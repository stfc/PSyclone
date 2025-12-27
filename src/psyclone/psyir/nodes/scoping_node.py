# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

from psyclone.core import VariablesAccessMap, Signature, AccessType
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import (
    RoutineSymbol, SymbolError, SymbolTable)


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
    :type symbol_table:
            Optional[:py:class:`psyclone.psyir.symbols.SymbolTable`]

    '''
    # Polymorphic parameter to initialize the Symbol Table of the ScopingNode
    _symbol_table_class = SymbolTable

    def __init__(self, children=None, parent=None, symbol_table=None):
        self._symbol_table = None

        # As Routines (a ScopingNode subclass) add their own symbol
        # into their parent's symbol table its necessary to ensure
        # that the _symbol_table exists before we add the children
        # to the Node (which happens in the super constructor).
        if symbol_table is not None:
            # Attach the provided symbol table to this scope
            symbol_table.attach(self)
        else:
            # Create a new symbol table attached to this scope
            self._symbol_table = self._symbol_table_class(self)

        super().__init__(children=children, parent=parent)

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

        This method creates a deep copy of the SymbolTable associated with
        the `other` scoping node and then calls `replace_symbols_using` to
        update all Symbols referenced in the tree below this node.

        .. warning::

          Since `replace_symbols_using` only uses symbol *names*, this won't
          get the correct symbol if the PSyIR has symbols shadowed in nested
          scopes, e.g.:

          .. code-block::

              subroutine test
                integer :: a
                integer :: b = 1
                if condition then
                  ! PSyIR declares a shadowed, locally-scoped a'
                  a' = 1
                  if condition2 then
                    ! PSyIR declares a shadowed, locally-scoped b'
                    b' = 2
                    a = a' + b'

          Here, the final assignment will end up being `a' = a' + b'` and
          thus the semantics of the code are changed. TODO #2666.

        :param other: object we are copying from.
        :type other: :py:class:`psyclone.psyir.node.Node`

        '''
        # Reorganise the symbol table construction to occur before we add
        # the children.
        self._symbol_table = other.symbol_table.deep_copy(self)

        # Remove symbols corresponding to Routines that are contained in this
        # ScopingNode. These symbols will be added automatically by the Routine
        # objects when we add them to our child list in the super refine_copy
        # call.
        # We have to import Routine here to avoid a circular dependency.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.routine import Routine
        removed_tags = {}
        for node in other.walk(Routine):
            try:
                if isinstance(self._symbol_table.lookup(node.name),
                              RoutineSymbol):
                    # We have to force the removal of the symbol.
                    symbol = self._symbol_table.lookup(node.name)
                    norm_name = self._symbol_table._normalize(symbol.name)
                    self._symbol_table._symbols.pop(norm_name)
                    # If the symbol had a tag, it should be disassociated
                    for tag, tagged_symbol in list(
                            self._symbol_table._tags.items()):
                        if symbol is tagged_symbol:
                            removed_tags[tag] = tagged_symbol
                            del self._symbol_table._tags[tag]
            except KeyError:
                pass

        super(ScopingNode, self)._refine_copy(other)

        # Add any routine tags back
        for tag in removed_tags.keys():
            # pylint: disable-next=protected-access
            self._symbol_table._tags[tag] = self._symbol_table.lookup(
                    removed_tags[tag].name)

        # Now we've updated the symbol table, walk back down the tree and
        # update any Symbols.
        # Since this will get called from every ScopingNode in the tree, there
        # could be a lot of repeated walks back down the tree. If this becomes
        # a performance issue we could keep track of the depth of the recursive
        # call to _refine_copy and only do this call when that depth is zero.
        self.replace_symbols_using(self._symbol_table)

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()
        # During the updating process when moving a Routine (and its
        # associated Symbol), it's possible that we won't have a SymbolTable.
        if self._symbol_table:
            for symbol in self._symbol_table.get_all_accessed_symbols():
                var_accesses.add_access(
                    Signature(symbol.name), AccessType.CONSTANT, self)
        var_accesses.update(super().reference_accesses())
        return var_accesses

    def replace_symbols_using(self, table_or_symbol):
        '''
        Update any Symbols referenced by this Node (and its descendants) with
        those in the supplied table (or just the supplied Symbol instance) if
        they have matching names. If there is no match for a given Symbol then
        it is left unchanged.

        Since this is a ScopingNode, it is associated with a symbol table.
        Therefore, if the supplied table is the one for the scope containing
        this node (if any), the one passed to the child nodes is updated to be
        the one associated with this node.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        next_table = table_or_symbol
        if self.parent and isinstance(table_or_symbol, SymbolTable):
            try:
                # If this node is not within a scope we get a SymbolError
                if table_or_symbol is self.parent.scope.symbol_table:
                    next_table = self.symbol_table
            except SymbolError:
                pass

        for child in self.children:
            child.replace_symbols_using(next_table)

    @property
    def symbol_table(self):
        '''
        :returns: table containing symbol information for this scope.
        :rtype: :py:class:`psyclone.psyGen.SymbolTable`
        '''
        return self._symbol_table


# For AutoAPI documentation generation
__all__ = ['ScopingNode']
