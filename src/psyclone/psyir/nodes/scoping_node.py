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

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import (
    ArrayType, DataType, DataTypeSymbol, RoutineSymbol, StructureType, Symbol,
    SymbolError, SymbolTable, UnsupportedFortranType)


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
        self._symbol_table = other.symbol_table.deep_copy()
        self._symbol_table._node = self  # Associate to self

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

    def reference_accesses(self, access_info: VariablesAccessInfo):
        '''
        Get all variable access information. This specialisation is required
        to query the SymbolTable associated with a Scoping node and ensure
        that any Symbols appearing in precision specifications, array shapes or
        initialisation expressions are captured.

        :param var_accesses: VariablesAccessInfo instance that stores the
            information about variable accesses.

        '''
        def _get_accesses(dtype: DataType, info: VariablesAccessInfo):
            '''
            Store information on any symbols referenced within the supplied
            datatype.

            :param dtype: the datatype to query.
            :param info: the VariablesAccessInfo instance in which to store
                         information.
            '''
            if (hasattr(dtype, "precision") and isinstance(dtype.precision,
                                                           Symbol)):
                # The use of a Symbol to specify precision does not constitute
                # a read (since it is resolved at compile time).
                access_info.add_access(
                    Signature(dtype.precision.name),
                    AccessType.TYPE_INFO, self)

            if isinstance(dtype, DataTypeSymbol):
                # The use of a DataTypeSymbol in a declaration is a compile-
                # time access.
                info.add_access(Signature(dtype.name),
                                AccessType.TYPE_INFO, self)
            elif isinstance(dtype, StructureType):
                for cmpt in sym.datatype.components.values():
                    # Recurse for members of a StructureType
                    _get_accesses(cmpt.datatype, info)
                    if cmpt.initial_value:
                        cmpt.initial_value.reference_accesses(info)
            elif isinstance(dtype, ArrayType):
                for dim in dtype.shape:
                    if isinstance(dim, ArrayType.ArrayBounds):
                        dim.lower.reference_accesses(access_info)
                        dim.upper.reference_accesses(access_info)
            elif (isinstance(dtype, UnsupportedFortranType) and
                  dtype.partial_datatype):
                # Recurse to examine partial datatype information.
                _get_accesses(dtype.partial_datatype, info)

        # Examine the datatypes and initial values of all DataSymbols.
        for sym in self._symbol_table.datasymbols:
            _get_accesses(sym.datatype, access_info)

            if sym.initial_value:
                sym.initial_value.reference_accesses(access_info)

        # Examine the definition of each DataTypeSymbol.
        for sym in self._symbol_table.datatypesymbols:
            _get_accesses(sym.datatype, access_info)

        super().reference_accesses(access_info)

    def replace_symbols_using(self, table):
        '''
        Update any Symbols referenced by this Node (and its descendants) with
        those in the supplied table with matching names. If there is no match
        for a given Symbol then it is left unchanged.

        Since this is a ScopingNode, it is associated with a symbol table.
        Therefore, if the supplied table is the one for the scope containing
        this node (if any), the one passed to the child nodes is updated to be
        the one associated with this node.

        :param table: the symbol table in which to look up replacement symbols.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        next_table = table
        if self.parent:
            try:
                # If this node is not within a scope we get a SymbolError
                if table is self.parent.scope.symbol_table:
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
