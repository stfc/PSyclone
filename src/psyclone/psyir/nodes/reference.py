# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Reference node.'''


from psyclone.core import AccessType, Signature, VariablesAccessMap
# We cannot import from 'nodes' directly due to circular import
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import Symbol, AutomaticInterface
from psyclone.psyir.symbols.datatypes import UnresolvedType


class Reference(DataNode):
    '''
    Node representing a Reference Expression.

    :param symbol: the symbol being referenced.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Reference"
    _colour = "yellow"

    def __init__(self, symbol, **kwargs):
        super().__init__(**kwargs)
        self.symbol = symbol

    def __eq__(self, other):
        '''
        Checks equivalence of two References. References are considered
        equivalent if they are the same type of Reference and their symbol
        name is the same.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        # TODO #1698. Is reference equality enough comparing the symbols by
        # name? (Currently it is needed because symbol equality is not fully
        # implemented)
        is_eq = is_eq and (self.symbol.name == other.symbol.name)
        return is_eq

    @property
    def is_array(self):
        '''
        :returns: whether this reference is an array, False if it can not be
            determined.
        :rtype: bool

        '''
        return self.symbol.is_array

    @property
    def is_read(self):
        '''
        :returns: whether this reference is reading from its symbol.
        :rtype: bool
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.assignment import Assignment
        from psyclone.psyir.nodes.intrinsic_call import IntrinsicCall
        parent = self.parent
        if isinstance(parent, Assignment):
            if parent.lhs is self:
                return False

        # If we have an intrinsic call parent then we need to check if its
        # an inquiry. Inquiry functions don't read from their first argument.
        if isinstance(parent, IntrinsicCall):
            if parent.arguments[0] is self and parent.is_inquiry:
                return False

        # All references other than LHS of assignments represent a read. This
        # can be improved in the future by looking at Call intents.
        return True

    @property
    def is_write(self):
        '''
        :returns: whether this reference is writing to its symbol.
        :rtype: bool
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.assignment import Assignment
        from psyclone.psyir.nodes.call import Call
        from psyclone.psyir.nodes.intrinsic_call import IntrinsicCall
        parent = self.parent
        # pure or inquiry IntrinsicCall nodes do not write to their arguments.
        if (isinstance(parent, IntrinsicCall) and (parent.is_inquiry or
                                                   parent.is_pure)):
            return False
        # All other arguments of all other Calls are assumed to write to their
        # arguments. This could be improved in the future by looking at
        # intents where available.
        if isinstance(parent, Call):
            return True
        # The reference that is the LHS of an assignment is a write.
        if isinstance(parent, Assignment) and parent.lhs is self:
            return True
        return False

    @property
    def symbol(self):
        ''' Return the referenced symbol.

        :returns: the referenced symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._symbol

    @symbol.setter
    def symbol(self, symbol):
        '''
        :param symbol: the new symbol being referenced.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the symbol argument is not of type Symbol.

        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(
                f"The {type(self).__name__} symbol setter expects a PSyIR "
                f"Symbol object but found '{type(symbol).__name__}'.")
        self._symbol = symbol

    @property
    def name(self):
        ''' Return the name of the referenced symbol.

        :returns: Name of the referenced symbol.
        :rtype: str

        '''
        return self._symbol.name

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include colour control codes.

        :return: text description of this node.
        :rtype: str
        '''
        return f"{self.coloured_name(colour)}[name:'{self.name}']"

    def __str__(self):
        return self.node_str(False)

    def get_signature_and_indices(self):
        ''':returns: the Signature of this reference, and \
            an empty list of lists as 'indices' since this reference does \
            not represent an array access.
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            list of indices)
        '''
        return (Signature(self.name), [[]])

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()
        sig, all_indices = self.get_signature_and_indices()
        for indices in all_indices:
            for index in indices:
                var_accesses.update(index.reference_accesses())
        var_accesses.add_access(sig, AccessType.READ, self, all_indices)
        return var_accesses

    @property
    def datatype(self):
        '''
        :returns: the datatype of this reference.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        '''
        # pylint: disable=unidiomatic-typecheck
        # Use type() directly as we need to ignore inheritance.
        if type(self.symbol) is Symbol:
            # We don't even have a DataSymbol
            return UnresolvedType()
        return self.symbol.datatype

    def previous_accesses(self):
        '''
        :returns: the nodes accessing the same symbol directly before this
                  reference. It can be multiple nodes if the control flow
                  diverges and there are multiple possible accesses.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools import DefinitionUseChain
        chain = DefinitionUseChain(self)
        return chain.find_backward_accesses()

    def next_accesses(self):
        '''
        :returns: the nodes accessing the same symbol directly after this
                  reference. It can be multiple nodes if the control flow
                  diverges and there are multiple possible accesses.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools import DefinitionUseChain
        chain = DefinitionUseChain(self)
        return chain.find_forward_accesses()

    def escapes_scope(self, scope, visited_nodes=None) -> bool:
        '''
        :param scope: the given scope that we evaluate.
        :param visited_nodes: a set of nodes already visited, this is necessary
            because the dependency chains may contain cycles. Defaults to an
            empty set.
        :returns: whether the symbol lifetime continues after the given scope.
        '''

        # Populate visited_nodes, and stop recursion when appropriate
        if visited_nodes is None:
            visited_nodes = set()
        if id(self) in visited_nodes:
            return False
        visited_nodes.add(id(self))

        # If it's not a local symbol, we cannot guarantee its lifetime
        if not isinstance(self.symbol.interface, AutomaticInterface):
            return True

        # Check if this instance is in the provided scope
        if not self.is_descendent_of(scope):
            return True

        # Now check all possible next accesses
        for ref in self.next_accesses():
            if ref.escapes_scope(scope, visited_nodes):
                return True

        return False

    def replace_symbols_using(self, table_or_symbol):
        '''
        Update any Symbols referenced by this Node with those in the
        supplied table (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given
        Symbol then it is left unchanged.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if isinstance(table_or_symbol, Symbol):
            if self.symbol.name.lower() == table_or_symbol.name.lower():
                self.symbol = table_or_symbol
        else:
            try:
                self.symbol = table_or_symbol.lookup(self.symbol.name)
            except KeyError:
                pass

        # Walk on down the tree.
        super().replace_symbols_using(table_or_symbol)


# For AutoAPI documentation generation
__all__ = ['Reference']
