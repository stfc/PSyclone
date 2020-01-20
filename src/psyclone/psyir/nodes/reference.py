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

''' This module contains the implementation of the Reference and Array
nodes.'''

from psyclone.psyir.nodes.node import Node
from psyclone.core.access_info import AccessType
from psyclone.psyir.symbols import Symbol, SymbolError


class Reference(Node):
    '''
    Node representing a Reference Expression.

    :param symbol: the symbol being referenced.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param parent: the parent node of this Reference in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    def __init__(self, symbol, parent=None):
        if not isinstance(symbol, Symbol):
            raise TypeError("In Reference initialisation expectinG a symbol "
                            "but found '{0}'".format(type(symbol).__name__))
        super(Reference, self).__init__(parent=parent)
        self._symbol = symbol


    @property
    def symbol(self):
        ''' Return the referenced symbol.

        :returns: the referenced symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._symbol
        
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
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyir.nodes.Node`

        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(Reference, self).math_equal(other):
            return False
        return self.name == other.name

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node, i.e.
        it sets this variable to be read.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        var_accesses.add_access(self._symbol.name, AccessType.READ, self)

    def check_declared(self):
        '''Check whether this reference has an associated symbol table entry.

        raises SymbolError: if one or more ancestor symbol table(s) \
        are found and the this reference is not found in any \
        of them.

        '''
        found_symbol_table = False
        test_node = self.parent
        while test_node:
            if hasattr(test_node, 'symbol_table'):
                found_symbol_table = True
                symbol_table = test_node.symbol_table
                if self.name in symbol_table:
                    return
            test_node = test_node.parent

        # TODO: remove this if test, remove the initialisation of the
        # found_symbol_table boolean variable and update the doc
        # string when SymbolTables are suppported in the NEMO API, see
        # issue #500. After this change has been made this method could
        # make use of the symbol method to determine
        # whether the reference has been declared (or not).
        if found_symbol_table:
            raise SymbolError(
                "Undeclared reference '{0}' found.".format(self.name))

    @staticmethod
    def get_symbol(name, node):
        ''' return symbol with name. Raise exception if not found.'''
        test_node = node
        # Iterate over ancestor Nodes of this Node.
        while test_node:
            # For simplicity, test every Node for the existence of a
            # SymbolTable (rather than checking for the particular
            # Node types which we know to have SymbolTables).
            if hasattr(test_node, 'symbol_table'):
                # This Node does have a SymbolTable.
                symbol_table = test_node.symbol_table
                try:
                    # If the reference matches a Symbol in this
                    # SymbolTable then return the Symbol.
                    return symbol_table.lookup(name)
                except KeyError:
                    # The Reference Node does not match any Symbols in
                    # this SymbolTable.
                    pass
            # Move on to the next ancestor.
            test_node = test_node.parent
        # all Nodes have been checked (up to the root Node) but there
        # has been no match so raise an exception.
        raise SymbolError(
            "Undeclared reference '{0}' found.".format(name))
        
    # ?????????????????? NO LONGER REQUIRED ??????????????????????
    def symbol(self, scope_limit=None):
        '''Returns the symbol from a symbol table associated with this
        reference or None is it is not found. The scope_limit variable
        limits the symbol table search to nodes within the scope.

        :param scope_limit: optional Node which limits the symbol \
        search space to the symbol tables of the nodes within the \
        given scope. If it is None (the default), the whole scope (all \
        symbol tables in ancestor nodes) is searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or `None`

        :returns: the Symbol associated with this reference if one is \
        found or None if not.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol` or `None`

        '''
        if scope_limit:

            if not isinstance(scope_limit, Node):
                raise TypeError(
                    "The scope_limit argument '{0}' provided to the symbol "
                    "method, is not of type `Node`."
                    "".format(str(scope_limit), str(self)))

            # Check that the scope_limit Node is an ancestor of this
            # Reference Node and raise an exception if not.
            found = False
            mynode = self.parent
            while mynode is not None:
                if mynode is scope_limit:
                    found = True
                    break
                mynode = mynode.parent
            if not found:
                # The scope_limit node is not an ancestor of this reference
                # so raise an exception.
                raise ValueError(
                    "The scope_limit node '{0}' provided to the symbol "
                    "method, is not an ancestor of this reference node '{1}'."
                    "".format(str(scope_limit), str(self)))
        test_node = self.parent
        # Iterate over ancestor Nodes of this Reference Node.
        while test_node:
            # For simplicity, test every Node for the existence of a
            # SymbolTable (rather than checking for the particular
            # Node types which we know to have SymbolTables).
            if hasattr(test_node, 'symbol_table'):
                # This Node does have a SymbolTable.
                symbol_table = test_node.symbol_table
                try:
                    # If the reference matches a Symbol in this
                    # SymbolTable then return the Symbol.
                    return symbol_table.lookup(self.name)
                except KeyError:
                    # The Reference Node does not match any Symbols in
                    # this SymbolTable.
                    pass
            if test_node is scope_limit:
                # The ancestor scope Node has been reached and nothing
                # has matched so return with None.
                return None
            # Move on to the next ancestor.
            test_node = test_node.parent
        # scope has not been set and all Nodes have been checked (up
        # to the root Node) but there has been no match so return with
        # None.
        return None


class Array(Reference):
    '''
    Node representing an Array reference. As such it has a reference and a
    subscript list as children 0 and 1, respectively.

    :param str reference_name: name of the array symbol.
    :param parent: the parent node of this Array in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    def __init__(self, symbol, parent=None):
        super(Array, self).__init__(symbol, parent=parent)
        self._text_name = "ArrayReference"
        self._colour_key = "Reference"

    @staticmethod
    def create(symbol, children):
        '''Create an Array instance given a symbol and a list of Node
        array indices.

        :param strXXX name: XXXXXXXXXXXXXXXXX
        :param children: a list of Nodes describing the array indices.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an Array instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Array`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        from psyclone.psyGen import GenerationError
        # ******* Check it is a symbol. Check numargs in symbol match children
        from psyclone.psyir.symbols import Symbol
        if not isinstance(symbol, Symbol):
            raise GenerationError(
                "name argument in create method of Array class should be a "
                "Symbol but found '{0}'.".format(type(symbol).__name__))
        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of Array class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        for child in children:
            if not isinstance(child, Node):
                raise GenerationError(
                    "child of children argument in create method of "
                    "Array class should be a PSyIR Node but found '{0}'."
                    "".format(type(child).__name__))

        array = Array(symbol)
        for child in children:
            child.parent = array
        array.children = children
        return array

    def __str__(self):
        result = "Array" + super(Array, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All variables used as indices
        in the access of the array will be added as READ.
        :param var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # This will set the array-name as READ
        super(Array, self).reference_accesses(var_accesses)

        # Now add all children: Note that the class Reference
        # does not recurse to the children (which store the indices), so at
        # this stage no index information has been stored:
        list_indices = []
        for child in self._children:
            child.reference_accesses(var_accesses)
            list_indices.append(child)

        if list_indices:
            var_info = var_accesses[self.name]
            # The last entry in all_accesses is the one added above
            # in super(Array...). Add the indices to that entry.
            var_info.all_accesses[-1].indices = list_indices
