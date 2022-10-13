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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the SymbolTable implementation. '''

# pylint: disable=too-many-lines

from __future__ import print_function, absolute_import
from collections import OrderedDict
import inspect
import copy
from psyclone.configuration import Config
from psyclone.psyir.symbols import Symbol, DataSymbol, ImportInterface, \
    ContainerSymbol, DataTypeSymbol, RoutineSymbol, SymbolError, \
    UnresolvedInterface
from psyclone.psyir.symbols.typed_symbol import TypedSymbol
from psyclone.errors import InternalError


class SymbolTable():
    # pylint: disable=too-many-public-methods
    '''Encapsulates the symbol table and provides methods to add new
    symbols and look up existing symbols. Nested scopes are supported
    and, by default, the add and lookup methods take any ancestor
    symbol tables into consideration (ones attached to nodes that are
    ancestors of the node that this symbol table is attached to). If the
    default visibility is not specified then it defaults to
    Symbol.Visbility.PUBLIC.

    :param node: reference to the Schedule or Container to which this \
        symbol table belongs.
    :type node: :py:class:`psyclone.psyir.nodes.Schedule`, \
        :py:class:`psyclone.psyir.nodes.Container` or NoneType
    :param default_visibility: optional default visibility value for this \
        symbol table, if not provided it defaults to PUBLIC visibility.
    :type default_visibillity: \
        :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

    :raises TypeError: if node argument is not a Schedule or a Container.

    '''
    def __init__(self, node=None, default_visibility=Symbol.Visibility.PUBLIC):
        # Dict of Symbol objects with the symbol names as keys. Make
        # this ordered so that different versions of Python always
        # produce code with declarations in the same order.
        self._symbols = OrderedDict()
        # Ordered list of the arguments.
        self._argument_list = []
        # Dict of tags. Some symbols can be identified with a tag.
        self._tags = {}

        # Reference to the node to which this symbol table belongs.
        self._node = None
        if node:
            self.attach(node)

        # The default visibility of symbols in this symbol table. The
        # setter does validation of the supplied quantity.
        self._default_visibility = None
        self.default_visibility = default_visibility

    @property
    def default_visibility(self):
        '''
        :returns: the default visibility of symbols in this table.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
        '''
        return self._default_visibility

    @default_visibility.setter
    def default_visibility(self, vis):
        '''
        Sets the default visibility of symbols in this table.

        :param vis: the default visibility.
        :type vis: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

        :raises TypeError: if the supplied value is of the wrong type.

        '''
        if not isinstance(vis, Symbol.Visibility):
            raise TypeError(
                f"Default visibility must be an instance of psyir.symbols."
                f"Symbol.Visibility but got '{type(vis).__name__}'")
        self._default_visibility = vis

    @property
    def node(self):
        '''
        :returns: the Schedule or Container to which this symbol table belongs.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`, \
            :py:class:`psyclone.psyir.nodes.Container` or NoneType

        '''
        return self._node

    def is_empty(self):
        '''
        :returns: True if the symbol table is empty, and False otherwise.
        :rtype: bool

        '''
        return len(self._symbols) == 0

    def parent_symbol_table(self, scope_limit=None):
        '''If this symbol table is enclosed in another scope, return the
        symbol table of the next outer scope. Otherwise return None.

        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
            `NoneType`

        :returns: the 'parent' SymbolTable of the current SymbolTable (i.e.
                  the one that encloses this one in the PSyIR hierarchy).
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable` or NoneType

        '''
        # Validate the supplied scope_limit
        if scope_limit is not None:
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import Node
            if not isinstance(scope_limit, Node):
                raise TypeError(
                    f"The scope_limit argument '{scope_limit}', is not of "
                    f"type `Node`.")

        # We use the Node with which this table is associated in order to
        # move up the Node hierarchy
        if self.node:
            search_next = self.node
            while search_next is not scope_limit and search_next.parent:
                search_next = search_next.parent
                if hasattr(search_next, 'symbol_table'):
                    return search_next.symbol_table
        return None

    def get_symbols(self, scope_limit=None):
        '''Return symbols from this symbol table and all symbol tables
        associated with ancestors of the node that this symbol table
        is attached to. If there are name duplicates we only return the
        one from the closest ancestor including self. It accepts an
        optional scope_limit argument.

        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
            `NoneType`

        :returns: ordered dictionary of symbols indexed by symbol name.
        :rtype: OrderedDict[str] = :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        all_symbols = OrderedDict()
        current = self
        while current:
            for symbol_name, symbol in current.symbols_dict.items():
                if symbol_name not in all_symbols:
                    all_symbols[symbol_name] = symbol
            current = current.parent_symbol_table(scope_limit)
        return all_symbols

    def get_tags(self, scope_limit=None):
        '''Return tags from this symbol table and all symbol tables associated
        with ancestors of the node that this symbol table is attached
        to. If there are tag duplicates we only return the one from the closest
        ancestor including self. It accepts an optional scope_limit argument.

        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
            `NoneType`

        :returns: ordered dictionary of symbols indexed by tag.
        :rtype: OrderedDict[str] = :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        all_tags = OrderedDict()
        current = self
        while current:
            for tag, symbol in current.tags_dict.items():
                if tag not in all_tags:
                    all_tags[tag] = symbol
            current = current.parent_symbol_table(scope_limit)
        return all_tags

    def shallow_copy(self):
        '''Create a copy of the symbol table with new instances of the
        top-level data structures but keeping the same existing symbol
        objects. Symbols added to the new symbol table will not be added
        in the original but the existing objects are still the same.

        :returns: a shallow copy of this symbol table.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        # pylint: disable=protected-access
        new_st = type(self)()
        new_st._symbols = copy.copy(self._symbols)
        new_st._argument_list = copy.copy(self._argument_list)
        new_st._tags = copy.copy(self._tags)
        new_st._node = self.node
        new_st._default_visibility = self.default_visibility
        return new_st

    def deep_copy(self):
        '''Create a copy of the symbol table with new instances of the
        top-level data structures and also new instances of the symbols
        contained in these data structures. Modifying a symbol attribute
        will not affect the equivalent named symbol in the original symbol
        table.

        The only attribute not copied is the _node reference to the scope,
        since that scope can only have one symbol table associated to it.

        :returns: a deep copy of this symbol table.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        # pylint: disable=protected-access
        new_st = type(self)()

        # Make a copy of each symbol in the symbol table
        for symbol in self.symbols:
            new_st.add(symbol.copy())

        # Prepare the new argument list
        new_arguments = []
        for name in [arg.name for arg in self.argument_list]:
            new_arguments.append(new_st.lookup(name))
        new_st.specify_argument_list(new_arguments)

        # Prepare the new tag dict
        for tag, symbol in self._tags.items():
            new_st._tags[tag] = new_st.lookup(symbol.name)

        # Fix the container links for imported symbols
        for symbol in new_st.imported_symbols:
            name = symbol.interface.container_symbol.name
            new_container = new_st.lookup(name)
            symbol.interface = ImportInterface(new_container)

        # Set the default visibility
        new_st._default_visibility = self.default_visibility

        return new_st

    @staticmethod
    def _normalize(key):
        '''Normalises the symboltable key strings.

        :param str key: an input key.

        :returns: the normalized key.
        :rtype: str

        '''
        # The symbol table is currently case insensitive
        new_key = key.lower()
        return new_key

    def new_symbol(self, root_name=None, tag=None, shadowing=False,
                   symbol_type=None, **symbol_init_args):
        ''' Create a new symbol. Optional root_name and shadowing
        arguments can be given to choose the name following the rules of
        next_available_name(). An optional tag can also be given.
        By default it creates a generic symbol but a symbol_type argument
        and any additional initialization keyword arguments of this
        symbol_type can be provided to refine the created Symbol.

        :param root_name: optional name to use when creating a new \
            symbol name. This will be appended with an integer if the name \
            clashes with an existing symbol name.
        :type root_name: str or NoneType
        :param str tag: optional tag identifier for the new symbol.
        :param bool shadowing: optional logical flag indicating whether the \
            name can be overlapping with a symbol in any of the ancestors \
            symbol tables. Defaults to False.
        :param symbol_type: class type of the new symbol.
        :type symbol_type: type object of class (or subclasses) of \
                           :py:class:`psyclone.psyir.symbols.Symbol`
        :param symbol_init_args: arguments to create a new symbol.
        :type symbol_init_args: unwrapped Dict[str] = object

        :raises TypeError: if the type_symbol argument is not the type of a \
                           Symbol object class or one of its subclasses.

        '''
        # Only type-check symbol_type, the other arguments are just passed down
        # and type checked inside each relevant method.
        if symbol_type is not None:
            if not (isinstance(symbol_type, type) and
                    Symbol in inspect.getmro(symbol_type)):
                raise TypeError(
                    f"The symbol_type parameter should be a type class of "
                    f"Symbol or one of its sub-classes but found "
                    f"'{type(symbol_type).__name__}' instead.")
        else:
            symbol_type = Symbol

        # If no visibility parameter has been provided use this symbol table's
        # default visibility
        if "visibility" not in symbol_init_args:
            symbol_init_args["visibility"] = self.default_visibility

        available_name = self.next_available_name(root_name, shadowing)
        symbol = symbol_type(available_name, **symbol_init_args)
        self.add(symbol, tag)
        return symbol

    def find_or_create(self, name, **new_symbol_args):
        ''' Lookup a symbol by its name, if it doesn't exist create a new
        symbol with the given properties.

        :param str name: name of the symbol to lookup or create.
        :param new_symbol_args: arguments to create a new symbol.
        :type new_symbol_args: unwrapped Dict[str, object]

        :raises SymbolError: if the symbol already exists but the type_symbol \
                             argument does not match the type of the symbol \
                             found.

        '''
        try:
            symbol = self.lookup(name)
            # Check that the symbol found matches the requested description
            if 'symbol_type' in new_symbol_args:
                symbol_type = new_symbol_args['symbol_type']
                if not isinstance(symbol, new_symbol_args['symbol_type']):
                    raise SymbolError(
                        f"Expected symbol with name '{name}' to be of type "
                        f"'{symbol_type.__name__}' but found type "
                        f"'{type(symbol).__name__}'.")
            # TODO #1057: If the symbol is found and some unmatching arguments
            # were given it should also fail here.
            return symbol
        except KeyError:
            return self.new_symbol(name, **new_symbol_args)

    def find_or_create_tag(self, tag, root_name=None, **new_symbol_args):
        ''' Lookup a tag, if it doesn't exist create a new symbol with the
        given tag. By default it creates a generic Symbol with the tag as the
        root of the symbol name. Optionally, a different root_name or any of
        the arguments available in the new_symbol() method can be given to
        refine the name and the type of the created Symbol.

        :param str tag: tag identifier.
        :param str root_name: optional name of the new symbol if it needs \
                              to be created. Otherwise it is ignored.
        :param new_symbol_args: arguments to create a new symbol.
        :type new_symbol_args: unwrapped Dict[str, object]

        :returns: symbol associated with the given tag.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises SymbolError: if the symbol already exists but the type_symbol \
                             argument does not match the type of the symbol \
                             found.

        '''
        try:
            symbol = self.lookup_with_tag(tag)
            # Check that the symbol found matches the requested description
            if 'symbol_type' in new_symbol_args:
                symbol_type = new_symbol_args['symbol_type']
                if not isinstance(symbol, new_symbol_args['symbol_type']):
                    raise SymbolError(
                        f"Expected symbol with tag '{tag}' to be of type "
                        f"'{symbol_type.__name__}' but found type "
                        f"'{type(symbol).__name__}'.")
            # TODO #1057: If the symbol is found and some unmatching arguments
            # were given it should also fail here.
            return symbol
        except KeyError:
            if not root_name:
                root_name = tag
            return self.new_symbol(root_name, tag, **new_symbol_args)

    def next_available_name(self, root_name=None, shadowing=False,
                            other_table=None):
        '''Return a name that is not in the symbol table and therefore can
        be used to declare a new symbol.
        If the `root_name` argument is not supplied or if it is
        an empty string then the name is generated internally,
        otherwise the `root_name` is used. If required, an additional
        integer is appended to avoid clashes.
        If the shadowing argument is True (is False by default), the names
        in parent symbol tables will not be considered.
        If `other_table` is supplied, the new name is constructed so as not
        to clash with any entries in that table.

        :param str root_name: optional name to use when creating a new \
            symbol name. This will be appended with an integer if the name \
            clashes with an existing symbol name.
        :param bool shadowing: optional logical flag indicating whether the \
            name can be overlapping with a symbol in any of the ancestors \
            symbol tables. Defaults to False.
        :param other_table: an optional, second symbol table to take into \
            account when constructing a new name.
        :type other_table: :py:class`psyclone.psyir.symbols.SymbolTable`

        :returns: the new unique symbol name.
        :rtype: str

        :raises TypeError: if any of the arguments are of the wrong type.

        '''
        if not isinstance(shadowing, bool):
            raise TypeError(
                f"Argument 'shadowing' should be of type bool"
                f" but found '{type(shadowing).__name__}'.")

        if other_table and not isinstance(other_table, SymbolTable):
            raise TypeError(
                f"If supplied, argument 'other_table' should be of type "
                f"SymbolTable but found '{type(other_table).__name__}'.")

        if shadowing:
            symbols = self._symbols
        else:
            # If symbol shadowing is not permitted, the list of symbols names
            # that can't be used includes all the symbols from all the ancestor
            # symbol tables.
            symbols = self.get_symbols()

        # Construct the set of existing names.
        existing_names = set(symbols.keys())

        if other_table:
            # If a second symbol table has been supplied, include its entries
            # in the list of names to exclude.
            other_names = set(other_table.symbols_dict.keys())
            existing_names = existing_names.union(other_names)

        if root_name is not None:
            if not isinstance(root_name, str):
                raise TypeError(
                    f"Argument root_name should be of type str or NoneType "
                    f"but found '{type(root_name).__name__}'.")
        if not root_name:
            root_name = Config.get().psyir_root_name
        candidate_name = root_name
        idx = 1
        while self._normalize(candidate_name) in existing_names:
            candidate_name = f"{root_name}_{idx}"
            idx += 1
        return candidate_name

    def add(self, new_symbol, tag=None):
        '''Add a new symbol to the symbol table if the symbol name is not
        already in use.

        :param new_symbol: the symbol to add to the symbol table.
        :type new_symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str tag: a tag identifier for the new symbol, by default no \
            tag is given.

        :raises InternalError: if the new_symbol argument is not a \
            symbol.
        :raises KeyError: if the symbol name is already in use.
        :raises KeyError: if a tag is supplied and it is already in \
            use.

        '''
        if not isinstance(new_symbol, Symbol):
            raise InternalError(f"Symbol '{new_symbol}' is not a symbol, but "
                                f"'{type(new_symbol).__name__}'.'")

        key = self._normalize(new_symbol.name)
        if key in self._symbols:
            raise KeyError(f"Symbol table already contains a symbol with "
                           f"name '{new_symbol.name}'.")

        if tag:
            if tag in self.get_tags():
                raise KeyError(
                    f"This symbol table, or an outer scope ancestor symbol "
                    f"table, already contains the tag '{tag}' for the symbol"
                    f" '{self.lookup_with_tag(tag).name}', so it can not be "
                    f"associated with symbol '{new_symbol.name}'.")
            self._tags[tag] = new_symbol

        self._symbols[key] = new_symbol

    def swap_symbol_properties(self, symbol1, symbol2):
        '''Swaps the properties of symbol1 and symbol2 apart from the symbol
        name. Argument list positions are also updated appropriately.

        :param symbol1: the first symbol.
        :type symbol1: :py:class:`psyclone.psyir.symbols.Symbol`
        :param symbol2: the second symbol.
        :type symbol2: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises KeyError: if either of the supplied symbols are not in \
                          the symbol table.
        :raises TypeError: if the supplied arguments are not symbols, \
                 or the names of the symbols are the same in the SymbolTable \
                 instance.

        '''
        for symbol in [symbol1, symbol2]:
            if not isinstance(symbol, Symbol):
                raise TypeError(f"Arguments should be of type 'Symbol' but "
                                f"found '{type(symbol).__name__}'.")
            if symbol.name not in self._symbols:
                raise KeyError(f"Symbol '{symbol.name}' is not in the symbol "
                               f"table.")
        if symbol1.name == symbol2.name:
            raise ValueError(f"The symbols should have different names, but "
                             f"found '{symbol1.name}' for both.")

        tmp_symbol = symbol1.copy()
        symbol1.copy_properties(symbol2)
        symbol2.copy_properties(tmp_symbol)

        # Update argument list if necessary
        index1 = None
        if symbol1 in self._argument_list:
            index1 = self._argument_list.index(symbol1)
        index2 = None
        if symbol2 in self._argument_list:
            index2 = self._argument_list.index(symbol2)
        if index1 is not None:
            self._argument_list[index1] = symbol2
        if index2 is not None:
            self._argument_list[index2] = symbol1

    def specify_argument_list(self, argument_symbols):
        '''
        Sets-up the internal list storing the order of the arguments to this
        kernel.

        :param list argument_symbols: ordered list of the DataSymbols \
            representing the kernel arguments.

        :raises ValueError: if the new argument_list is not consistent with \
            the existing entries in the SymbolTable.

        '''
        self._validate_arg_list(argument_symbols)
        self._argument_list = argument_symbols[:]

    def lookup(self, name, visibility=None, scope_limit=None):
        '''Look up a symbol in the symbol table. The lookup can be limited
        by visibility (e.g. just show public methods) or by scope_limit (e.g.
        just show symbols up to a certain scope).

        :param str name: name of the symbol.
        :param visibilty: the visibility or list of visibilities that the \
                          symbol must have.
        :type visibility: [list of] :py:class:`psyclone.symbols.Visibility`
        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.
        :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
            `NoneType`

        :returns: the symbol with the given name and, if specified, visibility.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the name argument is not a string.
        :raises SymbolError: if the name exists in the Symbol Table but does \
                             not have the specified visibility.
        :raises TypeError: if the visibility argument has the wrong type.
        :raises KeyError: if the given name is not in the Symbol Table.

        '''
        if not isinstance(name, str):
            raise TypeError(
                f"Expected the name argument to the lookup() method to be "
                f"a str but found '{type(name).__name__}'.")

        try:
            symbol = self.get_symbols(scope_limit)[self._normalize(name)]
            if visibility:
                if not isinstance(visibility, list):
                    vis_list = [visibility]
                else:
                    vis_list = visibility
                if symbol.visibility not in vis_list:
                    vis_names = []
                    # Take care here in case the 'visibility' argument
                    # is of the wrong type
                    for vis in vis_list:
                        if not isinstance(vis, Symbol.Visibility):
                            raise TypeError(
                                f"the 'visibility' argument to lookup() must "
                                f"be an instance (or list of instances) of "
                                f"Symbol.Visibility but got "
                                f"'{type(vis).__name__}' when searching for "
                                f"symbol '{name}'")
                        vis_names.append(vis.name)
                    raise SymbolError(
                        f"Symbol '{name}' exists in the Symbol Table but has "
                        f"visibility '{symbol.visibility.name}' which does not"
                        f" match with the requested visibility: {vis_names}")
            return symbol
        except KeyError as err:
            raise KeyError(f"Could not find '{name}' in the Symbol Table.") \
                from err

    def lookup_with_tag(self, tag, scope_limit=None):
        '''Look up a symbol by its tag. The lookup can be limited by
        scope_limit (e.g. just show symbols up to a certain scope).

        :param str tag: tag identifier.
        :param scope_limit: optional Node which limits the symbol \
            search space to the symbol tables of the nodes within the \
            given scope. If it is None (the default), the whole \
            scope (all symbol tables in ancestor nodes) is searched \
            otherwise ancestors of the scope_limit node are not \
            searched.

        :returns: symbol with the given tag.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the tag argument is not a string.
        :raises KeyError: if the given tag is not in the Symbol Table.

        '''
        if not isinstance(tag, str):
            raise TypeError(
                f"Expected the tag argument to the lookup_with_tag() method "
                f"to be a str but found '{type(tag).__name__}'.")

        try:
            return self.get_tags(scope_limit)[tag]
        except KeyError as err:
            raise KeyError(f"Could not find the tag '{tag}' in the Symbol "
                           f"Table.") from err

    def __contains__(self, key):
        '''Check if the given key is part of the Symbol Table.

        :param str key: key to check for existance.

        :returns: whether the Symbol Table contains the given key.
        :rtype: bool
        '''
        return self._normalize(key.lower()) in self._symbols

    def symbols_imported_from(self, csymbol):
        '''
        Examines the contents of this symbol table to see which DataSymbols
        (if any) are imported from the supplied ContainerSymbol (which must
        be present in the SymbolTable).

        :param csymbol: the ContainerSymbol to search for imports from.
        :type csymbol: :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :returns: list of DataSymbols that are imported from the supplied \
                  ContainerSymbol. If none are found then the list is empty.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the supplied object is not a ContainerSymbol.
        :raises KeyError: if the supplied object is not in this SymbolTable.

        '''
        if not isinstance(csymbol, ContainerSymbol):
            raise TypeError(
                f"symbols_imported_from() expects a ContainerSymbol but got "
                f"an object of type '{type(csymbol).__name__}'")
        # self.lookup(name) will raise a KeyError if there is no symbol with
        # that name in the table.
        if self.lookup(csymbol.name) is not csymbol:
            raise KeyError(f"The '{csymbol.name}' entry in this SymbolTable "
                           f"is not the supplied ContainerSymbol.")

        return [symbol for symbol in self.imported_symbols if
                symbol.interface.container_symbol is csymbol]

    def swap(self, old_symbol, new_symbol):
        '''
        Remove the `old_symbol` from the table and replace it with the
        `new_symbol`.

        :param old_symbol: the symbol to remove from the table.
        :type old_symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param new_symbol: the symbol to add to the table.
        :type new_symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if either old/new_symbol are not Symbols.
        :raises SymbolError: if `old_symbol` and `new_symbol` don't have \
                             the same name (after normalising).
        '''
        if not isinstance(old_symbol, Symbol):
            raise TypeError(f"Symbol to remove must be of type Symbol but "
                            f"got '{type(old_symbol).__name__}'")
        if not isinstance(new_symbol, Symbol):
            raise TypeError(f"Symbol to add must be of type Symbol but "
                            f"got '{type(new_symbol).__name__}'")
        # The symbol table is not case sensitive so we must normalise the
        # symbol names before comparing them.
        if (self._normalize(old_symbol.name) !=
                self._normalize(new_symbol.name)):
            raise SymbolError(
                f"Cannot swap symbols that have different names, got: "
                f"'{old_symbol.name}' and '{new_symbol.name}'")
        # TODO #898 remove() does not currently check for any uses of
        # old_symbol.
        self.remove(old_symbol)
        self.add(new_symbol)

    def remove(self, symbol):
        '''
        Remove the supplied symbol from the Symbol Table. This has a high
        potential to leave broken links, so this method checks for some
        references to the removed symbol depending on the symbol type.

        Currently, generic Symbols, ContainerSymbols and RoutineSymbols are
        supported. Support for removing other types of Symbol will be added
        as required.

        TODO #898. This method should check for any references/uses of
        the target symbol.

        :param symbol: the container symbol to remove.
        :type symbol: :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :raises TypeError: if the supplied symbol is not of type Symbol.
        :raises NotImplementedError: the removal of this symbol type is not \
                                     supported yet.
        :raises KeyError: if the supplied symbol is not in the symbol table.
        :raises ValueError: if the supplied container symbol is referenced \
                            by one or more DataSymbols.
        :raises InternalError: if the supplied symbol is not the same as the \
                               entry with that name in this SymbolTable.
        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(f"remove() expects a Symbol argument but found: "
                            f"'{type(symbol).__name__}'.")

        # pylint: disable=unidiomatic-typecheck
        if not (isinstance(symbol, (ContainerSymbol, RoutineSymbol)) or
                type(symbol) == Symbol):
            raise NotImplementedError(
                f"remove() currently only supports generic Symbol, "
                f"ContainerSymbol and RoutineSymbol types but got: "
                f"'{type(symbol).__name__}'")
        # pylint: enable=unidiomatic-typecheck

        # Since we are manipulating the _symbols dict directly we must use
        # the normalised name of the symbol.
        norm_name = self._normalize(symbol.name)

        if norm_name not in self._symbols:
            raise KeyError(f"Cannot remove Symbol '{symbol.name}' from symbol "
                           f"table because it does not exist.")
        # Sanity-check that the entry in the table is the symbol we've
        # been passed.
        if self._symbols[norm_name] is not symbol:
            raise InternalError(
                f"The Symbol with name '{symbol.name}' in this symbol table "
                f"is not the same Symbol object as the one that has been "
                f"supplied to the remove() method.")

        # We can only remove a ContainerSymbol if no DataSymbols are
        # being imported from it
        if (isinstance(symbol, ContainerSymbol) and
                self.symbols_imported_from(symbol)):
            raise ValueError(
                f"Cannot remove ContainerSymbol '{symbol.name}' since symbols"
                f" {[sym.name for sym in self.symbols_imported_from(symbol)]} "
                f"are imported from it - remove them first.")

        # If the symbol had a tag, it should be disassociated
        for tag, tagged_symbol in list(self._tags.items()):
            if symbol is tagged_symbol:
                del self._tags[tag]

        self._symbols.pop(norm_name)

    @property
    def argument_list(self):
        '''
        Checks that the contents of the SymbolTable are self-consistent
        and then returns the list of kernel arguments.

        :returns: ordered list of arguments.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises InternalError: if the entries of the SymbolTable are not \
            self-consistent.

        '''
        try:
            self._validate_arg_list(self._argument_list)
            self._validate_non_args()
        except ValueError as err:
            # If the SymbolTable is inconsistent at this point then
            # we have an InternalError.
            raise InternalError(str(err.args)) from err
        return self._argument_list

    @staticmethod
    def _validate_arg_list(arg_list):
        '''
        Checks that the supplied list of Symbols are valid kernel arguments.

        :param arg_list: the proposed kernel arguments.
        :type param_list: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if any item in the supplied list is not a \
            DataSymbol.
        :raises ValueError: if any of the symbols does not have an argument \
            interface.

        '''
        for symbol in arg_list:
            if not isinstance(symbol, DataSymbol):
                raise TypeError(f"Expected a list of DataSymbols but found an "
                                f"object of type '{type(symbol)}'.")
            if not symbol.is_argument:
                raise ValueError(
                    f"DataSymbol '{symbol}' is listed as a kernel argument "
                    f"but has an interface of type '{type(symbol.interface)}' "
                    f"rather than ArgumentInterface")

    def _validate_non_args(self):
        '''
        Performs internal consistency checks on the current entries in the
        SymbolTable that do not represent kernel arguments.

        :raises ValueError: if a symbol that is not in the argument list \
            has an argument interface.

        '''
        for symbol in self.datasymbols:
            if symbol not in self._argument_list:
                # DataSymbols not in the argument list must not have a
                # Symbol.Argument interface
                if symbol.is_argument:
                    raise ValueError(
                        f"Symbol '{symbol}' is not listed as a kernel argument"
                        f" and yet has an ArgumentInterface interface.")

    def get_unresolved_datasymbols(self, ignore_precision=False):
        '''
        Create a list of the names of all of the DataSymbols in the table that
        do not have a resolved interface. If ignore_precision is True then
        those DataSymbols that are used to define the precision of other
        DataSymbols are ignored. If no unresolved DataSymbols are found then an
        empty list is returned.

        :param bool ignore_precision: whether or not to ignore DataSymbols \
                    that are used to define the precision of other DataSymbols.

        :returns: the names of those DataSymbols with unresolved interfaces.
        :rtype: list of str

        '''
        unresolved_symbols = [sym for sym in self.datasymbols
                              if sym.is_unresolved]
        if ignore_precision:
            unresolved_datasymbols = list(set(unresolved_symbols) -
                                          set(self.precision_datasymbols))
        else:
            unresolved_datasymbols = unresolved_symbols
        return [sym.name for sym in unresolved_datasymbols]

    @property
    def symbols_dict(self):
        '''
        :returns: ordered dictionary of symbols indexed by symbol name.
        :rtype: OrderedDict[str] = :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._symbols

    @property
    def tags_dict(self):
        '''
        :returns: ordered dictionary of symbols indexed by tag.
        :rtype: OrderedDict[str] = :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._tags

    def get_reverse_tags_dict(self):
        '''
        Constructs and returns a reverse of the map returned by tags_dict
        method.

        :returns: ordered dictionary of tags indexed by symbol.
        :rtype: OrderedDict[:py:class:`psyclone.psyir.symbols.Symbol`, str]

        '''
        tags_dict_reversed = OrderedDict()
        # TODO #1654. This assumes that there is only ever one tag associated
        # with a particular Symbol. At present this is guaranteed by the
        # SymbolTable interface but this restriction may be lifted in future.
        for tag, sym in self._tags.items():
            tags_dict_reversed[sym] = tag
        return tags_dict_reversed

    @property
    def symbols(self):
        '''
        :returns: list of symbols.
        :rtype: list of :py:class:`psyclone.psyir.symbols.Symbol`
        '''
        return list(self._symbols.values())

    @property
    def datasymbols(self):
        '''
        :returns: list of symbols representing data variables.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return [sym for sym in self._symbols.values() if
                isinstance(sym, DataSymbol)]

    @property
    def local_datasymbols(self):
        '''
        :returns: list of symbols representing local variables.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return [sym for sym in self.datasymbols if sym.is_local]

    @property
    def argument_datasymbols(self):
        '''
        :returns: list of symbols representing arguments.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return [sym for sym in self.datasymbols if sym.is_argument]

    @property
    def imported_symbols(self):
        '''
        :returns: list of symbols that have an imported interface (are \
            associated with data that exists outside the current scope).
        :rtype: list of :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return [sym for sym in self.symbols if sym.is_import]

    @property
    def precision_datasymbols(self):
        '''
        :returns: list of all symbols used to define the precision of \
                  other symbols within the table.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        # Accumulate into a set so as to remove any duplicates
        precision_symbols = set()
        for sym in self.datasymbols:
            # Not all types have the 'precision' attribute (e.g. DeferredType)
            if (hasattr(sym.datatype, "precision") and
                    isinstance(sym.datatype.precision, DataSymbol)):
                precision_symbols.add(sym.datatype.precision)
        return list(precision_symbols)

    @property
    def containersymbols(self):
        '''
        :returns: a list of the ContainerSymbols present in the Symbol Table.
        :rtype: list of :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        '''
        return [sym for sym in self.symbols if isinstance(sym,
                                                          ContainerSymbol)]

    @property
    def local_datatypesymbols(self):
        '''
        :returns: the local DataTypeSymbols present in the Symbol Table.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        '''
        return [sym for sym in self.symbols if
                (isinstance(sym, DataTypeSymbol) and sym.is_local)]

    @property
    def iteration_indices(self):
        '''
        :returns: list of symbols representing kernel iteration indices.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises NotImplementedError: this method is abstract.
        '''
        raise NotImplementedError(
            "Abstract property. Which symbols are iteration indices is"
            " API-specific.")

    @property
    def data_arguments(self):
        '''
        :returns: list of symbols representing kernel data arguments.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises NotImplementedError: this method is abstract.
        '''
        raise NotImplementedError(
            "Abstract property. Which symbols are data arguments is"
            " API-specific.")

    def copy_external_import(self, imported_var, tag=None):
        '''
        Copy the given imported variable (and its referenced ContainerSymbol if
        needed) into the SymbolTable.

        :param imported_var: the variable to be copied in.
        :type imported_var: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param str tag: a tag identifier for the new copy, by default no tag \
            is given.

        :raises TypeError: if the given variable is not an imported variable.
        :raises KeyError: if the given variable name already exists in the \
            symbol table.

        '''
        if not isinstance(imported_var, DataSymbol):
            raise TypeError(
                f"The imported_var argument of "
                f"SymbolTable.copy_external_import method should be a "
                f"DataSymbol, but found '{type(imported_var).__name__}'.")

        if not imported_var.is_import:
            raise TypeError(
                f"The imported_var argument of SymbolTable.copy_external_"
                f"import method should have an ImportInterface interface, "
                f"but found '{type(imported_var.interface).__name__}'.")

        external_container_name = imported_var.interface.container_symbol.name

        # If the Container is not yet in the SymbolTable we need to
        # create one and add it.
        if external_container_name not in self:
            self.add(ContainerSymbol(external_container_name))
        container_ref = self.lookup(external_container_name)

        # Copy the variable into the SymbolTable with the appropriate interface
        if imported_var.name not in self:
            new_symbol = imported_var.copy()
            # Update the interface of this new symbol
            new_symbol.interface = ImportInterface(container_ref)
            self.add(new_symbol, tag)
        else:
            # If it already exists it must refer to the same Container and have
            # the same tag.
            local_instance = self.lookup(imported_var.name)
            if not (local_instance.is_import and
                    local_instance.interface.container_symbol.name ==
                    external_container_name):
                raise KeyError(
                    f"Couldn't copy '{imported_var}' into the SymbolTable. The"
                    f" name '{imported_var.name}' is already used by another "
                    f"symbol.")
            if tag:
                # If the symbol already exists and a tag is provided
                try:
                    self.lookup_with_tag(tag)
                except KeyError:
                    # If the tag was not used, it will now be attached
                    # to the symbol.
                    self._tags[tag] = self.lookup(imported_var.name)

                # The tag should not refer to a different symbol
                if self.lookup(imported_var.name) != self.lookup_with_tag(tag):
                    raise KeyError(
                        f"Couldn't copy '{imported_var}' into the SymbolTable."
                        f" The tag '{tag}' is already used by another symbol.")

    def resolve_imports(self, container_symbols=None, symbol_target=None):
        ''' Try to resolve deferred and unknown information from imported
        symbols in this symbol table by searching for their definitions in
        referred external container. A single symbol to resolve can be
        specified for a more targeted import.

        :param container_symbols: list of container symbols to search in \
            order to resolve imported symbols. Defaults to all container \
            symbols in the symbol table.
        :type container_symbols: list of \
            :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        :param symbol_target: If a symbol is given, this method will just \
            resolve information for the given symbol. Otherwise it will \
            resolve all possible symbols information. Defaults to None.
        :type symbol_target: :py:class:`psyclone.psyir.symbols.Symbol` \
            or NoneType

        :raises SymbolError: if a symbol name clash is found between multiple \
            imports or an import and a local symbol.
        :raises TypeError: if the provided container_symbols is not a list of \
            ContainerSymbols.
        :raises TypeError: if the provided symbol_target is not a Symbol.
        :raises KeyError: if a symbol_target has been specified but this has \
            not been found in any of the searched containers.

        '''
        if container_symbols is not None:
            if not isinstance(container_symbols, list):
                raise TypeError(
                    f"The resolve_imports container_symbols argument must be a"
                    f" list but found '{type(container_symbols).__name__}' "
                    f"instead.")
            for item in container_symbols:
                if not isinstance(item, ContainerSymbol):
                    raise TypeError(
                        f"The resolve_imports container_symbols argument list "
                        f"elements must be ContainerSymbols, but found a "
                        f"'{type(item).__name__}' instead.")
        else:
            # If no container_symbol is given, search in all the containers
            container_symbols = self.containersymbols

        if symbol_target and not isinstance(symbol_target, Symbol):
            raise TypeError(
                f"The resolve_imports symbol_target argument must be a Symbol "
                f"but found '{type(symbol_target).__name__}' instead.")

        for c_symbol in container_symbols:
            try:
                external_container = c_symbol.container
            # pylint: disable=broad-except
            except Exception:
                # Ignore this container if the associated module file has not
                # been found in the given include_path or any issue has arised
                # during parsing.
                # TODO #11: It would be useful to log this.
                continue

            # Examine all Symbols defined within this external container
            for symbol in external_container.symbol_table.symbols:
                if symbol.visibility == Symbol.Visibility.PRIVATE:
                    continue  # We must ignore this symbol

                if isinstance(symbol, ContainerSymbol):
                    # TODO #1540: We also skip other ContainerSymbols but in
                    # reality if this is a wildcard import we would have to
                    # process the nested external container.
                    continue

                # If we are just resolving a single specific symbol we don't
                # need to process this symbol unless the name matches.
                if symbol_target and symbol.name != symbol_target.name:
                    continue

                # This Symbol matches the name of a symbol in the current table
                if symbol.name in self:

                    symbol_match = self.lookup(symbol.name)
                    interface = symbol_match.interface
                    visibility = symbol_match.visibility

                    # If the import statement is not a wildcard import, the
                    # matching symbol must have the appropriate interface
                    # referring to this c_symbol
                    if not c_symbol.wildcard_import:
                        if not isinstance(interface, ImportInterface) or \
                                interface.container_symbol is not c_symbol:
                            continue  # It doesn't come from this import

                    # Found a match, update the interface if necessary or raise
                    # an error if it is an ambiguous match
                    if isinstance(interface, UnresolvedInterface):
                        # Now we know where the symbol is coming from
                        interface = ImportInterface(c_symbol)
                    elif isinstance(interface, ImportInterface):
                        # If it is already an ImportInterface we don't need
                        # to update the interface information
                        pass
                    else:
                        raise SymbolError(
                            f"Found a name clash with symbol '{symbol.name}' "
                            f"when importing symbols from container "
                            f"'{c_symbol.name}'.")

                    # If the external symbol is a subclass of the local
                    # symbol_match, copy the external symbol properties,
                    # otherwise ignore this step.
                    if isinstance(symbol, type(symbol_match)):
                        # pylint: disable=unidiomatic-typecheck
                        if type(symbol) != type(symbol_match):
                            if isinstance(symbol, TypedSymbol):
                                # All TypedSymbols have a mandatory datatype
                                # argument
                                symbol_match.specialise(
                                    type(symbol), datatype=symbol.datatype)
                            else:
                                symbol_match.specialise(type(symbol))

                        symbol_match.copy_properties(symbol)
                        # Restore the interface and visibility as this are
                        # local (not imported) properties
                        symbol_match.interface = interface
                        symbol_match.visibility = visibility

                    if symbol_target:
                        # If we were looking just for this symbol we don't need
                        # to continue searching
                        return
                else:
                    if c_symbol.wildcard_import:
                        # This symbol is PUBLIC and inside a wildcard import,
                        # so it needs to be declared in the symbol table.
                        new_symbol = symbol.copy()
                        new_symbol.interface = ImportInterface(c_symbol)
                        new_symbol.visibility = self.default_visibility
                        self.add(new_symbol)

        if symbol_target:
            raise KeyError(
                f"The target symbol '{symbol_target.name}' was not found in "
                f"any of the searched containers: "
                f"{[cont.name for cont in container_symbols]}.")

    def rename_symbol(self, symbol, name):
        '''
        Rename the given symbol which should belong to this symbol table
        with the new name provided.

        :param symbol: the symbol to be renamed.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str name: the new name.

        :raises TypeError: if the symbol is not a Symbol.
        :raises TypeError: if the name is not a str.
        :raises ValueError: if the given symbol does not belong to this \
                            symbol table.
        :raises KeyError: if the given variable name already exists in the \
                          symbol table.

        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(
                f"The symbol argument of rename_symbol() must be a Symbol, but"
                f" found: '{type(symbol).__name__}'.")

        if symbol not in self.symbols:
            raise ValueError(
                f"The symbol argument of rename_symbol() must belong to this "
                f"symbol_table instance, but '{symbol}' does not.")

        if not isinstance(name, str):
            raise TypeError(
                f"The name argument of rename_symbol() must be a str, but"
                f" found: '{type(symbol).__name__}'.")

        if self._normalize(name) in self._symbols:
            raise KeyError(
                f"The name argument of rename_symbol() must not already exist "
                f"in this symbol_table instance, but '{name}' does.")

        # Delete current dictionary entry
        del self._symbols[self._normalize(symbol.name)]

        # Rename symbol using protected access as the Symbol class should not
        # expose a name attribute setter.
        # pylint: disable=protected-access
        symbol._name = name

        # Re-insert modified symbol
        self.add(symbol)

    def has_wildcard_imports(self):
        '''
        Searches this symbol table and then up through any parent symbol
        tables for a ContainerSymbol that has a wildcard import.

        :returns: True if a wildcard import is found, False otherwise.
        :rtype: bool

        '''
        current_table = self
        while current_table:
            for sym in current_table.containersymbols:
                if sym.wildcard_import:
                    return True
            current_table = current_table.parent_symbol_table()
        return False

    def view(self):
        '''
        :returns: a representation of this Symbol Table.
        :rtype: str

        '''
        return str(self)

    def __str__(self):
        header = "Symbol Table"
        if self.node:
            header += f" of {self.node.coloured_name(False)}"
            if hasattr(self.node, 'name'):
                header += f" '{self.node.name}'"
        header += ":"
        header += "\n" + "-" * len(header) + "\n"

        return header + "\n".join(map(str, self._symbols.values())) + "\n"

    @property
    def scope(self):
        '''
        :returns: the scope associated to this symbol table.
        :rtype: :py:class:`psyclone.psyir.nodes.ScopingNode`

        '''
        return self._node

    def detach(self):
        ''' Detach this symbol table from the associated scope and return self.

        :returns: this symbol table.
        :rtype: py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        if self._node:
            # pylint: disable=protected-access
            self._node._symbol_table = None
            self._node = None
        return self

    def attach(self, node):
        ''' Attach this symbol table to the provided scope.

        :param node: the scoped node this symbol table will attach to.
        :type node: py:class:`psyclone.psyir.nodes.ScopingNode`

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import ScopingNode
        if not isinstance(node, ScopingNode):
            raise TypeError(
                f"A SymbolTable must be attached to a ScopingNode"
                f" but found '{type(node).__name__}'.")

        if node.symbol_table is not None:
            raise ValueError(
                "The provided scope already has a symbol table attached "
                "to it. You may need to detach that one first.")

        if self._node is not None:
            raise ValueError(
                f"The symbol table is already bound to another "
                f"scope ({self.node.node_str(False)}). Consider "
                f"detaching or deepcopying the symbol table first.")

        self._node = node
        # pylint: disable=protected-access
        node._symbol_table = self
