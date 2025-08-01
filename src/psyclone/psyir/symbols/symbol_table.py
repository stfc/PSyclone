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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab
#          J. Remy, Université Grenoble Alpes, Inria
# -----------------------------------------------------------------------------

''' This module contains the SymbolTable implementation. '''

# pylint: disable=too-many-lines

from collections import OrderedDict
from collections.abc import Iterable
import inspect
import copy
import logging
from typing import Any, List, Optional, Set, Union

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyir.symbols import (
    DataSymbol, ContainerSymbol, DataTypeSymbol,
    ImportInterface, RoutineSymbol, Symbol, SymbolError, UnresolvedInterface)
from psyclone.psyir.symbols.intrinsic_symbol import IntrinsicSymbol
from psyclone.psyir.symbols.typed_symbol import TypedSymbol


# Used to provide a unique default value for methods within the
# SymbolTable class. This enables us to determine when the user has
# supplied a value of 'None' for arguments that have a default value.
DEFAULT_SENTINEL = object()


class SymbolTable():
    # pylint: disable=too-many-public-methods
    '''Encapsulates the symbol table and provides methods to add new
    symbols and look up existing symbols. Nested scopes are supported
    and, by default, the add and lookup methods take any ancestor
    symbol tables into consideration (ones attached to nodes that are
    ancestors of the node that this symbol table is attached to). If the
    default visibility is not specified then it defaults to
    Symbol.Visbility.PUBLIC.

    :param node: reference to the Schedule or Container to which this
        symbol table belongs.
    :type node: Optional[:py:class:`psyclone.psyir.nodes.Schedule` |
                         :py:class:`psyclone.psyir.nodes.Container`]
    :param default_visibility: optional default visibility value for this
        symbol table, if not provided it defaults to PUBLIC visibility.
    :type default_visibillity: Optional[
        :py:class:`psyclone.psyir.symbols.Symbol.Visibility`]

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

        :param scope_limit: optional Node which limits the symbol search
            space to the symbol tables of the nodes within the given scope.
            If it is None (the default), the whole scope (all symbol tables
            in ancestor nodes) is searched otherwise ancestors of the
            scope_limit node are not searched.
        :type scope_limit: Optional[:py:class:`psyclone.psyir.nodes.Node`]

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
        :type scope_limit: Optional[:py:class:`psyclone.psyir.nodes.Node`]

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

    def deep_copy(self, new_node=None):
        '''Create a copy of the symbol table with new instances of the
        top-level data structures and also new instances of the symbols
        contained in these data structures. Modifying a symbol attribute
        will not affect the equivalent named symbol in the original symbol
        table.

        The only attribute not copied is the _node reference to the scope,
        since that scope can only have one symbol table associated to it.

        :param new_node: the PSyIR Node to be associated with the copied
            table (if different from self.node).
        :type new_node: :py:class:`psyclone.psyir.nodes.ScopingNode`

        :returns: a deep copy of this symbol table.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        # pylint: disable=protected-access
        new_st = type(self)()
        if new_node:
            new_st._node = new_node

        # Make a copy of each symbol in the symbol table ensuring we do any
        # ContainerSymbols first as there may be imports from them.
        for symbol in self.containersymbols:
            new_st.add(symbol.copy())
        for symbol in self.symbols:
            if not isinstance(symbol, ContainerSymbol):
                new_sym = symbol.copy()
                if new_sym.is_import:
                    name = new_sym.interface.container_symbol.name
                    new_sym.interface.container_symbol = new_st.lookup(name)
                new_st.add(new_sym)

        # Prepare the new argument list
        new_arguments = []
        for name in [arg.name for arg in self.argument_list]:
            new_arguments.append(new_st.lookup(name))
        new_st.specify_argument_list(new_arguments)

        # Prepare the new tag dict
        for tag, symbol in self._tags.items():
            try:
                new_st._tags[tag] = new_st.lookup(symbol.name)
            except KeyError:
                # TODO 898: If the lookup fails it means that the symbol was
                # removed from the symbol table but not the tags dictionary
                pass

        # Update any references to Symbols within Symbols (initial values,
        # precision etc.)
        for symbol in new_st.symbols:
            symbol.replace_symbols_using(new_st)

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

    @classmethod
    def _has_same_name(cls, first, second):
        ''' Compare if two symbols have the same normalized name. For
        convenience it accepts symbols and strings.

        :param first: first item of the comparison.
        :type first: str | :py:class:`psyclone.psyir.symbols.Symbol`
        :param second: second item of the comparison.
        :type second: str | :py:class:`psyclone.psyir.symbols.Symbol`

        :returns: whether the two symbols names are the same.
        :rtype: bool

        '''
        string1 = first if isinstance(first, str) else first.name
        string2 = second if isinstance(second, str) else second.name
        return cls._normalize(string1) == cls._normalize(string2)

    def new_symbol(self, root_name=None, tag=None, shadowing=False,
                   symbol_type=None, allow_renaming=True, **symbol_init_args):
        ''' Create a new symbol. Optional root_name and shadowing
        arguments can be given to choose the name following the rules of
        next_available_name(). An optional tag can also be given.
        By default it creates a generic symbol but a symbol_type argument
        and any additional initialization keyword arguments of this
        symbol_type can be provided to refine the created Symbol.

        :param root_name: optional name to use when creating a
                          new symbol name. This will be appended
                          with an integer if the name
                          clashes with an existing symbol name.
        :type root_name: Optional[str]
        :param str tag: optional tag identifier for the new symbol.
        :param bool shadowing: optional logical flag indicating whether the
            name can be overlapping with a symbol in any of the ancestors
            symbol tables. Defaults to False.
        :param symbol_type: class type of the new symbol.
        :type symbol_type: type object of class (or subclasses) of
                           :py:class:`psyclone.psyir.symbols.Symbol`
        :param bool allow_renaming: whether to allow the newly created
                                    symbol to be renamed from root_name.
                                    Defaults to True.
        :param symbol_init_args: arguments to create a new symbol.
        :type symbol_init_args: unwrapped dict[str, Any]

        :raises TypeError: if the type_symbol argument is not the type of a
                           Symbol object class or one of its subclasses.
        :raises SymbolError: if the the symbol needs to be created but would
                             need to be renamed to be created and
                             allow_renaming is False.

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
        # TODO #2546 - we should disallow renaming of UnsupportedFortranTypes
        if (not allow_renaming and available_name != root_name):
            raise SymbolError(
                f"Cannot create symbol '{root_name}' as a symbol with that "
                f"name already exists in this scope, and renaming is "
                f"disallowed."
            )
        if "interface" in symbol_init_args and available_name != root_name:
            interface = symbol_init_args["interface"]

            if isinstance(interface, ImportInterface):
                # Since the name we're giving the new symbol is not exactly
                # the specified root name then we have to take care if this
                # symbol is imported. If it's not already being renamed then
                # we specify its original name as being the supplied root name.
                orig_name = (root_name if not interface.orig_name else
                             interface.orig_name)
                symbol_init_args["interface"] = ImportInterface(
                    symbol_init_args["interface"].container_symbol,
                    orig_name
                )
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

    def find_or_create_tag(self, tag, root_name=None,
                           **new_symbol_args):
        ''' Lookup a tag, if it doesn't exist create a new symbol with the
        given tag. By default it creates a generic Symbol with the tag as the
        root of the symbol name. Optionally, a different root_name or any of
        the arguments available in the new_symbol() method can be given to
        refine the name and the type of the created Symbol.

        :param str tag: tag identifier.
        :param str root_name: optional name of the new symbol if it needs \
                              to be created. Otherwise it is ignored.
        :param bool shadowing: optional logical flag indicating whether the
            name can be overlapping with a symbol in any of the ancestors
            symbol tables. Defaults to False.
        :param symbol_type: class type of the new symbol.
        :type symbol_type: type object of class (or subclasses) of
                           :py:class:`psyclone.psyir.symbols.Symbol`
        :param bool allow_renaming: whether to allow the newly created
        :param bool allow_renaming: whether to allow the newly created
                                    symbol to be renamed from root_name.
                                    Defaults to True.
        :param new_symbol_args: arguments to create a new symbol.
        :type new_symbol_args: unwrapped dict[str, Any]

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

        :raises InternalError: if the new_symbol argument is not a symbol.
        :raises KeyError: if the symbol name is already in use.
        :raises KeyError: if a tag is supplied and it is already in use.

        '''
        if not isinstance(new_symbol, Symbol):
            raise InternalError(f"Symbol '{new_symbol}' is not a symbol, but "
                                f"'{type(new_symbol).__name__}'.'")

        key = self._normalize(new_symbol.name)
        if key in self._symbols:
            raise KeyError(f"Symbol table already contains a symbol with "
                           f"name '{new_symbol.name}'.")

        # TODO #1734 - enable this check to ensure that an imported Symbol is
        # only ever added to the table containing the ContainerSymbol from
        # which it is imported.
        # if new_symbol.is_import:
        #     if (new_symbol.interface.container_symbol not in
        #             self._symbols.values()):
        #         raise SymbolError(
        #             f"Symbol '{new_symbol.name}' is imported from Container "
        #             f"'{new_symbol.interface.container_symbol.name}' but the"
        #             f" associated ContainerSymbol does not exist in this "
        #             f"table.")

        if tag:
            if tag in self.get_tags():
                raise KeyError(
                    f"This symbol table, or an outer scope ancestor symbol "
                    f"table, already contains the tag '{tag}' for the symbol"
                    f" '{self.lookup_with_tag(tag).name}', so it can not be "
                    f"associated with symbol '{new_symbol.name}'.")
            self._tags[tag] = new_symbol

        self._symbols[key] = new_symbol

    def check_for_clashes(self, other_table, symbols_to_skip=()):
        '''
        Checks the symbols in the supplied table against those in
        this table. If there is a name clash that cannot be resolved by
        renaming then a SymbolError is raised. Any symbols appearing
        in `symbols_to_skip` are excluded from the checks.

        :param other_table: the table for which to check for clashes.
        :type other_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param symbols_to_skip: an optional list of symbols to exclude from
            the check.
        :type symbols_to_skip: Iterable[
            :py:class:`psyclone.psyir.symbols.Symbol`]

        :raises TypeError: if symbols_to_skip is supplied but is not an
            instance of Iterable.
        :raises SymbolError: if there would be an unresolvable name clash
            when importing symbols from `other_table` into this table.

        '''
        # pylint: disable-next=import-outside-toplevel
        from psyclone.psyir.nodes import IntrinsicCall

        if not isinstance(symbols_to_skip, Iterable):
            raise TypeError(
                f"check_for_clashes: 'symbols_to_skip' must be an instance of "
                f"Iterable but got '{type(symbols_to_skip).__name__}'")

        if other_table is self:
            return

        # Check whether there are any wildcard imports common to both tables.
        self_imports = set(sym.name for sym in self.wildcard_imports())
        other_imports = set(sym.name for sym in other_table.wildcard_imports())
        shared_wildcard_imports = self_imports & other_imports
        # Any wildcard imports that appear in one table but not in the other.
        unique_wildcard_imports = self_imports ^ other_imports

        for other_sym in other_table.symbols:
            if other_sym.name not in self or other_sym in symbols_to_skip:
                continue
            # We have a name clash.
            this_sym = self.lookup(other_sym.name)

            # If they are both ContainerSymbols then that's OK as they refer to
            # the same Container.
            if (isinstance(this_sym, ContainerSymbol) and
                    isinstance(other_sym, ContainerSymbol)):
                continue

            # If they are both IntrinsicSymbol then that's fine.
            if (isinstance(this_sym, IntrinsicSymbol) and
                    isinstance(other_sym, IntrinsicSymbol)):
                continue

            if other_sym.is_import and this_sym.is_import:
                # Both symbols are imported. That's fine as long as they have
                # the same import interface (are imported from the same
                # Container and refer to the same Symbol in that Container).
                if this_sym.interface != other_sym.interface:
                    raise SymbolError(
                        f"This table has an import of '{this_sym.name}' via "
                        f"interface '{this_sym.interface}' but the supplied "
                        f"table imports it via '{other_sym.interface}'.")
                continue

            if other_sym.is_unresolved and this_sym.is_unresolved:
                # Both Symbols are unresolved.
                if shared_wildcard_imports and not unique_wildcard_imports:
                    # The tables have one or more wildcard imports in common
                    # and no wildcard imports unique to one table. Therefore
                    # the two symbols represent the same memory location.
                    continue
                if not (self_imports or other_imports):
                    # Neither table has any wildcard imports.
                    try:
                        # An unresolved symbol representing an intrinisc is OK
                        _ = IntrinsicCall.Intrinsic[this_sym.name.upper()]
                        # Take this opportunity to specialise the symbol(s).
                        if not isinstance(this_sym, IntrinsicSymbol):
                            this_sym.specialise(IntrinsicSymbol)
                        if not isinstance(other_sym, IntrinsicSymbol):
                            other_sym.specialise(IntrinsicSymbol)
                        continue
                    except KeyError:
                        pass
                # We can't rename a symbol if we don't know its origin.
                raise SymbolError(
                    f"A symbol named '{this_sym.name}' is present but "
                    f"unresolved in both tables.")

            if other_sym.is_unresolved or this_sym.is_unresolved:
                # Only one is unresolved. If the resolved one is imported,
                # could the unresolved one be imported from the same
                # location?
                if this_sym.is_unresolved:
                    rsym = other_sym
                    imports = self_imports
                else:
                    rsym = this_sym
                    imports = other_imports
                if rsym.is_import:
                    # The resolved symbol is an import (as opposed to being
                    # local to one of the tables).
                    import_source = rsym.interface.container_symbol.name
                    if import_source in imports:
                        # It is being imported from the same location.
                        continue

                    raise SymbolError(
                        f"A symbol named '{this_sym.name}' is present in both "
                        f"tables but is unresolved in one. That scope does "
                        f"not contain a direct wildcard import from the "
                        f"module '{import_source}' from which it is "
                        f"imported in the other scope.")

            # Can either of them be renamed?
            try:
                self.rename_symbol(this_sym, "", dry_run=True)
            except SymbolError as err1:
                try:
                    other_table.rename_symbol(other_sym, "", dry_run=True)
                except SymbolError as err2:
                    # pylint: disable=raise-missing-from
                    raise SymbolError(
                        f"There is a name clash for symbol '{this_sym.name}' "
                        f"that cannot be resolved by renaming "
                        f"one of the instances because:\n- {err1}\n- {err2}")

    def _add_container_symbols_from_table(self, other_table):
        '''
        Takes container symbols from the supplied symbol table and adds them to
        this table. All references to each container symbol are also updated.
        (This is a preliminary step to adding all symbols from other_table to
        this table.)

        :param other_table: the symbol table from which to take container
                            symbols.
        :type other_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        for csym in other_table.containersymbols:
            outer_sym = self.lookup(csym.name, otherwise=None)
            if not outer_sym:
                # No clash so add the symbol.
                self.add(csym)
            else:
                # There is a match but which scope is it in?
                if not self.node:
                    outer_table = self
                else:
                    outer_table = outer_sym.find_symbol_table(self.node)
                if not isinstance(outer_sym, ContainerSymbol):
                    # The match is not for a ContainerSymbol so can it
                    # be renamed?
                    next_name = outer_table.next_available_name(
                        outer_sym.name,
                        other_table=other_table)
                    outer_table.rename_symbol(outer_sym, next_name)
                    outer_table.add(csym)
                else:
                    # The symbol in an outer scope (outer_sym) is also a
                    # ContainerSymbol so must refer to the same Container.
                    # If there is a wildcard import from this Container
                    # then we update the one in the outer scope to have
                    # that too.
                    if csym.wildcard_import:
                        outer_sym.wildcard_import = True
            # We must update all references to this ContainerSymbol
            # so that they point to the one in scope in this table instead.
            imported_syms = other_table.symbols_imported_from(csym)
            for isym in imported_syms:
                other_sym = self.lookup(isym.name, otherwise=None)
                if other_sym:
                    # We have a potential clash with a symbol imported
                    # into the other table.
                    if not other_sym.is_import:
                        # The calling merge() method has already checked that
                        # we don't have a clash between symbols of the same
                        # name imported from different containers. We don't
                        # support renaming an imported symbol but the
                        # symbol in this table can be renamed so we do that.
                        self.rename_symbol(
                            other_sym,
                            self.next_available_name(
                                other_sym.name, other_table=other_table))
                isym.interface = ImportInterface(
                        self.lookup(csym.name),
                        orig_name=isym.interface.orig_name)

    def _add_symbols_from_table(self, other_table, symbols_to_skip=()):
        '''
        Takes symbols from the supplied symbol table and adds them to this
        table (unless they appear in `symbols_to_skip`).
        _add_container_symbols_from_table() MUST have been called
        before this method in order to handle any Container Symbols and update
        those Symbols imported from them.

        :param other_table: the symbol table from which to add symbols.
        :type other_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param symbols_to_skip: an optional list of symbols to exclude from
                                the merge.
        :type symbols_to_skip: Iterable[
            :py:class:`psyclone.psyir.symbols.Symbol`]

        :raises InternalError: if an imported symbol is found that has not
            already been updated to refer to a Container in this table.

        '''
        for old_sym in other_table.symbols:

            if old_sym in symbols_to_skip or isinstance(old_sym,
                                                        ContainerSymbol):
                # We've dealt with Container symbols in _add_container_symbols.
                continue

            try:
                self.add(old_sym)

            except KeyError:
                # We have a clash with a symbol in this table.
                self._handle_symbol_clash(old_sym, other_table)

    def _handle_symbol_clash(self, old_sym, other_table):
        '''
        Adds the supplied Symbol to the current table in the presence
        of a name clash. `check_for_clashes` MUST have been called
        prior to this method in order to check for any unresolvable cases.

        :param old_sym: the Symbol to be added to self.
        :type old_sym: :py:class:`psyclone.psyir.symbols.Symbol`
        :param other_table: the other table containing the symbol that we are
                            trying to add to self.
        :type other_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :raises InternalError: if an unresolvable name clash is found (since
            this should have been detected in a previous call to
            check_for_clashes()).

        '''
        self_sym = self.lookup(old_sym.name)
        if old_sym.is_import:
            # The clashing symbol is imported from a Container and the table
            # must contain the ContainerSymbol from which it is imported.
            self_csym = self.lookup(self_sym.interface.container_symbol.name)
            if old_sym.interface.container_symbol is self_csym:
                return

            if self._has_same_name(old_sym.interface.container_symbol,
                                   self_csym):
                # The Containers have the same name so must in fact be the
                # same. Update the symbol's interface to point to the Container
                # that is in scope here.
                old_sym.interface.container_symbol = self_csym
                return

            raise InternalError(
                f"Symbol '{old_sym.name}' imported from '{self_csym.name}' "
                f"clashes with a Symbol of the same name imported from "
                f"'{old_sym.interface.container_symbol.name}'. This should "
                f"have been caught by SymbolTable.check_for_clashes().")

        self_sym = self.lookup(old_sym.name)
        if old_sym.is_unresolved and self_sym.is_unresolved:
            # The clashing symbols are both unresolved so we ASSUME that
            # check_for_clashes has previously determined that they must
            # refer to the same thing and we don't have to do anything.
            return

        # A Symbol with the same name already exists so we attempt to rename
        # first the one that we are adding and failing that, the existing
        # symbol in this table.
        new_name = self.next_available_name(
            old_sym.name, other_table=other_table)
        try:
            other_table.rename_symbol(old_sym, new_name)
            self.add(old_sym)
        except SymbolError:
            self.rename_symbol(self_sym, new_name)
            self.add(old_sym)

    def merge(self, other_table, symbols_to_skip=()):
        '''Merges all of the symbols found in `other_table` into this
        table. Symbol objects in *either* table may be renamed in the
        event of clashes.

        Any Symbols appearing in `symbols_to_skip` are excluded.

        :param other_table: the symbol table from which to add symbols.
        :type other_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param symbols_to_skip: an optional list of Symbols to exclude from
                                the merge.
        :type symbols_to_skip: Iterable[
            :py:class:`psyclone.psyir.symbols.Symbol`]

        :raises TypeError: if `other_table` is not a SymbolTable.
        :raises TypeError: if `symbols_to_skip` is not an Iterable.
        :raises SymbolError: if name clashes prevent the merge.

        '''
        if not isinstance(other_table, SymbolTable):
            raise TypeError(f"SymbolTable.merge() expects a SymbolTable "
                            f"instance but got '{type(other_table).__name__}'")
        if not isinstance(symbols_to_skip, Iterable):
            raise TypeError(
                f"SymbolTable.merge() expects 'symbols_to_skip' to be an "
                f"Iterable but got '{type(symbols_to_skip).__name__}'")

        try:
            self.check_for_clashes(other_table,
                                   symbols_to_skip=symbols_to_skip)
        except SymbolError as err:
            raise SymbolError(
                f"Cannot merge {other_table.view()} with {self.view()} due to "
                f"unresolvable name clashes: {err.value}") from err

        # Deal with any Container symbols first.
        self._add_container_symbols_from_table(other_table)

        # Copy each Symbol from the supplied table into this one, excluding
        # ContainerSymbols and any listed in `symbols_to_skip`.
        self._add_symbols_from_table(other_table,
                                     symbols_to_skip=symbols_to_skip)

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
        if self._has_same_name(symbol1.name, symbol2.name):
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

    def lookup(
            self,
            name: str,
            visibility: Optional[Union[Symbol.Visibility,
                                       List[Symbol.Visibility]]] = None,
            scope_limit=None,
            otherwise: Any = DEFAULT_SENTINEL) -> Any:
        '''Look up a symbol in the symbol table. The lookup can be limited
        by visibility (e.g. just show public methods) or by scope_limit (e.g.
        just show symbols up to a certain scope).

        :param name: name of the symbol.
        :param visibilty: the visibility or list of visibilities that the
                          symbol must have.
        :param scope_limit: optional Node which limits the symbol
            search space to the symbol tables of the nodes within the
            given scope. If it is None (the default), the whole
            scope (all symbol tables in ancestor nodes) is searched
            otherwise ancestors of the scope_limit node are not
            searched.
        :type scope_limit: Optional[:py:class:`psyclone.psyir.nodes.Node`]
        :param otherwise: an optional value to return if the named symbol
            cannot be found (rather than raising a KeyError).

        :returns: the symbol with the given name and, if specified, visibility.
                  If no match is found and 'otherwise' is supplied then that
                  value is returned.

        :raises TypeError: if the name argument is not a string.
        :raises SymbolError: if the name exists in the Symbol Table but does
                             not have the specified visibility.
        :raises TypeError: if the visibility argument has the wrong type.
        :raises KeyError: if the given name is not in the Symbol Table and
                          `otherwise` is not supplied.

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
            if otherwise is DEFAULT_SENTINEL:
                # No 'otherwise' value supplied so we raise an exception.
                raise KeyError(f"Could not find '{name}' in the Symbol "
                               f"Table.") from err
            return otherwise

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
        `new_symbol`. Any references to `old_symbol` in the PSyIR tree
        associated with this table (if any) will also be updated.

        :param old_symbol: the symbol to remove from the table.
        :type old_symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param new_symbol: the symbol to add to the table.
        :type new_symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if either old/new_symbol are not Symbols.
        :raises SymbolError: if `old_symbol` and `new_symbol` don't have
                             the same name (after normalising).
        '''
        if not isinstance(old_symbol, Symbol):
            raise TypeError(f"Symbol to remove must be of type Symbol but "
                            f"got '{type(old_symbol).__name__}'")
        if not isinstance(new_symbol, Symbol):
            raise TypeError(f"Symbol to add must be of type Symbol but "
                            f"got '{type(new_symbol).__name__}'")
        if not self._has_same_name(old_symbol, new_symbol):
            raise SymbolError(
                f"Cannot swap symbols that have different names, got: "
                f"'{old_symbol.name}' and '{new_symbol.name}'")
        for sym in self.symbols:
            sym.replace_symbols_using(new_symbol)
        if self.node:
            # Update the PSyIR tree associated with this table.
            self.node.replace_symbols_using(new_symbol)
        self.remove(old_symbol)
        self.add(new_symbol)

    def _validate_remove_routinesymbol(self, symbol):
        '''
        Checks whether the supplied RoutineSymbol can be removed from this
        table.

        If the symbol is the target of a Call or is a member of an interface
        definition then removal is not possible.

        :param symbol: the Symbol to validate for removal.
        :type symbol: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        :raises ValueError: if the Symbol cannot be removed.

        '''
        parent_table = self.parent_symbol_table()
        if parent_table:
            try:
                # If the supplied symbol object has already been added to an
                # outer scope then we can safely delete its entry in this table
                # as all references to it will remain valid.
                outer_sym = parent_table.lookup(symbol.name)
                if outer_sym is symbol:
                    return
            except KeyError:
                pass

        # Check for any references to it.
        # pylint: disable=import-outside-toplevel
        from psyclone.core import Signature, VariablesAccessMap
        vam = VariablesAccessMap()
        if self.node:
            vam.update(self.node.reference_accesses())
        vam.update(self.reference_accesses())
        sig = Signature(symbol.name)
        if sig not in vam:
            return

        # TODO #2424 - ideally SingleVariableAccessInfo.AccessInfo or
        # Signature would store the actual Symbol that the access is to. In
        # the absence of that, we have to examine each access to determine
        # the Symbol.
        from psyclone.psyir.nodes.reference import Reference
        from psyclone.psyir.symbols.generic_interface_symbol import (
            GenericInterfaceSymbol)
        try:
            for access in vam[sig].all_accesses:
                if isinstance(access.node, GenericInterfaceSymbol):
                    for rinfo in access.node.routines:
                        if rinfo.symbol is symbol:
                            raise ValueError()
                else:
                    for ref in access.node.walk(Reference):
                        if ref.symbol is symbol:
                            raise ValueError()
        except ValueError:
            # pylint: disable-next=raise-missing-from
            raise ValueError(
                f"Cannot remove RoutineSymbol '{symbol.name}' because it is "
                f"referenced by {access.description}")

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

        :param symbol: the symbol to remove.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the supplied parameter is not of type Symbol.
        :raises NotImplementedError: the removal of this symbol type is not
                                     supported yet.
        :raises KeyError: if the supplied symbol is not in the symbol table.
        :raises ValueError: if the supplied container symbol is referenced
                            by one or more DataSymbols.
        :raises InternalError: if the supplied symbol is not the same as the
                               entry with that name in this SymbolTable.
        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(f"remove() expects a Symbol argument but found: "
                            f"'{type(symbol).__name__}'.")

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

        # RoutineSymbols require special consideration as they may be the
        # target of a Call or a member of a GenericInterfaceSymbol.
        if isinstance(symbol, RoutineSymbol):
            self._validate_remove_routinesymbol(symbol)

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

        :raises InternalError: if the entries of the SymbolTable are not
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

    def insert_argument(self, index, argument):
        '''
        Insert a new argument at a given index in the argument list and add
        it in the symbol table itself.

        :param int index: the position in the argument list where the new
                      argument should be inserted.
        :param argument: the new argument to add to the list.
        :type argument: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises InternalError: if the entries of the SymbolTable are not
                               self-consistent.
        :raises TypeError: if the supplied index is not an integer.
        :raises TypeError: if the supplied argument is not a DataSymbol.
        :raises ValueError: if the supplied argument is not marked as a
                            kernel argument.
        '''
        if not isinstance(index, int):
            raise TypeError(
                f"Expected an integer index for the position at which to "
                f"insert the argument but found '{type(index).__name__}'.")
        if not isinstance(argument, DataSymbol):
            raise TypeError(
                f"Expected a DataSymbol for the argument to insert but found "
                f"'{type(argument).__name__}'.")
        if not argument.is_argument:
            raise ValueError(
                f"DataSymbol '{argument.name}' is not marked as a kernel "
                "argument.")

        self._argument_list.insert(index, argument)
        self.add(argument)

        try:
            self._validate_arg_list(self._argument_list)
            self._validate_non_args()
        except ValueError as err:
            # If the SymbolTable is inconsistent at this point then
            # we have an InternalError.
            raise InternalError(str(err.args)) from err

    def append_argument(self, argument):
        '''
        Append the given argument to the argument list and add it in the symbol
        table itself. If the argument is already part of the argument_list it
        does nothing.

        :param argument: the new argument to add to the list.
        :type argument: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises InternalError: if the entries of the SymbolTable are not
                               self-consistent.
        :raises TypeError: if the supplied argument is not a DataSymbol.
        :raises ValueError: if the supplied argument is not marked as a
                            kernel argument.
        '''
        if not isinstance(argument, DataSymbol):
            raise TypeError(
                f"Expected a DataSymbol for the argument to insert but found "
                f"'{type(argument).__name__}'.")
        if not argument.is_argument:
            raise ValueError(
                f"DataSymbol '{argument.name}' is not marked as a kernel "
                "argument.")
        if argument in self._argument_list:
            return

        self._argument_list.append(argument)
        if argument not in self.get_symbols().values():
            self.add(argument)

        try:
            self._validate_arg_list(self._argument_list)
            self._validate_non_args()
        except ValueError as err:
            # If the SymbolTable is inconsistent at this point then
            # we have an InternalError.
            raise InternalError(str(err.args)) from err

    @staticmethod
    def _validate_arg_list(arg_list):
        '''
        Checks that the supplied list of Symbols are valid kernel arguments.

        :param arg_list: the proposed kernel arguments.
        :type param_list: list of :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if any item in the supplied list is not a
                           DataSymbol.
        :raises ValueError: if any of the symbols does not have an argument
                            interface.

        '''
        for symbol in arg_list:
            if not isinstance(symbol, DataSymbol):
                raise TypeError(f"Expected a list of DataSymbols but found an "
                                f"object of type '{type(symbol).__name__}'.")
            if not symbol.is_argument:
                raise ValueError(
                    f"DataSymbol '{symbol}' is listed as a kernel argument "
                    f"but has an interface of type "
                    f"'{type(symbol.interface).__name__}' "
                    f"rather than ArgumentInterface")

    def _validate_non_args(self):
        '''
        Performs internal consistency checks on the current entries in the
        SymbolTable that do not represent kernel arguments.

        :raises ValueError: if a symbol that is not in the argument list
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
        :rtype: List[:py:class:`psyclone.psyir.symbols.Symbol`]
        '''
        return list(self._symbols.values())

    @property
    def datasymbols(self):
        '''
        :returns: list of symbols representing data variables.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return [sym for sym in self._symbols.values() if
                isinstance(sym, DataSymbol)]

    @property
    def automatic_datasymbols(self):
        '''
        :returns: list of symbols representing automatic variables.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return [sym for sym in self.datasymbols if sym.is_automatic]

    @property
    def argument_datasymbols(self):
        '''
        :returns: list of symbols representing arguments.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return [sym for sym in self.datasymbols if sym.is_argument]

    @property
    def imported_symbols(self):
        '''
        :returns: list of symbols that have an imported interface (are \
            associated with data that exists outside the current scope).
        :rtype: List[:py:class:`psyclone.psyir.symbols.Symbol`]

        '''
        return [sym for sym in self.symbols if sym.is_import]

    @property
    def unresolved_datasymbols(self):
        '''
        :returns: list of symbols representing unresolved variables.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return [sym for sym in self.datasymbols if sym.is_unresolved]

    @property
    def precision_datasymbols(self):
        '''
        :returns: list of all symbols used to define the precision of \
                  other symbols within the table.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        '''
        # Accumulate into a set so as to remove any duplicates
        precision_symbols = set()
        for sym in self.datasymbols:
            # Not all types have the 'precision' attribute (e.g.
            # UnresolvedType)
            if (hasattr(sym.datatype, "precision") and
                    isinstance(sym.datatype.precision, DataSymbol)):
                precision_symbols.add(sym.datatype.precision)
        return list(precision_symbols)

    @property
    def containersymbols(self):
        '''
        :returns: a list of the ContainerSymbols present in the Symbol Table.
        :rtype: List[:py:class:`psyclone.psyir.symbols.ContainerSymbol`]
        '''
        return [sym for sym in self.symbols if isinstance(sym,
                                                          ContainerSymbol)]

    @property
    def datatypesymbols(self):
        '''
        :returns: the DataTypeSymbols present in the Symbol Table.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataTypeSymbol`]
        '''
        return [sym for sym in self.symbols if isinstance(sym, DataTypeSymbol)]

    @property
    def iteration_indices(self):
        '''
        :returns: list of symbols representing kernel iteration indices.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :raises NotImplementedError: this method is abstract.
        '''
        raise NotImplementedError(
            "Abstract property. Which symbols are iteration indices is"
            " API-specific.")

    @property
    def data_arguments(self):
        '''
        :returns: list of symbols representing kernel data arguments.
        :rtype: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

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
                    self._has_same_name(
                        local_instance.interface.container_symbol,
                        external_container_name)):
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

    def _import_symbols_from(self, csymbol: ContainerSymbol,
                             container,
                             symbol_target: Optional[Symbol] = None
                             ) -> Set[Symbol]:
        '''
        Imports symbols from the supplied Container into this table. If
        `target_symbol` is specified then only that symbol is imported.
        Otherwise, this method imports all symbols named in the import
        (i.e. if it corresponds to a USE xx, Only: yy) or, if it
        is a wildcard import, all public symbols in the source Container.

        :param csymbol: the ContainerSymbol from which to import.
        :param container: the PSyIR of the Container from which to import.
        :type container: :py:class:`psyclone.psyir.nodes.Container`
        :param symbol_target: optional, single symbol to attempt to import.

        :returns: the Symbols that have been added to this table.

        '''
        # Map from symbol name in source container to name at import site.
        qualified_imported_names = {}
        # Set holding all Symbols that we succeed in importing.
        imported_symbols = set()
        if not csymbol.wildcard_import:
            # The import from this Container is for certain, specific
            # symbols. Make a map holding the name of the symbol in the
            # source container and its name at the site of the import.
            for isym in self.symbols_imported_from(csymbol):
                iname = self._normalize(isym.name)
                if isym.interface.orig_name:
                    qualified_imported_names[
                        self._normalize(isym.interface.orig_name)] = iname
                else:
                    qualified_imported_names[iname] = iname

        # Examine all Symbols defined within this external container
        for imported_sym in container.symbol_table.symbols:
            if imported_sym.visibility == Symbol.Visibility.PRIVATE:
                continue  # We must ignore this symbol

            if isinstance(imported_sym, ContainerSymbol):
                # TODO #1540: We also skip other ContainerSymbols but in
                # reality if this is a wildcard import we would have to
                # process the nested external container.
                continue

            # If we are just resolving a single specific symbol then we don't
            # need to process this symbol unless the name matches.
            if symbol_target and not self._has_same_name(
                    imported_sym, symbol_target):
                continue

            norm_name = self._normalize(imported_sym.name)

            if (not csymbol.wildcard_import and
                    norm_name not in qualified_imported_names):
                # This symbol is not being imported.
                continue

            # Allow for symbol renaming on import:
            #   orig_name is the name of the symbol in the src Container;
            #   local_name is the name it has at the import site.
            if (qualified_imported_names and
                    norm_name != qualified_imported_names[norm_name]):
                orig_name = norm_name
                local_name = qualified_imported_names[norm_name]
            else:
                orig_name = None
                local_name = norm_name

            if local_name in self:
                # This Symbol matches the name of a symbol at the import site.
                local_sym = self.lookup(local_name)
                interface = local_sym.interface
                visibility = local_sym.visibility

                # Found a match, update the interface if necessary or raise
                # an error if it is an ambiguous match
                if isinstance(interface, UnresolvedInterface):
                    # Now we know where the symbol is coming from
                    interface = ImportInterface(csymbol,
                                                orig_name=orig_name)
                elif isinstance(interface, ImportInterface):
                    # If it is already an ImportInterface we don't need
                    # to update the interface information
                    pass
                else:
                    raise SymbolError(
                        f"Found a name clash with symbol '{imported_sym.name}'"
                        f" when importing symbols from container "
                        f"'{csymbol.name}'.")

                # If the external symbol is a subclass of the local
                # symbol_match, copy the external symbol properties,
                # otherwise ignore this step.
                if isinstance(imported_sym, type(local_sym)):
                    # pylint: disable=unidiomatic-typecheck
                    if type(imported_sym) is not type(local_sym):
                        if isinstance(imported_sym, TypedSymbol):
                            # All TypedSymbols have a mandatory datatype
                            # argument
                            local_sym.specialise(
                                type(imported_sym),
                                datatype=imported_sym.datatype)
                        else:
                            local_sym.specialise(type(imported_sym))

                    local_sym.copy_properties(imported_sym)
                    # Restore the interface and visibility as these are
                    # local (not imported) properties
                    local_sym.interface = interface
                    local_sym.visibility = visibility
            else:
                # This table did not already contain a symbol with this
                # name.
                if csymbol.wildcard_import:
                    # This symbol is PUBLIC and inside a wildcard import,
                    # so it needs to be declared in the symbol table.
                    local_sym = imported_sym.copy()
                    local_sym.interface = ImportInterface(csymbol)
                    local_sym.visibility = self.default_visibility
                    self.add(local_sym)
            imported_symbols.add(local_sym)
        return imported_symbols

    def _update_with_resolved_symbol(self, symbol: Symbol):
        '''
        Given a newly-resolved symbol, walk down through the scopes below
        the scope associated with this table and replace any instances of
        symbols that are now known to be this symbol.

        :param symbol: the Symbol that has been resolved.

        '''
        norm_name = self._normalize(symbol.name)
        # Keep a record of any wildcard imports in this scope as they can be
        # ignored since two symbols with the same name but of different types
        # can't be imported from >1 location.
        wildcards_to_skip = set(self.wildcard_imports(scope_limit=self.node))
        # Import here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import ScopingNode
        # Walk down through the scopes below this one.
        for scoping_node in self.node.walk(ScopingNode):
            if scoping_node is self.node:
                # Skip ourself.
                continue
            symbol_table = scoping_node.symbol_table
            test_symbol = symbol_table.lookup(norm_name,
                                              scope_limit=scoping_node,
                                              otherwise=None)
            if not test_symbol or not test_symbol.is_unresolved:
                # Either this table doesn't contain a symbol with the
                # same name as the imported one or it does but it is
                # resolved (and thus shadows this one) so we ignore it.
                continue

            # Check whether there are any wildcard imports in this scope (we've
            # already looked at any parent scopes) that could be bringing this
            # symbol into scope.
            # Get all wildcard imports into the current scope.
            wildcard_imports = set(symbol_table.wildcard_imports(
                scope_limit=self.node))
            if wildcard_imports != wildcards_to_skip:
                # There are wildcard imports so we can't be certain of
                # the origin of this symbol and therefore leave it as is. We
                # can't immediately return here because there may be siblings
                # of this ScopingNode that do not have a wildcard import.
                continue

            # We want to replace the local symbol with the newly
            # imported one in the outer scope (`symbol`).
            # Update any references to it within the SymbolTable itself.
            for sym in symbol_table.symbols:
                sym.replace_symbols_using(symbol)
            # Then update any references in the associated PSyIR tree.
            self.node.replace_symbols_using(symbol)
            # Finally, we can remove the local symbol.
            symbol_table.remove(test_symbol)

    def resolve_imports(self, container_symbols=None, symbol_target=None):
        ''' Try to resolve deferred and unknown information from imported
        symbols in this symbol table by searching for their definitions in
        referred external container. A single symbol to resolve can be
        specified for a more targeted import.

        :param container_symbols: list of container symbols to search in
            order to resolve imported symbols. Defaults to all container
            symbols in the symbol table.
        :type container_symbols: Iterable[
            :py:class:`psyclone.psyir.symbols.ContainerSymbol`]
        :param symbol_target: If a symbol is given, this method will just
            resolve information for the given symbol. Otherwise it will
            resolve all possible symbols information. Defaults to None.
        :type symbol_target: Optional[
            :py:class:`psyclone.psyir.symbols.Symbol`]

        :raises SymbolError: if a symbol name clash is found between multiple
            imports or an import and a local symbol.
        :raises TypeError: if the provided container_symbols is not an Iterable
            of ContainerSymbols.
        :raises TypeError: if the provided symbol_target is not a Symbol.
        :raises KeyError: if a symbol_target has been specified but this has
            not been found in any of the searched containers.

        '''
        if container_symbols is not None:
            if not isinstance(container_symbols, Iterable):
                raise TypeError(
                    f"The 'container_symbols' argument to resolve_imports() "
                    f"must be an Iterable but found "
                    f"'{type(container_symbols).__name__}' instead.")
            for item in container_symbols:
                if not isinstance(item, ContainerSymbol):
                    raise TypeError(
                        f"The 'container_symbols' argument to "
                        f"resolve_imports() must be an Iterable containing "
                        f"ContainerSymbols, but found a "
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
                external_container = c_symbol.find_container_psyir(
                    local_node=self.node)
            # pylint: disable-next=broad-except
            except Exception:
                external_container = None

            if not external_container:
                message = f"Module '{c_symbol.name}' not found"
                logger = logging.getLogger(__name__)
                logger.warning(message)
                continue

            imported_symbols = self._import_symbols_from(
                c_symbol,
                external_container,
                symbol_target=symbol_target)

            for isym in imported_symbols:
                # Determine if there is an Unresolved Symbol in a descendent
                # symbol table that matches the name of the symbol we are
                # importing. If there is no intervening wildcard import then
                # we must have now resolved that symbol so move it to this
                # symbol table.
                self._update_with_resolved_symbol(isym)

                if symbol_target:
                    # If we were looking just for this symbol we don't need
                    # to continue searching
                    return

        if symbol_target:
            raise KeyError(
                f"The target symbol '{symbol_target.name}' was not found in "
                f"any of the searched containers: "
                f"{[cont.name for cont in container_symbols]}.")

    def rename_symbol(self, symbol, name, dry_run=False):
        '''
        Rename the given symbol which should belong to this symbol table
        with the new name provided.

        :param symbol: the symbol to be renamed.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param str name: the new name.
        :param bool dry_run: if True then only the validation checks are
                             performed.

        :raises TypeError: if the symbol is not a Symbol.
        :raises TypeError: if the name is not a str.
        :raises ValueError: if the given symbol does not belong to this
                            symbol table.
        :raises KeyError: if the given variable name already exists in the
                symbol table.
        :raises SymbolError: if the specified Symbol is a ContainerSymbol, is
            imported/unresolved or is a formal routine argument.
        :raises SymbolError: if the specified Symbol is accessed within a
            CodeBlock in the scope of this table.
        :raises SymbolError: if the symbol has a common block interface.

        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(
                f"The symbol argument of rename_symbol() must be a Symbol, but"
                f" found: '{type(symbol).__name__}'.")

        if symbol not in self.symbols:
            raise ValueError(
                f"The symbol argument of rename_symbol() must belong to this "
                f"symbol_table instance, but '{symbol}' does not.")

        if isinstance(symbol, ContainerSymbol):
            raise SymbolError(f"Cannot rename symbol '{symbol.name}' because "
                              f"it is a ContainerSymbol.")

        if symbol.is_import:
            raise SymbolError(
                f"Cannot rename symbol '{symbol.name}' because it is imported "
                f"(from Container '{symbol.interface.container_symbol.name}')."
            )

        if symbol.is_unresolved:
            raise SymbolError(
                f"Cannot rename symbol '{symbol.name}' because it is "
                f"unresolved.")

        if symbol.is_argument:
            raise SymbolError(
                f"Cannot rename symbol '{symbol.name}' because it is a routine"
                f" argument and as such may be named in a Call.")

        if symbol.is_commonblock:
            raise SymbolError(
                f"Cannot rename symbol '{symbol.name}' because it has a "
                f"CommonBlock interface.")

        if not isinstance(name, str):
            raise TypeError(
                f"The name argument of rename_symbol() must be a str, but"
                f" found: '{type(symbol).__name__}'.")

        if self._normalize(name) in self._symbols:
            raise KeyError(
                f"The name argument of rename_symbol() must not already exist "
                f"in this symbol_table instance, but '{name}' does.")

        old_name = self._normalize(symbol.name)
        if self.node:
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import CodeBlock
            cblocks = self.node.walk(CodeBlock)
            for cblock in cblocks:
                sym_names = [self._normalize(sname) for sname in
                             cblock.get_symbol_names()]
                if old_name in sym_names:
                    cblk_txt = "\n".join(str(anode) for anode in
                                         cblock.get_ast_nodes)
                    raise SymbolError(
                        f"Cannot rename Symbol '{symbol.name}' because it is "
                        f"accessed in a CodeBlock:\n"
                        f"{cblk_txt}")

        if dry_run:
            return

        # Delete current dictionary entry
        del self._symbols[old_name]

        # Rename symbol using protected access as the Symbol class should not
        # expose a name attribute setter.
        # pylint: disable=protected-access
        symbol._name = name

        # Re-insert modified symbol
        self.add(symbol)

    def reference_accesses(self):
        '''
        :returns: a map of all the symbol accessed inside this object, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are SingleVariableAccessInfo
            (a sequence of AccessTypes).
        :rtype: :py:class:`psyclone.core.VariablesAccessMap`

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.core import VariablesAccessMap
        vam = VariablesAccessMap()
        for sym in self.symbols:
            if not sym.is_import:
                vam.update(sym.reference_accesses())
        return vam

    def wildcard_imports(self, scope_limit=None) -> List[ContainerSymbol]:
        '''
        Searches this symbol table and then up through any parent symbol
        tables for ContainerSymbols that have a wildcard import.

        :param scope_limit: optional Node which limits the search to the
            symbol tables of the nodes within the given scope.
            If it is None (the default), the whole scope (all symbol tables
            in ancestor nodes) is searched, otherwise ancestors of the
            scope_limit node are not searched.
        :type scope_limit: Optional[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: the ContainerSymbols which have wildcard imports
            into the current scope.

        '''
        wildcards = {}
        current_table = self
        while current_table:
            for sym in current_table.containersymbols:
                if sym.wildcard_import and sym.name.lower() not in wildcards:
                    wildcards[sym.name.lower()] = sym
            current_table = current_table.parent_symbol_table(
                scope_limit=scope_limit)
        return list(wildcards.values())

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

        # Unique types of symbols, alpha-sorted.
        symbol_types = list({type(symbol)
                             for symbol in self._symbols.values()})
        symbol_types.sort(key=lambda t: t.__name__)
        for symbol_type in symbol_types:
            header += symbol_type.__name__ + ":\n"
            # _symbols is an OrderDict so these are alpha-sorted.
            for symbol in self._symbols.values():
                # pylint: disable=unidiomatic-typecheck
                if type(symbol) is symbol_type:
                    header += f"  {symbol}\n"

        return header

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

    def __eq__(self, other):
        '''
        Checks whether two SymbolTables are equal.

        # TODO 1698: Improve. Currently it uses a quick implementation
        # that only checks that the view() lines of each symbol_table
        # are exactly the same.
        # The current implementation does not check tags, order
        # of arguments or visibilities.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        # pylint: disable=unidiomatic-typecheck
        if type(self) is not type(other):
            return False
        this_lines = self.view().split('\n')
        other_lines = other.view().split('\n')
        for line in other_lines:
            if line not in this_lines:
                return False
            this_lines.remove(line)
        return len(this_lines) == 0
