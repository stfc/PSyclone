# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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

"""
The fparser2 symbol-table module. Defines various classes as well as
the single, global SYMBOL_TABLES instance. The latter is a container
for all of the top-level scoping units encountered during parsing.

"""
from collections import namedtuple


class SymbolTableError(Exception):
    """Base class exception for symbol-table related errors."""


class SymbolTables:
    """
    Class encapsulating functionality for the global symbol-tables object.
    This is a container for all symbol tables constructed while parsing
    code. All names are converted to lower case (since Fortran is not
    case sensitive).

    """

    def __init__(self):
        self._symbol_tables = {}
        # The symbol table of the current scope
        self._current_scope = None
        # Whether or not we enable consistency checks in the symbol tables
        # that are created.
        self._enable_checks = False

    def __str__(self):
        result = (
            f"SymbolTables: {len(self._symbol_tables)} tables\n"
            "========================\n"
        )
        return result + "\n".join(sorted(self._symbol_tables.keys()))

    def enable_checks(self, value):
        """
        Sets whether or not to enable consistency checks in every symbol
        table that is created during a parse.

        :param bool value: whether or not checks are enabled.

        """
        self._enable_checks = value

    def clear(self):
        """
        Deletes any stored SymbolTables.

        """
        self._symbol_tables = {}
        self._current_scope = None

    def add(self, name, node=None):
        """
        Add a new symbol table with the supplied name. The name will be
        converted to lower case if necessary.

        :param str name: the name for the new table.
        :param node: the node in the parse tree associated with this table.
        :type node: Optional[:py:class:`fparser.two.utils.Base`]

        :returns: the new symbol table.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        :raises SymbolTableError: if there is already an entry with the
                                  supplied name.
        """
        lower_name = name.lower()
        if lower_name in self._symbol_tables:
            raise SymbolTableError(
                f"The table of top-level (un-nested) symbol tables already "
                f"contains an entry for '{lower_name}'"
            )
        table = SymbolTable(lower_name, checking_enabled=self._enable_checks, node=node)
        self._symbol_tables[lower_name] = table
        return table

    def lookup(self, name):
        """
        Find the named symbol table and return it.

        :param str name: the name of the required symbol table (not case \
                         sensitive).

        :returns: the named symbol table.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        """
        return self._symbol_tables[name.lower()]

    @property
    def current_scope(self):
        """
        :returns: the symbol table for the current scoping unit or None.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable` or NoneType
        """
        return self._current_scope

    def enter_scope(self, name, node=None):
        """
        Called when the parser enters a new scoping region (i.e. when it
        encounters one of the classes listed in `_scoping_unit_classes`).
        Sets the 'current scope' to be the symbol table with the supplied name.
        If we are not currently within a tree of scoping regions then a
        new entry is created in the internal dict of symbol tables. If there
        is an existing tree then a new table is created and added to the
        bottom.

        :param str name: name of the scoping region.
        :param node: the node of the parse tree associated with this region.
        :type node: Optional[:py:class:`fparser.two.utils.Base`]
        """
        lname = name.lower()

        if not self._current_scope:
            # We're not already inside a nested scope.
            try:
                table = self.lookup(lname)
            except KeyError:
                # Create a new, top-level symbol table with the supplied name.
                table = self.add(lname, node=node)
        else:
            # We are already inside a scoping region so create a new table
            # and setup its parent/child connections.
            table = SymbolTable(
                lname,
                parent=self._current_scope,
                checking_enabled=self._enable_checks,
                node=node,
            )
            self._current_scope.add_child(table)

        # Finally, make this new table the current scope
        self._current_scope = table

    def exit_scope(self):
        """
        Marks the end of the processing of the current scoping unit. Since
        we are exiting the current scoping region, the new 'current scoping
        region' will be its parent.

        :raises SymbolTableError: if there is no current scope from which to \
                                  exit.
        """
        if not self._current_scope:
            raise SymbolTableError("exit_scope() called but no current scope exists.")
        self._current_scope = self._current_scope.parent

    def remove(self, name):
        """
        Removes the named symbol table and any descendants it may have.
        When searching for the named table, the current scope takes priority
        followed by the list of top-level symbol tables.

        :param str name: the name of the symbol table to remove (not case \
                         sensitive).
        :raises SymbolTableError: if the named symbol table is not in the \
            current scope or in the list of top-level symbol tables.

        """
        lname = name.lower()
        if self._current_scope:
            try:
                self._current_scope.del_child(lname)
                # We succeeded in removing it from the current scope so we
                # are done.
                return
            except KeyError:
                pass

        if lname not in self._symbol_tables:
            msg = f"Failed to find a table named '{name}' in "
            if self._current_scope:
                msg += (
                    f"either the current scope (which contains "
                    f"{[child.name for child in self._current_scope.children]}) or "
                )
            msg += (
                f"the list of top-level symbol tables "
                f"({list(self._symbol_tables.keys())})."
            )
            raise SymbolTableError(msg)

        # Check that we're not currently somewhere inside the scope of the
        # named table.
        top_table = self._symbol_tables[lname]
        if self._current_scope:
            if self._current_scope.root is top_table:
                raise SymbolTableError(
                    f"Cannot remove top-level symbol table '{name}' because the "
                    f"current scope '{self._current_scope.name}' has it as an ancestor."
                )

        del self._symbol_tables[lname]


class ModuleUse:
    """
    Class capturing information on all USE statements referring to a given
    Fortran module.

    A USE statement can rename an imported symbol so as to avoid a clash in
    the local scope, e.g. `USE my_mod, alocal => amod` where `amod` is the
    name of the symbol declared in `my_mod`. This renaming can also occur
    inside an Only_List, e.g. `USE my_mod, only: alocal => amod`.

    :param str name: the name of the module.
    :param only_list: list of 2-tuples giving the (local-name, module-name) \
        of symbols that appear in an Only_List. If a symbol is not re-named \
        then module-name can be None.
    :type only_list: Optional[List[Tuple[str, str | NoneType]]]
    :param rename_list: list of 2-tuples given the (local-name, module-name) \
        of symbols that appear in a Rename_List.
    :type rename_list: Optional[List[Tuple[str, str]]]

    :raises TypeError: if any of the supplied parameters are of the wrong type.

    """

    def __init__(self, name, only_list=None, rename_list=None):
        if not isinstance(name, str):
            raise TypeError(
                f"The name of the module must be a str but got "
                f"'{type(name).__name__}'"
            )
        self._validate_tuple_list("only", only_list)
        self._validate_tuple_list("rename", rename_list)

        if only_list and not all(
            isinstance(item[0], str) and (item[1] is None or isinstance(item[1], str))
            for item in only_list
        ):
            raise TypeError(
                f"If present, the only_list must be a list of "
                f"2-tuples of (str, str | NoneType) but got: {only_list}"
            )

        if rename_list and not all(
            isinstance(item[0], str) and isinstance(item[1], str)
            for item in rename_list
        ):
            raise TypeError(
                f"If present, the rename_list must be a list of "
                f"2-tuples of (str, str) but got: {rename_list}"
            )

        self._name = name.lower()

        # dict of Symbols known to be accessed in this module.
        self._symbols = {}
        # Mapping from local symbol name in current scope to actual, declared
        # name in the module from which it is imported.
        self._local_to_module_map = {}

        if only_list is not None:
            self._store_symbols(only_list)
            self._wildcard_import = False
            self._only_set = set(local_name.lower() for local_name, _ in only_list)
        else:
            self._only_set = None
            self._wildcard_import = True

        if rename_list:
            self._store_symbols(rename_list)
            self._rename_set = set(local_name.lower() for local_name, _ in rename_list)
        else:
            self._rename_set = None

    @staticmethod
    def _validate_tuple_list(name, tlist):
        """
        Validate the supplied list of tuples.

        :param str name: the name of the list being validated.
        :param tlist: the list of tuples to validate.
        :type tlist: Optional[List[Tuple[str, str | NoneType]]]

        :raises TypeError: if the supplied list is of the wrong type.

        """
        if not tlist:
            # None or an empty list is fine.
            return

        if not isinstance(tlist, list):
            raise TypeError(
                f"If present, the {name}_list must be a list but "
                f"got '{type(tlist).__name__}'"
            )
        if not all(isinstance(item, tuple) and len(item) == 2 for item in tlist):
            raise TypeError(
                f"If present, the {name}_list must be a list of "
                f"2-tuples but got: {tlist}"
            )

    def _store_symbols(self, rename_list):
        """
        Utility that updates the local list of symbols and renaming map for
        the given list of local-name, module-name tuples. If a symbol is
        not renamed then module-name can be None.

        :param rename_list: list of local-name, module-name pairs.
        :type rename_list: List[Tuple[str, str | NoneType]]

        """
        for local_name, orig_name in rename_list:
            lname = local_name.lower()
            oname = orig_name.lower() if orig_name else None
            if lname not in self._symbols:
                self._symbols[lname] = SymbolTable.Symbol(lname, "unknown")
            if oname:
                self._local_to_module_map[lname] = oname

    def update(self, other):
        """
        Update the current information with the information on the USE held
        in 'other'.

        :param other: the other Use instance from which to update this one.
        :type other: :py:class:`fparser.two.symbol_table.Use`

        :raises TypeError: if 'other' is of the wrong type.
        :raises ValueError: if 'other' is for a module with a different \
                            name to the current one.

        """
        if not isinstance(other, ModuleUse):
            raise TypeError(
                f"update() must be supplied with an instance of "
                f"ModuleUse but got '{type(other).__name__}'"
            )

        if self.name != other.name:
            raise ValueError(
                f"The ModuleUse supplied to update() is for module "
                f"'{other.name}' but this ModuleUse is for module "
                f"'{self.name}'"
            )

        # Take the union of the only and rename lists and update the list
        # of symbols.
        if other.only_list:
            for local_name in other.only_list:
                if local_name not in self._symbols:
                    self._symbols[local_name] = SymbolTable.Symbol(
                        local_name, "unknown"
                    )
            # pylint: disable=protected-access
            if self._only_set is None:
                self._only_set = other._only_set
            else:
                self._only_set = self._only_set.union(other._only_set)
            # pylint: enable=protected-access

        if other.rename_list:
            for local_name in other.rename_list:
                if local_name not in self._symbols:
                    self._symbols[local_name] = SymbolTable.Symbol(
                        local_name, "unknown"
                    )
            # pylint: disable=protected-access
            if self._rename_set is None:
                self._rename_set = other._rename_set
            else:
                self._rename_set = self._rename_set.union(other._rename_set)
            # pylint: enable=protected-access

        # pylint: disable=protected-access
        self._local_to_module_map.update(other._local_to_module_map)
        # pylint: enable=protected-access

        self._wildcard_import = self._wildcard_import or other.wildcard_import

    @property
    def name(self):
        """
        :returns: the name of the Fortran module.
        :rtype: str
        """
        return self._name

    @property
    def symbol_names(self):
        """
        :returns: the names of all symbols associated with USE(s) of this
                  module.
        :rtype: List[str]
        """
        return list(self._symbols.keys())

    def lookup(self, name):
        """
        :returns: the symbol with the supplied name imported from this module (if any).
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable.Symbol`

        :raises KeyError: if no symbol with the supplied name is imported from
                          this module into the current scope.
        """
        return self._symbols[name.lower()]

    @property
    def only_list(self):
        """
        :returns: the local names that appear in an Only_List or None if there
                  is no such list.
        :rtype: Optional[List[str]]
        """
        if self._only_set is None:
            return None
        return list(self._only_set)

    @property
    def rename_list(self):
        """
        :returns: the local names that appear in a Rename_List or None if there
                  is no such list.
        :rtype: Optional[List[str]]
        """
        if self._rename_set is None:
            return None
        return list(self._rename_set)

    @property
    def wildcard_import(self):
        """
        :returns: whether there is a wildcard import from this module.
        :rtype: bool
        """
        return self._wildcard_import

    def get_declared_name(self, name):
        """
        :returns: the name of the supplied symbol as declared in the module.
        :rtype: str
        """
        return self._local_to_module_map.get(name, name)


class SymbolTable:
    """
    Class implementing a single symbol table.

    Since this functionality is not yet fully mature, checks that new symbols
    don't clash with existing symbols are disabled by default.
    Once #201 is complete it is planned to switch this so that the checks
    are instead enabled by default.

    :param str name: the name of this scope. Will be the name of the
        associated module or routine.
    :param parent: the symbol table within which this one is nested (if any).
    :type parent: :py:class:`fparser.two.symbol_table.SymbolTable.Symbol`
    :param bool checking_enabled: whether or not validity checks are
        performed for symbols added to the table.
    :param node: the node in the parse tree associated with this table.
    :type node: Optional[:py:class:`fparser.two.utils.Base`]

    :raises TypeError: if the supplied node is of the wrong type.
    """

    # TODO #201 add support for other symbol properties (kind, shape
    # and visibility). We may need a distinct Symbol class so as to provide
    # type checking for the various properties.
    Symbol = namedtuple("Symbol", "name primitive_type")

    def __init__(self, name, parent=None, checking_enabled=False, node=None):
        self._name = name.lower()
        # Symbols defined in this scope that represent data.
        self._data_symbols = {}
        # dict of ModuleUse objects (indexed by module name) representing
        # modules imported into this scope.
        self._modules = {}
        # Reference to a SymbolTable that contains this one (if any). Actual
        # value (if any) is set via setter method.
        self._parent = None
        self.parent = parent
        # The node in the parse tree with which this table is associated (if any).
        from fparser.two.utils import Base

        if node and not isinstance(node, Base):
            raise TypeError(
                f"The 'node' argument to the SymbolTable constructor must be a "
                f"valid parse tree node (instance of utils.Base) but got "
                f"'{type(node).__name__}'"
            )
        self._node = node
        # Whether or not to perform validity checks when symbols are added.
        self._checking_enabled = checking_enabled
        # Symbol tables nested within this one.
        self._children = []

    def __str__(self):
        header = "===========\n"
        symbols = "Symbols:\n"
        if self._data_symbols:
            symbols += "\n".join(list(self._data_symbols.keys())) + "\n"
        uses = "Used modules:\n"
        if self._modules:
            uses += "\n".join(list(self._modules.keys())) + "\n"
        return f"{header}Symbol Table '{self._name}'\n{symbols}{uses}{header}"

    def add_data_symbol(self, name, primitive_type):
        """
        Creates a new Symbol with the specified properties and adds it to
        the symbol table. The supplied name is converted to lower case.

        TODO #201 add support for other symbol properties (kind, shape
        and visibility).

        :param str name: the name of the symbol.
        :param str primitive_type: the primitive type of the symbol.

        :raises TypeError: if any of the supplied parameters are of the \
                           wrong type.
        :raises SymbolTableError: if the symbol table already contains an
                                  entry with the supplied name.
        """
        if not isinstance(name, str):
            raise TypeError(
                f"The name of the symbol must be a str but got "
                f"'{type(name).__name__}'"
            )
        # TODO #201 use an enumeration for the primitive type
        if not isinstance(primitive_type, str):
            raise TypeError(
                f"The primitive type of the symbol must be specified as a str "
                f"but got '{type(primitive_type).__name__}'"
            )
        lname = name.lower()

        if self._checking_enabled:
            if lname in self._data_symbols:
                raise SymbolTableError(
                    f"Symbol table already contains a symbol for"
                    f" a variable with name '{name}'"
                )
            if lname in self._modules:
                raise SymbolTableError(
                    f"Symbol table already contains a use of a "
                    f"module with name '{name}'"
                )
            for mod_name, mod in self._modules.items():
                if mod.symbol_names and lname in mod.symbol_names:
                    raise SymbolTableError(
                        f"Symbol table already contains a use of a symbol "
                        f"named '{name}' from module '{mod_name}'"
                    )

        self._data_symbols[lname] = SymbolTable.Symbol(lname, primitive_type.lower())

    def add_use_symbols(self, name, only_list=None, rename_list=None):
        """
        Creates an entry in the table for the USE of a module with the supplied
        name. If no `only_list` is supplied then this USE represents a wildcard
        import of all public symbols in the named module. If the USE statement
        has an ONLY clause but without any named symbols then `only_list`
        should be an empty list.

        A USE can also have one or more rename entries *without* an only list.

        :param str name: the name of the module being imported via a USE. Not \
            case sensitive.
        :param only_list: if there is an 'only:' clause on the USE statement \
            then this contains a list of tuples, each holding the local name \
            of the symbol and its name in the module from which it is \
            imported. These names are case insensitive.
        :type only_list: Optional[List[Tuple[str, str | NoneType]]]
        :param rename_list: a list of symbols that are renamed from the scope \
            being imported. Each entry is a tuple containing the name in the \
            local scope and the corresponding name in the module from which it\
            is imported. These names are case insensitive.
        :type rename_list: Optional[List[Tuple[str, str]]]

        """
        use = ModuleUse(name, only_list, rename_list)

        if use.name in self._modules:
            # The same module can appear in more than one use statement
            # in Fortran.
            self._modules[use.name].update(use)
        else:
            self._modules[use.name] = use

    def lookup(self, name):
        """
        Lookup the symbol with the supplied name.

        :param str name: the name of the symbol to lookup (not case sensitive).

        :returns: the named symbol.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable.Symbol`

        :raises KeyError: if the named symbol cannot be found in this or any \
                          parent scope.
        """
        # Fortran is not case sensitive so convert input to lowercase.
        lname = name.lower()
        if lname in self._data_symbols:
            # Found a match in this table.
            return self._data_symbols[lname]
        for module in self._modules.values():
            try:
                # Look to see whether the symbol is imported into this table.
                return module.lookup(lname)
            except KeyError:
                pass
        # No match in this scope - search in parent scope (if any). This will
        # recurse upwards through parent tables as necessary.
        if self.parent:
            return self.parent.lookup(lname)
        raise KeyError(f"Failed to find symbol named '{lname}'")

    @property
    def name(self):
        """
        :returns: the name of this symbol table (scoping region).
        :rtype: str
        """
        return self._name

    @property
    def parent(self):
        """
        :returns: the parent symbol table (scoping region) that contains \
                  this one (if any).
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable` or NoneType
        """
        return self._parent

    @parent.setter
    def parent(self, value):
        """
        Set the parent scope for this symbol table.

        :param value: the parent symbol table.
        :type value: :py:class:`fparser.two.symbol_table.SymbolTable`

        :raises TypeError: if the supplied value is not None or a SymbolTable.

        """
        if value is not None and not isinstance(value, SymbolTable):
            raise TypeError(
                f"Unless it is None, the parent of a SymbolTable must also be "
                f"a SymbolTable but got '{type(value).__name__}'"
            )
        self._parent = value

    @property
    def node(self):
        """
        :returns: the scoping node (in the parse tree) asssociated with this SymbolTable.
        :rtype: :py:class:`fparser.two.utils.Base`
        """
        return self._node

    def add_child(self, child):
        """
        Adds a child symbol table (scoping region nested within this one).

        :param child: the nested symbol table.
        :type child: :py:class:`fparser.two.symbol_table.SymbolTable`

        :raises TypeError: if the supplied child is not a SymbolTable.

        """
        if not isinstance(child, SymbolTable):
            raise TypeError(
                f"Expected a SymbolTable instance but got '{type(child).__name__}'"
            )
        self._children.append(child)

    def del_child(self, name):
        """
        Removes the named symbol table.

        :param str name: the name of the child symbol table to delete (not \
                         case sensitive).

        :raises KeyError: if the named table is not a child of this one.

        """
        lname = name.lower()
        for child in self._children:
            if child.name == lname:
                self._children.remove(child)
                break
        else:
            raise KeyError(
                f"Symbol table '{self.name}' does not contain a table named '{name}'"
            )

    @property
    def children(self):
        """
        :returns: the child (nested) symbol tables, if any.
        :rtype: list of :py:class:`fparser.two.symbol_table.SymbolTable`
        """
        return self._children

    @property
    def root(self):
        """
        :returns: the top-level symbol table that contains the current \
                  scoping region (symbol table).
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        """
        current = self
        while current.parent:
            current = current.parent
        return current

    @property
    def wildcard_imports(self):
        """
        :returns: names of all modules with wildcard imports into this scope or an
                  empty list if there are none.
        :rtype: List[Optional[str]]
        """
        mod_names = set()
        for mod_name, mod in self._modules.items():
            if mod.wildcard_import:
                mod_names.add(mod_name)
        if self.parent:
            # Any wildcard imports in a parent scope will affect this scoping
            # region so carry on up. Note that if the root scoping region in
            # the current file is a SUBMODULE then we will be missing whatever
            # is brought into scope in the parent MODULE (since that will typically
            # be in a separate source file).
            mod_names.update(self.parent.wildcard_imports)

        return sorted(list(mod_names))

    @property
    def all_symbols_resolved(self):
        """
        :returns: whether all symbols in this scope have been resolved. i.e. if
            there are any wildcard imports or this table is within a submodule
            then there could be symbols we don't have definitions for.
        :rtype: bool
        """
        # wildcard_imports checks all parent scopes.
        if self.wildcard_imports:
            return False

        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Submodule_Stmt

        cursor = self
        while cursor:
            if isinstance(cursor.node, Submodule_Stmt):
                return False
            cursor = cursor.parent
        return True


#: The single, global container for all symbol tables constructed while
#: parsing.
SYMBOL_TABLES = SymbolTables()


__all__ = ["SymbolTableError", "SymbolTables", "SymbolTable", "SYMBOL_TABLES"]
