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
# -----------------------------------------------------------------------------

'''This module contains the SymbolError class and the generic Symbol class.
'''

from enum import Enum
from psyclone.errors import PSycloneError, InternalError
from psyclone.psyir.symbols.interfaces import (
    AutomaticInterface, SymbolInterface, ArgumentInterface,
    UnresolvedInterface, ImportInterface, UnknownInterface,
    CommonBlockInterface, DefaultModuleInterface, StaticInterface)
from psyclone.psyir.commentable_mixin import CommentableMixin


class SymbolError(PSycloneError):
    '''
    PSyclone-specific exception for use with errors relating to the Symbol and
    SymbolTable in the PSyIR.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "PSyclone SymbolTable error: "+str(value)


class Symbol(CommentableMixin):
    '''Generic Symbol item for the Symbol Table and PSyIR References.
    It has an immutable name label because it must always match with the
    key in the SymbolTable. If the symbol is private then it is only visible
    to those nodes that are descendants of the Node to which its containing
    Symbol Table belongs.

    :param str name: name of the symbol.
    :param visibility: the visibility of the symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: optional object describing the interface to this \
        symbol (i.e. whether it is passed as a routine argument or \
        accessed in some other way). Defaults to \
        :py:class:`psyclone.psyir.symbols.AutomaticInterface`
    :type interface: Optional[ \
        :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`]

    :raises TypeError: if the name is not a str.

    '''

    class Visibility(Enum):
        ''' Enumeration of the different visibility attributes supported in
        the PSyIR. If no visibility information is supplied for a Symbol then
        it is given the DEFAULT_VISIBILITY value.

        PUBLIC: the symbol is visible in any scoping region that has access to
                the SymbolTable containing it.
        PRIVATE: the symbol is only visibile inside the scoping region that
                 contains the SymbolTable to which it belongs.
        '''
        PUBLIC = 1
        PRIVATE = 2

    # The visibility given to any PSyIR symbols that are created without being
    # given an explicit visibility.
    DEFAULT_VISIBILITY = Visibility.PUBLIC

    def __init__(self, name, visibility=DEFAULT_VISIBILITY, interface=None):

        if not isinstance(name, str):
            raise TypeError(
                f"{type(self).__name__} 'name' attribute should be of type "
                f"'str' but '{type(name).__name__}' found.")

        self._name = name

        # The following attributes have a setter method (with error checking)
        self._visibility = None
        self._interface = None

        self._process_arguments(visibility=visibility, interface=interface)

    def _process_arguments(self, visibility=None, interface=None):
        ''' Process the visibility and interface arguments of the constructor
        and the specialise methods.

        :param visibility: the visibility of the symbol.
        :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
        :param interface: optional object describing the interface to this \
            symbol (i.e. whether it is passed as a routine argument or \
            accessed in some other way). Defaults to \
            :py:class:`psyclone.psyir.symbols.AutomaticInterface`
        :type interface: Optional[ \
            :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`]

        '''
        if interface:
            self.interface = interface
        elif not self.interface:
            self.interface = AutomaticInterface()

        if visibility:
            self.visibility = visibility

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this \
                  symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # The constructors for all Symbol-based classes have 'name' as the
        # first positional argument.
        copy = type(self)(self.name, visibility=self.visibility,
                          interface=self.interface.copy())
        copy.preceding_comment = self.preceding_comment
        copy.inline_comment = self.inline_comment
        return copy

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.

        :param symbol_in: the symbol from which the properties are copied.
        :type symbol_in: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, Symbol):
            raise TypeError(f"Argument should be of type 'Symbol' but "
                            f"found '{type(symbol_in).__name__}'.")
        self._interface = symbol_in.interface

    def specialise(self, subclass, **kwargs):
        '''Specialise this symbol so that it becomes an instance of the class
        provided in the subclass argument. This allows this instance
        to become a subclass without any references to it becoming
        invalid.

        :param subclass: the class that this symbol will become.
        :type subclass: type of sub-class of \
            :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if subclass is not a sub-class of Symbol.

        '''
        try:
            is_subclass = issubclass(subclass, self.__class__)
        except TypeError as info:
            raise TypeError(
                f"The specialise method in '{self.name}' expects the "
                f"subclass argument to be a class.") from info
        # pylint: disable = unidiomatic-typecheck
        if not is_subclass or type(self) is subclass:
            raise TypeError(
                f"The specialise method in '{self.name}', an instance of "
                f"'{type(self).__name__}', expects the subclass argument to "
                f"be a subclass of '{type(self).__name__}', but found "
                f"'{subclass.__name__}'.")
        self.__class__ = subclass
        self._process_arguments(**kwargs)

    # pylint: disable=inconsistent-return-statements
    def get_external_symbol(self):
        '''
        Looks-up and returns the Symbol referred to by this Symbol's
        Import Interface.

        :raises SymbolError: if the module pointed to by the symbol interface
                             does not contain the symbol (or the symbol is
                             not public).
        :raises NotImplementedError: if this symbol does not have an
                                     ImportInterface.
        '''
        if not self.is_import:
            raise NotImplementedError(
                f"Error trying to resolve symbol '{self.name}' properties, "
                f"the lazy evaluation of '{self.interface}' interfaces is "
                f"not supported.")

        csym = self.interface.container_symbol

        try:
            container = csym.find_container_psyir()
            if not container:
                raise SymbolError(
                    f"Error trying to resolve the properties of symbol "
                    f"'{self.name}'. The interface points to module "
                    f"'{csym.name}' but could not obtain its PSyIR.")
            return container.symbol_table.lookup(
                self.name, visibility=self.Visibility.PUBLIC)
        except KeyError as kerr:
            raise SymbolError(
                f"Error trying to resolve the properties of symbol "
                f"'{self.name}'. The interface points to module "
                f"'{csym.name}' but could not find the definition of "
                f"'{self.name}' in that module.") from kerr
        except SymbolError as err:
            raise SymbolError(
              f"Error trying to resolve the properties of symbol "
              f"'{self.name}' in module '{csym.name}': {err.value}") from err

    def resolve_type(self):
        '''
        Update the properties of this Symbol by using the definition imported
        from the external Container. If this symbol does not have an
        ImportInterface then there is no lookup needed and we just return this
        symbol.

        :returns: a symbol object with the class and type determined by
                  examining the Container from which it is imported.
        :rtype: subclass of :py:class:`psyclone.psyir.symbols.Symbol`

        :raises SymbolError: if the type could not be resolved.
        '''
        if self.is_import:
            extern_symbol = self.get_external_symbol()
            # pylint: disable-next=import-outside-toplevel
            from psyclone.psyir.symbols import RoutineSymbol, TypedSymbol
            if not isinstance(extern_symbol, TypedSymbol):
                raise SymbolError(
                    f"The external symbol '{extern_symbol.name}' was found "
                    f"but it does not have a type. Maybe it is a transitive "
                    f"(indirect) import which is currently not resolvable.")
            if isinstance(extern_symbol, RoutineSymbol):
                # Specialise the existing Symbol in-place so that all
                # References to it remain valid.
                self.specialise(type(extern_symbol),
                                datatype=extern_symbol.datatype)
            else:
                init_value = None
                if extern_symbol.initial_value:
                    init_value = extern_symbol.initial_value.copy()
                # Specialise the existing Symbol in-place so that all
                # References to it remain valid.
                self.specialise(type(extern_symbol),
                                datatype=extern_symbol.datatype,
                                is_constant=extern_symbol.is_constant,
                                initial_value=init_value)
        return self

    @property
    def name(self):
        '''
        :returns: name of the Symbol.
        :rtype: str
        '''
        return self._name

    @property
    def visibility(self):
        '''
        :returns: the visibility of this Symbol.
        :rtype: :py:class:`psyclone.psyir.symbol.Symbol.Visibility`
        '''
        return self._visibility

    @visibility.setter
    def visibility(self, value):
        '''
        Setter for the visibility attribute.

        :raises TypeError: if the supplied value is not an instance of \
                           Symbol.Visibility.
        '''
        if not isinstance(value, Symbol.Visibility):
            raise TypeError(
                f"{type(self).__name__} 'visibility' attribute should be of "
                f"type psyir.symbols.Symbol.Visibility but got "
                f"'{type(value).__name__}'.")
        self._visibility = value

    @property
    def interface(self):
        '''
        :returns: the an object describing the interface to this Symbol.
        :rtype: Sub-class of \
            :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`
        '''
        return self._interface

    @interface.setter
    def interface(self, value):
        '''
        Setter for the Interface associated with this Symbol.

        :param value: an Interface object describing how the Symbol is \
                      accessed.
        :type value: Sub-class of \
            :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`

        :raises TypeError: if the supplied `value` is of the wrong type.
        '''
        if not isinstance(value, SymbolInterface):
            raise TypeError(f"The interface to a Symbol must be a "
                            f"SymbolInterface but got "
                            f"'{type(value).__name__}'")
        self._interface = value

    @property
    def is_automatic(self):
        '''
        :returns: whether the Symbol has an AutomaticInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, AutomaticInterface)

    @property
    def is_modulevar(self):
        '''
        :returns: whether the Symbol has a DefaultModuleInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, DefaultModuleInterface)

    @property
    def is_import(self):
        '''
        :returns: whether the Symbol has an ImportInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, ImportInterface)

    @property
    def is_argument(self):
        '''
        :returns: whether the Symbol has an ArgumentInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, ArgumentInterface)

    @property
    def is_commonblock(self):
        '''
        :returns: whether the Symbol has a CommonBlockInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, CommonBlockInterface)

    @property
    def is_static(self):
        '''
        :returns: whether the Symbol has a StaticInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, StaticInterface)

    @property
    def is_unresolved(self):
        '''
        :returns: whether the Symbol has an UnresolvedInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, UnresolvedInterface)

    @property
    def is_unknown_interface(self):
        '''
        :returns: whether the Symbol has an UnknownInterface.
        :rtype: bool

        '''
        return isinstance(self._interface, UnknownInterface)

    def find_symbol_table(self, node):
        '''
        Searches back up the PSyIR tree for the SymbolTable that contains
        this Symbol.

        :param node: the PSyIR node from which to search.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the SymbolTable containing this Symbol or None.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable` or NoneType

        :raises TypeError: if the supplied `node` argument is not a PSyIR Node.

        '''
        # This import has to be local to this method to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Node
        if not isinstance(node, Node):
            raise TypeError(
                f"find_symbol_table: expected to be passed an instance of "
                f"psyir.nodes.Node but got '{type(node).__name__}'")

        try:
            current = node.scope.symbol_table
            while current:
                if self.name in current:
                    # TODO #2949: Add: and current.lookup(self.name) is self:
                    return current
                if current.node.parent:
                    current = current.node.parent.scope.symbol_table
                else:
                    # We can't go any further up the hierarchy and we haven't
                    # found a SymbolTable that contains this symbol.
                    return None
        except SymbolError:
            # Failed to find any enclosing symbol table
            return None

    def __str__(self):
        return f"{self.name}: Symbol<{self.interface}>"

    @property
    def is_array(self):
        '''
        :returns: True if this symbol is an array and False if it is not or
            there is not enough symbol information to determine it.
        :rtype: bool

        '''
        return False

    def is_array_access(self, index_variable=None, access_info=None):
        '''This method detects if a variable is used as an array or not.
        If available, it will use the access information, i.e. how the
        variable is used (e.g. if it has indices somewhere, like `a(i)%b`).
        This can be incorrect in case of implicit loops (e.g. `a=b+1`,
        where `a` and `b` are arrays) where the variable usage information
        will not have information about indices. In this case this function
        will fallback to querying the symbol itself.

        This can cause significant slowdown if this symbol is imported
        from a module, since then these modules need to be parsed.
        # TODO #1213: Parsing modules is not yet supported.

        If a `loop_variable` is specified, a variable access will only be
        considered an array access if the specified variable is used in
        at least one of the indices. For example:
        >>> do i=1, n
        >>>     a(j) = 2

        the access to `a` is not considered an array if `loop_variable` is
        set to `i`. If `loop_variable` is specified, `access_information`
        must be specified.

        :param str index_variable: optional loop variable that is used to
            to determine if an access is an array access using this variable.
        :param access_info: variable access information, optional.
        :type access_info:
            :py:class:`psyclone.core.SingleVariableAccessInfo`

        :returns: whether or not the variable is an array.
        :rtype bool:

        :raises InternalError: if a loop_variable is specified, but no
            access information is given.

        '''
        # TODO #1270: this function might either be better off elsewhere,
        # or even do not implement one function that uses both access
        # information and symbol table - if required, the user can
        # query both in two simple statements anyway.
        if index_variable and not access_info:
            raise InternalError(f"In Symbol.is_array_access: index variable "
                                f"'{index_variable}' specified, but no access "
                                f"information given.")

        # TODO #1244: If as a result of 1244 we have more reliable
        # information in the symbol table, the implementation here might
        # be changed.

        # Prioritise access information, since this can take the index
        # variable into account, and avoids potentially parsing other
        # modules to find type information.
        if access_info:
            # Access Info might not have information if a variable is used
            # as array (e.g. in case of an array expression). In this case
            # we still need to check the type information in the symbol table.
            is_array = access_info.is_array(index_variable=index_variable)

            # Access information might indicate that a variable is not an
            # array if it is used in array expressions only. In order to
            # verify, we need to check the symbol type information. But
            # if an index variable is specified, an array used in an array
            # expression without the index variable is not considered to
            # be an array access, since it is independent of the loop
            # variable. In this case also return the value from `is_array`.
            if is_array or index_variable is not None:
                return is_array

        # Either we don't have access information, or the access information
        # does not indicate an array. In the latter case we still need to
        # test the symbol table, since the variable might be used in array
        # expressions only. Note that we cannot check for index variable usage
        # in this case.
        # TODO #1213: check for wildcard imports
        return self.is_array

    def replace_symbols_using(self, table_or_symbol):
        '''
        Replace any Symbols referred to by this object with those in the
        supplied SymbolTable (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given Symbol then it
        is left unchanged.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if not isinstance(self.interface, ImportInterface):
            return
        name = self.interface.container_symbol.name
        orig_name = self.interface.orig_name
        if isinstance(table_or_symbol, Symbol):
            if name.lower() == table_or_symbol.name.lower():
                self.interface = ImportInterface(table_or_symbol,
                                                 orig_name=orig_name)
        else:
            try:
                new_container = table_or_symbol.lookup(name)
                self.interface = ImportInterface(new_container,
                                                 orig_name=orig_name)
            except KeyError:
                pass

    def reference_accesses(self):
        '''
        :returns: a map of all the symbol accessed inside this Symbol, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are SingleVariableAccessInfo
            (a sequence of AccessTypes).
        :rtype: :py:class:`psyclone.core.VariablesAccessMap`

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.core import VariablesAccessMap
        return VariablesAccessMap()
