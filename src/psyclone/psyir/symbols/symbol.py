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

'''This module contains the SymbolError class, the SymbolInterface
class and its subclasses and the generic Symbol class.

'''

from __future__ import absolute_import
from enum import Enum
import six
from psyclone.errors import PSycloneError, InternalError


class SymbolError(PSycloneError):
    '''
    PSyclone-specific exception for use with errors relating to the Symbol and
    SymbolTable in the PSyIR.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "PSyclone SymbolTable error: "+str(value)


class SymbolInterface(object):   # pylint: disable=too-few-public-methods
    ''' Abstract class of a Symbol Interface '''

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__()


class LocalInterface(SymbolInterface):
    # pylint: disable=too-few-public-methods
    ''' The symbol just exists in the Local context '''

    def __str__(self):
        return "Local"


class UnresolvedInterface(SymbolInterface):
    # pylint: disable=too-few-public-methods
    '''We have a symbol but we don't know where it is declared.'''

    def __str__(self):
        return "Unresolved"


class ImportInterface(SymbolInterface):
    '''Describes the interface to a Symbol that is imported from an external
    PSyIR container.

    :param container_symbol: symbol representing the external container \
        from which the symbol is imported.
    :type container_symbol: \
        :py:class:`psyclone.psyir.symbols.ContainerSymbol`

    :raise TypeError: if the container_symbol is not a ContainerSymbol.

    '''
    def __init__(self, container_symbol):
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import ContainerSymbol

        super(ImportInterface, self).__init__()

        if not isinstance(container_symbol, ContainerSymbol):
            raise TypeError(
                f"ImportInterface container_symbol parameter must be of type"
                f" ContainerSymbol, but found "
                f"'{type(container_symbol).__name__}'.")

        self._container_symbol = container_symbol

    @property
    def container_symbol(self):
        '''
        :return: symbol representing the container containing this Symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        '''
        return self._container_symbol

    def __str__(self):
        return f"Import(container='{self.container_symbol.name}')"

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__(self.container_symbol)


class ArgumentInterface(SymbolInterface):
    '''
    Captures the interface to a Symbol that is accessed as a routine
    argument.

    :param access: specifies how the argument is used in the Schedule
    :type access: :py:class:`psyclone.psyir.symbols.ArgumentInterface.Access`
    '''

    class Access(Enum):
        '''
        Enumeration for the different types of access that an Argument
        Symbol is permitted to have.

        '''
        ## The symbol is only ever read within the current scoping block.
        READ = 1
        ## The first access of the symbol in the scoping block is a write and
        # therefore any value that it may have had upon entry is discarded.
        WRITE = 2
        ## The first access of the symbol in the scoping block is a read but
        # it is subsequently written to.
        READWRITE = 3
        ## The way in which the symbol is accessed in the scoping block is
        # unknown
        UNKNOWN = 4

    def __init__(self, access=None):
        super(ArgumentInterface, self).__init__()
        self._access = None
        # Use the setter as that has error checking
        if not access:
            self.access = ArgumentInterface.Access.UNKNOWN
        else:
            self.access = access

    @property
    def access(self):
        '''
        :returns: the access-type for this argument.
        :rtype: :py:class:`psyclone.psyir.symbols.ArgumentInterface.Access`
        '''
        return self._access

    @access.setter
    def access(self, value):
        '''
        :param value: the new access type.
        :type value: :py:class:`psyclon.psyir.symbols.ArgumentInterface.Access`

        :raises TypeError: if the supplied value is not an \
            ArgumentInterface.Access
        '''
        if not isinstance(value, ArgumentInterface.Access):
            raise TypeError(
                f"SymbolInterface.access must be an 'ArgumentInterface.Access'"
                f" but got '{type(value).__name__}'.")
        self._access = value

    def __str__(self):
        return f"Argument({self.access})"

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__(access=self.access)


class Symbol(object):
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
        :py:class:`psyclone.psyir.symbols.LocalInterface`
    :type interface: :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`

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

        if not isinstance(name, six.string_types):
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
            :py:class:`psyclone.psyir.symbols.LocalInterface`
        :type interface: \
            :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`

        '''
        if interface:
            self.interface = interface
        elif not self.interface:
            self.interface = LocalInterface()

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
        return type(self)(self.name, visibility=self.visibility,
                          interface=self.interface.copy())

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

        :raises SymbolError: if the module pointed to by the symbol interface \
                             does not contain the symbol (or the symbol is \
                             not public).
        :raises NotImplementedError: if this symbol does not have an \
                                     ImportInterface.
        '''
        if not self.is_import:
            raise NotImplementedError(
                f"Error trying to resolve symbol '{self.name}' properties, "
                f"the lazy evaluation of '{self.interface}' interfaces is "
                f"not supported.")

        module = self.interface.container_symbol
        try:
            return module.container.symbol_table.lookup(
                self.name, visibility=self.Visibility.PUBLIC)
        except KeyError as kerr:
            raise SymbolError(
                f"Error trying to resolve the properties of symbol "
                f"'{self.name}'. The interface points to module "
                f"'{module.name}' but could not find the definition of "
                f"'{self.name}' in that module.") from kerr
        except SymbolError as err:
            raise SymbolError(
              f"Error trying to resolve the properties of symbol "
              f"'{self.name}' in module '{module.name}': {err.value}") from err

    def resolve_deferred(self):
        '''
        Search for the Container in which this Symbol is defined and
        create and return a symbol of the correct class and type. If the
        class and type of the looked-up symbol are the same as this one,
        some specialisations of this method update the differing
        properties in place rather than create a new symbol.
        If this symbol does not have an ImportInterface then there is no
        lookup needed and we just return this symbol.

        :returns: a symbol object with the class and type determined by \
                  examining the Container from which it is imported.
        :rtype: subclass of :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if self.is_import:
            extern_symbol = self.get_external_symbol()
            # Create a new symbol object of the same class as the one
            # we've just looked up but with the interface and visibility
            # of the current symbol.
            new_sym = extern_symbol.copy()
            new_sym.interface = self.interface
            new_sym.visibility = self.visibility
            return new_sym
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
    def is_local(self):
        '''
        :returns: whether the Symbol has a Local interface.
        :rtype: bool

        '''
        return isinstance(self._interface, LocalInterface)

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
        :returns: whether the Symbol has an Argument interface.
        :rtype: bool

        '''
        return isinstance(self._interface, ArgumentInterface)

    @property
    def is_unresolved(self):
        '''
        :returns: whether the Symbol has an unresolved interface.
        :rtype: bool

        '''
        return isinstance(self._interface, UnresolvedInterface)

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
        '''This is the basic implementation for the method that checks
        if a symbol is declared to be an array. In this base class it
        will just raise an exception, indicating that no information
        is available. The function will be overwritten, e.g. in
        DataSymbol.

        :returns: True if this symbol is an array and False otherwise.
        :rtype: bool

        :raises ValueError: if this function is called for the base class \
            since there is no information available.

        '''
        raise ValueError(f"No array information is available for the "
                         f"symbol '{self.name}'.")

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

        :param str index_variable: optional loop variable that is used to \
            to determine if an access is an array access using this variable.
        :param access_info: variable access information, optional.
        :type access_info: \
            :py:class:`psyclone.core.access_info.SingleVariableAccessInfo`

        :returns: if the variable is an array.
        :rtype bool:

        :raises InternalError: if a loop_variable is specified, but no \
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
        # in this case. If there is no type information available (i.e. `self`
        # is just a Symbol, not a DataSymbol), the `is_array` function will
        # raise an exception.
        # TODO #1213: check for wildcard imports
        try:
            return self.is_array
        except ValueError:
            # Generic symbols produce a ValueError, since this does not have
            # a datatype and an Array access was not found, we don't consider
            # it an array.
            return False
