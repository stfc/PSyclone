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

''' This module contains the generic Symbol and the SymbolError.'''

from __future__ import absolute_import
from enum import Enum
import six


class SymbolError(Exception):
    '''
    PSyclone-specific exception for use with errors relating to the Symbol and
    SymbolTable in the PSyIR.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "PSyclone SymbolTable error: "+value

    def __str__(self):
        return str(self.value)


class SymbolInterface(object):
    ''' Abstract class of a Symbol Interface '''


class LocalInterface(SymbolInterface):
    ''' The symbol just exists in the Local context '''

    def __str__(self):
        return "Local"


class UnresolvedInterface(SymbolInterface):
    '''We have a symbol but we don't know where it is declared.'''

    def __str__(self):
        return "Unresolved"


class GlobalInterface(SymbolInterface):
    '''Describes the interface to a Symbol that is supplied externally to
    this container, and therefore it is defined in an external PSyIR
    container.

    :param container_symbol: symbol representing the external container \
        from which the symbol is imported.
    :type container_symbol: \
        :py:class:`psyclone.psyir.symbols.ContainerSymbol`

    :raise TypeError: if the container_symbol is not a ContainerSymbol.

    '''
    def __init__(self, container_symbol):
        from psyclone.psyir.symbols import ContainerSymbol

        super(GlobalInterface, self).__init__()

        if not isinstance(container_symbol, ContainerSymbol):
            raise TypeError(
                "Global container_symbol parameter must be of type"
                " ContainerSymbol, but found '{0}'."
                "".format(type(container_symbol).__name__))

        self._container_symbol = container_symbol

    @property
    def container_symbol(self):
        '''
        :return: symbol representing the container containing this Symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        '''
        return self._container_symbol

    def __str__(self):
        return "Global(container='{0}')".format(self.container_symbol.name)


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
        self._pass_by_value = False
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
                "SymbolInterface.access must be an 'ArgumentInterface.Access' "
                "but got '{0}'.".format(type(value).__name__))
        self._access = value

    def __str__(self):
        return "Argument(pass-by-value={0})".format(self._pass_by_value)


class Symbol(object):
    '''
    Generic Symbol item for the Symbol Table. It always has a fixed name label
    that matches with the key in the SymbolTables that contain the symbol.
    If the symbol is not public then it is only visible to those nodes that
    are descendents of the Node to which its containing Symbol Table belongs.

    :param str name: name of the symbol.
    :param visibility: the visibility of the symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: the interface of the symbol.
    :type interface: :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`

    :raises TypeError: if the name is not a str or visibility is not an \
                       instance of Visibility.
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
                "{0} 'name' attribute should be of type 'str'"
                " but '{1}' found.".format(
                    type(self).__name__, type(name).__name__))
        if not isinstance(visibility, Symbol.Visibility):
            raise TypeError(
                "{0} 'visibility' attribute should be of type "
                "psyir.symbols.Symbol.Visibility but '{1}' found.".format(
                    type(self).__name__, type(visibility).__name__))
        if interface and not isinstance(interface, SymbolInterface):
            raise TypeError(
                "{0} 'interface' attribute should be of type "
                "psyir.symbols.symbol.SymbolInterface but '{1}' found."
                "".format(type(self).__name__, type(interface).__name__))

        self._name = name
        self._visibility = visibility

        # The following attributes has a setter method (with error checking)
        self._interface = None
        # If an interface is not provided, use LocalInterface by default
        if not interface:
            self._interface = LocalInterface()
        else:
            self._interface = interface

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
            raise TypeError("The interface to a Symbol must be a "
                            "SymbolInterface but got '{0}'".
                            format(type(value).__name__))
        self._interface = value

    @property
    def is_local(self):
        '''
        :returns: whether the Symbol has a Local interface.
        :rtype: bool

        '''
        return isinstance(self._interface, LocalInterface)

    @property
    def is_global(self):
        '''
        :returns: whether the Symbol has a Global interface.
        :rtype: bool

        '''
        return isinstance(self._interface, GlobalInterface)

    @property
    def is_argument(self):
        '''
        :returns: whether the Symbol has an Argument interface.
        :rtype: bool

        '''
        return isinstance(self._interface, ArgumentInterface)

    @property
    def unresolved_interface(self):
        '''
        :returns: whether the Symbol has an unresolved interface.
        :rtype: bool

        '''
        return isinstance(self._interface, UnresolvedInterface)

    def __str__(self):
        return self.name
