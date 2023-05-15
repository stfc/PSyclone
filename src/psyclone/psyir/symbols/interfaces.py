# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Author: S. Siso STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the SymbolInterface class and its subclasses. '''

from enum import Enum

# pylint: disable=too-few-public-methods


class SymbolInterface():
    ''' Abstract class of a Symbol Interface '''

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__()


class AutomaticInterface(SymbolInterface):
    ''' The symbol is declared without attributes. Its data will live
    during the local context.
    '''

    def __str__(self):
        return "Automatic"


class StaticInterface(SymbolInterface):
    ''' The symbol contains data that is kept alive through the execution
    of the program.
    '''

    def __str__(self):
        return "Static"


class DefaultModuleInterface(SymbolInterface):
    ''' The symbol contains data declared in a module scope without additional
    attributes.
    '''

    def __str__(self):
        return "DefaultModule"


class UnknownInterface(SymbolInterface):
    ''' We have a symbol with a declaration but PSyclone does not support its
    attributes. '''

    def __str__(self):
        return "Unknown"


class CommonBlockInterface(SymbolInterface):
    ''' All the CommonBlock symbols with the same name reference the same data
    that is available during the whole execution of the program. '''

    def __str__(self):
        return "CommonBlock"


class UnresolvedInterface(SymbolInterface):
    ''' We have a symbol but we don't know where it is declared. '''

    def __str__(self):
        return "Unresolved"


class ImportInterface(SymbolInterface):
    '''Describes the interface to a Symbol that is imported from an external
    PSyIR container.

    :param container_symbol: symbol representing the external container \
        from which the symbol is imported.
    :type container_symbol: \
        :py:class:`psyclone.psyir.symbols.ContainerSymbol`


    '''
    def __init__(self, container_symbol, orig_name=None):
        super().__init__()
        if not isinstance(orig_name, (str, type(None))):
            raise TypeError(
                f"ImportInterface orig_name parameter must be of type "
                f"str or None, but found '{type(orig_name).__name__}'.")
        self._orig_name = orig_name
        # Use error-checking setter
        self.container_symbol = container_symbol

    @property
    def orig_name(self):
        '''
        :returns: the symbol's original name if it has been renamed.
        :rtype: Optional[str]
        '''
        return self._orig_name

    @property
    def container_symbol(self):
        '''
        :return: symbol representing the container containing this Symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        '''
        return self._container_symbol

    @container_symbol.setter
    def container_symbol(self, value):
        '''
        :param value: the ContainerSymbol that imports the symbol with \
            this interface.
        :type value: :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :raise TypeError: if the provided value is not a ContainerSymbol.
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import ContainerSymbol

        if not isinstance(value, ContainerSymbol):
            raise TypeError(
                f"ImportInterface container_symbol parameter must be of type"
                f" ContainerSymbol, but found "
                f"'{type(value).__name__}'.")

        self._container_symbol = value

    def __str__(self):
        orig_name_str = ""
        if self.orig_name:
            orig_name_str = f", orig_name='{self.orig_name}'"
        return (f"Import(container='{self.container_symbol.name}"
                f"'{orig_name_str})")

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__(self.container_symbol, orig_name=self.orig_name)


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
        super().__init__()
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
