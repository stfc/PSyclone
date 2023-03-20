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

''' This module contains the SymbolInterface class and its subclasses.

'''

from enum import Enum


class SymbolInterface():   # pylint: disable=too-few-public-methods
    ''' Abstract class of a Symbol Interface '''

    def copy(self):
        '''
        :returns: a copy of this object.
        :rtype: :py:class:`psyclone.psyir.symbol.SymbolInterface`
        '''
        return self.__class__()


class AutomaticInterface(SymbolInterface):
    # pylint: disable=too-few-public-methods
    ''' The symbol is declared without specifiers. It's properties depend
    on which context it is found.
    '''

    def __str__(self):
        return "Automatic"


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


    '''
    def __init__(self, container_symbol):
        super().__init__()
        # Use error-checking setter
        self.container_symbol = container_symbol

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
