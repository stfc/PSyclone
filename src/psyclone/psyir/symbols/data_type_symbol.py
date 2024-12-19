# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the DataTypeSymbol. '''

from __future__ import absolute_import
from psyclone.psyir.symbols.symbol import Symbol


class DataTypeSymbol(Symbol):
    '''
    Symbol identifying a user-defined type (e.g. a derived type in Fortran).

    :param str name: the name of this symbol.
    :param datatype: the type represented by this symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param visibility: the visibility of this symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: the interface to this symbol.
    :type interface: :py:class:`psyclone.psyir.symbols.SymbolInterface`
    :param bool is_class: whether this symbol is used in a 'class' or 'type'
                          declaration.

    '''
    def __init__(self, name, datatype,
                 visibility=Symbol.DEFAULT_VISIBILITY,
                 interface=None,
                 is_class=False):
        super(DataTypeSymbol, self).__init__(name, visibility, interface)

        # The following attributes have setter methods (with error checking)
        self._datatype = None
        self.datatype = datatype

        self.is_class = is_class

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this
                  symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.TypeSymbol`

        '''
        return type(self)(self.name, self.datatype, visibility=self.visibility,
                          interface=self.interface.copy(),
                          is_class=self.is_class)

    def __str__(self):
        return f"{self.name}: {type(self).__name__}"

    @property
    def datatype(self):
        '''
        :returns: datatype that this DataTypeSymbol represents.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`
        '''
        return self._datatype

    @datatype.setter
    def datatype(self, value):
        ''' Setter for DataTypeSymbol datatype. Since C permits the programmer
        to typedef anything, we place no restriction on the type other
        than that it must be an instance of DataType.

        :param value: new value for datatype.
        :type value: sub-class of :py:class:`psyclone.psyir.symbols.DataType`

        :raises TypeError: if value is not an instance of DataType.

        '''
        # pylint: disable=import-outside-toplevel
        # This import has to be here to avoid circular dependencies
        from psyclone.psyir.symbols import DataType
        if not isinstance(value, DataType):
            raise TypeError(
                f"The datatype of a DataTypeSymbol must be specified using a "
                f"DataType but got: '{type(value).__name__}'")
        self._datatype = value

    @property
    def is_class(self):
        '''
        :returns: whether this DataTypeSymbol is a 'class' declaration, i.e.
                  not a 'type' one.
        :rtype: bool
        '''
        return self._is_class

    @is_class.setter
    def is_class(self, value):
        ''' Setter for DataTypeSymbol is_class.

        :param bool value: whether this DataTypeSymbol is a 'class'
                           declaration, i.e. not a 'type' one.

        :raises TypeError: if value is not a bool.

        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The is_class attribute of a DataTypeSymbol must be a bool "
                f"but got: '{type(value).__name__}'")
        self._is_class = value

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.

        :param symbol_in: the symbol from which the properties are copied.
        :type symbol_in: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, DataTypeSymbol):
            raise TypeError(f"Argument should be of type 'DataTypeSymbol' but "
                            f"found '{type(symbol_in).__name__}'.")
        super(DataTypeSymbol, self).copy_properties(symbol_in)
        self._datatype = symbol_in.datatype
        self._is_class = symbol_in.is_class

    def _process_arguments(self, visibility=None, interface=None,
                           is_class=False):
        super()._process_arguments(visibility, interface)
        self.is_class = is_class


# For automatic documentation generation
__all__ = ['DataTypeSymbol']
