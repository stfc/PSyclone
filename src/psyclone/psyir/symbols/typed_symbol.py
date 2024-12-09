# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

'''This module contains the TypedSymbol class.

'''

import abc
from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
from psyclone.psyir.symbols.symbol import Symbol


class TypedSymbol(Symbol, metaclass=abc.ABCMeta):
    '''
    Abstract base class for those Symbols that have an associated datatype.

    :param str name: name of the symbol.
    :param datatype: data type of the symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param kwargs: additional keyword arguments provided by \
                   :py:class:`psyclone.psyir.symbols.Symbol`
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, datatype, **kwargs):
        self._datatype = None
        super(TypedSymbol, self).__init__(name)
        self._process_arguments(datatype=datatype, **kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case the datatype argument.

        :param kwargs: keyword arguments which can be:\n
            :param datatype: data type of the symbol.\n
            :type datatype: :py:class:`psyclone.psyir.symbols.DataType`\n
            and the arguments in :py:class:`psyclone.psyir.symbols.Symbol`
        :type kwargs: unwrapped dict.

        :raises AttributeError: if the datatype argument is not given \
            and it isn't already a property of this symbol.

        '''
        if "datatype" in kwargs:
            self.datatype = kwargs.pop("datatype")
        elif not hasattr(self, '_datatype'):
            raise AttributeError(f"Missing mandatory 'datatype' attribute for "
                                 f"symbol '{self.name}'.")

        super(TypedSymbol, self)._process_arguments(**kwargs)

    @abc.abstractmethod
    def __str__(self):
        ''' Abstract method. Must be overridden in sub-class. '''

    @property
    def datatype(self):
        '''
        :returns: datatype of the TypedSymbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType` or \
                :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        '''
        return self._datatype

    @datatype.setter
    def datatype(self, value):
        ''' Setter for the datatype of a TypedSymbol.

        :param value: new value for datatype.
        :type value: :py:class:`psyclone.psyir.symbols.DataType` or \
                     :py:class:`psyclone.psyir.symbols.DataTypeSymbol`

        :raises TypeError: if value is not of the correct type.

        '''
        # We can't do this import at the toplevel as we get a circular
        # dependency with the datatypes module.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import DataType
        if not isinstance(value, (DataType, DataTypeSymbol)):
            raise TypeError(
                f"The datatype of a {type(self).__name__} must be specified "
                f"using either a DataType or a DataTypeSymbol but got: "
                f"'{type(value).__name__}'")
        self._datatype = value

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this
                  symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.TypedSymbol`

        '''
        # The constructors for all Symbol-based classes have 'name' as the
        # first positional argument.
        return type(self)(self.name, self.datatype.copy(),
                          visibility=self.visibility,
                          interface=self.interface.copy())

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.

        :param symbol_in: the symbol from which the properties are copied.
        :type symbol_in: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, TypedSymbol):
            raise TypeError(f"Argument should be of type 'TypedSymbol' but "
                            f"found '{type(symbol_in).__name__}'.")
        super(TypedSymbol, self).copy_properties(symbol_in)
        self._datatype = symbol_in.datatype

    def resolve_type(self):
        ''' If the symbol has an Unresolved datatype, find where it is defined
        (i.e. an external container) and obtain the properties of the symbol.

        :returns: this TypedSymbol with its properties updated. This is for \
                  consistency with the equivalent method in the Symbol \
                  class which returns a new Symbol object.
        :rtype: :py:class:`psyclone.psyir.symbols.TypedSymbol`

        '''
        # This import has to be local to this method to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import UnresolvedType
        if isinstance(self.datatype, UnresolvedType):
            # Copy all the symbol properties but the interface and
            # visibility (the latter is determined by the current
            # scoping unit)
            tmp = self.interface
            extern_symbol = self.get_external_symbol()
            self.copy_properties(extern_symbol)
            self.interface = tmp

        return self

    @property
    def is_scalar(self):
        '''
        :returns: True if this symbol is a scalar and False otherwise.
        :rtype: bool

        '''
        # This import has to be local to this method to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import ScalarType
        return isinstance(self.datatype, ScalarType)

    @property
    def is_array(self):
        '''
        :returns: True if this symbol is an array and False if it is not or
            there is not enough symbol information to determine it.
        :rtype: bool

        '''
        # This import has to be local to this method to avoid circular
        # dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import (
            ArrayType, UnsupportedFortranType)
        if isinstance(self.datatype, ArrayType):
            return True
        return (isinstance(self.datatype, UnsupportedFortranType) and
                isinstance(self.datatype.partial_datatype, ArrayType))

    @property
    def shape(self):
        '''
        :returns: shape of the symbol in column-major order (leftmost
                  index is contiguous in memory). Each entry represents
                  an array dimension. If it is 'None' the extent of that
                  dimension is unknown, otherwise it holds an integer
                  literal or a reference to an integer symbol with the
                  extent. If it is an empty list then the symbol
                  represents a scalar.
        :rtype: List[Optional[:py:class:`psyclone.psyir.nodes.Literal` |
                              :py:class:`psyclone.psyir.nodes.Reference`]]

        '''
        if self.is_array:
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.symbols.datatypes import UnsupportedFortranType
            if isinstance(self.datatype, UnsupportedFortranType):
                # An UnsupportedFortranType that has is_array True must have a
                # partial_datatype.
                return self._datatype.partial_datatype.shape
            return self._datatype.shape
        return []

    def replace_symbols_using(self, table):
        '''
        Replace any Symbols referred to by this object with those in the
        supplied SymbolTable with matching names. If there
        is no match for a given Symbol then it is left unchanged.

        :param table: the symbol table from which to get replacement symbols.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        super().replace_symbols_using(table)

        from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
        if isinstance(self.datatype, DataTypeSymbol):
            try:
                self._datatype = table.lookup(self.datatype.name)
            except KeyError:
                pass
        else:
            self._datatype.replace_symbols_using(table)
