# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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

from __future__ import annotations
import abc
from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
from psyclone.psyir.symbols.symbol import Symbol


class TypedSymbol(Symbol, metaclass=abc.ABCMeta):
    '''
    Abstract base class for those Symbols that have an associated datatype.

    :param str name: name of the symbol.
    :param datatype: data type of the symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param kwargs: additional keyword arguments provided by
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
        copy = type(self)(self.name, self.datatype.copy(),
                          visibility=self.visibility,
                          interface=self.interface.copy())
        copy.preceding_comment = self.preceding_comment
        copy.inline_comment = self.inline_comment
        return copy

    def copy_properties(self, symbol_in: TypedSymbol,
                        exclude_interface: bool = False):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name (which is immutable) and visibility.
        If `exclude_interface` is True, the interface is also not updated.

        :param symbol_in: the symbol from which the properties are copied.
        :param exclude_interface: whether or not to copy the interface
            property of the provided Symbol (default is to include it).

        :raises TypeError: if the argument is not the expected type.

        '''
        if not isinstance(symbol_in, TypedSymbol):
            raise TypeError(f"Argument should be of type 'TypedSymbol' but "
                            f"found '{type(symbol_in).__name__}'.")
        super().copy_properties(symbol_in, exclude_interface=exclude_interface)
        self._datatype = symbol_in.datatype

    def resolve_type(self) -> TypedSymbol:
        ''' If the symbol has an Unresolved datatype or is an import and
        is of NoType (indicating a RoutineSymbol), find where it is defined
        (i.e. an external container) and obtain the properties of the symbol.

        :returns: this TypedSymbol with its properties updated. This is for
                  consistency with the equivalent method in the Symbol
                  class which returns a new Symbol object.

        '''
        # Avoid circular dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import NoType, UnresolvedType
        if (isinstance(self.datatype, UnresolvedType) or
                (isinstance(self.datatype, NoType) and self.is_import)):
            # Copy all the symbol properties but the interface and
            # visibility (the latter is determined by the current
            # scoping unit)
            extern_symbol = self.get_external_symbol()
            self.copy_properties(extern_symbol, exclude_interface=True)

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
        super().replace_symbols_using(table_or_symbol)

        from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
        if isinstance(self.datatype, DataTypeSymbol):
            if isinstance(table_or_symbol, Symbol):
                if table_or_symbol.name.lower() == self.datatype.name.lower():
                    self._datatype = table_or_symbol
            else:
                try:
                    self._datatype = table_or_symbol.lookup(self.datatype.name)
                except KeyError:
                    pass
        else:
            self._datatype.replace_symbols_using(table_or_symbol)

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this DataType.
        '''
        symbols = super().get_all_accessed_symbols()

        if self.is_import:
            # We ignore any dependencies associated with imported symbols.
            return symbols

        if isinstance(self.datatype, DataTypeSymbol):
            symbols.add(self.datatype)
        else:
            symbols.update(self.datatype.get_all_accessed_symbols())
        return symbols
