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

''' This module contains the LFRic-specific SymbolTable implementation.
It provides convenience functions to create often used symbols.
'''

from psyclone.domain.lfric import psyir
from psyclone.psyir.symbols import ArrayType, DataSymbol, SymbolTable


class LFRicSymbolTable(SymbolTable):
    '''
    Sub-classes SymbolTable to provide a LFRic-specific implementation.
    '''
    # pylint: disable=abstract-method

    def find_or_create_integer_symbol(self, name, tag=None):
        '''This function returns a symbol for an integer reference. If a
        tag is specified, it will be used to search for an existing symbol,
        otherwise the name will be used. If the symbol should not already
        exist in the symbol table, it will be returned, otherwise a new
        symbol will be created.

        :param str name: name of the integer variable to declare.
        :param tag: optional tag of the integer variable to declare.
        :type tag: Optional[str]

        :returns: the symbol for the variable.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol

        :raises TypeError: TypeError if the symbol exists but is not \
            a DataSymbol.
        :raises TypeError: TypeError if the symbol exists and is a \
            DataSymbol, but not an Integer.

        '''
        if tag:
            try:
                sym = self.lookup_with_tag(tag)
            except KeyError:
                sym = None
        else:
            try:
                sym = self.lookup(name)
            except KeyError:
                sym = None

        datatype = psyir.LfricIntegerScalarDataType()
        if sym is None:
            # Create a DataSymbol for this kernel argument.
            sym = self.new_symbol(name, tag=tag,
                                  symbol_type=DataSymbol,
                                  datatype=datatype)
        else:
            # The symbol already exists, check that is the right type:
            if not isinstance(sym, DataSymbol):
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not a DataSymbol, but {type(sym)}.")
            if sym.datatype != datatype:
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not an integer, but {sym.datatype}.")
        return sym

    # ------------------------------------------------------------------------
    def find_or_create_array(self, array_name, num_dimensions, intrinsic_type,
                             tag=None):
        '''This function returns a symbol for an ArrayReference. If the
        symbol does not exist, it is created.

        :param str array_name: the name and tag of the array.
        :param int num_dimensions: the number of dimensions of this array.
        :param str intrinsic_type: the intrinsic type of the array, must \
            be one of "real" or "integer".
        :param tag: optional tag to be used in searching and defining.
        :type tag: Optional[str]

        :returns: the requested symbol
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol

        :raises TypeError: if the symbol exists, but is not a DataSymbol, \
            or not an Array, or has different number of dimensions.

        '''
        if intrinsic_type == "real":
            datatype = psyir.LfricRealScalarDataType()
        elif intrinsic_type == "integer":
            datatype = psyir.LfricIntegerScalarDataType()
        elif intrinsic_type == "logical":
            datatype = psyir.LfricLogicalScalarDataType()
        else:
            raise TypeError(f"Unsupported data type "
                            f"'{intrinsic_type}' in "
                            f"find_or_create_array")

        try:
            if tag:
                sym = self.lookup_with_tag(tag)
            else:
                sym = self.lookup(array_name)
        except KeyError:
            # pylint: disable=raise-missing-from
            # Create a DataSymbol for this kernel argument.
            array_type = ArrayType(datatype,
                                   [ArrayType.Extent.ATTRIBUTE]*num_dimensions)

            sym = self.new_symbol(array_name, tag=tag,
                                  symbol_type=DataSymbol,
                                  datatype=array_type)
            return sym

        # Symbol already exists, check consistency:
        if not isinstance(sym, DataSymbol):
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not a DataSymbol, but '{type(sym)}'.")

        if not isinstance(sym.datatype, ArrayType):
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not an ArraySymbol, but "
                            f"'{type(sym.datatype)}'.")

        if sym.datatype.datatype != datatype:
            raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                            f"not of type {intrinsic_type}, but "
                            f"'{type(sym.datatype.datatype)}'.")

        if len(sym.shape) != num_dimensions:
            raise TypeError(f"Array '{sym.name}' already exists, but has "
                            f"{len(sym.shape)} dimensions, not "
                            f"{num_dimensions}.")

        return sym
