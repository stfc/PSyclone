# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# Modified: O.Brunt, Met Office
# -----------------------------------------------------------------------------

''' This module contains the LFRic-specific SymbolTable implementation.
It provides convenience functions to create often used symbols.
'''

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
# Avoid circular import:
from psyclone.domain.lfric.lfric_types import LFRicTypes

from psyclone.psyir.symbols import (ArrayType, ContainerSymbol, DataSymbol,
                                    ImportInterface, INTEGER_TYPE, ScalarType,
                                    Symbol, SymbolTable)


class LFRicSymbolTable(SymbolTable):
    # pylint: disable=abstract-method
    '''
    Sub-classes SymbolTable to provide a LFRic-specific implementation.

    :param node: reference to the Schedule or Container to which this \
        symbol table belongs.
    :type node: :py:class:`psyclone.psyir.nodes.Schedule`, \
        :py:class:`psyclone.psyir.nodes.Container` or NoneType
    :param default_visibility: optional default visibility value for this \
        symbol table, if not provided it defaults to PUBLIC visibility.
    :type default_visibillity: \
        :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

    '''

    # The container symbol for all precision variables
    _constants_mod = None
    # A mapping of 'i_def' etc. to the corresponding DataSymbole
    _precision_map = {}

    def __init__(self, node=None, default_visibility=Symbol.Visibility.PUBLIC):
        super().__init__(node, default_visibility)
        # First time an instance of this is created, define
        # the precision mapping.
        if not LFRicSymbolTable._precision_map:
            const = LFRicConstants()
            mod_name = const.UTILITIES_MOD_MAP["constants"]["module"]
            LFRicSymbolTable._constants_mod = ContainerSymbol(mod_name)

            api_config = Config.get().api_conf("dynamo0.3")
            for precision in api_config.precision_map:
                LFRicSymbolTable._precision_map[precision] = \
                    DataSymbol(precision, INTEGER_TYPE,
                               interface=ImportInterface(self._constants_mod))

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
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

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

        datatype = LFRicTypes("LFRicIntegerScalarDataType")()
        if sym is None:
            # Create a DataSymbol for this kernel argument.
            sym = self.new_symbol(name, tag=tag,
                                  symbol_type=DataSymbol,
                                  datatype=datatype)
        else:
            # The symbol already exists, check that is the right type:
            if not isinstance(sym, DataSymbol):
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not a DataSymbol, but '{type(sym)}'.")
            if sym.datatype != datatype:
                raise TypeError(f"Symbol '{sym.name}' already exists, but is "
                                f"not an integer, but '{sym.datatype}'.")
        return sym

    # ------------------------------------------------------------------------
    def find_or_create_array(self, array_name, num_dimensions, intrinsic_type,
                             tag=None):
        '''This function returns a symbol for an ArrayReference. If the
        symbol does not exist, it is created. If a new array symbol is
        created, it gets the DEFERRED attribute, which in Fortran means
        it will be declared as an allocatable array.

        :param str array_name: the name and tag of the array.
        :param int num_dimensions: the number of dimensions of this array.
        :param intrinsic_type: the intrinsic type of the array.
        :type intrinsic_type: \
            :py:class:`psyclone.psyir.symbols.datatypes.ScalarType.Intrinsic`
        :param tag: optional tag to be used in searching and defining.
        :type tag: Optional[str]

        :returns: the requested symbol
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the symbol exists, but is not a DataSymbol, \
            or not an Array, or has different number of dimensions.

        '''
        if intrinsic_type == ScalarType.Intrinsic.REAL:
            datatype = LFRicTypes("LFRicRealScalarDataType")()
        elif intrinsic_type == ScalarType.Intrinsic.INTEGER:
            datatype = LFRicTypes("LFRicIntegerScalarDataType")()
        elif intrinsic_type == ScalarType.Intrinsic.BOOLEAN:
            datatype = LFRicTypes("LFRicLogicalScalarDataType")()
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
                                   [ArrayType.Extent.DEFERRED]*num_dimensions)

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
                            f"not of type '{intrinsic_type}', but "
                            f"'{type(sym.datatype.datatype)}'.")

        if len(sym.shape) != num_dimensions:
            raise TypeError(f"Array '{sym.name}' already exists, but has "
                            f"{len(sym.shape)} dimensions, not "
                            f"{num_dimensions}.")

        return sym

    # ------------------------------------------------------------------------
    def add_lfric_precision_symbol(self, name):
        '''
        If the named LFRic precision symbol is not already in the table then
        add it. Also ensure that the Container symbol from which it is
        imported is in the table.

        :param str name: name of the LFRic precision symbol to add to table.

        :returns: the specified LFRic precision symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises ValueError: if the supplied name is not a recognised LFRic \
            precision variable.
        :raises ValueError: if a symbol with the same name is already in the \
            table but is not imported from the correct container.

        '''
        api_config = Config.get().api_conf("dynamo0.3")
        if name not in api_config.precision_map.keys():
            raise ValueError(f"'{name}' is not a recognised LFRic precision.")
        sym = LFRicSymbolTable._precision_map[name]

        if name in self:
            # Sanity check that the existing symbol is the right one.
            existing_sym = self.lookup(name)
            if (not isinstance(existing_sym.interface, ImportInterface) or
                    existing_sym.interface.container_symbol.name !=
                    self._constants_mod.name):
                raise ValueError(
                    f"Precision symbol '{name}' already exists in the "
                    f"supplied symbol table but is not imported from the "
                    f"LFRic constants module ({self._constants_mod.name}).")
            return existing_sym

        # pylint: disable=undefined-variable
        if self._constants_mod.name not in self:
            self.add(self._constants_mod)
        self.add(sym)
        return sym
