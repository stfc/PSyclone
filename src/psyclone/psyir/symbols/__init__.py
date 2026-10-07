# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Symbols package module '''

from psyclone.psyir.symbols.datasymbol import DataSymbol
from psyclone.psyir.symbols.containersymbol import ContainerSymbol
from psyclone.psyir.symbols.data_type_symbol import DataTypeSymbol
from psyclone.psyir.symbols.generic_interface_symbol import (
    GenericInterfaceSymbol)
from psyclone.psyir.symbols.interfaces import (
     ArgumentInterface, AutomaticInterface, CommonBlockInterface,
     DefaultModuleInterface, ImportInterface, PreprocessorInterface,
     StaticInterface, UnknownInterface, UnresolvedInterface)
from psyclone.psyir.symbols.intrinsic_symbol import IntrinsicSymbol
from psyclone.psyir.symbols.routinesymbol import RoutineSymbol
from psyclone.psyir.symbols.symbol import Symbol, SymbolError
from psyclone.psyir.symbols.symbol_table import SymbolTable
from psyclone.psyir.symbols.typed_symbol import TypedSymbol
from psyclone.psyir.symbols.datatypes import (
     ArrayType, NoType, ScalarType, StructureType, UnresolvedType,
     UnsupportedType, UnsupportedFortranType, DataType)

# For auto documentation generation.
__all__ = ['ArgumentInterface',
           'ArrayType',
           'AutomaticInterface',
           'CommonBlockInterface',
           'ContainerSymbol',
           'DataSymbol',
           'DataType',
           'DataTypeSymbol',
           'DefaultModuleInterface',
           'GenericInterfaceSymbol',
           'ImportInterface',
           'IntrinsicSymbol',
           'NoType',
           'PreprocessorInterface',
           'RoutineSymbol',
           'ScalarType',
           'StaticInterface',
           'StructureType',
           'Symbol',
           'SymbolError',
           'SymbolTable',
           'TypedSymbol',
           'UnsupportedFortranType',
           'UnknownInterface',
           'UnsupportedType',
           'UnresolvedInterface',
           'UnresolvedType']
