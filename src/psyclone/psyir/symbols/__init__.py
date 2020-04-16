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
# Author S. Siso, STFC Daresbury Lab
# Modified by A. R. Porter and R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Symbols package module '''

from psyclone.psyir.symbols.symbol import Symbol, SymbolError
from psyclone.psyir.symbols.datasymbol import DataSymbol, LocalInterface, \
    GlobalInterface, ArgumentInterface, UnresolvedInterface
from psyclone.psyir.symbols.containersymbol import ContainerSymbol
from psyclone.psyir.symbols.symboltable import SymbolTable
from psyclone.psyir.symbols.datatypes import DataType, ScalarType, \
    ArrayType, TYPE_MAP_TO_PYTHON, REAL_TYPE, REAL_SINGLE_TYPE, \
    REAL_DOUBLE_TYPE, REAL4_TYPE, REAL8_TYPE, INTEGER_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER_DOUBLE_TYPE, INTEGER4_TYPE, INTEGER8_TYPE, \
    BOOLEAN_TYPE, CHARACTER_TYPE, DeferredType

# The entities in the __all__ list are made available to import directly from
# this package e.g. 'from psyclone.psyir.symbols import DataSymbol'
__all__ = ['TYPE_MAP_TO_PYTHON',
           'Symbol',
           'SymbolError',
           'SymbolTable',
           'DataSymbol',
           'DataType',
           'LocalInterface',
           'GlobalInterface',
           'ArgumentInterface',
           'UnresolvedInterface',
           'ContainerSymbol',
           'ScalarType',
           'ArrayType',
           'REAL_TYPE',
           'REAL_SINGLE_TYPE',
           'REAL_DOUBLE_TYPE',
           'REAL4_TYPE',
           'REAL8_TYPE',
           'INTEGER_TYPE',
           'INTEGER_SINGLE_TYPE',
           'INTEGER_DOUBLE_TYPE',
           'INTEGER4_TYPE',
           'INTEGER8_TYPE',
           'BOOLEAN_TYPE',
           'CHARACTER_TYPE',
           'DeferredType']
