# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Test that the LFRic-specific PSyIR classes are created and declared
correctly'''

import pytest
from psyclone.domain.lfric.psyir import CONSTANTS_MOD, I_DEF, R_DEF, L_DEF, LfricIntegerScalarDataType, LfricRealScalarDataType, LfricLogicalScalarDataType, LfricIntegerScalarDataSymbol, LfricRealScalarDataSymbol, LfricLogicalScalarDataSymbol
from psyclone.psyir.symbols import ContainerSymbol, DataSymbol, GlobalInterface, ScalarType, LocalInterface, ArgumentInterface

# Modules and their arguments
@pytest.mark.parametrize("module, symbol_list", [(CONSTANTS_MOD, [I_DEF, R_DEF, L_DEF])])
def test_constants_mod(module, symbol_list):
    '''Test the generated module symbol and its argument symbols are
    created correctly.

    '''
    assert module
    assert isinstance(module, ContainerSymbol)
    for symbol in symbol_list:
        assert symbol
        assert isinstance(symbol, DataSymbol)
        assert isinstance(symbol.interface, GlobalInterface)
        assert symbol.interface.container_symbol is module

# Generic scalars
@pytest.mark.parametrize("DataType, Symbol, intrinsic, precision", [
    (LfricIntegerScalarDataType, LfricIntegerScalarDataSymbol, ScalarType.Intrinsic.INTEGER, I_DEF),
    (LfricRealScalarDataType, LfricRealScalarDataSymbol, ScalarType.Intrinsic.REAL, R_DEF),
    (LfricLogicalScalarDataType, LfricLogicalScalarDataSymbol, ScalarType.Intrinsic.BOOLEAN, L_DEF)])
def test_generic_scalar_types(DataType, Symbol, intrinsic, precision):
    '''Test the generated generic scalar datatypes and symbols are created
    correctly.

    '''
    # datatype
    lfric_datatype = DataType()
    assert lfric_datatype.intrinsic == intrinsic
    assert lfric_datatype.precision is precision
    # symbol
    lfric_symbol = Symbol("symbol")
    assert lfric_symbol.name == "symbol"
    assert isinstance(lfric_symbol.interface, LocalInterface)
    assert isinstance(lfric_symbol.datatype, DataType)
    lfric_symbol = Symbol("symbol", interface=ArgumentInterface(ArgumentInterface.Access.READ))
    assert isinstance(lfric_symbol.interface, ArgumentInterface)
    assert lfric_symbol.interface.access == ArgumentInterface.Access.READ

# TBD
# Check the specific LFRic scalar datatypes and symbols are created correctly
# Check the LFRic field/array datatypes and symbols are created correctly
# Check the LFRic vector-field-data symbols are created correctly
