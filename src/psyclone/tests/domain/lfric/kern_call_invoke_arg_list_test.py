# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Modified: J. Henrichs, Bureau of Meteorology
#           L. Turner, Met Office

''' Module containing pytest tests for the KernCallInvokeArgList class. '''

import pytest

from psyclone.domain.lfric import (KernCallInvokeArgList, LFRicSymbolTable)
from psyclone.psyir.symbols import DataTypeSymbol, UnresolvedType, DataSymbol


def test_kcial_construct(lfrickern):
    ''' Tests for the KernCallInvokeArgList constructor. '''
    with pytest.raises(TypeError) as err:
        KernCallInvokeArgList(lfrickern, None)
    assert ("Argument 'symbol_table' must be a SymbolTable instance but got "
            "'NoneType'" in str(err.value))
    obj = KernCallInvokeArgList(lfrickern, LFRicSymbolTable())
    assert obj.fields == []
    assert obj.scalars == []
    assert obj.quadrature_objects == []


def test_kcial_generate(lfrickern):
    ''' Tests for the KernCallInvokeArgList.generate() method. '''
    # generate() assumes a suitably initialised symbol table so create
    # that here.
    table = LFRicSymbolTable()
    table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                     datatype=UnresolvedType())
    kcial = KernCallInvokeArgList(lfrickern, table)
    kcial.generate()
    assert len(kcial.fields) == 5
    assert len(kcial.scalars) == 3
    # Check that we can call it repeatedly.
    kcial.generate()
    assert len(kcial.fields) == 5
    # Check that an unsupported scalar type gives the expected error.
    lfrickern.arguments.args[0]._intrinsic_type = 'wrong'
    kcial = KernCallInvokeArgList(lfrickern, table)
    with pytest.raises(NotImplementedError) as err:
        kcial.generate()
    assert "Scalar of type 'wrong' not supported" in str(err.value)


def test_kcial_generate_operator(lfrickern_op):
    '''Test the generate() method correctly populates the list of operator
    arguments required by the kernel.'''
    # generate() assumes a suitably initialised symbol table so create
    # that here.
    table = LFRicSymbolTable()
    table.new_symbol("operator_type", symbol_type=DataTypeSymbol,
                     datatype=UnresolvedType())
    table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                     datatype=UnresolvedType())
    kcial = KernCallInvokeArgList(lfrickern_op, table)
    kcial.generate()
    opers = kcial.operators
    assert len(opers) == 1
    assert len(opers[0]) == 3
    assert isinstance(opers[0][0], DataSymbol)
    assert opers[0][1] == "w3"
    assert opers[0][2] == "w2"


def test_kcial_not_implemented(lfrickern):
    ''' Check all the methods that handle unsupported types of kernel
    argument. '''
    kcial = KernCallInvokeArgList(lfrickern, LFRicSymbolTable())
    with pytest.raises(NotImplementedError) as err:
        kcial.stencil(None)
    assert "Stencils are not yet supported" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        kcial.stencil_2d(None)
    assert "Stencils are not yet supported" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        kcial.stencil_unknown_extent(None)
    assert "stencil_unknown_extent not yet implemented" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        kcial.stencil_2d_unknown_extent(None)
    assert "stencil_2d_unknown_extent not yet implemented" in str(err.value)
