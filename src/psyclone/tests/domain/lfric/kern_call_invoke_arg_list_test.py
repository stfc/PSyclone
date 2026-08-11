# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the KernCallInvokeArgList class. '''

import pytest

from psyclone.domain.lfric import KernCallInvokeArgList
from psyclone.psyir.symbols import (DataSymbol, DataTypeSymbol,
                                    SymbolTable, UnresolvedType)


def test_kcial_construct(lfrickern):
    ''' Tests for the KernCallInvokeArgList constructor. '''
    with pytest.raises(TypeError) as err:
        KernCallInvokeArgList(lfrickern, None)
    assert ("Argument 'symbol_table' must be a SymbolTable instance but got "
            "'NoneType'" in str(err.value))
    obj = KernCallInvokeArgList(lfrickern, SymbolTable())
    assert obj.fields == []
    assert obj.scalars == []
    assert obj.quadrature_objects == []


def test_kcial_generate(lfrickern):
    ''' Tests for the KernCallInvokeArgList.generate() method. '''
    # generate() assumes a suitably initialised symbol table so create
    # that here.
    table = SymbolTable()
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
    table = SymbolTable()
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


def test_kcial_generate_halos(lfrickern_halo):
    '''
    Test that appropriate arguments are generated for a kernel that operates
    on halo cells.
    '''
    # generate() assumes a suitably initialised symbol table so create
    # that here.
    table = SymbolTable()
    table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                     datatype=UnresolvedType())
    kcial = KernCallInvokeArgList(lfrickern_halo, table)
    kcial.generate()
    assert len(kcial.arglist) == 9
    assert kcial.arglist[0] == "halo_depth"


def test_kcial_not_implemented(lfrickern):
    ''' Check all the methods that handle unsupported types of kernel
    argument. '''
    kcial = KernCallInvokeArgList(lfrickern, SymbolTable())
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
