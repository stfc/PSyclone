# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''Tests complex literals'''

import pytest
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    Literal, ComplexLiteral, Assignment, BinaryOperation, Reference)
from psyclone.psyir.symbols import ScalarType, DataSymbol, SymbolTable
from psyclone.core import Signature, AccessType
from psyclone.utils import colored


def test_complex_literal_create():
    '''Test complex literal creation.'''
    lit1 = Literal("1.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit2 = Literal("2.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit = ComplexLiteral.create(lit1, lit2)
    assert isinstance(lit, ComplexLiteral)
    assert isinstance(lit.datatype, ScalarType)
    assert lit.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert lit.datatype.precision == ScalarType.Precision.UNDEFINED
    assert len(lit.children) == 2


def test_complex_literal_validation():
    '''Tests that complex literal validation.'''
    lit1 = Literal("true", ScalarType.boolean_type())
    lit2 = Literal("2.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    with pytest.raises(GenerationError) as err:
        ComplexLiteral.create(lit1.copy(), lit2.copy())
    assert ("Generation Error: Item 'Literal' can't be child 0 of "
            "'ComplexLiteral'. The valid format is: '[Literal|Reference], "
            "[Literal|Reference]'.") in str(err.value)
    with pytest.raises(GenerationError) as err:
        ComplexLiteral.create(lit2.copy(), BinaryOperation.create(
            BinaryOperation.Operator.ADD, lit2.copy(), lit2.copy()))
    assert ("Generation Error: Item 'BinaryOperation' can't be child 1 "
            "of 'ComplexLiteral'. The valid format is: '[Literal|Reference], "
            "[Literal|Reference]'.") in str(err.value)
    with pytest.raises(GenerationError) as err:
        lit = ComplexLiteral.create(lit2.copy(), lit2.copy())
        lit.children.append(lit2.copy())
    assert ("Generation Error: Item 'Literal' can't be child 2 of "
            "'ComplexLiteral'. The valid format is: '[Literal|Reference], "
            "[Literal|Reference]'.") in str(err.value)


def test_complex_literal_selectors():
    '''Tests that complex literal selectors and equality.'''
    lit1 = Literal("1.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit2 = Literal("2.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit3 = ComplexLiteral.create(lit1.copy(), lit2.copy())
    assert lit3.re_part == lit1
    assert lit3.im_part == lit2
    assert lit3.re_part != lit3.im_part
    assert lit3 == lit3


def test_complex_literal_node_str():
    '''Tests that complex literal node_str() method'''
    lit1 = Literal("1.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit2 = Literal("2.0", ScalarType(ScalarType.Intrinsic.REAL,
                                     ScalarType.Precision.UNDEFINED))
    lit3 = ComplexLiteral.create(lit1.copy(), lit2.copy())
    coloured_lit = colored("ComplexLiteral", "yellow")
    assert (lit3.node_str() == coloured_lit +
            "[datatype: Scalar<COMPLEX, UNDEFINED>]")


def test_complex_literal_reference_accesses(fortran_reader):
    '''Tests complex literal reference_accesses() method'''
    code = '''
subroutine foo()
  complex(4) :: c
  real, parameter :: foo = 1.0
  integer, parameter :: k = 4
  c = (1.0_k, foo)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    ass = psyir.walk(Assignment)[0]
    accs = ass.rhs.reference_accesses()
    assert Signature("k") in accs
    assert Signature("foo") in accs
    for (_, seq) in accs.items():
        for info in seq:
            assert info.access_type == AccessType.CONSTANT


def test_complex_literal_replace_symbols_using():
    '''Test the replace_symbols_using() method of ComplexLiteral.'''
    idef = DataSymbol("idef", ScalarType.integer_single_type())
    stype = ScalarType(ScalarType.Intrinsic.REAL, Reference(idef))
    lit = ComplexLiteral.create(Literal("1.0", stype), Literal("2.0", stype))
    table = SymbolTable()
    lit.replace_symbols_using(table)
    assert lit.datatype.precision.symbol is idef
