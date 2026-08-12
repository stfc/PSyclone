# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' Performs py.test tests on the support for complex literals in the fparser2
    PSyIR front-end '''

from psyclone.psyir.nodes import (
    Literal, ComplexLiteral, Assignment, Reference, Routine, Call)
from psyclone.psyir.symbols import ScalarType, ArrayType


def test_complex_type(fortran_reader):
    '''Test that a complex type is correctly represented in PSyIR'''
    code = '''
subroutine foo()
  complex :: c
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    sub = psyir.walk(Routine)[0]
    sym = sub.symbol_table.lookup("c")
    assert sym.name == 'c'
    assert isinstance(sym.datatype, ScalarType)
    assert sym.datatype == ScalarType.complex_type()
    assert sym.is_automatic


def test_complex_precision(fortran_reader):
    '''Test that the precision of a complex type is correctly represented
    in PSyIR'''
    code = '''
subroutine foo()
  complex(8) :: c
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    sub = psyir.walk(Routine)[0]
    sym = sub.symbol_table.lookup("c")
    assert sym.name == 'c'
    assert isinstance(sym.datatype, ScalarType)
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert isinstance(sym.datatype.precision, Literal)
    assert sym.datatype.precision.value == "8"
    assert sym.is_automatic


def test_complex_literal(fortran_reader):
    '''Test that a complex literal is represented as an CMPLX
    IntrinsicCall in PSyIR'''
    code = '''
subroutine foo()
  complex :: c
  c = (1.0, 2.0)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert ass.rhs.datatype.precision == ScalarType.Precision.UNDEFINED

    assert isinstance(ass.rhs.children[0], Literal)
    assert ass.rhs.children[0].value == "1.0"
    assert isinstance(ass.rhs.children[0].datatype, ScalarType)
    assert ass.rhs.children[0].datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert ass.rhs.children[0].datatype.precision == \
           ScalarType.Precision.UNDEFINED

    assert isinstance(ass.rhs.children[1], Literal)
    assert ass.rhs.children[1].value == "2.0"
    assert isinstance(ass.rhs.children[1].datatype, ScalarType)
    assert ass.rhs.children[1].datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert ass.rhs.children[1].datatype.precision == \
           ScalarType.Precision.UNDEFINED


def test_complex_literal_initialiser(fortran_reader):
    '''Test that a complex literal initialiser is represented as
    a ComplexLiteral PSyIR node'''
    code = '''
subroutine foo()
  complex :: c = (1.0, 2.0)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    sub = psyir.walk(Routine)[0]
    sym = sub.symbol_table.lookup("c")
    lit = sym.initial_value
    assert isinstance(lit, ComplexLiteral)
    assert isinstance(lit.datatype, ScalarType)
    assert lit.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert lit.datatype.precision == ScalarType.Precision.UNDEFINED

    assert isinstance(lit.children[0], Literal)
    assert lit.children[0].value == "1.0"
    assert isinstance(lit.children[0].datatype, ScalarType)
    assert lit.children[0].datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert lit.children[0].datatype.precision == \
           ScalarType.Precision.UNDEFINED

    assert isinstance(lit.children[1], Literal)
    assert lit.children[1].value == "2.0"
    assert isinstance(lit.children[1].datatype, ScalarType)
    assert lit.children[1].datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert lit.children[1].datatype.precision == \
           ScalarType.Precision.UNDEFINED


def test_complex_literal_precision(fortran_reader):
    '''Test that a complex literal's precision is correctly propagated
    from the precision of its children'''
    code = '''
subroutine foo()
  complex :: c
  c = (1.0d-1, 2.0d-1)
  c = (1.0e-1, 2.0e-1)
  c = (1.0_4, 2.0_4)
  c = (1.0_8, 2.0_8)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype == ScalarType.complex_double_type()

    ass = psyir.walk(Assignment)[1]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype == ScalarType.complex_single_type()

    ass = psyir.walk(Assignment)[2]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype == ScalarType.complex4_type()

    ass = psyir.walk(Assignment)[3]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype == ScalarType.complex8_type()


def test_complex_literal_kind(fortran_reader):
    '''Test that a complex literal's kind is correctly propagated
    from the kind of its children'''
    code = '''
subroutine foo()
  integer, parameter :: k = 8
  complex :: c
  c = (1.0_k, 2.0_k)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert isinstance(ass.rhs.datatype.precision, Reference)
    assert ass.rhs.datatype.precision.name == "k"


def test_complex_literal_named_constant(fortran_reader):
    '''Test that a complex literal can contain named constants'''
    code = '''
subroutine foo()
  real, parameter :: r = 10.0
  complex :: c
  c = (1.0, r)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs, ComplexLiteral)
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert isinstance(ass.rhs.children[1], Reference)
    assert ass.rhs.children[1].name == "r"


def test_complex_literal_nested(fortran_reader):
    '''Test that a complex literal can occur deeply within other constructs'''
    code = '''
subroutine foo()
  use some_mode, only: some_sub
  complex :: c
  c = c + (1.0, 2.0) * c
  call some_sub((1.0, 2.0))
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)

    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert ass.rhs.datatype.precision == ScalarType.Precision.UNDEFINED

    call = psyir.walk(Call)[0]
    arg = call.arguments[0]
    assert isinstance(arg, ComplexLiteral)
    assert arg.datatype.intrinsic == ScalarType.Intrinsic.COMPLEX
    assert arg.datatype.precision == ScalarType.Precision.UNDEFINED


def test_complex_abs_intrinsic(fortran_reader):
    '''Test ABS intrinsic applied to a complex number'''
    code = '''
subroutine foo()
  real :: r, r_arr(1)
  complex :: c_arr(1)
  r_arr = ABS((1.0e1, 2.0e1))
  r_arr = ABS(c_arr)
end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)

    ass = psyir.walk(Assignment)[0]
    assert isinstance(ass.rhs.datatype, ScalarType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert ass.rhs.datatype.precision == ScalarType.Precision.SINGLE

    ass = psyir.walk(Assignment)[1]
    assert isinstance(ass.rhs.datatype, ArrayType)
    assert ass.rhs.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert ass.rhs.datatype.precision == ScalarType.Precision.UNDEFINED
