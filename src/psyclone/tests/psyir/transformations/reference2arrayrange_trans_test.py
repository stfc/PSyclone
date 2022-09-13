# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the Reference2ArrayRangeLoopTrans
transformation.'''

import pytest

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Reference, Literal, BinaryOperation
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE
from psyclone.psyir.transformations import Reference2ArrayRangeTrans, \
    TransformationError


CODE = (
    "program test\n"
    "  real, dimension(10) :: a\n"
    "  real :: b\n\n"
    "  a = b\n"
    "end program test\n")


def apply_trans(fortran_reader, fortran_writer, code):
    '''Utility function that applies the transformation to the code.

    :param fortran_reader: a fortran reader object
    :type fortran_reader: \
        :py:class:`psyclone.psyir.frontend.fortran.FortranReader`
    :param fortran_writer: a fortran writer object
    :type fortran_writer: \
        :py:class:`psyclone.psyir.backend.fortran.FortranWriter`
    :param str code: the Fortran input code.

    returns: the transformed Fortran code.
    :rtype str

    '''
    trans = Reference2ArrayRangeTrans()
    psyir = fortran_reader.psyir_from_source(code)
    for reference in psyir.walk(Reference):
        try:
            trans.apply(reference)
        except TransformationError:
            pass
    return fortran_writer(psyir)


def test_get_array_bound(fortran_reader):
    ''' Test that the get_array_bound utility works as expected '''
    # known bounds
    psyir = fortran_reader.psyir_from_source(CODE)
    node = psyir.walk(Reference)[0]
    symbol = node.symbol
    lower_bound, upper_bound, step = \
        Reference2ArrayRangeTrans._get_array_bound(symbol, 0)
    assert isinstance(lower_bound, Literal)
    assert lower_bound.value == "1"
    assert isinstance(upper_bound, Literal)
    assert upper_bound.value == "10"
    assert isinstance(step, Literal)
    assert step.value == "1"

    # unknown bounds
    psyir = fortran_reader.psyir_from_source(
        CODE.replace("dimension(10)", "dimension(:)"))
    node = psyir.walk(Reference)[0]
    symbol = node.symbol
    lower_bound, upper_bound, step = \
        Reference2ArrayRangeTrans._get_array_bound(symbol, 0)
    assert isinstance(lower_bound, BinaryOperation)
    assert lower_bound.operator == BinaryOperation.Operator.LBOUND
    assert isinstance(upper_bound, BinaryOperation)
    assert upper_bound.operator == BinaryOperation.Operator.UBOUND
    assert isinstance(step, Literal)
    assert step.value == "1"
    reference = lower_bound.children[0]
    assert symbol is reference.symbol
    reference = upper_bound.children[0]
    assert symbol is reference.symbol

    # non-zero array index
    psyir = fortran_reader.psyir_from_source(
        CODE.replace("dimension(10)", "dimension(10,20)"))
    node = psyir.walk(Reference)[0]
    symbol = node.symbol
    lower_bound, upper_bound, step = \
        Reference2ArrayRangeTrans._get_array_bound(symbol, 1)
    assert isinstance(lower_bound, Literal)
    assert lower_bound.value == "1"
    assert isinstance(upper_bound, Literal)
    assert upper_bound.value == "20"
    assert isinstance(step, Literal)
    assert step.value == "1"


def test_transform():
    '''Check that it is possible to create an instance of
    Reference2ArrayRangeTrans and that it is a Transformation.

    '''
    assert Reference2ArrayRangeTrans()
    assert isinstance(Reference2ArrayRangeTrans(), Transformation)


def test_notation(fortran_reader, fortran_writer):
    '''Test that array notation gets replaced with an array range when the
    size of the array range is known.

    '''
    result = apply_trans(fortran_reader, fortran_writer, CODE)
    assert "a(:) = b\n" in result


def test_dimension(fortran_reader, fortran_writer):
    '''Test that array notation gets replaced with an array range when the
    size of the array dimension is unknown.

    '''
    code = CODE.replace("dimension(10)", "dimension(:)")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "a(:) = b\n" in result


def test_variable(fortran_reader, fortran_writer):
    '''Test that a reference to an array gets replaced with an array range
    when the size of the array dimension is a variable.

    '''
    code = CODE.replace("  real, dimension(10) :: a\n",
                        "  integer :: n\n  real, dimension(n) :: a\n")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "a(:n) = b\n" in result


def test_range(fortran_reader, fortran_writer):
    '''Test that an array slice remains unchanged.'''
    code = CODE.replace("a = b", "a(2:4) = b\n")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert result == code


def test_rhs(fortran_reader, fortran_writer):
    '''Test that an array notation on the rhs also gets modified.'''
    code = CODE.replace("a = b", "a = b * c\n")
    code = code.replace(":: a", ":: a, b, c")
    code = code.replace("  real :: b\n\n", "")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "a(:) = b(:) * c(:)\n" in result


def test_multid(fortran_reader, fortran_writer):
    '''Test that multiple-dimensions are dealt with. '''
    code = CODE.replace(
        "  real, dimension(10) :: a\n", "  integer :: n,m\n  real, "
        "dimension(n,m,10) :: a, b, c\n")
    code = code.replace("a = b", "a = b * c\n")
    code = code.replace("  real :: b\n\n", "")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "a(:n,:m,:) = b(:n,:m,:) * c(:n,:m,:)\n" in result


def test_validate():
    ''' Test the validate method '''
    trans = Reference2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("The supplied node should be a Reference but found 'NoneType'."
            in str(info.value))
    with pytest.raises(TransformationError) as info:
        trans.validate(Reference(DataSymbol("x", REAL_TYPE)))
    assert ("The supplied node should be a Reference to a symbol "
            "that is an array, but 'x' is not." in str(info.value))


def test_validate_range(fortran_reader):
    '''Test that an ArrayReference raises an exception.'''
    code = CODE.replace("a = b", "a(2:4) = b\n")
    psyir = fortran_reader.psyir_from_source(code)
    reference = psyir.walk(Reference)[0]
    trans = Reference2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(reference)
    assert("The supplied node should be a Reference but found "
           "'ArrayReference'." in str(info.value))


def test_validate_structure(fortran_reader):
    '''Test that a StructureReference raises an exception. This limitation
    will be removed once issue #1858 is addressed.

    '''
    code = (
        "program test\n"
        "  type :: array_type\n"
        "      real, dimension(10) :: a\n"
        "  end type\n"
        "  type(array_type) :: ref\n"
        "  real :: b\n\n"
        "  ref%a = b\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    reference = psyir.walk(Reference)[0]
    trans = Reference2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(reference)
    assert("The supplied node should be a Reference but found "
           "'StructureReference'." in str(info.value))


def test_validate_bounds(fortran_reader, fortran_writer):
    '''Test that an exception is raised if the transformation is applied
    to a reference to an array within an LBOUND or UBOUND binary
    operator, as we do not want these to be modified.

    '''
    code = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  real, dimension(10,10,10) :: b\n"
        "  real, dimension(10,10,10) :: c\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n\n"
        "  do idx = LBOUND(a, 3), UBOUND(a, 3), 1\n"
        "    do idx_1 = LBOUND(a, 1), UBOUND(a, 1), 1\n"
        "      a = b * c\n"
        "    enddo\n"
        "  enddo\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  real, dimension(10,10,10) :: a\n"
        "  real, dimension(10,10,10) :: b\n"
        "  real, dimension(10,10,10) :: c\n"
        "  integer :: idx\n"
        "  integer :: idx_1\n\n"
        "  do idx = LBOUND(a, 3), UBOUND(a, 3), 1\n"
        "    do idx_1 = LBOUND(a, 1), UBOUND(a, 1), 1\n"
        "      a(:,:,:) = b(:,:,:) * c(:,:,:)\n"
        "    enddo\n"
        "  enddo\n\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Reference2ArrayRangeTrans()
    for idx, reference in enumerate(psyir.walk(Reference)):
        if idx < 4:
            with pytest.raises(TransformationError) as info:
                trans.apply(reference)
            assert ("Arrays within LBOUND and UBOUND operators should not "
                    "be transformed." in str(info.value))
        else:
            trans.apply(reference)
    result = fortran_writer(psyir)
    assert result == expected


def test_apply_validate():
    '''Test that the apply method calls validate by checking that the
    exception raised by validate is raised when apply is called.

    '''
    trans = Reference2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("The supplied node should be a Reference but found 'NoneType'."
            in str(info.value))
