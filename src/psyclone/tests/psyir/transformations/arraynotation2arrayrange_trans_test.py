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
# Author R. W. Ford

'''Module containing tests for the ArrayNotation2ArrayRangeLoopTrans
transformation.'''

import pytest

from psyclone.psyir.nodes import Literal, BinaryOperation, Reference, \
    Range, ArrayReference, Assignment, Node, DataNode, KernelSchedule
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, \
    INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import ArrayNotation2ArrayRangeTrans, \
    TransformationError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import Compile


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
    trans = ArrayNotation2ArrayRangeTrans()
    psyir = fortran_reader.psyir_from_source(code)
    for reference in psyir.walk(Reference):
        try:
            trans.apply(reference)
        except TransformationError:
            pass
    return (fortran_writer(psyir))
    

def test_transform():
    '''Check that it is possible to create an instance of
    ArrayNotation2ArrayRangeTrans and that it is a Transformation.

    '''
    assert ArrayNotation2ArrayRangeTrans()
    assert isinstance(ArrayNotation2ArrayRangeTrans(), Transformation)


def test_notation(fortran_reader, fortran_writer):
    '''Test that array notation gets replaced with an array range when the
    size of the array range is known.

    '''
    result = apply_trans(fortran_reader, fortran_writer, CODE)
    assert ("a(1:10) = b\n" in result)


def test_dimension(fortran_reader, fortran_writer):
    '''Test that array notation gets replaced with an array range when the
    size of the array dimension is unknown.

    '''
    code = CODE.replace("dimension(10)", "dimension(:)")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert ("a(:) = b\n" in result)


def test_dimension(fortran_reader, fortran_writer):
    '''Test that array notation gets replaced with an array range when the
    size of the array dimension is unknown.

    '''
    code = CODE.replace("dimension(10)", "dimension(:)")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert ("a(:) = b\n" in result)


def test_variable(fortran_reader, fortran_writer):
    '''Test that a array notation gets replaced with an array range when
    the size of the array dimension is a variable.

    '''
    code = CODE.replace("  real, dimension(10) :: a\n",
                        "  integer :: n\n  real, dimension(n) :: a\n")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert ("a(1:n) = b\n" in result)


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
    assert ("a(1:10) = b(1:10) * c(1:10)\n" in result)


def test_multid(fortran_reader, fortran_writer):
    '''Test that multiple-dimensions are dealt with. '''
    code = CODE.replace(
        "  real, dimension(10) :: a\n", "  integer :: n,m\n  real, "
        "dimension(n,m,10) :: a, b, c\n")
    code = code.replace("a = b", "a = b * c\n")
    code = code.replace("  real :: b\n\n", "")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert ("a(1:n,1:m,1:10) = b(1:n,1:m,1:10) * c(1:n,1:m,1:10)\n" in result)


def test_validate():
    ''' Test the validate method '''
    trans = ArrayNotation2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("The supplied node should be a Reference but found 'NoneType'."
            in str(info.value))
    with pytest.raises(TransformationError) as info:
        trans.validate(Reference(DataSymbol("x", REAL_TYPE)))
    assert ("The supplied node should be a Reference that references a "
            "symbol that is an array, but 'x' is not." in str(info.value))


def test_apply_validate():
    '''Test that the apply method calls validate by checking that the
    exception raised by validate is raised when apply is called.

    '''
    trans = ArrayNotation2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("The supplied node should be a Reference but found 'NoneType'."
            in str(info.value))
