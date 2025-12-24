# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Modified: A. R. Porter and S. Siso, STFC Daresbury Lab

'''Module containing tests for the Reference2ArrayRangeLoopTrans
transformation.'''

import pytest

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Reference, Assignment)
from psyclone.psyir.symbols import (
    Symbol, DataSymbol, UnresolvedType, UnsupportedFortranType)
from psyclone.psyir.transformations import (Reference2ArrayRangeTrans,
                                            TransformationError)

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
    assert "a(:) = b\n" in result


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
    assert "a(:,:,:) = b(:,:,:) * c(:,:,:)\n" in result


def test_assumed_shape(fortran_reader, fortran_writer):
    '''Test when the underlying array is of assumed shape.'''
    code = ('''\
    subroutine sub(var, istart)
      integer, intent(in) :: istart
      integer, dimension(istart:) :: var
      var = 1
    end subroutine sub
    ''')
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "var(:) = 1" in result


def test_intrinsics(fortran_reader, fortran_writer):
    '''Test that references to arrays within intrinsics are not transformed to
    array slice notation, using dotproduct as the example.

    '''
    code = CODE.replace("a = b", "b = dot_product(a, a(:))")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "b = DOT_PRODUCT(a, a(:))" in result


def test_call(fortran_reader, fortran_writer):
    '''Test that references to arrays that are arguments to a call are
    *not* transformed to array slice notation (since this affects the
    bounds of the array seen within the called routine).

    '''
    code = (
        "program test\n"
        "  use workmod, only : work\n"
        "  real, dimension(10) :: a\n"
        "  real :: b\n\n"
        "  call work(a,b)\n"
        "end program test\n")
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "call work(a, b)" in result


def test_ambiguous_call_array_reference(fortran_reader, fortran_writer):
    '''
    Test that references to arrays that *may* be arguments to a call are *not*
    transformed because, if it is a call, this will affect the bounds of the
    array seen within the called routine.

    '''
    code = '''\
    program test
      use some_mod, only: work
      integer, dimension(10) :: a, b
      a = 1
      ! Without resolving 'work', we don't know whether it is an array
      ! or a function.
      b = work(a)
    end program test'''
    result = apply_trans(fortran_reader, fortran_writer, code)
    assert "a(:) = 1" in result
    assert "b(:) = work(a)" in result


def test_validate_no_known_datatype():
    ''' Test the validate method fails if there is no known associated
    datatype.
    '''
    # If it is not even a Reference
    trans = Reference2ArrayRangeTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("The supplied node should be a Reference but found 'NoneType'."
            in str(info.value))
    # If it is a generic symbol
    with pytest.raises(TransformationError) as info:
        trans.validate(Reference(Symbol("x")))
    assert ("The supplied node should be a Reference to a DataSymbol but "
            "found 'x: Symbol<Automatic>'. Consider adding the declaration"
            "'s filename to RESOLVE_IMPORTS." in str(info.value))
    # If it is a datasymbol of UnresolvedType
    with pytest.raises(TransformationError) as info:
        trans.validate(Reference(DataSymbol("x", UnresolvedType())))
    assert ("The supplied node should be a Reference to a symbol of known "
            "type, but 'x' is 'UnresolvedType'. Consider adding the "
            "declaration's filename to RESOLVE_IMPORTS."
            in str(info.value))
    # If it is a datasymbol of UnsupportedType
    with pytest.raises(TransformationError) as info:
        trans.validate(Reference(
           DataSymbol("x", UnsupportedFortranType("decl"))))
    assert ("The supplied node should be a Reference to a symbol of known "
            "type, but 'x' is 'UnsupportedFortranType('decl')'. Consider "
            "adding the declaration's filename to RESOLVE_IMPORTS."
            in str(info.value))


def test_apply_inquiry(fortran_reader, fortran_writer):
    '''Test that the transformation does not modify arguments in inquiry
    intrinsic calls such as ALLOCATED, LBOUND, UBOUND or SIZE.
    '''
    code = (
        "program test\n"
        "  real :: a(10),b(10)\n"
        "  integer :: i,c\n"
        "  real, dimension(:), allocatable :: igor\n"
        "  do i = lbound(a,1), ubound(a,1)\n"
        "     a(i) = 0.0\n"
        "  end do\n"
        "  b(:) = 0.0\n"
        "  c = size(b,1)\n"
        "  if(allocated(igor))then\n"
        "     igor = 4.0\n"
        "  end if\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Reference2ArrayRangeTrans()
    for ref in psyir.walk(Reference):
        trans.apply(ref)
    output = fortran_writer(psyir)
    assert (
        "  do i = LBOUND(a, dim=1), UBOUND(a, dim=1), 1\n"
        "    a(i) = 0.0\n"
        "  enddo\n"
        "  b(:) = 0.0\n"
        "  c = SIZE(b, dim=1)\n"
        "  if (ALLOCATED(igor)) then\n"
        "    igor(:) = 4.0\n"
        "  end if\n") in output


def test_structure_references(fortran_reader, fortran_writer):
    ''' Test that the transformation can be applied to StructureReferences
    and its Members.
    '''
    code = (
        "program test\n"
        "  type :: inner_type\n"
        "      real :: inner\n"
        "  end type\n"
        "  type :: array_type\n"
        "      real :: scalar\n"
        "      real, dimension(10) :: field\n"
        "      type(inner_type), dimension(10) :: field_of_fields\n"
        "  end type\n"
        "  type(array_type) :: ref\n"
        "  type(array_type), dimension(10) :: array_of_ref\n"
        "  real :: b\n\n"
        "  ! These pass the transformation unmodified\n"
        "  ref%scalar = 1\n"
        "  array_of_ref(:)%scalar = 1\n"
        "  ref%field(:) = 1\n"
        "  array_of_ref(1)%field(:) = 1\n"
        "  array_of_ref(:)%field(1) = 1\n"
        "  array_of_ref(1)%field_of_fields(:)%inner = 1\n"
        "  ! These need range expressions added\n"
        "  array_of_ref%scalar = 1\n"
        "  array_of_ref(1)%field = 1\n"
        "  array_of_ref%field(1) = 1\n"
        "  ref%field = 1\n"
        "  ref%field_of_fields%inner = 1\n"
        "  array_of_ref(1)%field_of_fields%inner = 1\n"
        "  array_of_ref%field_of_fields(1)%inner = 1\n"
        "  ! This is not supported (TODO #3265)\n"
        "  ! ref%ptr => b\n"
        "end program test\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    trans = Reference2ArrayRangeTrans()
    assign = psyir.walk(Assignment)

    # The first statements pass unmodified
    trans.apply(assign[0].lhs)
    trans.apply(assign[1].lhs)
    trans.apply(assign[2].lhs)
    trans.apply(assign[3].lhs)
    trans.apply(assign[4].lhs)
    trans.apply(assign[5].lhs)

    # The following get some component replaced
    trans.apply(assign[6].lhs)
    trans.apply(assign[7].lhs)
    trans.apply(assign[8].lhs)
    trans.apply(assign[9].lhs)
    trans.apply(assign[10].lhs)
    trans.apply(assign[11].lhs)
    trans.apply(assign[12].lhs)

    output = fortran_writer(psyir)
    print(output)
    expected_unmodified = """
  ref%scalar = 1
  array_of_ref(:)%scalar = 1
  ref%field(:) = 1
  array_of_ref(1)%field(:) = 1
  array_of_ref(:)%field(1) = 1
  array_of_ref(1)%field_of_fields(:)%inner = 1
  """
    assert expected_unmodified in output

    expected_modified = """
  array_of_ref(1:10)%scalar = 1
  array_of_ref(1)%field(1:10) = 1
  array_of_ref(1:10)%field(1) = 1
  ref%field(1:10) = 1
  ref%field_of_fields(1:10)%inner = 1
  array_of_ref(1)%field_of_fields(1:10)%inner = 1
  array_of_ref(1:10)%field_of_fields(1)%inner = 1"""
    assert expected_modified in output


def test_validate_pointer_assignment(fortran_reader):
    ''' Test that a reference in a PointerAssignment raises an exception
    Even if we have a partial_datatype that could tell us that it was
    an array, pointer assignemnts must not be converted.'''
    code = (
        "program test\n"
        "  integer, dimension(10), target :: a\n"
        "  integer, dimension(10), pointer :: b\n"
        "  b => a\n"
        "end program test\n")
    psyir = fortran_reader.psyir_from_source(code)
    trans = Reference2ArrayRangeTrans()
    for reference in psyir.walk(Reference):
        with pytest.raises(TransformationError) as info:
            trans.validate(reference)
        assert ("The supplied node should be a Reference to a symbol of known "
                "type, but " in str(info.value))
