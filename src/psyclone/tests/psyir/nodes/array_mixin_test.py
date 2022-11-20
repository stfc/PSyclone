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
# Author S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests of the ArrayMixin PSyIR nodes trait. '''

import pytest
from psyclone.errors import InternalError
from psyclone.psyir.nodes import ArrayReference, ArrayOfStructuresReference, \
    BinaryOperation, Range, Literal, Routine, Assignment, Reference
from psyclone.psyir.symbols import DataSymbol, DeferredType, ArrayType, \
    INTEGER_TYPE, REAL_TYPE

# _validate_child, is_array, get_signature_and_indices and
# _validate_index methods are all tested in the ArrayReference class.


# _is_bound_op

def test_is_bound_op():
    '''Check the _is_bound_op utility function works as expected.  Create
    and use an ArrayReference node to test the abstract ArrayMixin
    class.

    '''
    one = Literal("1", INTEGER_TYPE)
    array = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array2 = DataSymbol("my_symbol2", ArrayType(INTEGER_TYPE, [10]))
    scalar = DataSymbol("tmp", INTEGER_TYPE)
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(array), one)
    my_range = Range.create(one.copy(), ubound)
    array_ref = ArrayReference.create(array, [my_range])
    array2_ref = ArrayReference.create(array2, [my_range.copy()])
    # not a binary operation
    oper = None
    assert not array_ref._is_bound_op(
        oper, BinaryOperation.Operator.UBOUND, one)
    # not a match with the binary operator
    oper = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array_ref, one.copy())
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # 1st dimension of the bound is not the same array
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array2_ref, one.copy())
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # 2nd dimension of the bound not a literal
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array_ref.copy(), Reference(scalar))
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # 2nd dimension of the bound not an integer literal
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array_ref.copy(),
        Literal("1.0", REAL_TYPE))
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # 2nd dimension of the bound not the expected index
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array_ref.copy(),
        Literal("2", INTEGER_TYPE))
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # OK
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array_ref.copy(), one.copy())
    assert array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)


# is_lower_bound and is_upper_bound

def test_is_upper_lower_bound(fortran_reader):
    '''Test the is_lower_bound() and is_upper_bound() methods return the
    expected values if an array reference has literal bounds. Create
    and use an ArrayReference node to test the abstract ArrayMixin
    class.

    '''
    code = (
        "subroutine test()\n"
        "real a(n)\n"
        "a(1:n) = 0.0\n"
        "end subroutine\n")

    # Return True as the symbolic values of the declaration and array
    # reference match.
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert array_ref.is_lower_bound(0)
    assert array_ref.is_upper_bound(0)


# _is_bound

def test_is_bound_validate_index(fortran_reader):
    '''Test the _is_bound method calls the _validate_index method.'''
    code = (
        "subroutine test()\n"
        "real :: a(10)\n"
        "a(:) = 0.0\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    with pytest.raises(ValueError) as info:
        array_ref._is_bound(2, "upper")
    assert ("In ArrayReference 'a' the specified index '2' must be less than "
            "the number of dimensions '1'." in str(info.value))


@pytest.mark.parametrize("bounds,lower,upper", [
    (":", True, True), ("2:", False, True), (":n-1", True, False),
    ("2:n-1", False, False)])
def test_is_bound_ulbound(fortran_reader, bounds, lower, upper):
    '''Test the _is_bound method returns True when the access to the bound
    is [ul]bound due to the use of array notation in the original
    Fortran code. Test with an access to a regular array and and
    access to an array in a structure. Also test with the declarations
    of the arrays not being available.

    '''
    code = (
        f"subroutine test()\n"
        f"use my_mod\n"
        f"type(my_type) :: b\n"
        f"a({bounds}) = 0.0\n"
        f"b%c({bounds}) = 0.0\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert array_ref._is_bound(0, "lower") is lower
    assert array_ref._is_bound(0, "upper") is upper
    array_ref = assigns[1].lhs.children[0]
    assert array_ref._is_bound(0, "lower") is lower
    assert array_ref._is_bound(0, "upper") is upper


@pytest.mark.parametrize("access,lower,upper", [
    ("1", False, False), ("10", False, False)])
def test_is_bound_structure(fortran_reader, access, lower, upper):
    '''Test that _is_bound always returns False if the array access within
    a structure is a single access, even if it matches the upper or
    lower bound of the declaration.

    '''
    code = (
        f"subroutine test()\n"
        f"type my_type\n"
        f"  real :: b(10)\n"
        f"end type\n"
        f"type(my_type) :: a\n"
        f"a%b({access}) = 0.0\n"
        f"end subroutine\n")

    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)

    structure_ref = assigns[0].lhs
    array_member = structure_ref.children[0]
    assert not array_member._is_bound(0, "lower")
    assert not array_member._is_bound(0, "upper")


def test_get_symbol_imported(fortran_reader):
    '''Test the _is_bound utility method returns False when the access
    can't be matched with the declaration as the declaration is
    imported.

    '''
    code = (
        "subroutine test()\n"
        "use some_mod\n"
        "var1(3:4) = 0\n"
        "end subroutine\n")

    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert not array_ref._is_bound(0, "lower")
    assert not array_ref._is_bound(0, "upper")


def test_get_symbol_unknownfortrantype(fortran_reader):
    '''Test the _is_bound method returns False when the array access
    datatype is an UnknownFortranType.

    '''
    code = (
        "subroutine test()\n"
        "character(10) my_str\n"
        "my_str(2:2) = 'b'\n"
        "end subroutine\n")

    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert not array_ref._is_bound(0, "lower")
    assert not array_ref._is_bound(0, "upper")


@pytest.mark.parametrize("bounds,access,lower,upper", [
    ("10", "1", True, False), ("10", "10", False, True),
    ("10", "5", False, False), ("n", "1", True, False),
    ("n", "n", False, True), ("n", "n-4", False, False),
    ("10", "5+5", False, True)])
def test_is_bound_access(fortran_reader, bounds, access, lower, upper):
    '''Test the _is_bound method returns True when the array access
    matches the array declaration and False if not. Note, the method
    supports array declarations that are expressions, however,
    currently the PSyIR does not recognise these so they can't be
    tested.

    '''
    code = (
        f"subroutine test()\n"
        f"integer :: n\n"
        f"real :: a({bounds})\n"
        f"a({access}) = 0.0\n"
        f"end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert array_ref._is_bound(0, "lower") is lower
    assert array_ref._is_bound(0, "upper") is upper


# is_same_array, is_full_range, and indices methods are all tested in
# the ArrayReference class.


# _get_effective_shape

def test_get_effective_shape(fortran_reader, fortran_writer):
    '''Tests for the _get_effective_shape() method.'''
    code = (
        "subroutine test()\n"
        "  use some_mod\n"
        "  integer :: indices(8,3)\n"
        "  real a(10), b(10,10)\n"
        "  a(1:10) = 0.0\n"
        "  b(indices(2:3,1), 2:5) = 2.0\n"
        "  b(indices(2:3,1:2), 2:5) = 2.0\n"
        "  a(f()) = 2.0\n"
        "  a(2+3) = 1.0\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Direct array slice.
    shape = routine.children[0].lhs._get_effective_shape()
    assert len(shape) == 1
    assert isinstance(shape[0], BinaryOperation)
    # Indirect array slice.
    shape = routine.children[1].lhs._get_effective_shape()
    assert len(shape) == 2
    assert isinstance(shape[0], BinaryOperation)
    code = fortran_writer(shape[0])
    # An ArrayType does not store the number of elements, just lower and upper
    # bounds. Therefore, we end up recursively computing the no. of elements.
    # The answer is still "2" though!
    assert code == "((3 - 2) / 1 + 1 - 1) / 1 + 1"
    code = fortran_writer(shape[1])
    assert code == "(5 - 2) / 1 + 1"
    # An indirect array slice can only be 1D.
    with pytest.raises(InternalError) as err:
        _ = routine.children[2].lhs._get_effective_shape()
    assert ("array defining a slice of a dimension of another array must be "
            "1D but 'indices' used to index into 'b' has 2 dimensions" in
            str(err.value))
    # Indirect array access using function call.
    with pytest.raises(NotImplementedError) as err:
        _ = routine.children[3].lhs._get_effective_shape()
    assert "include a function call or expression" in str(err.value)
    # Array access with expression in indices.
    with pytest.raises(NotImplementedError) as err:
        _ = routine.children[4].lhs._get_effective_shape()
    assert "include a function call or expression" in str(err.value)


# get_outer_range_index

def test_get_outer_range_index():
    '''Check that the get_outer_range_index method returns the outermost index
    of the children list that is a range. Use ArrayReference and
    ArrayOfStructuresReference as concrete implementations of ArrayMixins.
    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10, 10, 10]))
    array = ArrayReference.create(symbol, [Range(), Range(), Range()])
    assert array.get_outer_range_index() == 2

    symbol = DataSymbol("my_symbol", DeferredType())
    aos = ArrayOfStructuresReference.create(
        symbol, [Range(), Range(), Range()], ["nx"])
    assert aos.get_outer_range_index() == 3  # +1 for the member child


def test_get_outer_range_index_error():
    '''Check that the get_outer_range_index method raises an IndexError if
    no range exist as child of the given array. Use ArrayReference as concrete
    implementation of ArrayMixin.
    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array = ArrayReference.create(symbol, [Literal("2", INTEGER_TYPE)])
    with pytest.raises(IndexError):
        _ = array.get_outer_range_index()
