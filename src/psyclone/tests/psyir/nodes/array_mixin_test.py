# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Modified: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests of the ArrayMixin PSyIR nodes trait. '''

import pytest
from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    ArrayOfStructuresReference, ArrayReference, BinaryOperation, Range,
    Literal, Routine, StructureReference, Assignment, Reference)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, DeferredType, INTEGER_TYPE,
    REAL_TYPE, StructureType, Symbol)


_ONE = Literal("1", INTEGER_TYPE)
_TWO = Literal("2", INTEGER_TYPE)


# _is_bound_op

def test_is_bound_op():
    '''Check the _is_bound_op utility function works as expected.  Create
    and use an ArrayReference node to test the abstract ArrayMixin
    class.

    '''
    array = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array2 = DataSymbol("my_symbol2", ArrayType(INTEGER_TYPE, [10]))
    scalar = DataSymbol("tmp", INTEGER_TYPE)
    ubound = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, Reference(array), _ONE.copy())
    my_range = Range.create(_ONE.copy(), ubound)
    array_ref = ArrayReference.create(array, [my_range])
    array2_ref = ArrayReference.create(array2, [my_range.copy()])
    # not a binary operation
    oper = None
    assert not array_ref._is_bound_op(
        oper, BinaryOperation.Operator.UBOUND, _ONE)
    # not a match with the binary operator
    oper = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, array_ref, _ONE.copy())
    assert not array_ref._is_bound_op(oper, BinaryOperation.Operator.UBOUND, 0)
    # 1st dimension of the bound is not the same array
    oper = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, array2_ref, _ONE.copy())
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
        BinaryOperation.Operator.UBOUND, array_ref.copy(), _ONE.copy())
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
    ("1", True, False), ("10", False, True)])
def test_is_bound_structure(fortran_reader, access, lower, upper):
    '''Test that _is_bound returns True if the array access within
    a structure is a single access and it matches the upper or
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
    if array_member._is_bound(0, "lower") != lower:
        pytest.xfail(
            "issue #2006: structures not yet supported in bounds tests.")
    if array_member._is_bound(0, "upper") != upper:
        pytest.xfail(
            "issue #2006: structures not yet supported in bounds tests.")


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
        "my_str(1:10) = 'b'\n"
        "end subroutine\n")

    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert not array_ref._is_bound(0, "lower")
    assert not array_ref._is_bound(0, "upper")


def test_is_bound_extent(fortran_reader):
    '''Test the _is_bound method returns False when the array declaration
    is the Extent type (i.e. is not known at compile time).

    '''
    code = (
        "subroutine test(a)\n"
        "real :: a(:)\n"
        "a(1) = 0.0\n"
        "end subroutine\n")
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    array_ref = assigns[0].lhs
    assert array_ref._is_bound(0, "lower") is False
    assert array_ref._is_bound(0, "upper") is False


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
    tested (TODO issue #949).

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


def test_get_bound_expression():
    '''
    Tests for the _get_bound_expression method on an ArrayReference defined
    by an ArrayType.

    '''
    # Symbol is of ArrayType.
    lbound = DataSymbol("jmin", INTEGER_TYPE, is_constant=True,
                        initial_value=Literal("3", INTEGER_TYPE))
    lbnd_ref = Reference(lbound)
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE,
                                               [10, (2, 10), (lbnd_ref, 10)]))
    aref = ArrayReference.create(symbol,
                                 [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    assert aref._get_bound_expression(0, "lower") == _ONE
    assert aref._get_bound_expression(1, "lower") == _TWO
    lb2 = aref._get_bound_expression(2, "lower")
    assert isinstance(lb2, Reference)
    # Returned lower bound should be a *copy* of the original.
    assert lb2 is not lbnd_ref
    assert lb2.symbol is lbound
    with pytest.raises(IndexError):
        aref._get_bound_expression(3, "lower")
    # Symbol is of DeferredType so the result should be an instance of the
    # LBOUND intrinsic.
    dtsym = DataSymbol("oops", DeferredType())
    dtref = ArrayReference.create(dtsym,
                                  [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    lbnd = dtref._get_bound_expression(1, "lower")
    assert isinstance(lbnd, BinaryOperation)
    assert lbnd.operator == BinaryOperation.Operator.LBOUND
    assert lbnd.children[0].symbol is dtsym
    assert lbnd.children[1] == Literal("2", INTEGER_TYPE)

    # Tests for ubound

    # Symbol is of ArrayType.
    ubound = DataSymbol("jmin", INTEGER_TYPE, is_constant=True,
                        initial_value=Literal("10", INTEGER_TYPE))
    ubnd_ref = Reference(ubound)
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE,
                                               [1, (1, 2), (1, ubnd_ref)]))
    aref = ArrayReference.create(symbol,
                                 [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    assert aref._get_bound_expression(0, "upper") == _ONE
    assert aref._get_bound_expression(1, "upper") == _TWO
    ub2 = aref._get_bound_expression(2, "upper")
    assert isinstance(ub2, Reference)
    # Returned lower bound should be a *copy* of the original.
    assert ub2 is not ubnd_ref
    assert ub2 == ubnd_ref

    # Symbol is of DeferredType so the result should be an instance of the
    # UBOUND intrinsic.
    dtsym = DataSymbol("oops", DeferredType())
    dtref = ArrayReference.create(dtsym,
                                  [_ONE.copy(), _ONE.copy(), _ONE.copy()])
    ubnd = dtref._get_bound_expression(1, "upper")
    assert isinstance(ubnd, BinaryOperation)
    assert ubnd.operator == BinaryOperation.Operator.UBOUND
    assert ubnd.children[0].symbol is dtsym
    assert ubnd.children[1] == Literal("2", INTEGER_TYPE)


@pytest.mark.parametrize("extent", [ArrayType.Extent.DEFERRED,
                                    ArrayType.Extent.ATTRIBUTE])
def test_get_bound_expression_unknown_size(extent):
    '''
    Test the _get_bound_expression method when we have the definition of the
    array type but its dimensions are unknown.

    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE,
                                               [extent, extent]))
    aref = ArrayReference.create(symbol, [_ONE.copy(), _ONE.copy()])
    lbnd = aref._get_bound_expression(1, "lower")
    assert isinstance(lbnd, BinaryOperation)
    assert lbnd.operator == BinaryOperation.Operator.LBOUND
    assert lbnd.children[0].symbol is symbol

    ubnd = aref._get_bound_expression(1, "upper")
    assert isinstance(ubnd, BinaryOperation)
    assert ubnd.operator == BinaryOperation.Operator.UBOUND
    assert ubnd.children[0].symbol is symbol


def test_aref_to_aos_bound_expression():
    '''
    Test the _get_bound_expression() method for an ArrayReference to an array
    of structures.

    '''
    sgrid_type = StructureType.create(
        [("ID", INTEGER_TYPE, Symbol.Visibility.PUBLIC)])
    sgrid_type_sym = DataTypeSymbol("subgrid_type", sgrid_type)
    sym = DataSymbol("subgrids", ArrayType(sgrid_type_sym, [(3, 10)]))
    lbound = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                    Reference(sym), _ONE.copy())
    ubound = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                    Reference(sym), _ONE.copy())
    array = ArrayReference.create(sym, [Range.create(lbound, ubound)])
    lbnd = array._get_bound_expression(0, "lower")
    assert lbnd.value == "3"
    ubnd = array._get_bound_expression(0, "upper")
    assert ubnd.value == "10"


def test_member_get_bound_expression(fortran_writer):
    '''
    Tests for the _get_bound_expression() method when used with a
    sub-class of Member.

    '''
    # First, test when we don't have type information.
    grid_type = DataTypeSymbol("grid_type", DeferredType())
    sym = DataSymbol("grid_var", grid_type)
    # Use upper case to test the upper case is converted to lower case
    # correctly.
    ref = StructureReference.create(sym, [("DATA", [_ONE.copy()])])
    # Test invalid argument
    with pytest.raises(InternalError) as excinfo:
        ref.member._get_bound_expression(0, "notvalid")
    assert ("'bound' argument must be 'lower' or 'upper. "
            "Found 'notvalid'" in str(excinfo.value))
    lbnd = ref.member._get_bound_expression(0, "lower")
    assert isinstance(lbnd, BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(grid_var%data, 1)"
    usym = DataSymbol("uvar", DeferredType())
    ref = ArrayOfStructuresReference.create(
        usym, [_ONE.copy()],
        [("map", [_ONE.copy(), _TWO.copy()]),
         ("data", [_ONE.copy()])])
    lbnd = ref.member.member._get_bound_expression(0, "lower")
    assert isinstance(lbnd, BinaryOperation)
    out = fortran_writer(lbnd).lower()
    assert out == "lbound(uvar(1)%map(1,2)%data, 1)"

    ubnd = ref.member._get_bound_expression(0, "upper")
    assert isinstance(ubnd, BinaryOperation)
    out = fortran_writer(ubnd).lower()
    assert out == "ubound(uvar(1)%map, 1)"
    # Second, test when we do have type information.
    a2d = ArrayType(REAL_TYPE, [2, (2, 8)])
    # Structure that contains "map" which is a 2D array.
    stypedef = StructureType.create(
        [("map", a2d, Symbol.Visibility.PUBLIC)])
    stypedefsym = DataTypeSymbol("map_type", stypedef)
    # Structure containing a structure of stypedef and an array of such
    # structures.
    stypedef2 = StructureType.create(
        [("grid", stypedef, Symbol.Visibility.PUBLIC),
         ("subgrids", ArrayType(stypedefsym, [3, (2, 6)]),
          Symbol.Visibility.PUBLIC)])
    ssym = DataSymbol("var", stypedef2)
    sref = StructureReference.create(ssym,
                                     ["grid",
                                      ("map", [_TWO.copy(), _TWO.copy()])])
    assert sref.member.member._get_bound_expression(0, "lower") == _ONE
    assert sref.member.member._get_bound_expression(1, "lower") == _TWO
    assert sref.member.member._get_bound_expression(0, "upper").value == "2"
    assert sref.member.member._get_bound_expression(1, "upper").value == "8"
    sref2 = StructureReference.create(
        ssym, [("subgrids", [_TWO.copy(), _TWO.copy()]),
               ("map", [_TWO.copy(), _TWO.copy()])])
    assert sref2.member._get_bound_expression(1, "lower") == _TWO
    assert sref2.member.member._get_bound_expression(1, "lower") == _TWO

    assert sref2.member._get_bound_expression(1, "upper").value == "6"
    assert sref2.member.member._get_bound_expression(1, "upper").value == "8"

    # Check that get_lbound_expression gives the same result
    assert (sref2.member.member._get_bound_expression(1, "lower") ==
            sref2.member.member.get_lbound_expression(1))
    # Check that get_ubound_expression gives the same result
    assert (sref2.member.member._get_bound_expression(1, "upper") ==
            sref2.member.member.get_ubound_expression(1))

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
    no Range exists as child of the given array. Use ArrayReference as
    concrete implementation of ArrayMixin.

    '''
    symbol = DataSymbol("my_symbol", ArrayType(INTEGER_TYPE, [10]))
    array = ArrayReference.create(symbol, [Literal("2", INTEGER_TYPE)])
    with pytest.raises(IndexError):
        _ = array.get_outer_range_index()
