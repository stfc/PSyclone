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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

"""
This module contains pytest tests for the IntrinsicCall node.

TODO #2341 - tests need to be added for all of the supported intrinsics.

"""

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    ArrayReference,
    Literal,
    Reference,
    Schedule,
    Assignment,
    Call
)
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall,
    IAttr,
    _get_named_argument_type,
    _type_of_named_arg_with_optional_kind_and_dim,
    _type_with_specified_precision_and_optional_dim,
    _type_of_scalar_with_optional_kind,
    _get_intrinsic_of_argname_kind_with_optional_dim,
    _get_intrinsic_with_named_arg_precision,
    _findloc_return_type,
    _int_return_type,
    _iparity_return_type,
    _get_bound_function_return_type,
    _matmul_return_type,
    _maxval_return_type,
)
from psyclone.psyir.symbols import (
    ArrayType,
    DataSymbol,
    INTEGER_TYPE,
    IntrinsicSymbol,
    REAL_TYPE,
    BOOLEAN_TYPE,
    CHARACTER_TYPE,
    ScalarType,
    UnresolvedType,
    UnsupportedFortranType,
    NoType
)


def test_intrinsic_enum():
    """Basic test for the IntrinsicCall.Intrinsic enum."""
    assert isinstance(IntrinsicCall.Intrinsic.MINVAL, IAttr)
    assert hash(IntrinsicCall.Intrinsic.MINVAL) == hash("MINVAL")


def test_intrinsiccall_constructor():
    """Tests that the class' constructor and its parent are called
    correctly.

    """
    # Wrong type of routine argument.
    with pytest.raises(TypeError) as err:
        _ = IntrinsicCall(None)
    assert (
        "IntrinsicCall 'intrinsic' argument should be an instance of "
        "IntrinsicCall.Intrinsic, but found 'NoneType'." in str(err.value)
    )
    # Check that supplied intrinsic and optional parent node is stored
    # correctly.
    sched = Schedule()
    call = IntrinsicCall(IntrinsicCall.Intrinsic.MINVAL, parent=sched)
    assert call.routine.symbol.intrinsic is IntrinsicCall.Intrinsic.MINVAL
    assert isinstance(call.routine.symbol, IntrinsicSymbol)
    assert call.routine.name == "MINVAL"
    assert call.parent is sched


def test_intrinsiccall_intrinsic():
    """Tests the intrinsic property returns the type of intrinsics from
    the intrinsic property.

    """
    call = IntrinsicCall(IntrinsicCall.Intrinsic.MAXVAL)
    assert call.intrinsic is IntrinsicCall.Intrinsic.MAXVAL


def test_intrinsiccall_datatype(fortran_reader):
    """Test the datatype property returns the correct return types.
    """
    call = IntrinsicCall(IntrinsicCall.Intrinsic.NULLIFY)
    assert isinstance(call.datatype, NoType)

    code = """subroutine test
    integer :: i
    i = BIT_SIZE(i)
    i = ABS(i)
    end subroutine test
    """
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(IntrinsicCall)[0]
    assert call.datatype == INTEGER_TYPE

    call = psyir.walk(IntrinsicCall)[1]
    assert call.datatype == INTEGER_TYPE

    code = """subroutine test
    use my_mod

    i = ABS(i)
    end subroutine test
    """
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(IntrinsicCall)[0]
    assert isinstance(call.datatype, UnresolvedType)

    # ValueError test.
    call = IntrinsicCall(IntrinsicCall.Intrinsic.ABS)
    with pytest.raises(InternalError) as err:
        _ = call.datatype
    assert ("Failed to compute the datatype of a 'ABS' intrinsic. This is "
            "likely due to not fully initialising the intrinsic correctly."
            in str(err.value))


def test_intrinsiccall_is_elemental():
    """Tests the is_elemental() method works as expected. There are
    currently no elemental intrinsics so we can only test for
    False.

    """
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_elemental is False


def test_intrinsiccall_is_pure():
    """Tests that the is_pure() method works as expected."""
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_pure is True
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATE)
    assert intrinsic.is_pure is False


def test_intrinsiccall_is_inquiry():
    """Test that the is_inquiry() method works as expected."""
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_inquiry is False
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATED)
    assert intrinsic.is_inquiry is True


@pytest.mark.parametrize(
    "intrinsic, result",
    [
        (IntrinsicCall.Intrinsic.ABS, True),
        (IntrinsicCall.Intrinsic.MIN, True),
        (IntrinsicCall.Intrinsic.MAX, True),
        (IntrinsicCall.Intrinsic.MAXVAL, True),
        (IntrinsicCall.Intrinsic.ALLOCATE, False),
        (IntrinsicCall.Intrinsic.MATMUL, False),
        (IntrinsicCall.Intrinsic.ACOS, True),
        (IntrinsicCall.Intrinsic.AINT, True),
        (IntrinsicCall.Intrinsic.ANINT, True),
        (IntrinsicCall.Intrinsic.ASIN, True),
        (IntrinsicCall.Intrinsic.ATAN, True),
        (IntrinsicCall.Intrinsic.ATAN2, True),
        (IntrinsicCall.Intrinsic.COS, True),
        (IntrinsicCall.Intrinsic.COSH, True),
        (IntrinsicCall.Intrinsic.DBLE, True),
        (IntrinsicCall.Intrinsic.DPROD, True),
        (IntrinsicCall.Intrinsic.EXP, True),
        (IntrinsicCall.Intrinsic.IAND, True),
        (IntrinsicCall.Intrinsic.IEOR, True),
        (IntrinsicCall.Intrinsic.INT, True),
        (IntrinsicCall.Intrinsic.IOR, True),
        (IntrinsicCall.Intrinsic.LOG, True),
        (IntrinsicCall.Intrinsic.LOG10, True),
        (IntrinsicCall.Intrinsic.MOD, True),
        (IntrinsicCall.Intrinsic.NINT, True),
        (IntrinsicCall.Intrinsic.NOT, True),
        (IntrinsicCall.Intrinsic.REAL, True),
        (IntrinsicCall.Intrinsic.SIGN, True),
        (IntrinsicCall.Intrinsic.SIN, True),
        (IntrinsicCall.Intrinsic.SINH, True),
        (IntrinsicCall.Intrinsic.SQRT, True),
        (IntrinsicCall.Intrinsic.TAN, True),
        (IntrinsicCall.Intrinsic.TANH, True),
        (IntrinsicCall.Intrinsic.PRODUCT, True),
        (IntrinsicCall.Intrinsic.SUM, True),
        (IntrinsicCall.Intrinsic.LBOUND, True),
        (IntrinsicCall.Intrinsic.UBOUND, True),
    ],
)
def test_intrinsiccall_is_available_on_device(intrinsic, result):
    """Tests that the is_available_on_device() method works as expected."""
    intrinsic_call = IntrinsicCall(intrinsic)
    # For now default and nvfortran-all are the same
    assert intrinsic_call.is_available_on_device() is result
    assert intrinsic_call.is_available_on_device("nvfortran-all") is result


def test_intrinsiccall_reductions_is_available_on_device():
    """Tests that the is_available_on_device() refuses reduction intrinsics
    with optional arguments"""
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic_call.addchild(Reference(DataSymbol("result", REAL_TYPE)))
    # This is avaliabe on the device
    assert intrinsic_call.is_available_on_device()
    # But not when it has more arguments as it sometimes fails for complex
    # reductions with arguments
    intrinsic_call.addchild(Literal("1", INTEGER_TYPE))
    assert not intrinsic_call.is_available_on_device()


def test_intrinsiccall_is_available_on_device_with_device_string():
    """Tests that the is_available_on_device() method with a device_string
    argument provides different results with the 'nvfortran-uniform'
    """
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.LOG10)
    assert not intrinsic_call.is_available_on_device("nvfortran-uniform")
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.REAL)
    assert not intrinsic_call.is_available_on_device("nvfortran-uniform")

    with pytest.raises(ValueError) as err:
        assert not intrinsic_call.is_available_on_device("invalid")
    assert (
        "Unsupported device_string value 'invalid', the supported values"
        " are '' (default), 'nvfortran-all', 'nvfortran-uniform'"
        in str(err.value)
    )


def test_intrinsiccall_alloc_create():
    """Tests the create() method supports various forms of 'allocate'."""
    sym = DataSymbol(
        "my_array", ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    )
    bsym = DataSymbol(
        "my_array2", ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    )
    isym = DataSymbol("ierr", INTEGER_TYPE)
    csym = DataSymbol("msg", CHARACTER_TYPE)
    # Straightforward allocation of an array.
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])],
    )
    assert isinstance(alloc, IntrinsicCall)
    assert alloc.intrinsic is IntrinsicCall.Intrinsic.ALLOCATE
    assert isinstance(alloc.routine.symbol, IntrinsicSymbol)
    assert alloc.routine.name == "ALLOCATE"
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(sym), ("Mold", Reference(bsym))],
    )
    assert isinstance(alloc, IntrinsicCall)
    assert alloc.argument_names == [None, "mold"]
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(sym), ("Source", Reference(bsym)),
         ("stat", Reference(isym)), ("errmsg", Reference(csym))])
    assert alloc.argument_names == [None, "source", "stat", "errmsg"]


def test_intrinsiccall_dealloc_create():
    """Tests for the creation of a 'deallocate' call."""
    sym = DataSymbol(
        "my_array", ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    )
    ierr = DataSymbol("ierr", INTEGER_TYPE)
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym)]
    )
    assert isinstance(dealloc, IntrinsicCall)
    assert dealloc.intrinsic is IntrinsicCall.Intrinsic.DEALLOCATE
    assert isinstance(dealloc.routine.symbol, IntrinsicSymbol)
    assert dealloc.routine.name == "DEALLOCATE"
    assert dealloc.arguments[0].symbol is sym
    # With 'stat' optional argument.
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym),
                                             ("Stat", Reference(ierr))])
    assert dealloc.argument_names == [None, "stat"]


def test_intrinsiccall_random_create():
    """Tests for the creation of a 'random' call."""
    sym = DataSymbol(
        "my_array", ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED])
    )
    rand = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.RANDOM_NUMBER, [Reference(sym)]
    )
    assert isinstance(rand, IntrinsicCall)
    assert rand.intrinsic is IntrinsicCall.Intrinsic.RANDOM_NUMBER
    assert isinstance(rand.routine.symbol, IntrinsicSymbol)
    assert rand.routine.name == "RANDOM_NUMBER"
    assert rand.arguments[0].symbol is sym


@pytest.mark.parametrize(
    "intrinsic_call",
    [
        IntrinsicCall.Intrinsic.MINVAL,
        IntrinsicCall.Intrinsic.MAXVAL,
        IntrinsicCall.Intrinsic.SUM,
    ],
)
def test_intrinsiccall_minmaxsum_create(intrinsic_call):
    """Tests for the creation of the different argument options for
    'minval', 'maxval' and 'sum' IntrinsicCalls.

    """
    array = DataSymbol(
        "my_array", ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED])
    )
    dim = DataSymbol("dim", INTEGER_TYPE)
    mask = DataSymbol("mask", BOOLEAN_TYPE)

    # array only
    intrinsic = IntrinsicCall.create(intrinsic_call, [Reference(array)])
    assert isinstance(intrinsic, IntrinsicCall)
    assert intrinsic.intrinsic is intrinsic_call
    assert isinstance(intrinsic.routine.symbol, IntrinsicSymbol)
    intrinsic_name = intrinsic_call.name
    assert intrinsic.routine.name == intrinsic_name
    assert intrinsic.arguments[0].symbol is array
    # array and optional dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("dim", Reference(dim))])
    assert intrinsic.argument_names == ["array", "dim"]
    # array and optional mask
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("mask", Reference(mask))])
    assert intrinsic.argument_names == ["array", "mask"]
    # array and optional dim then optional mask
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("dim", Reference(dim)),
                         ("mask", Reference(mask))])
    assert intrinsic.argument_names == ["array", "dim", "mask"]
    # array and optional mask then optional dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("mask", Reference(mask)),
                         ("dim", Reference(dim))])
    assert intrinsic.argument_names == ["array", "mask", "dim"]
    assert intrinsic.children[2].symbol.name == "mask"
    assert intrinsic.children[3].symbol.name == "dim"
    # array and optional literal mask and optional literal dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call,
        [
            Reference(array),
            ("mask", Literal("1", INTEGER_TYPE)),
            ("dim", Literal("false", BOOLEAN_TYPE))])
    assert intrinsic.argument_names == ["array", "mask", "dim"]
    assert intrinsic.children[2].value == "1"
    assert intrinsic.children[3].value == "false"


@pytest.mark.parametrize(
    "intrinsic_call",
    [IntrinsicCall.Intrinsic.TINY, IntrinsicCall.Intrinsic.HUGE],
)
@pytest.mark.parametrize("form", ["array", "literal"])
def test_intrinsiccall_tinyhuge_create(intrinsic_call, form):
    """Tests for the creation of the different argument options for
    'tiny' and 'huge' IntrinsicCalls.

    """
    if form == "array":
        array = DataSymbol(
            "my_array", ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED])
        )
        arg = Reference(array)
    else:  # "literal"
        arg = Literal("1.0", REAL_TYPE)
    intrinsic = IntrinsicCall.create(intrinsic_call, [arg])
    assert isinstance(intrinsic, IntrinsicCall)
    assert intrinsic.intrinsic is intrinsic_call
    assert isinstance(intrinsic.routine.symbol, IntrinsicSymbol)
    intrinsic_name = intrinsic_call.name
    assert intrinsic.routine.name == intrinsic_name
    if form == "array":
        assert intrinsic.arguments[0].symbol is array
    else:  # "literal"
        assert intrinsic.arguments[0] is arg


def test_intrinsiccall_create_errors():
    """Checks for the validation/type checking in the create() method."""
    sym = DataSymbol(
        "my_array", ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    )
    aref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create("ALLOCATE", [Reference(sym)])
    assert (
        "'intrinsic' argument should be an instance of "
        "IntrinsicCall.Intrinsic, but found 'str'" in str(err.value)
    )
    # Supplied arguments must be a list.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, aref)
    assert (
        "IntrinsicCall.create() 'arguments' argument should be an Iterable"
        " but found 'ArrayReference'" in str(err.value)
    )
    # An allocate must have one or more References as argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [])
    assert ("Found too few arguments when computing argument names for "
            "the 'ALLOCATE' IntrinsicCall. Requires at least 1 arguments "
            "but found 0." in str(err.value))
    # The random intrinsic only accepts one argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                             [aref, aref.copy()])
    assert ("Found too many arguments when computing argument names for the "
            "'RANDOM_NUMBER' IntrinsicCall. Requires at most 1 arguments but "
            "found 2." in str(err.value))
    # Wrong type for a positional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [sym])
    assert (
        "The 'ALLOCATE' intrinsic requires that positional arguments be "
        "of type " in str(err.value)
    )
    assert "but got a 'DataSymbol'" in str(err.value)
    # Wrong type for a named position argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.TAN,
                             [("x", sym)])
    assert ("The argument 'x' to intrinsic 'TAN' must be of type 'DataNode' "
            "but got 'DataSymbol'" in str(err.value))
    # Positional argument after named argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(
            IntrinsicCall.Intrinsic.DEALLOCATE,
            [Reference(sym), ("stat", aref), aref],
        )
    assert (
        "Found a positional argument *after* a named argument ('stat'). "
        "This is invalid." in str(err.value)
    )

    # Test invalid optional argument provision
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                             [aref.detach(), ("willow", Reference(sym))])
    assert ("Found invalid argument name 'willow' when computing argument "
            "names for the 'RANDOM_NUMBER' IntrinsicCall. Allowed argument "
            "names are '['harvest']'." in str(err.value))

    # Wrong type for the name of an optional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(
            IntrinsicCall.Intrinsic.ALLOCATE, [aref, (sym, sym)]
        )
    assert (
        "Optional arguments to an IntrinsicCall must be specified by a "
        "(str, Reference) tuple but got a DataSymbol instead of a str"
        in str(err.value)
    )
    # Wrong type for an optional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(
            IntrinsicCall.Intrinsic.ALLOCATE, [aref, ("stat", sym)]
        )
    assert (
        "The optional argument 'stat' to intrinsic 'ALLOCATE' must be "
        "of type 'Reference' but got 'DataSymbol'" in str(err.value)
    )


def test_create_positional_arguments_with_names():
    """Test the create method when given named positional arguments."""
    sym = DataSymbol(
        "my_array", ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])
    )
    aref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])
    bref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])

    # All of these are valid
    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DOT_PRODUCT, [aref.copy(), bref.copy()]
    )
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == ["vector_a", "vector_b"]

    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DOT_PRODUCT,
        [aref.copy(), ("vector_b", bref.copy())],
    )
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == ["vector_a", "vector_b"]

    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DOT_PRODUCT,
        [("vector_a", aref.copy()), ("vector_b", bref.copy())],
    )
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == ["vector_a", "vector_b"]

    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DOT_PRODUCT,
        [("vector_b", bref.copy()), ("vector_a", aref.copy())],
    )
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == bref
    assert intr.arguments[1] == aref
    assert intr.argument_names == ["vector_b", "vector_a"]


@pytest.mark.parametrize("operator", ["lbound", "ubound", "size"])
def test_reference_accesses_bounds(operator, fortran_reader):
    """Test that the reference_accesses method behaves as expected when
    the reference is the first argument to either the lbound or ubound
    intrinsic as that is simply looking up the array bounds (therefore
    the access is an enquiry) and when the reference is the
    second argument of either the lbound or ubound intrinsic (in which
    case the access should be a read).

    """
    code = f"""module test
        contains
        subroutine tmp()
          real, dimension(:,:), allocatable:: a, b
          integer :: n
          n = {operator}(a, b(1,1))
        end subroutine tmp
        end module test"""
    psyir = fortran_reader.psyir_from_source(code)
    schedule = psyir.walk(Assignment)[0]

    # The access to 'a' should be reported as 'INQUIRY' as its
    # actual data is not accessed.
    vam = schedule.reference_accesses()
    assert str(vam) == "a: INQUIRY, b: READ, n: WRITE"


def test_enumerator_name_matches_name_field():
    """
    Test that the name given to every IntrinsicCall matches the
    corresponding name field in the IAttr namedtuple.
    """
    for intrinsic_entry in IntrinsicCall.Intrinsic:
        assert intrinsic_entry._name_ == intrinsic_entry.name


def test_allocate_intrinsic(fortran_reader, fortran_writer):
    """
    Test the ALLOCATE 'intrinsic'.
    """
    code = """
program test_prog
  implicit none
  integer :: ierr
  character(len=128) :: msg
  real, allocatable, dimension(:) :: arr1, arr2
  allocate(arr1(10), stat=ierr)
  allocate(arr2, mold=arr1)
  allocate(arr2, source=arr1, errmsg=msg)
end program test_prog
"""
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 3
    result = fortran_writer(psyir).lower()
    assert "allocate(arr1(1:10), stat=ierr)" in result
    assert "allocate(arr2, mold=arr1)" in result
    assert "allocate(arr2, source=arr1, errmsg=msg)" in result


def test_deallocate_intrinsic(fortran_reader, fortran_writer):
    """
    Test the DEALLOCATE 'intrinsic'.
    """
    code = """
program test_prog
  implicit none
  integer :: ierr
  real, allocatable, dimension(:) :: arr1
  deallocate(arr1)
  deallocate(arr1, stat=ierr)
end program test_prog
"""
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 2
    result = fortran_writer(psyir).lower()
    assert "deallocate(arr1)" in result
    assert "deallocate(arr1, stat=ierr)" in result


def test_index_intrinsic(fortran_reader, fortran_writer):
    """
    Test the INDEX intrinsic.
    """
    code = """
program test_prog
  implicit none
  character(len=10) :: clname
  integer :: ind1, ind2

  ind1 = INDEX( clname, '_', back = .TRUE. ) + 1
  ind2 = INDEX( clname, '.') - 1
  ind2 = INDEX( clname, '.', kind=4) - 1

end program test_prog
"""
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 3
    result = fortran_writer(psyir).lower()
    assert ("ind1 = index(clname, '_', back=.true.) + 1" in
            result)
    assert "ind2 = index(clname, '.') - 1" in result
    assert "ind2 = index(clname, '.', kind=4) - 1" in result


def test_verify_intrinsic(fortran_reader, fortran_writer):
    """
    Test the VERIFY intrinsic.
    """
    code = """
program test_prog
  implicit none
  character(len=10) :: clname
  integer :: ind1, ind2, idom, jpdom_local

  ind1 = 2
  ind2 = 5
  IF( VERIFY( clname(ind1:ind2), '0123456789' ) == 0 ) idom = jpdom_local
  IF( VERIFY( clname(ind1:ind2), '0123456789', back=.true. ) == 0 ) &
idom = jpdom_local
  IF( VERIFY( clname(ind1:ind2), '0123456789', kind=kind(1) ) == 0 ) &
idom = jpdom_local
  IF( VERIFY( clname(ind1:ind2), '0123456789', kind=kind(1), back=.true. ) &
== 0 ) idom = jpdom_local

end program test_prog
"""
    psyir = fortran_reader.psyir_from_source(code)
    # Should have 4 VERIFY and 2 KIND
    assert len(psyir.walk(IntrinsicCall)) == 6
    result = fortran_writer(psyir).lower()
    assert ("if (verify(clname(ind1:ind2), '0123456789') == 0) "
            "then" in result)
    assert ("if (verify(clname(ind1:ind2), '0123456789', "
            "back=.true.) == 0) then" in result)
    assert ("if (verify(clname(ind1:ind2), '0123456789', "
            "kind=kind(1)) == 0) then" in result)
    assert ("if (verify(clname(ind1:ind2), '0123456789', "
            "kind=kind(1), back=.true.) == 0) then" in result)


def test_intrinsic_compute_argument_names_value_errors():
    '''
    Test the compute_argument_names function of the IntrinsicCall class raises
    ValueErrors with bad inputs.
    '''

    # Test argument name computation fails if we have an incorrect named
    # argument.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    # Set up the argument_names array
    _ = intrinsic.argument_names
    intrinsic._argument_names[0] = (intrinsic._argument_names[0][0], "wrong")
    with pytest.raises(ValueError) as err:
        intrinsic.compute_argument_names()
    assert ("Found invalid argument name 'wrong' when computing argument "
            "names for the 'SUM' IntrinsicCall. Allowed argument names are "
            "'['array', 'dim', 'mask']'." in str(err.value))

    # Test argument name computation fails if we don't have enough arguments.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    with pytest.raises(ValueError) as err:
        intrinsic.compute_argument_names()
    assert ("Found too few arguments when computing argument names for the "
            "'SUM' IntrinsicCall. Requires at least 1 arguments but found 0."
            in str(err.value))

    # Test argument name computation fails if we have too many arguments
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    with pytest.raises(ValueError) as err:
        intrinsic.compute_argument_names()
    assert ("Found too many arguments when computing argument names for the "
            "'SUM' IntrinsicCall. Requires at most 3 arguments but found 4."
            in str(err.value))


def test_intrinsic_compute_argument_names_not_implemented_errors():
    '''
    Test the compute_argument_names function of the IntrinsicCall class raises
    NotImplementedErrors for Intrinsic structures PSyclone can't handle.
    '''
    # Test computing argument names doesn't work when we have 2 arguments for
    # SUM with no naming, as it can't determine between the SUM variants.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    with pytest.raises(NotImplementedError) as err:
        intrinsic.compute_argument_names()
    assert ("Cannot add argument names to 'SUM' IntrinsicCall as PSyclone "
            "can't determine which argument set it should use. This can be "
            "resolved by using named arguments in the Fortran source."
            in str(err.value))

    # The only case I can see that can hit line 2473
    # (i not in available args: continue) is an invalid BESSEL_JN Intrinsic
    # This is future-proofing for context-sensitive argument handling.
    # TODO #2302
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.BESSEL_JN)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    # Set up the argument_names array and set the argument names
    _ = intrinsic.argument_names
    intrinsic._argument_names[0] = (intrinsic._argument_names[0][0], "n1")
    intrinsic._argument_names[1] = (intrinsic._argument_names[1][0], "n2")
    with pytest.raises(NotImplementedError) as err:
        intrinsic.compute_argument_names()
    assert ("Cannot add argument names to 'BESSEL_JN' IntrinsicCall as "
            "PSyclone can't determine which argument set it should use. "
            "This can be resolved by using named arguments in the Fortran "
            "source" in str(err.value))

    # Test we get the expected error when non-optional argument names are
    # passed to an intrinsic where PSyclone can't handle the required argument
    # names.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATED)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    _ = intrinsic.argument_names
    intrinsic._argument_names[0] = (intrinsic._argument_names[0][0], "array")
    with pytest.raises(NotImplementedError) as err:
        intrinsic.compute_argument_names()
    assert ("Cannot add argument names to 'ALLOCATED' as non-optional "
            "argument name 'array' found but the Intrinsic has "
            "context-sensitive argument names which is unsupported by "
            "PSyclone." in str(err.value))


def test_compute_argument_names():
    '''
    Test that the compute_argument_names function works as expected for
    cases that can have argument names computed.
    '''
    # Test argument name computation works if we have 1 argument for SUM.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names[0] == "array"

    # Test argument name compuutation works when we give a name to the
    # 2nd argument.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    # Set up the argument_names array and set the second ones name to be mask
    _ = intrinsic.argument_names
    intrinsic._argument_names[1] = (intrinsic._argument_names[1][0], "mask")
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == ["array", "mask"]

    # Test that the correct argument name computation is performed when we
    # have a named argument only in one of the lists.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    # Set up the argument_names array and set the second ones name to be dim
    _ = intrinsic.argument_names
    intrinsic._argument_names[1] = (intrinsic._argument_names[1][0], "dim")
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names[0] == "array"
    assert intrinsic.argument_names[1] == "dim"

    # Test that argument name computation works when optional arguments appear
    # first when all arguments are named.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    a_arg = Reference(DataSymbol("a", INTEGER_TYPE))
    b_arg = Reference(DataSymbol("b", INTEGER_TYPE))
    intrinsic.addchild(a_arg)
    intrinsic.addchild(b_arg)
    # Set up the argument_names array and set the first ones name to be mask
    # (optional) and the second to be array.
    _ = intrinsic.argument_names
    intrinsic._argument_names[0] = (intrinsic._argument_names[0][0], "mask")
    intrinsic._argument_names[1] = (intrinsic._argument_names[1][0], "array")
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == ["mask", "array"]
    assert intrinsic.arguments[0] is a_arg
    assert intrinsic.arguments[1] is b_arg

    # Check we can compute argument names for an intrinsic with only one
    # argument set.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SIN)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == ["x"]

    # Check that argument name computation succeeds for an intrinsic with no
    # required arguments
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SYSTEM_CLOCK)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("c", INTEGER_TYPE)))
    intrinsic.argument_names
    intrinsic._argument_names[0] = (intrinsic._argument_names[0][0],
                                    "count_rate")
    intrinsic._argument_names[1] = (intrinsic._argument_names[1][0],
                                    "count_max")
    intrinsic._argument_names[2] = (intrinsic._argument_names[2][0], "count")
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == ["count_rate", "count_max", "count"]
    assert intrinsic.children[1].symbol.name == "a"
    assert intrinsic.children[2].symbol.name == "b"
    assert intrinsic.children[3].symbol.name == "c"

    # Test canonicliation for intrinsic when PSyclone can't
    # compute argument names of non-optional arguments.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATE)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("c", INTEGER_TYPE)))
    intrinsic.argument_names
    intrinsic._argument_names[2] = (intrinsic._argument_names[2][0], "mold")
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == [None, None, "mold"]

    # Check that we canoncalise when we have unnamed optional arguments.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("b", INTEGER_TYPE)))
    intrinsic.addchild(Reference(DataSymbol("c", INTEGER_TYPE)))
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == ["array", "dim", "mask"]

    # Check that we don't fail when the required argument name is None
    # and no argument name can be generated by PSyclone.
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATED)
    intrinsic.addchild(Reference(DataSymbol("a", INTEGER_TYPE)))
    intrinsic.compute_argument_names()
    assert intrinsic.argument_names == [None]


def test_get_all_accessed_symbols(fortran_reader):
    ''' Test the get_all_accessed_symbols method of the IntrinsicCall class.'''

    code = '''subroutine test_sub()
    use other

    a = SIN(COS(RESHAPE(b, SHAPE=3)))
    end subroutine'''

    assign = fortran_reader.psyir_from_source(code).walk(Assignment)[0]
    symbol_names = [s.name for s in assign.get_all_accessed_symbols()]
    assert "a" in symbol_names
    # Intrinsic names and argument names are not accessed symbols. (Intrinsic
    # names could be considered, as they are IntrinsicSymbols, but currently
    # this are not in symbol tables, so it is easier to not consider them)
    assert "SIN" not in symbol_names
    assert "COS" not in symbol_names
    assert "RESHAPE" not in symbol_names
    assert "SHAPE" not in symbol_names


def test_get_named_argument_type(fortran_reader):
    """Test the _get_named_argument_type helper function."""
    code = """subroutine x
    integer :: a, b
    a = 1
    b = ABS(a)
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    abs_call = psyir.walk(IntrinsicCall)[0]
    dtype = _get_named_argument_type(abs_call, "a")
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision == ScalarType.Precision.UNDEFINED


def test_type_of_named_arg_with_optional_kind_and_dim(
        fortran_reader
):
    """Test the _type_of_named_arg_with_optional_kind_and_dim
    helper function."""
    code = """subroutine x
    logical, dimension(100,100) :: a
    logical, dimension(100) :: b
    logical :: c
    c = MAXLOC(a)
    b = MAXLOC(a, dim=1)
    c = MAXLOC(b, dim=1, kind=8)
    end subroutine x
    """
    psyir = fortran_reader.psyir_from_source(code)
    all_calls = psyir.walk(IntrinsicCall)
    dtype = _type_of_named_arg_with_optional_kind_and_dim(
            all_calls[0], "array"
    )
    assert dtype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert dtype.precision == ScalarType.Precision.UNDEFINED
    dtype = _type_of_named_arg_with_optional_kind_and_dim(
            all_calls[1], "array"
    )
    assert isinstance(dtype, ArrayType)
    assert len(dtype.shape) == 1
    assert dtype.shape[0] == ArrayType.Extent.DEFERRED
    assert dtype.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert dtype.datatype.precision == ScalarType.Precision.UNDEFINED
    dtype = _type_of_named_arg_with_optional_kind_and_dim(
            all_calls[2], "array"
    )
    assert dtype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert dtype.precision.value == "8"


def test_type_with_specified_precision_and_optional_dim(fortran_reader):
    """Test the _type_with_specified_precision_and_optional_dim
    helper function."""
    code = """subroutine test
    integer, dimension(100, 100) :: x
    integer :: y
    y = PRODUCT(x)
    y = PRODUCT(x, dim=2)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsics = psyir.walk(IntrinsicCall)

    dtype = _type_with_specified_precision_and_optional_dim(
        intrinsics[0], "array", ScalarType.Intrinsic.INTEGER,
    )
    assert isinstance(dtype, ScalarType)
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision == ScalarType.Precision.UNDEFINED

    dtype = _type_with_specified_precision_and_optional_dim(
        intrinsics[1], "array", ScalarType.Intrinsic.INTEGER,
    )
    assert isinstance(dtype, ArrayType)
    assert len(dtype.shape) == 1
    assert dtype.shape[0] == ArrayType.Extent.DEFERRED
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision == ScalarType.Precision.UNDEFINED


def test_get_intrinsic_with_named_arg_precision(fortran_reader):
    """Test the _get_intrinsic_with_named_arg_precision helper function."""
    code = """subroutine y
    real*8 :: x
    x = BESSEL_J0(x)
    end subroutine y"""
    psyir = fortran_reader.psyir_from_source(code)
    bessel_call = psyir.walk(Call)[0]
    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.BESSEL_J0, [bessel_call.arguments[0].copy()]
    )
    dtype = _get_intrinsic_with_named_arg_precision(
            intr, ScalarType.Intrinsic.REAL, "x"
    )
    assert dtype.intrinsic == ScalarType.Intrinsic.REAL
    assert dtype.precision == 8


def test_type_of_scalar_with_optional_kind(fortran_reader):
    """Test the _type_of_scalar_with_optional_kind function."""
    code = """subroutine y
    real :: i
    integer :: j
    j = CEILING(i)
    j = CEILING(i, kind=8)
    end subroutine y
    """
    psyir = fortran_reader.psyir_from_source(code)
    intrs = psyir.walk(IntrinsicCall)

    dtype = _type_of_scalar_with_optional_kind(
            intrs[0], ScalarType.Intrinsic.INTEGER, "kind"
    )
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision == ScalarType.Precision.UNDEFINED
    dtype = _type_of_scalar_with_optional_kind(
            intrs[1], ScalarType.Intrinsic.INTEGER, "kind"
    )
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision.value == "8"


def test_get_intrinsic_of_argname_kind_with_optional_dim(fortran_reader):
    """Test the _get_intrinsic_of_argname_kind_with_optional_dim function."""
    code = """
    subroutine y
    logical, dimension(100,100) :: a
    integer :: b
    integer*8, dimension(100) :: c

    b = COUNT(a)
    c = COUNT(a, dim=1, kind=8)
    b = COUNT(c, dim=1)
    end subroutine y"""
    psyir = fortran_reader.psyir_from_source(code)
    intrs = psyir.walk(IntrinsicCall)

    res = _get_intrinsic_of_argname_kind_with_optional_dim(
            intrs[0], ScalarType.Intrinsic.INTEGER,
            "mask", "kind")
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == ScalarType.Precision.UNDEFINED

    res = _get_intrinsic_of_argname_kind_with_optional_dim(
            intrs[1], ScalarType.Intrinsic.INTEGER,
            "mask", "kind")
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision.value == "8"
    assert len(res.shape) == 1
    assert res.shape[0] == ArrayType.Extent.DEFERRED

    res = _get_intrinsic_of_argname_kind_with_optional_dim(
            intrs[2], ScalarType.Intrinsic.INTEGER,
            "mask", "kind")
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == ScalarType.Precision.UNDEFINED


def test_findloc_return_type(fortran_reader):
    """Test the _findloc_return_type helper function."""
    code = """
    subroutine y
    integer, dimension(100) :: a
    integer, dimension(1) :: b
    integer, dimension(10, 10, 10) :: c
    b = FINDLOC(a, 1)
    b = FINDLOC(c, 1)
    end subroutine y"""
    psyir = fortran_reader.psyir_from_source(code)
    intrs = psyir.walk(Call)
    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.FINDLOC, [x.copy() for x in intrs[0].arguments]
    )
    res = _findloc_return_type(intr)
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == ScalarType.Precision.UNDEFINED
    assert len(res.shape) == 1
    assert res.shape[0].upper.value == "1"
    assert res.shape[0].lower.value == "1"

    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.FINDLOC, [
            ("array", intrs[0].arguments[0].copy()),
            ("value", intrs[0].arguments[1].copy()),
            ("kind", Literal("8", INTEGER_TYPE))
        ]
    )
    res = _findloc_return_type(intr)
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision.value == "8"
    assert len(res.shape) == 1
    assert res.shape[0].upper.value == "1"
    assert res.shape[0].lower.value == "1"

    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.FINDLOC, [
            ("array", intrs[0].arguments[0].copy()),
            ("value", intrs[0].arguments[1].copy()),
            ("kind", Literal("8", INTEGER_TYPE)),
            ("dim", Literal("1", INTEGER_TYPE)),
        ]
    )
    res = _findloc_return_type(intr)
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision.value == "8"

    # Test multidimensional array input with dim specified
    intr = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.FINDLOC, [
            ("array", intrs[1].arguments[0].copy()),
            ("value", intrs[1].arguments[1].copy()),
            ("dim", Literal("1", INTEGER_TYPE)),
        ]
    )
    res = _findloc_return_type(intr)
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == ScalarType.Precision.UNDEFINED
    assert len(res.shape) == 2
    assert res.shape[0] == ArrayType.Extent.DEFERRED
    assert res.shape[1] == ArrayType.Extent.DEFERRED

    # TODO #2823 adding namedargs makes this a CodeBlock so can't test yet via
    # fortran_reader.


def test_int_return_type(fortran_reader):
    """Test the _int_return_type helper function."""
    code = """subroutine z
    real*4 :: x
    integer :: y
    y = INT(x)
    end subroutine z"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    assert _int_return_type(intrinsic) == INTEGER_TYPE

    code = """subroutine z
    real*4, dimension(100) :: x
    integer*8, dimension(100) :: y
    y = INT(x, kind=8)
    end subroutine z"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    rtype = _int_return_type(intrinsic)
    assert isinstance(rtype, ArrayType)
    assert rtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert rtype.precision.value == "8"


def test_iparity_return_type(fortran_reader):
    """Test the _iparity_return_type helper function."""
    code = """
    subroutine x
    integer, dimension(100, 100) :: array
    integer :: k
    k = IPARITY(array)
    end subroutine x
    """
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(Call)[0]
    intrinsic = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.IPARITY,
        [x.copy() for x in intrinsic.arguments]
    )

    assert _iparity_return_type(intrinsic) == INTEGER_TYPE

    # Can't test the other case with fortran reader, so need to
    # create it manually.
    k_sym = psyir.children[0].symbol_table.lookup("k")
    intrinsic = psyir.walk(Call)[0]
    intrinsic = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.IPARITY,
        [("array", intrinsic.arguments[0].copy()), ("dim", Reference(k_sym))],
    )
    res = _iparity_return_type(intrinsic)
    assert isinstance(res, ArrayType)
    assert len(res.shape) == 1
    assert res.shape[0] == ArrayType.Extent.DEFERRED


def test_get_bound_function_return_type(fortran_reader):
    """Test the _get_bound_function_return_type helper function."""
    code = """subroutine x
    integer, dimension(100,100) :: array
    integer, dimension(2) :: out1
    integer*8 :: out2
    out1 = LBOUND(array)
    out2 = LBOUND(array, dim=1, kind=8)
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsics = psyir.walk(IntrinsicCall)

    res = _get_bound_function_return_type(intrinsics[0])
    assert isinstance(res, ArrayType)
    assert len(res.shape) == 1
    assert res.datatype == INTEGER_TYPE
    assert res.shape[0].lower.value == "1"
    assert res.shape[0].upper.value == "2"

    res = _get_bound_function_return_type(intrinsics[1])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision.value == "8"


def test_matmul_return_type(fortran_reader):
    code = """subroutine test
    real, dimension(100,100) :: a
    double precision, dimension(100, 100) :: b
    real, dimension(100) :: c
    real, dimension(:) :: d
    real, dimension(:,:) :: e

    e = MATMUL(a,b)
    d = MATMUL(a,c)
    d = MATMUL(c,a)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsics = psyir.walk(IntrinsicCall)
    res = _matmul_return_type(intrinsics[0])
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.REAL
    assert res.precision == ScalarType.Precision.UNDEFINED
    assert len(res.shape) == 2
    assert res.shape[0].lower.value == "1"
    assert isinstance(res.shape[0].upper, IntrinsicCall)
    assert res.shape[0].upper.intrinsic == IntrinsicCall.Intrinsic.SIZE
    assert res.shape[0].upper.arguments[0].symbol.name == "a"
    assert res.shape[0].upper.arguments[1].value == "1"
    assert res.shape[1].lower.value == "1"
    assert isinstance(res.shape[1].upper, IntrinsicCall)
    assert res.shape[1].upper.intrinsic == IntrinsicCall.Intrinsic.SIZE
    assert res.shape[1].upper.arguments[0].symbol.name == "b"
    assert res.shape[1].upper.arguments[1].value == "2"

    res = _matmul_return_type(intrinsics[1])
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.REAL
    assert res.precision == ScalarType.Precision.UNDEFINED
    assert len(res.shape) == 1
    assert res.shape[0].lower.value == "1"
    assert isinstance(res.shape[0].upper, IntrinsicCall)
    assert res.shape[0].upper.intrinsic == IntrinsicCall.Intrinsic.SIZE
    assert res.shape[0].upper.arguments[0].symbol.name == "a"
    assert res.shape[0].upper.arguments[1].value == "1"

    res = _matmul_return_type(intrinsics[2])
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.REAL
    assert res.precision == ScalarType.Precision.UNDEFINED
    assert len(res.shape) == 1
    assert res.shape[0].lower.value == "1"
    assert isinstance(res.shape[0].upper, IntrinsicCall)
    assert res.shape[0].upper.intrinsic == IntrinsicCall.Intrinsic.SIZE
    assert res.shape[0].upper.arguments[0].symbol.name == "a"
    assert res.shape[0].upper.arguments[1].value == "1"


def test_maxval_return_type(fortran_reader):
    '''Test for the _maxval_return_type function.'''
    code = """subroutine test
    integer*8, dimension(100,100) :: x
    integer, dimension(100) :: z
    integer :: y
    y = MAXVAL(x)
    z = MAXVAL(x, dim=2)
    end subroutine test
    """
    psyir = fortran_reader.psyir_from_source(code)
    intrs = psyir.walk(IntrinsicCall)

    res = _maxval_return_type(intrs[0])
    assert isinstance(res, ScalarType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == 8

    res = _maxval_return_type(intrs[1])
    assert isinstance(res, ArrayType)
    assert res.intrinsic == ScalarType.Intrinsic.INTEGER
    assert res.precision == 8
    assert len(res.shape) == 1
    assert res.shape[0] == ArrayType.Extent.DEFERRED


# FIXME Do we need ANINT (also REAL) tests (Reviewer/codecov decision).
@pytest.mark.parametrize(
    "code, expected",
    [
        (
            """subroutine x
     complex(4) :: z4
     real :: result
     result = aimag(z4)
     end subroutine x""",
            # AIMAG return type is UnsupportedFortranType
            lambda res: isinstance(res, UnsupportedFortranType),
        ),
        (
            """subroutine z
    real*4 :: x
    real :: y
    y = AINT(x)
    end subroutine z""",
            # AINT return type is that of x here.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.REAL
                and res.precision == 4
            ),
        ),
        (
            """subroutine z
    real*4 :: x
    real*8 :: y
    y = AINT(x, kind=8)
    end subroutine z""",
            # AINT return type is REAL with kind 8.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.REAL
                and isinstance(res.precision, Literal)
                and res.precision.value == "8"
            ),
        ),
        (
            """subroutine x
     complex(4) :: z4
     real :: r
     z4 = CMPLX(r)
     end subroutine x""",
            # CMPLX return type is UnsupportedFortranType
            lambda res: isinstance(res, UnsupportedFortranType),
        ),
        (
            """subroutine x
     complex(4) :: z4
     complex(4) :: r
     z4 = CONJG(r)
     end subroutine x""",
            # CONJG return type is UnsupportedFortranType
            lambda res: isinstance(res, UnsupportedFortranType),
        ),
        (
            """subroutine x
    integer :: a(100)
    integer :: b(100)
    integer :: c
    c = DOT_PRODUCT(a,b)
    end subroutine x""",
            # DOT_PRODUCT RETURN TYPE is Scalar type of input 1.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine z
    integer :: i, j, k
        k = IAND(i,j)
    end subroutine z""",
            # Kind is same as first input.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine z
    integer*8 :: i, j
    j = IAND(1, i)
    end subroutine z""",
            # Kind is same as 2nd input.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == 8
            ),
        ),
        (
            """subroutine z
    integer :: i, j, k
        k = IEOR(i,j)
    end subroutine z""",
            # Kind is same as first input.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine z
    integer*8 :: i, j
    j = IEOR(1, i)
    end subroutine z""",
            # Kind is same as 2nd input.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == 8
            ),
        ),
        (
            """subroutine z
    integer :: i, j
    integer :: k
    k = IOR(i,j)
    end subroutine z""",
            # Kind is Integer of first input kind.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine z
    integer*8 :: i,j
    j = IOR(2, i)
    end subroutine z""",
            # Result is same as second argument here.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == 8
            ),
        ),
        (
            """subroutine z
    integer*8 :: i,j
    j = NOT(i)
    end subroutine z""",
            # Result is INTEGER with kind as first argument.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == 8
            ),
        ),
        (
            """subroutine z
    integer, dimension(100) :: i
    integer, dimension(100) :: j
    logical, dimension(100) :: mask
    j = pack(i, mask)
    end subroutine z""",
            # Result is integer array with unknown size.
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED and
                res.shape[0] == ArrayType.Extent.DEFERRED
            ),
        ),
        (
            """subroutine z
        real, dimension(100,100) :: i
        real :: k
        k = PRODUCT(i)
        end subroutine z""",
            # Result is real scalar.
            lambda res: (
                isinstance(res, ScalarType) and
                res.intrinsic == ScalarType.Intrinsic.REAL
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine z
        real, dimension(100,100) :: i
        real, dimension(100) :: k
        k = PRODUCT(i, dim=1)
        end subroutine z""",
            # Result is real array with unknown size.
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL
                and res.precision == ScalarType.Precision.UNDEFINED and
                res.shape[0] == ArrayType.Extent.DEFERRED
            ),
        ),
        (
            """subroutine z
        real*8, dimension(100, 100) :: i
        integer, dimension(:) :: k
        k = SHAPE(i)
        end subroutine z""",
            # Result is INTEGER ARRAY with size 2
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED and
                len(res.shape) == 1 and
                res.shape[0].lower.value == "1" and
                res.shape[0].upper.value == "2"
            )
        ),
        (
            """subroutine z
        real*8, dimension(100, 100) :: i
        integer, dimension(:) :: k
        k = SHAPE(i, kind=8)
        end subroutine z""",
            # Result is INTEGER ARRAY with size 2
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision.value == "8" and
                len(res.shape) == 1 and
                res.shape[0].lower.value == "1" and
                res.shape[0].upper.value == "2"
            )
        ),
        (
            """subroutine z
        real*8, dimension(100) :: i
        real*8, dimension(:,:) :: k
        k = SPREAD(i,1,100)
        end subroutine z""",
            # Result is real array of dimension 2
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL
                and res.precision == 8 and
                len(res.shape) == 2 and
                res.shape[0] == ArrayType.Extent.DEFERRED and
                res.shape[1] == ArrayType.Extent.DEFERRED
            )
        ),
        (
            """subroutine z
        real*8 :: i
        real*8, dimension(:) :: k
        k = SPREAD(i,1,100)
        end subroutine z""",
            # Result is real array of dimension 1
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL
                and res.precision == 8 and
                len(res.shape) == 1 and
                res.shape[0] == ArrayType.Extent.DEFERRED
            )
        ),
        (
            """subroutine z
        real*8, dimension(100) :: arr
        real*8 :: res
        res = SUM(arr)
        end subroutine z""",
            # Result is real scalar.
            lambda res: (
                isinstance(res, ScalarType) and
                res.intrinsic == ScalarType.Intrinsic.REAL and
                res.precision == 8
            )
        ),
        (
            """subroutine z
        real*8, dimension(100, 100) :: arr
        real*8, dimension(:) :: res
        res = SUM(arr, dim=1)
        end subroutine z""",
            # Result is real scalar.
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL and
                res.precision == 8 and
                len(res.shape) == 1
                and res.shape[0] == ArrayType.Extent.DEFERRED
            )
        ),
        (
            """subroutine z
        real*8, dimension(100, 100) :: arr
        real*8, dimension(:) :: res
        res = TRANSFER(arr, res)
        end subroutine z""",
            # Result is real array.
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL and
                res.precision == 8 and
                len(res.shape) == 1
                and res.shape[0] == ArrayType.Extent.DEFERRED
            )
        ),
        (
            """subroutine z
        real*8, dimension(100, 100) :: arr
        real :: res
        res = TRANSFER(arr, res)
        end subroutine z""",
            # Result is scalar real.
            lambda res: (
                isinstance(res, ScalarType) and
                res.intrinsic == ScalarType.Intrinsic.REAL and
                res.precision == ScalarType.Precision.UNDEFINED
            )
        ),
        (
            """subroutine z
        real*8, dimension(100, 10) :: arr
        real*8, dimension(10, 100) :: arr2
        arr2 = TRANSPOSE(arr)
        end subroutine z""",
            # Result is real array (10,100).
            lambda res: (
                isinstance(res, ArrayType) and
                res.intrinsic == ScalarType.Intrinsic.REAL and
                res.precision == 8 and
                len(res.shape) == 2
                and res.shape[0].lower.value == "1"
                and res.shape[0].upper.value == "10"
                and res.shape[1].lower.value == "1"
                and res.shape[1].upper.value == "100"
            )
        ),
        (
            """subroutine z
        integer*8 :: i, j, k
        k = MAX(i,j)
        end subroutine z""",
            # Result is an integer*8.
            lambda res: (
                isinstance(res, ScalarType) and
                res.intrinsic == ScalarType.Intrinsic.INTEGER and
                res.precision == 8
            )
        ),
        (
            """subroutine z
        integer*8 :: i, j, k
        k = MIN(i,j)
        end subroutine z""",
            # Result is an integer*8.
            lambda res: (
                isinstance(res, ScalarType) and
                res.intrinsic == ScalarType.Intrinsic.INTEGER and
                res.precision == 8
            )
        ),
    ],
)
def test_specific_return_types(fortran_reader, code, expected):
    """Test the specific return types of each IntrinsicCall that has its own
    defined return type function."""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    assert expected(intrinsic.intrinsic.return_type(intrinsic))


@pytest.mark.parametrize(
    "code, intrinsic, expected",
    [
        (
            """subroutine test
        integer, dimension(100) :: x
        integer, dimension(100) :: y
        y = coshape(x)
        end subroutine test""",
            IntrinsicCall.Intrinsic.COSHAPE,
            # Return type is integer of kind of x with dimension of x.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
                and len(res.shape) == 1
                and res.shape[0].upper.value == "100"
                and res.shape[0].lower.value == "1"
            ),
        ),
        # TODO #2823 Can't do this test yet.
        # ("""subroutine test
        #    integer, dimension(100) :: x
        #    integer, dimension(100) :: y
        #    y = coshape(x, kind=4)
        #    end subroutine test""",
        # IntrinsicCall.Intrinsic.COSHAPE,
        # # Return type is integer of kind of x with dimension of x.
        # lambda res: (res.intrinsic == ScalarType.Intrinsic.INTEGER and
        #              res.precision.value == "4" and
        #              len(res.shape) == 1 and res.shape[0].upper.value
        #              == "100"
        #              and res.shape[0].lower.value == "1")
        # ),
        (
            """subroutine x
    integer :: i, j, k
    k = DSHIFTL(i,j,4)
    end subroutine x""",
            IntrinsicCall.Intrinsic.DSHIFTL,
            # Return type here is that of input 1.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == ScalarType.Precision.UNDEFINED
            ),
        ),
        (
            """subroutine x
    integer*8 :: j, k
    k = DSHIFTL(32,j,4)
    end subroutine x""",
            IntrinsicCall.Intrinsic.DSHIFTL,
            # Return type here is that of input 2.
            lambda res: (
                res.intrinsic == ScalarType.Intrinsic.INTEGER
                and res.precision == 8
            ),
        ),
        # TODO #2823 Can't do this test yet, PSyclone creates a CodeBlock.
        # ("""subroutine x
        # integer*8, dimension(100) :: i, j
        # i = FAILED_IMAGES()
        # end subroutine x""",
        # IntrinsicCall.Intrinsic.FAILED_IMAGES,
        # # Return type here is Array of deferred size with integer type.
        # lambda res: (isinstance(res, ArrayType) and
        #             res.intrinsic == ScalarType.Intrinsic.INTEGER and
        #             res.precision == ScalarType.Precision.UNDEFINED)
        # ),
        # TODO #2823 Can't do this test yet, PSyclone creates a CodeBlock.
        # ("""subroutine x
        # integer*8, dimension(100) :: i, j
        # integer :: bad
        # i = FAILED_IMAGES(kind=8)
        # end subroutine x""",
        # IntrinsicCall.Intrinsic.FAILED_IMAGES,
        # # Return type here is Array of deferred size with integer type and
        # # kind = 8
        # lambda res: (isinstance(res, ArrayType) and
        #             res.intrinsic == ScalarType.Intrinsic.INTEGER and
        #             res.precision.value == "8")
        # ),
        # TODO #2823 Can't do this test yet, PSyclone creates a CodeBlock.
        # ("""subroutine x
        # integer :: i
        # i = GET_TEAM()
        # end subroutine x""",
        # IntrinsicCall.Intrinsic.GET_TEAM,
        # lambda res: isinstance(res, UnsupportedFortranType)
        # ),
        # TODO #2823 Can't do this test yet, PSyclone creates a CodeBlock.
        # (
        #     """subroutine z
        # integer, dimension(:) :: result
        # result = STOPPED_IMAGES()
        # end subroutine z""",
        # IntrinsicCall.Intrinsic.STOPPED_IMAGES,
        #     # Result is an integer array of dimension 1
        #     lambda res: (
        #         isinstance(res, ArrayType) and
        #         res.intrinsic == ScalarType.Intrinsic.INTEGER
        #         and res.precision == ScalarType.Precision.UNDEFINED and
        #         len(res.shape) == 1 and
        #         res.shape[0] == ArrayType.Extent.DEFERRED
        #     )
        # ),
    ],
)
def test_specific_return_types_incorrect_parsed(
    fortran_reader, code, intrinsic, expected
):
    """Test the specific return types of IntrisicCalls that aren't recognised
    correctly by fparser."""
    psyir = fortran_reader.psyir_from_source(code)
    parsed = psyir.walk(Call)[0]
    indices = [x.copy() for x in parsed.arguments]
    intr = IntrinsicCall.create(intrinsic, indices)
    assert expected(intr.intrinsic.return_type(intr))
