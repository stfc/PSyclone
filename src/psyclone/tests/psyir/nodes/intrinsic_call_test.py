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

'''
This module contains pytest tests for the IntrinsicCall node.

TODO #2341 - tests need to be added for all of the supported intrinsics.

'''

import pytest

from psyclone.psyir.nodes import (
    ArrayReference, Literal, Reference, Schedule, Assignment)
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall, IAttr, _get_first_argument_type,
    _get_first_argument_logical_kind_with_optional_dim)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, INTEGER_TYPE, IntrinsicSymbol, REAL_TYPE,
    BOOLEAN_TYPE, CHARACTER_TYPE, ScalarType, UnresolvedType)


def test_intrinsic_enum():
    '''Basic test for the IntrinsicCall.Intrinsic enum.'''
    assert isinstance(IntrinsicCall.Intrinsic.MINVAL, IAttr)
    assert hash(IntrinsicCall.Intrinsic.MINVAL) == hash("MINVAL")


def test_intrinsiccall_constructor():
    '''Tests that the class' constructor and its parent are called
    correctly.

    '''
    # Wrong type of routine argument.
    with pytest.raises(TypeError) as err:
        _ = IntrinsicCall(None)
    assert ("IntrinsicCall 'intrinsic' argument should be an instance of "
            "IntrinsicCall.Intrinsic, but found 'NoneType'." in str(err.value))
    # Check that supplied intrinsic and optional parent node is stored
    # correctly.
    sched = Schedule()
    call = IntrinsicCall(IntrinsicCall.Intrinsic.MINVAL, parent=sched)
    assert call.routine.symbol.intrinsic is IntrinsicCall.Intrinsic.MINVAL
    assert isinstance(call.routine.symbol, IntrinsicSymbol)
    assert call.routine.name == "MINVAL"
    assert call.parent is sched


def test_intrinsiccall_intrinsic():
    '''Tests the intrinsic property returns the type of intrinsics from
    the intrinsic property.

    '''
    call = IntrinsicCall(IntrinsicCall.Intrinsic.MAXVAL)
    assert call.intrinsic is IntrinsicCall.Intrinsic.MAXVAL


def test_intrinsiccall_is_elemental():
    '''Tests the is_elemental() method works as expected. There are
    currently no elemental intrinsics so we can only test for
    False.

    '''
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_elemental is False


def test_intrinsiccall_is_pure():
    '''Tests that the is_pure() method works as expected.'''
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_pure is True
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATE)
    assert intrinsic.is_pure is False


def test_intrinsiccall_is_inquiry():
    '''Test that the is_inquiry() method works as expected.'''
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    assert intrinsic.is_inquiry is False
    intrinsic = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATED)
    assert intrinsic.is_inquiry is True


@pytest.mark.parametrize("intrinsic, result", [
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
                (IntrinsicCall.Intrinsic.UBOUND, True)])
def test_intrinsiccall_is_available_on_device(intrinsic, result):
    '''Tests that the is_available_on_device() method works as expected.'''
    intrinsic_call = IntrinsicCall(intrinsic)
    # For now default and nvfortran-all are the same
    assert intrinsic_call.is_available_on_device() is result
    assert intrinsic_call.is_available_on_device('nvfortran-all') is result


def test_intrinsiccall_reductions_is_available_on_device():
    '''Tests that the is_available_on_device() refuses reduction intrinsics
    with optional arguments'''
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.SUM)
    intrinsic_call.addchild(Reference(DataSymbol("result", REAL_TYPE)))
    # This is avaliabe on the device
    assert intrinsic_call.is_available_on_device()
    # But not when it has more arguments as it sometimes fails for complex
    # reductions with arguments
    intrinsic_call.addchild(Literal("1", INTEGER_TYPE))
    assert not intrinsic_call.is_available_on_device()


def test_intrinsiccall_is_available_on_device_with_device_string():
    '''Tests that the is_available_on_device() method with a device_string
    argument provides different results with the 'nvfortran-uniform'
    '''
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.LOG10)
    assert not intrinsic_call.is_available_on_device("nvfortran-uniform")
    intrinsic_call = IntrinsicCall(IntrinsicCall.Intrinsic.REAL)
    assert not intrinsic_call.is_available_on_device("nvfortran-uniform")

    with pytest.raises(ValueError) as err:
        assert not intrinsic_call.is_available_on_device("invalid")
    assert ("Unsupported device_string value 'invalid', the supported values"
            " are '' (default), 'nvfortran-all', 'nvfortran-uniform'"
            in str(err.value))


def test_intrinsiccall_alloc_create():
    '''Tests the create() method supports various forms of 'allocate'.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    bsym = DataSymbol("my_array2", ArrayType(INTEGER_TYPE,
                                             [ArrayType.Extent.DEFERRED]))
    isym = DataSymbol("ierr", INTEGER_TYPE)
    csym = DataSymbol("msg", CHARACTER_TYPE)
    # Straightforward allocation of an array.
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])])
    assert isinstance(alloc, IntrinsicCall)
    assert alloc.intrinsic is IntrinsicCall.Intrinsic.ALLOCATE
    assert isinstance(alloc.routine.symbol, IntrinsicSymbol)
    assert alloc.routine.name == "ALLOCATE"
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(sym), ("Mold", Reference(bsym))])
    assert isinstance(alloc, IntrinsicCall)
    assert alloc.argument_names == [None, "Mold"]
    alloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.ALLOCATE,
        [Reference(sym), ("Source", Reference(bsym)),
         ("stat", Reference(isym)), ("errmsg", Reference(csym))])
    assert alloc.argument_names == [None, "Source", "stat", "errmsg"]


def test_intrinsiccall_dealloc_create():
    '''Tests for the creation of a 'deallocate' call.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    ierr = DataSymbol("ierr", INTEGER_TYPE)
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym)])
    assert isinstance(dealloc, IntrinsicCall)
    assert dealloc.intrinsic is IntrinsicCall.Intrinsic.DEALLOCATE
    assert isinstance(dealloc.routine.symbol, IntrinsicSymbol)
    assert dealloc.routine.name == "DEALLOCATE"
    assert dealloc.arguments[0].symbol is sym
    # With 'stat' optional argument.
    dealloc = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.DEALLOCATE, [Reference(sym),
                                             ("Stat", Reference(ierr))])
    assert dealloc.argument_names == [None, "Stat"]


def test_intrinsiccall_random_create():
    '''Tests for the creation of a 'random' call.

    '''
    sym = DataSymbol("my_array", ArrayType(REAL_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    rand = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.RANDOM_NUMBER, [Reference(sym)])
    assert isinstance(rand, IntrinsicCall)
    assert rand.intrinsic is IntrinsicCall.Intrinsic.RANDOM_NUMBER
    assert isinstance(rand.routine.symbol, IntrinsicSymbol)
    assert rand.routine.name == "RANDOM_NUMBER"
    assert rand.arguments[0].symbol is sym


@pytest.mark.parametrize("intrinsic_call", [
    IntrinsicCall.Intrinsic.MINVAL, IntrinsicCall.Intrinsic.MAXVAL,
    IntrinsicCall.Intrinsic.SUM])
def test_intrinsiccall_minmaxsum_create(intrinsic_call):
    '''Tests for the creation of the different argument options for
    'minval', 'maxval' and 'sum' IntrinsicCalls.

    '''
    array = DataSymbol(
        "my_array", ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED]))
    dim = DataSymbol("dim", INTEGER_TYPE)
    mask = DataSymbol("mask", BOOLEAN_TYPE)

    # array only
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array)])
    assert isinstance(intrinsic, IntrinsicCall)
    assert intrinsic.intrinsic is intrinsic_call
    assert isinstance(intrinsic.routine.symbol, IntrinsicSymbol)
    intrinsic_name = intrinsic_call.name
    assert intrinsic.routine.name == intrinsic_name
    assert intrinsic.arguments[0].symbol is array
    # array and optional dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("dim", Reference(dim))])
    assert intrinsic.argument_names == [None, "dim"]
    # array and optional mask
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("mask", Reference(mask))])
    assert intrinsic.argument_names == [None, "mask"]
    # array and optional dim then optional mask
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("dim", Reference(dim)),
                         ("mask", Reference(mask))])
    assert intrinsic.argument_names == [None, "dim", "mask"]
    # array and optional mask then optional dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [Reference(array), ("mask", Reference(mask)),
                         ("dim", Reference(dim))])
    assert intrinsic.argument_names == [None, "mask", "dim"]
    # array and optional literal mask and optional literal dim
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [
            Reference(array),
            ("mask", Literal("1", INTEGER_TYPE)),
            ("dim", Literal("false", BOOLEAN_TYPE))])
    assert intrinsic.argument_names == [None, "mask", "dim"]


@pytest.mark.parametrize("intrinsic_call", [
    IntrinsicCall.Intrinsic.TINY, IntrinsicCall.Intrinsic.HUGE])
@pytest.mark.parametrize("form", ["array", "literal"])
def test_intrinsiccall_tinyhuge_create(intrinsic_call, form):
    '''Tests for the creation of the different argument options for
    'tiny' and 'huge' IntrinsicCalls.

    '''
    if form == "array":
        array = DataSymbol(
            "my_array", ArrayType(REAL_TYPE, [ArrayType.Extent.DEFERRED]))
        arg = Reference(array)
    else:  # "literal"
        arg = Literal("1.0", REAL_TYPE)
    intrinsic = IntrinsicCall.create(
        intrinsic_call, [arg])
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
    '''Checks for the validation/type checking in the create() method.

    '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    aref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create("ALLOCATE", [Reference(sym)])
    assert ("'intrinsic' argument should be an instance of "
            "IntrinsicCall.Intrinsic, but found 'str'" in str(err.value))
    # Supplied arguments must be a list.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, aref)
    assert ("IntrinsicCall.create() 'arguments' argument should be an Iterable"
            " but found 'ArrayReference'" in str(err.value))
    # An allocate must have one or more References as argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [])
    assert ("The 'ALLOCATE' intrinsic requires at least 1 arguments but "
            "got 0" in str(err.value))
    # The random intrinsic only accepts one argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                             [aref, aref.copy()])
    assert ("The 'RANDOM_NUMBER' intrinsic requires between 1 and 1 arguments "
            "but got 2" in str(err.value))
    # Wrong type for a positional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [sym])
    assert ("The 'ALLOCATE' intrinsic requires that positional arguments be "
            "of type " in str(err.value))
    assert "but got a 'DataSymbol'" in str(err.value)
    # Positional argument after named argument.
    with pytest.raises(ValueError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.DEALLOCATE,
                             [Reference(sym), ("stat", aref), aref])
    assert ("Found a positional argument *after* a named argument ('stat'). "
            "This is invalid." in str(err.value))

    # TODO #2303: We can not enable the validation of positional parameters
    # unless we store their name, otherwise when we parse a positional argument
    # by name, which is valid fortran, it will fail.
    # (e.g. RANDOM_NUMBER(harvest=4)

    # with pytest.raises(ValueError) as err:
    #     IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
    #                          [aref, ("willow", sym)])
    # assert ("The 'RANDOM_NUMBER' intrinsic does not support any optional "
    #         "arguments but got 'willow'" in str(err.value))
    # An allocate only supports the 'stat' and 'mold' arguments.
    # with pytest.raises(ValueError) as err:
    #     IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
    #                          [aref, ("yacht", Reference(sym))])
    # assert ("The 'ALLOCATE' intrinsic supports the optional arguments "
    #         "['errmsg', 'mold', 'source', 'stat'] but got 'yacht'"
    #         in str(err.value))

    # Wrong type for the name of an optional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [aref, (sym, sym)])
    assert ("Optional arguments to an IntrinsicCall must be specified by a "
            "(str, Reference) tuple but got a DataSymbol instead of a str"
            in str(err.value))
    # Wrong type for an optional argument.
    with pytest.raises(TypeError) as err:
        IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                             [aref, ("stat", sym)])
    assert ("The optional argument 'stat' to intrinsic 'ALLOCATE' must be "
            "of type 'Reference' but got 'DataSymbol'" in str(err.value))


def test_create_positional_arguments_with_names():
    ''' Test the create method when given named positional arguments.'''
    sym = DataSymbol("my_array",
                     ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED]))
    aref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])
    bref = ArrayReference.create(sym, [Literal("20", INTEGER_TYPE)])

    # All of these are valid
    intr = IntrinsicCall.create(IntrinsicCall.Intrinsic.DOT_PRODUCT,
                                [aref.copy(), bref.copy()])
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == [None, None]

    intr = IntrinsicCall.create(IntrinsicCall.Intrinsic.DOT_PRODUCT,
                                [aref.copy(), ("vector_b", bref.copy())])
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == [None, "vector_b"]

    intr = IntrinsicCall.create(IntrinsicCall.Intrinsic.DOT_PRODUCT,
                                [("vector_a", aref.copy()),
                                 ("vector_b", bref.copy())])
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == aref
    assert intr.arguments[1] == bref
    assert intr.argument_names == ["vector_a", "vector_b"]

    intr = IntrinsicCall.create(IntrinsicCall.Intrinsic.DOT_PRODUCT,
                                [("vector_b", bref.copy()),
                                 ("vector_a", aref.copy())])
    assert isinstance(intr, IntrinsicCall)
    assert intr.arguments[0] == bref
    assert intr.arguments[1] == aref
    assert intr.argument_names == ["vector_b", "vector_a"]


@pytest.mark.parametrize("operator", ["lbound", "ubound", "size"])
def test_reference_accesses_bounds(operator, fortran_reader):
    '''Test that the reference_accesses method behaves as expected when
    the reference is the first argument to either the lbound or ubound
    intrinsic as that is simply looking up the array bounds (therefore
    the access is an enquiry) and when the reference is the
    second argument of either the lbound or ubound intrinsic (in which
    case the access should be a read).

    '''
    code = f'''module test
        contains
        subroutine tmp()
          real, dimension(:,:), allocatable:: a, b
          integer :: n
          n = {operator}(a, b(1,1))
        end subroutine tmp
        end module test'''
    psyir = fortran_reader.psyir_from_source(code)
    schedule = psyir.walk(Assignment)[0]

    # The access to 'a' should be reported as 'NO_DATA_ACCESS' as its
    # actual data is not accessed.
    vam = schedule.reference_accesses()
    assert str(vam) == "a: INQUIRY, b: READ, n: WRITE"


def test_enumerator_name_matches_name_field():
    '''
    Test that the name given to every IntrinsicCall matches the
    corresponding name field in the IAttr namedtuple.
    '''
    for intrinsic_entry in IntrinsicCall.Intrinsic:
        assert intrinsic_entry._name_ == intrinsic_entry.name


def test_allocate_intrinsic(fortran_reader, fortran_writer):
    '''
    Test the ALLOCATE 'intrinsic'.
    '''
    code = '''
program test_prog
  implicit none
  integer :: ierr
  character(len=128) :: msg
  real, allocatable, dimension(:) :: arr1, arr2
  allocate(arr1(10), stat=ierr)
  allocate(arr2, mold=arr1)
  allocate(arr2, source=arr1, errmsg=msg)
end program test_prog
'''
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 3
    result = fortran_writer(psyir).lower()
    assert "allocate(arr1(1:10), stat=ierr)" in result
    assert "allocate(arr2, mold=arr1)" in result
    assert "allocate(arr2, source=arr1, errmsg=msg)" in result


def test_deallocate_intrinsic(fortran_reader, fortran_writer):
    '''
    Test the DEALLOCATE 'intrinsic'.
    '''
    code = '''
program test_prog
  implicit none
  integer :: ierr
  real, allocatable, dimension(:) :: arr1
  deallocate(arr1)
  deallocate(arr1, stat=ierr)
end program test_prog
'''
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 2
    result = fortran_writer(psyir).lower()
    assert "deallocate(arr1)" in result
    assert "deallocate(arr1, stat=ierr)" in result


def test_index_intrinsic(fortran_reader, fortran_writer):
    '''
    Test the INDEX intrinsic.
    '''
    code = '''
program test_prog
  implicit none
  character(len=10) :: clname
  integer :: ind1, ind2

  ind1 = INDEX( clname, '_', back = .TRUE. ) + 1
  ind2 = INDEX( clname, '.') - 1
  ind2 = INDEX( clname, '.', kind=4) - 1

end program test_prog
'''
    psyir = fortran_reader.psyir_from_source(code)
    assert len(psyir.walk(IntrinsicCall)) == 3
    result = fortran_writer(psyir).lower()
    assert "ind1 = index(clname, '_', back=.true.) + 1" in result
    assert "ind2 = index(clname, '.') - 1" in result
    assert "ind2 = index(clname, '.', kind=4) - 1" in result


def test_verify_intrinsic(fortran_reader, fortran_writer):
    '''
    Test the VERIFY intrinsic.
    '''
    code = '''
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
'''
    psyir = fortran_reader.psyir_from_source(code)
    # Should have 4 VERIFY and 2 KIND
    assert len(psyir.walk(IntrinsicCall)) == 6
    result = fortran_writer(psyir).lower()
    assert "if (verify(clname(ind1:ind2), '0123456789') == 0) then" in result
    assert ("if (verify(clname(ind1:ind2), '0123456789', back=.true.) "
            "== 0) then" in result)
    assert ("if (verify(clname(ind1:ind2), '0123456789', kind=kind(1)) "
            "== 0) then" in result)
    assert ("if (verify(clname(ind1:ind2), '0123456789', kind=kind(1), "
            "back=.true.) == 0) then" in result)


def test_get_first_argument_type(fortran_reader):
    '''Test the _get_first_argument_type helper function.'''
    code = """subroutine x
    integer :: a, b
    a = 1
    b = ABS(a)
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    abs_call = psyir.walk(IntrinsicCall)[0]
    dtype = _get_first_argument_type(abs_call)
    assert dtype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert dtype.precision == ScalarType.Precision.UNDEFINED


def test_get_first_argument_logical_kind_with_optional_dim(fortran_reader):
    '''Test the _get_first_argument_logical_kind_with_optional_dim helper
    function.'''
    code = """subroutine x
    logical, dimension(100,100) :: a
    logical, dimension(100) :: b
    logical :: c
    c = ALL(a)
    b = ALL(a, dim=1)
    end subroutine x
    """
    psyir = fortran_reader.psyir_from_source(code)
    all_calls = psyir.walk(IntrinsicCall)
    dtype = _get_first_argument_logical_kind_with_optional_dim(all_calls[0])
    assert dtype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert dtype.precision == ScalarType.Precision.UNDEFINED
    dtype = _get_first_argument_logical_kind_with_optional_dim(all_calls[1])
    assert isinstance(dtype, ArrayType)
    assert len(dtype.shape) == 1
    assert dtype.shape[0] == ArrayType.Extent.DEFERRED
    assert dtype.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert dtype.datatype.precision == ScalarType.Precision.UNDEFINED


@pytest.mark.parametrize("code, expected", [
    ("""subroutine x
     complex(4) :: z4
     real :: result
     result = aimag(z4)
     end subroutine x""",
     # AIMAG return type is UnresolvedType
     lambda res: isinstance(res, UnresolvedType)
     ),
    ("""subroutine z
    real*4 :: x
    real :: y
    y = AINT(x)
    end subroutine z""",
     # AINT return type is that of x here.
     lambda res: (res.intrinsic == ScalarType.Intrinsic.REAL and
                  res.precision == 4)
     ),
    ("""subroutine z
    real*4 :: x
    real*8 :: y
    y = AINT(x, kind=8)
    end subroutine z""",
     # AINT return type is REAL with kind 8.
     lambda res: (res.intrinsic == ScalarType.Intrinsic.REAL and
                  isinstance(res.precision, Literal) and
                  res.precision.value == "8")
     ),
    ])
def test_specific_return_types(fortran_reader, code, expected):
    ''' Test the specific return types of each IntrinsicCall that has its own
    defined return type function.'''
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    assert expected(intrinsic.intrinsic.return_type(intrinsic))
