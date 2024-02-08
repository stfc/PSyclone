# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end '''

import pytest

import fparser
from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003
from fparser.two.Fortran2003 import (
    Dimension_Attr_Spec, Execution_Part, Name, Return_Stmt,
    Specification_Part, Stmt_Function_Stmt, Subroutine_Subprogram,
    Type_Declaration_Stmt)
from fparser.two.utils import walk

from psyclone.errors import InternalError, GenerationError
from psyclone.psyGen import PSyFactory
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, _is_array_range_literal, _is_bound_full_extent,
    _is_range_full_extent, _check_args, default_precision,
    default_integer_type, default_real_type, _first_type_match,
    _get_arg_names)
from psyclone.psyir.nodes import (
    Schedule, CodeBlock, Assignment, Return, UnaryOperation, BinaryOperation,
    IfBlock, Reference, ArrayReference, Literal, Range, KernelSchedule,
    RegionDirective, Routine, StandaloneDirective, StructureReference,
    ArrayOfStructuresReference, Call, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, ContainerSymbol, SymbolTable, ArgumentInterface,
    SymbolError, ScalarType, ArrayType, INTEGER_TYPE, REAL_TYPE,
    UnsupportedFortranType, UnresolvedType, Symbol, UnresolvedInterface,
    ImportInterface, BOOLEAN_TYPE, StaticInterface, UnknownInterface,
    StructureType, DataTypeSymbol)

# pylint: disable=too-many-statements

# Tests

FAKE_KERNEL_METADATA = '''
module dummy_mod
  use argument_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type) meta_args(3) =                              &
          (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
             arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
             arg_type(gh_field, gh_real, gh_inc,       w1)      &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_first_type_match():
    '''Test that the _first_type_match utility function returns the first
    instance of the specified type from a list and that it raises a
    ValueError exception if one is not found.

    '''
    assert _first_type_match([1, 2], int) == 1
    assert _first_type_match(["a", 1], int) == 1
    with pytest.raises(ValueError):
        _first_type_match(["a", "b"], int)


def test_check_args():
    ''' Test the _check_args function. '''

    with pytest.raises(TypeError) as excinfo:
        _check_args(None, None)
    assert ("'array' argument should be some sort of array access (i.e. a "
            "sub-class of ArrayMixin) but found 'NoneType'." in
            str(excinfo.value))

    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [20])
    symbol = DataSymbol('a', array_type)
    array_reference = ArrayReference.create(symbol, [one])

    with pytest.raises(TypeError) as excinfo:
        _check_args(array_reference, None)
    assert ("'dim' argument should be an int type but found 'NoneType'."
            in str(excinfo.value))

    with pytest.raises(ValueError) as excinfo:
        _check_args(array_reference, 0)
    assert ("'dim' argument should be at least 1 but found 0."
            in str(excinfo.value))

    with pytest.raises(ValueError) as excinfo:
        _check_args(array_reference, 2)
    assert ("'dim' argument should be at most the number of dimensions of "
            "the array (1) but found 2." in str(excinfo.value))

    with pytest.raises(TypeError) as excinfo:
        _check_args(array_reference, 1)
    assert ("'array' argument index '0' should be a Range type but "
            "found 'Literal'." in str(excinfo.value))


def test_is_bound_full_extent():
    ''' Test the _is_bound_full_extent function.'''

    # Check that _is_bound_full_extent calls the check_args function.
    with pytest.raises(TypeError) as excinfo:
        _is_bound_full_extent(None, None, None)
    assert ("'array' argument should be some sort of array access (i.e. "
            "a sub-class of ArrayMixin) but found 'NoneType'." in
            str(excinfo.value))

    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [20])
    symbol = DataSymbol('a', array_type)
    my_range = Range.create(one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    with pytest.raises(TypeError) as excinfo:
        _is_bound_full_extent(array_reference, 1, None)
    assert ("'intrinsic' argument  expected to be LBOUND or UBOUND but found "
            "'NoneType'" in str(excinfo.value))

    # Expecting BinaryOperation but found Literal
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.UBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [one.copy(), ("dim", one.copy())])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting intrinsic to be LBOUND, but found UBOUND
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [one.copy(), ("dim", one.copy())])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting Reference but found Literal
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(DataSymbol("x", INTEGER_TYPE)), ("dim", one.copy())])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting Reference symbol x to be the same as array symbol a
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("1.0", REAL_TYPE))])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting integer but found real
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("2", INTEGER_TYPE))])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting literal value 2 to be the same as the current array
    # dimension 1
    assert not _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)

    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # valid
    assert _is_bound_full_extent(array_reference, 1,
                                 IntrinsicCall.Intrinsic.LBOUND)


def test_is_array_range_literal():
    ''' Test the _is_array_range_literal function.'''

    # Check that _is_array_range_literal calls the _check_args function.
    with pytest.raises(TypeError) as excinfo:
        _is_array_range_literal(None, None, None, None)
    assert ("'array' argument should be some sort of array access (i.e. a "
            "sub-class of ArrayMixin) but found 'NoneType'." in
            str(excinfo.value))

    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [20])
    symbol = DataSymbol('a', array_type)
    operator = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])
    my_range = Range.create(operator, one)
    array_reference = ArrayReference.create(symbol, [my_range])

    with pytest.raises(TypeError) as excinfo:
        _is_array_range_literal(array_reference, 1, None, None)
    assert ("'index' argument should be an int type but found 'NoneType'."
            in str(excinfo.value))

    with pytest.raises(ValueError) as excinfo:
        _is_array_range_literal(array_reference, 1, -1, None)
    assert ("'index' argument should be 0, 1 or 2 but found -1."
            in str(excinfo.value))

    with pytest.raises(ValueError) as excinfo:
        _is_array_range_literal(array_reference, 1, 3, None)
    assert ("'index' argument should be 0, 1 or 2 but found 3."
            in str(excinfo.value))

    with pytest.raises(TypeError) as excinfo:
        _is_array_range_literal(array_reference, 1, 2, None)
    assert ("'value' argument should be an int type but found 'NoneType'."
            in str(excinfo.value))

    # 1st dimension, second argument to range is an integer literal
    # with value 1
    assert _is_array_range_literal(array_reference, 1, 1, 1)

    # 1st dimension, first argument to range is an operator, not a literal
    assert not _is_array_range_literal(array_reference, 1, 0, 1)

    my_range = Range.create(operator.copy(), one.copy())

    # Range.create checks for valid datatype. Therefore change to
    # invalid after creation.
    my_range.children[1] = Literal("1.0", REAL_TYPE)
    array_reference = ArrayReference.create(symbol, [my_range])

    # 1st dimension, second argument to range is a real literal,
    # not an integer literal.
    assert not _is_array_range_literal(array_reference, 1, 1, 1)

    my_range = Range.create(operator.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])
    # 1st dimension, second argument to range has an unexpected
    # value.
    assert not _is_array_range_literal(array_reference, 1, 1, 2)


def test_is_range_full_extent():
    ''' Test the _is_range_full_extent function.'''
    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [2])
    symbol = DataSymbol('a', array_type)
    lbound_op = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])
    ubound_op = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])

    my_range = Range.create(lbound_op, ubound_op, one)
    _ = ArrayReference.create(symbol, [my_range])
    # Valid structure
    _is_range_full_extent(my_range)

    # Invalid start (as 1st argument should be lower bound)
    my_range = Range.create(ubound_op.copy(), ubound_op.copy(), one.copy())
    _ = ArrayReference.create(symbol, [my_range])
    assert not _is_range_full_extent(my_range)

    # Invalid stop (as 2nd argument should be upper bound)
    my_range = Range.create(lbound_op.copy(), lbound_op.copy(), one.copy())
    _ = ArrayReference.create(symbol, [my_range])
    assert not _is_range_full_extent(my_range)

    # Invalid step (as 3rd argument should be Literal)
    my_range = Range.create(lbound_op.copy(), ubound_op.copy(),
                            ubound_op.copy())
    _ = ArrayReference.create(symbol, [my_range])
    assert not _is_range_full_extent(my_range)


@pytest.mark.parametrize("value",
                         [ScalarType.Intrinsic.REAL,
                          ScalarType.Intrinsic.INTEGER,
                          ScalarType.Intrinsic.BOOLEAN,
                          ScalarType.Intrinsic.CHARACTER,
                          None])
def test_default_precision(value):
    '''Test the default_precision function returns the same precision
    irrespective of the argument passed to it'''
    assert default_precision(value) == ScalarType.Precision.UNDEFINED


def test_default_integer_type():
    '''Test the default_integer_type function returns the expected result'''
    result = default_integer_type()
    assert isinstance(result, ScalarType)
    assert result.intrinsic == ScalarType.Intrinsic.INTEGER
    assert result.precision == default_precision(ScalarType.Intrinsic.INTEGER)


def test_default_real_type():
    '''Test the default_real_type function returns the expected result'''
    result = default_real_type()
    assert isinstance(result, ScalarType)
    assert result.intrinsic == ScalarType.Intrinsic.REAL
    assert result.precision == default_precision(ScalarType.Intrinsic.REAL)


def test_get_arg_names(parser):
    '''Test the _get_arg_names utility function returns the expected
    results'''
    code = ("program test\n"
            "call sub(a, arg2=b, arg3=c)\n"
            "end program test\n")
    reader = FortranStringReader(code)
    ast = parser(reader)
    # pylint: disable=no-member
    arg_list = walk(ast, Fortran2003.Actual_Arg_Spec_List)[0].children
    args, names = _get_arg_names(arg_list)
    assert len(args) == 3
    for arg in args:
        assert isinstance(arg, Name)
    assert args[0].string == "a"
    assert args[1].string == "b"
    assert args[2].string == "c"
    assert names == [None, "arg2", "arg3"]


# Class Fparser2Reader


def test_array_notation_rank():
    '''Test the static method _array_notation_rank in the fparser2reader
    class.

    '''
    int_one = Literal("1", INTEGER_TYPE)
    # Wrong type of argument
    with pytest.raises(NotImplementedError) as err:
        Fparser2Reader._array_notation_rank(int_one)
    assert ("Expected either an ArrayReference, ArrayMember or a "
            "StructureReference but got 'Literal'" in str(err.value))

    # Structure reference containing no array access
    symbol = DataSymbol("field", UnresolvedType())
    with pytest.raises(InternalError) as err:
        Fparser2Reader._array_notation_rank(
            StructureReference.create(symbol, ["first", "second"]))
    assert "No array access found in node 'field'" in str(err.value)

    # Structure reference with ranges in more than one part reference.
    lbound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [StructureReference.create(symbol, ["first"]),
         ("dim", int_one.copy())])
    ubound = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [StructureReference.create(symbol, ["first"]),
         ("dim", int_one.copy())])
    my_range = Range.create(lbound, ubound)
    with pytest.raises(InternalError) as err:
        Fparser2Reader._array_notation_rank(
            StructureReference.create(symbol, [("first", [my_range]),
                                               ("second", [my_range.copy()])]))
    assert ("Found a structure reference containing two or more part "
            "references that have ranges: 'field%first(:)%second("
            "LBOUND(field%first, dim=1):UBOUND(field%first, dim=1))'. This is "
            "not valid within a WHERE in Fortran." in str(err.value))
    # Repeat but this time for an ArrayOfStructuresReference.
    with pytest.raises(InternalError) as err:
        Fparser2Reader._array_notation_rank(
            ArrayOfStructuresReference.create(symbol, [my_range.copy()],
                                              ["first",
                                               ("second", [my_range.copy()])]))
    assert ("Found a structure reference containing two or more part "
            "references that have ranges: 'field(LBOUND(field%first, dim=1):"
            "UBOUND(field%first, dim=1))%first%second("
            "LBOUND(field%first, dim=1):UBOUND(field%first, dim=1))'. This is "
            "not valid within a WHERE in Fortran." in str(err.value))

    # An array with no dimensions raises an exception
    array_type = ArrayType(REAL_TYPE, [10])
    symbol = DataSymbol("a", array_type)
    array = ArrayReference(symbol)
    with pytest.raises(InternalError) as excinfo:
        Fparser2Reader._array_notation_rank(array)
    assert ("ArrayReference malformed or incomplete: must have one or more "
            "children representing array-index expressions but array 'a' has "
            "none" in str(excinfo.value))

    # If array syntax notation is found, it must be for all elements
    # in that dimension
    array_type = ArrayType(REAL_TYPE, [10, 10, 10])
    symbol = DataSymbol("a", array_type)
    lbound_op1 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])
    ubound_op1 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol), ("dim", Literal("1", INTEGER_TYPE))])
    lbound_op3 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.LBOUND,
        [Reference(symbol), ("dim", Literal("3", INTEGER_TYPE))])
    ubound_op3 = IntrinsicCall.create(
        IntrinsicCall.Intrinsic.UBOUND,
        [Reference(symbol), ("dim", Literal("3", INTEGER_TYPE))])

    range1 = Range.create(lbound_op1, ubound_op1)
    range2 = Range.create(lbound_op3, ubound_op3)
    one = Literal("1", INTEGER_TYPE)
    array = ArrayReference.create(symbol, [range1, one.copy(), range2])
    result = Fparser2Reader._array_notation_rank(array)
    # Two array dimensions use array notation.
    assert result == 2

    # Make one of the array notation dimensions differ from what is required.
    range2 = Range.create(lbound_op3.copy(), one.copy())
    array = ArrayReference.create(symbol, [range1.copy(), one.copy(),
                                           range2.copy()])
    with pytest.raises(NotImplementedError) as excinfo:
        Fparser2Reader._array_notation_rank(array)
    assert ("Only array notation of the form my_array(:, :, ...) is "
            "supported." in str(excinfo.value))


def test_get_routine_schedules_wrong_module(parser):
    '''Test that get_routine_schedules() raises the expected errors if there
    are no or too many modules in the supplied parse tree.'''
    reader = FortranStringReader("subroutine mine()\n"
                                 "end subroutine mine\n")
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test without a module.
    with pytest.raises(GenerationError) as err:
        _ = processor.get_routine_schedules("dummy_code", ast)
    assert ("The parse tree supplied to get_routine_schedules() must contain "
            "a single module but found none when searching for kernel "
            "'dummy_code'" in str(err.value))
    reader = FortranStringReader("module my_mod1\n"
                                 "contains\n"
                                 "  subroutine mine()\n"
                                 "  end subroutine mine\n"
                                 "end module my_mod1\n"
                                 "module my_mod2\n"
                                 "end module my_mod2\n")
    ast = parser(reader)
    # Test with two modules.
    with pytest.raises(GenerationError) as err:
        _ = processor.get_routine_schedules("dummy_code", ast)
    assert ("The parse tree supplied to get_routine_schedules() must contain "
            "a single module but found more than one (['my_mod1', 'my_mod2']) "
            "when searching for kernel 'dummy_code'" in str(err.value))


def test_get_routine_schedules_empty_subroutine(parser):
    ''' Tests the fp2Reader get_routine_schedules method with an empty
    subroutine.
    '''
    reader = FortranStringReader(FAKE_KERNEL_METADATA)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test properly formed but empty kernel schedule
    schedule = processor.get_routine_schedules("dummy_code", ast)[0]
    assert isinstance(schedule, Routine)
    assert schedule.name == "dummy_code"

    # Test that we get an error for a nonexistent subroutine name
    with pytest.raises(GenerationError) as error:
        _ = processor.get_routine_schedules("nonexistent_code", ast)
    assert ("Could not find subroutine or interface 'nonexistent_code' in the "
            "module 'dummy_mod'" in str(error.value))

    # Test corrupting ast by deleting subroutine
    del ast.content[0].content[2]
    with pytest.raises(GenerationError) as error:
        schedule = processor.get_routine_schedules("dummy_code", ast)
    assert ("Could not find subroutine or interface 'dummy_code' in the "
            "module 'dummy_mod'" in str(error.value))


def test_get_routine_schedules_dummy_subroutine(parser):
    ''' Tests the fparser2Reader get_routine_schedules method with a simple
    subroutine.
    '''
    dummy_kernel_metadata = '''
    module dummy_mod
      use argument_mod
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                              &
              (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
                 arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_real, gh_inc,       w1)      &
               /)
         integer :: operates_on = cell_column
       contains
         procedure, nopass :: code => dummy_code
      end type dummy_type
    contains
     subroutine dummy_code(f1, f2, f3)
        real(wp), dimension(:,:), intent(in)  :: f1
        real(wp), dimension(:,:), intent(out)  :: f2
        real(wp), dimension(:,:) :: f3
        f2 = f1 + 1
      end subroutine dummy_code
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_kernel_metadata)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test properly formed kernel module
    schedules = processor.get_routine_schedules("dummy_code", ast)
    assert len(schedules) == 1
    assert isinstance(schedules[0], Routine)

    # Test that a kernel subroutine without Execution_Part still creates a
    # valid KernelSchedule
    del ast.content[0].content[2].content[1].content[2]
    schedules = processor.get_routine_schedules("dummy_code", ast)
    assert len(schedules) == 1
    assert isinstance(schedules[0], Routine)
    assert not schedules[0].children


def test_get_routine_schedules_no_args_subroutine(parser):
    ''' Tests the fparser2Reader get_routine_schedule method with a simple
    subroutine with no arguments.
    '''
    dummy_kernel_metadata = '''
    module dummy_mod
      use argument_mod
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                              &
              (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
                 arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_real, gh_inc,       w1)      &
               /)
         integer :: operates_on = cell_column
       contains
         procedure, nopass :: code => dummy_code
      end type dummy_type
    contains
     subroutine dummy_code()
        real(wp), dimension(:,:) :: f3
        f3 = f3 + 1
      end subroutine dummy_code
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_kernel_metadata)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test kernel with no arguments, should still proceed
    schedules = processor.get_routine_schedules("dummy_code", ast)
    assert isinstance(schedules[0], Routine)
    # TODO: In the future we could validate that metadata matches
    # the kernel arguments, then this test would fail. Issue #288


def test_get_routine_schedules_unmatching_arguments(parser):
    ''' Tests the fparser2Reader generate_schedule with unmatching kernel
    arguments and declarations raises the appropriate exception.
    '''
    dummy_kernel_metadata = '''
    module dummy_mod
      use kernel_mod
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                              &
              (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
                 arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_real, gh_inc,       w1)      &
               /)
         integer :: operates_on = cell_column
       contains
         procedure, nopass :: code => dummy_code
      end type dummy_type
    contains
     subroutine dummy_code(f1, f2, f3, f4)
        real(wp), dimension(:,:), intent(in)  :: f1
        real(wp), dimension(:,:), intent(out)  :: f2
        real(wp), dimension(:,:) :: f3
        f2 = f1 + 1
      end subroutine dummy_code
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_kernel_metadata)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test exception for unmatching argument list
    with pytest.raises(InternalError) as error:
        _ = processor.get_routine_schedules("dummy_code", ast)
    assert ("PSyclone internal error: The argument list ['f1', 'f2', 'f3', "
            "'f4'] for routine 'dummy_code' does not match the variable "
            "declarations:\n"
            "REAL(KIND = wp), DIMENSION(:, :), INTENT(IN) :: f1\n"
            "REAL(KIND = wp), DIMENSION(:, :), INTENT(OUT) :: f2\n"
            "REAL(KIND = wp), DIMENSION(:, :) :: f3\n"
            "(Note that PSyclone does not support implicit declarations.) "
            "Specific PSyIR error is \"Could not find 'f4' in the "
            "Symbol Table.\"." in str(error.value))


@pytest.mark.parametrize("interface_code",
                         ["        module procedure dummy_code_32\n"
                          "        module procedure dummy_CODE_64\n",
                          "        module procedure dummy_code_32, "
                          "dummy_codE_64\n",
                          "        procedure dummy_code_32\n"
                          "        procedure Dummy_Code_64\n"])
def test_get_routine_schedules_interface(interface_code, parser):
    '''
    Test that get_routine_schedules returns a schedule for each routine named
    in an interface. We test various ways of specifying the procedures in
    an interface.

    '''
    dummy_kernel_metadata = f'''
    module dummy_mod
      use kernel_mod
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                              &
              (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
                 arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_real, gh_inc,       w1)      &
               /)
         integer :: operates_on = cell_column
      end type dummy_type

      interface dummy_code
        {interface_code}
      end interface dummy_code

    contains
     subroutine dummy_CODE_32(f1)
        real*4, dimension(:,:), intent(in)  :: f1
      end subroutine dummy_code_32
     subroutine dummy_code_64(f1)
        real*8, dimension(:,:), intent(in)  :: f1
      end subroutine dummy_code_64
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_kernel_metadata)
    ast = parser(reader)
    processor = Fparser2Reader()
    scheds = processor.get_routine_schedules("dummy_code", ast)
    assert len(scheds) == 2
    assert scheds[0].name.lower() == "dummy_code_32"
    assert scheds[1].name.lower() == "dummy_code_64"


@pytest.mark.usefixtures("f2008_parser")
def test_get_partial_datatype():
    '''Test that the _get_partial_datatype method of Fparser2Reader
    works as expected.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Entry in symbol table with unmodified properties.
    reader = FortranStringReader("integer :: l1=2")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, {})
    assert isinstance(datatype, ScalarType)
    assert isinstance(init, Literal)
    assert init.parent is None
    assert datatype.intrinsic is ScalarType.Intrinsic.INTEGER
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]

    # Entry in symbol table with partial information. Example has one
    # unsupported attribute (and no others) and an unsupported assignment.
    reader = FortranStringReader("integer, pointer :: l1 => null()")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, {})
    assert isinstance(datatype, ScalarType)
    assert isinstance(init, CodeBlock)
    assert init.parent is None
    assert datatype.intrinsic is ScalarType.Intrinsic.INTEGER
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]

    # Entry in symbol table with partial information. Example has one
    # unsupported attribute and one supported attribute.
    reader = FortranStringReader("real*4, target, dimension(10,20) :: l1")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, {})
    assert isinstance(datatype, ArrayType)
    assert init is None
    assert datatype.intrinsic is ScalarType.Intrinsic.REAL
    assert datatype.precision == 4
    assert datatype.shape[0].upper.value == '10'
    assert datatype.shape[1].upper.value == '20'
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]

    # No entry in symbol table.
    # Notice the space before complex keyword. This avoids it being
    # treated as a comment.
    reader = FortranStringReader(" complex :: c\n")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    dtype, init = processor._get_partial_datatype(node, fake_parent, {})
    assert dtype is None
    assert init is None
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]

    # Multiple variables in the declaration are also supported but are
    # not used by PSyclone at the moment.
    reader = FortranStringReader(
        "integer, pointer :: l1 => null(), l2 => null()")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, {})
    assert isinstance(datatype, ScalarType)
    assert isinstance(init, CodeBlock)
    assert init.parent is None
    assert datatype.intrinsic is ScalarType.Intrinsic.INTEGER
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations():
    '''Test that process_declarations method of Fparser2Reader
    converts the fparser2 declarations to symbols in the provided
    parent Kernel Schedule.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader("integer :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = symtab.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert l1_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l1_var.is_automatic

    reader = FortranStringReader("Real      ::      l2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l2_var = symtab.lookup("l2")
    assert l2_var.name == "l2"
    assert isinstance(l2_var.datatype, ScalarType)
    assert l2_var.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert l2_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l2_var.is_automatic

    reader = FortranStringReader("LOGICAL      ::      b")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    b_var = symtab.lookup("b")
    assert b_var.name == "b"
    # Symbol should be public by default
    assert b_var.visibility == Symbol.Visibility.PUBLIC
    assert isinstance(b_var.datatype, ScalarType)
    assert b_var.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert b_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert b_var.is_automatic

    # public/private attribute
    reader = FortranStringReader("real, public :: p2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert symtab.lookup("p2").visibility == Symbol.Visibility.PUBLIC
    reader = FortranStringReader("real, private :: p3, p4")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert symtab.lookup("p3").visibility == Symbol.Visibility.PRIVATE
    assert symtab.lookup("p4").visibility == Symbol.Visibility.PRIVATE

    # Initialisations of static constant values (parameters)
    reader = FortranStringReader("integer, parameter :: i1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    newsymbol = symtab.lookup("i1")
    assert newsymbol.is_constant
    assert isinstance(newsymbol.initial_value, Literal)
    assert newsymbol.initial_value.value == "1"

    reader = FortranStringReader("real, parameter :: i2 = 2.2, i3 = 3.3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert symtab.lookup("i2").initial_value.value == "2.2"
    assert symtab.lookup("i3").initial_value.value == "3.3"

    # Initialisation with constant expressions
    reader = FortranStringReader("real, parameter :: i4 = 1.1, i5 = i4 * 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert symtab.lookup("i4").initial_value.value == "1.1"
    assert isinstance(symtab.lookup("i5").initial_value, BinaryOperation)

    # Initialisation with a constant expression (1) and with a symbol (val1)
    reader = FortranStringReader("integer, parameter :: val1 = 1, val2 = val1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("val1").initial_value.value == "1"
    assert isinstance(
        fake_parent.symbol_table.lookup("val2").initial_value, Reference)
    assert fake_parent.symbol_table.lookup("val2").initial_value.symbol == \
        fake_parent.symbol_table.lookup("val1")

    # Initialisation with a complex constant expression
    symtab.add(DataSymbol("precisionkind", INTEGER_TYPE, is_constant=True,
                          initial_value=4))
    reader = FortranStringReader(
        "integer, parameter :: val3 = 2 * (val1 + val2) + 2_precisionkind")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    # Val3 has been given an initial_value expression
    val3 = fake_parent.symbol_table.lookup("val3")
    assert isinstance(val3.initial_value, BinaryOperation)
    assert val3.is_constant
    # The new symbol (precisionkind) has been added to the parent Symbol Table
    assert fake_parent.symbol_table.lookup("precisionkind")

    # Initialisation of a variable
    reader = FortranStringReader(
        "integer :: val4 = 2 * (val1 + val2) + 2_precisionkind")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    val4 = fake_parent.symbol_table.lookup("val4")
    assert val4.initial_value
    assert isinstance(val4.initial_value, BinaryOperation)
    assert val4.is_constant is False

    # Check we catch duplicated symbols
    reader = FortranStringReader("integer :: i2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Symbol 'i2' already present in SymbolTable with a defined "
            "interface" in str(error.value))

    # Initialisation of a pointer.
    reader = FortranStringReader(
        "real, dimension(:), pointer :: dptr => null()")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    ptr_sym = fake_parent.symbol_table.lookup("dptr")
    assert isinstance(ptr_sym, DataSymbol)
    assert isinstance(ptr_sym.datatype, UnsupportedFortranType)
    assert isinstance(ptr_sym.initial_value, CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_unsupportedfortrantype():
    '''Test that process_declarations method of Fparser2Reader adds
    datatype information to an UnsupportedFortranType by calling the
    get_partial_datatype method, also from Fparser2Reader.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "integer, pointer :: l1 => null(), l2 => null()")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    for varname in ("l1", "l2"):
        var_symbol = symtab.lookup(varname)
        assert isinstance(var_symbol.datatype, UnsupportedFortranType)
        assert isinstance(var_symbol.datatype.partial_datatype, ScalarType)
        assert (var_symbol.datatype.partial_datatype.intrinsic is
                ScalarType.Intrinsic.INTEGER)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_errors():
    '''Test that process_declarations method of Fparser2Reader
    raises appropriate errors for fparser2 declarations that are not
    valid Fortran.

    TODO fparser/#413 could also fix these issues.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, parameter, save :: l1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(GenerationError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("SAVE and PARAMETER attributes are not compatible but found:\n "
            "INTEGER, PARAMETER, SAVE :: l1 = 1" in str(error.value))

    reader = FortranStringReader("integer, parameter, intent(in) :: l1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(GenerationError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("INTENT and PARAMETER attributes are not compatible but found:\n "
            "INTEGER, PARAMETER, INTENT(IN) :: l1 = 1" in str(error.value))

    reader = FortranStringReader("integer, parameter, allocatable :: l1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(GenerationError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("ALLOCATABLE and PARAMETER attributes are not compatible but found"
            ":\n INTEGER, PARAMETER, ALLOCATABLE :: l1 = 1"
            in str(error.value))

    reader = FortranStringReader("integer, intent(inout), save :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(GenerationError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Multiple or duplicated incompatible attributes found in "
            "declaration:\n INTEGER, INTENT(INOUT), SAVE :: l1"
            in str(error.value))

    reader = FortranStringReader("integer, intent(in), intent(out) :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(GenerationError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Multiple or duplicated incompatible attributes found in "
            "declaration:\n INTEGER, INTENT(IN), INTENT(OUT) :: l1"
            in str(error.value))


@pytest.mark.usefixtures("f2008_parser")
def test_declarations_with_initialisations(fortran_reader):
    '''Test that Fparser2Reader keeps all the variable initialisation
    expressions.
    '''

    psyir = fortran_reader.psyir_from_source(
        """
        module test
            integer :: a = 1, aa = 4
            integer, save :: b = 1
            integer, parameter :: c = 1
            contains
            subroutine mysub()
                integer :: d = 1
                integer, save :: e = 1
                integer, parameter :: f = 1
            end subroutine mysub
        end module test
        """)

    inner_st = psyir.walk(Routine)[0].symbol_table
    asym = inner_st.lookup('a')
    aasym = inner_st.lookup('aa')
    bsym = inner_st.lookup('b')
    csym = inner_st.lookup('c')
    dsym = inner_st.lookup('d')
    esym = inner_st.lookup('e')
    fsym = inner_st.lookup('f')
    all_syms = [asym, aasym, bsym, csym, dsym, esym, fsym]

    # All initialisation variables are DataSymbols
    assert all(isinstance(sym, DataSymbol) for sym in all_syms)

    # All of the data symbols should have a StaticInterface (because they
    # either have an explicit 'save' or 'parameter' or are given an
    # initial_value with then implies 'save').
    assert all(isinstance(sym.interface, StaticInterface) for sym in all_syms)

    # All symbols should have a known data type.
    assert all(isinstance(sym.datatype, ScalarType) for sym in all_syms)

    # When it is a parameter the initial_value is defined and is_constant
    # is True.
    assert isinstance(csym.initial_value, Literal)
    assert csym.is_constant is True

    assert isinstance(fsym.initial_value, Literal)
    assert fsym.is_constant is True


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_accessibility():
    ''' Check that process_declarations behaves as expected when a visibility
    map is or is not supplied. '''
    sched = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("private :: x\n"
                                 "real :: x\n")
    fparser2spec = Specification_Part(reader).content
    _, vis_map = processor.process_access_statements(fparser2spec)
    processor.process_declarations(sched, fparser2spec, [], vis_map)
    xsym = sched.symbol_table.lookup("x")
    assert xsym.visibility == Symbol.Visibility.PRIVATE
    # Repeat but change the default visibility in the parent table
    reader = FortranStringReader("real :: y\n")
    fparser2spec = Specification_Part(reader).content
    sched.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    processor.process_declarations(sched, fparser2spec, [])
    ysym = sched.symbol_table.lookup("y")
    assert ysym.visibility == Symbol.Visibility.PRIVATE
    # Repeat but provide a visibility mapping
    reader = FortranStringReader("real :: z\n")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(
        sched, fparser2spec, [],
        visibility_map={"z": Symbol.Visibility.PRIVATE})
    zsym = sched.symbol_table.lookup("z")
    assert zsym.visibility == Symbol.Visibility.PRIVATE


@pytest.mark.usefixtures("f2008_parser")
def test_process_multiple_access_statements():
    ''' Check that process_access_statements handles code containing multiple
    access statements. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "PUBLIC  fjb_typ\n"
        "private y\n"
        "PUBLIC  fbge_ctl_typ,  fbge_typ\n"
        "private :: x\n")
    fparser2spec = Specification_Part(reader).content
    _, vis_map = processor.process_access_statements(fparser2spec)
    assert vis_map["y"] == Symbol.Visibility.PRIVATE
    assert vis_map["x"] == Symbol.Visibility.PRIVATE
    assert vis_map["fjb_typ"] == Symbol.Visibility.PUBLIC
    assert vis_map["fbge_typ"] == Symbol.Visibility.PUBLIC


@pytest.mark.usefixtures("f2008_parser")
def test_process_unsupported_declarations(fortran_reader):
    ''' Check that the frontend handles unsupported declarations by
    creating symbols of UnsupportedFortranType. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Multiple symbols with a single attribute
    reader = FortranStringReader("integer, private, pointer :: d, e")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    dsym = fake_parent.symbol_table.lookup("d")
    assert isinstance(dsym.datatype, UnsupportedFortranType)
    assert dsym.datatype.declaration == "INTEGER, PRIVATE, POINTER :: d"
    esym = fake_parent.symbol_table.lookup("e")
    assert isinstance(esym.datatype, UnsupportedFortranType)
    assert esym.datatype.declaration == "INTEGER, PRIVATE, POINTER :: e"

    # Multiple attributes
    reader = FortranStringReader(
        "INTEGER, PRIVATE, DIMENSION(3), POINTER :: f, g")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fsym = fake_parent.symbol_table.lookup("f")
    assert isinstance(fsym.datatype, UnsupportedFortranType)
    assert (fsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3), POINTER :: f")
    gsym = fake_parent.symbol_table.lookup("g")
    assert isinstance(gsym.datatype, UnsupportedFortranType)
    assert (gsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3), POINTER :: g")

    # Test with unsupported intrinsic type. Note the space before complex
    # below which stops the line being treated as a comment.
    reader = FortranStringReader(" complex     ::      c2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    c2sym = fake_parent.symbol_table.lookup("c2")
    assert isinstance(c2sym.datatype, UnsupportedFortranType)
    assert c2sym.datatype.declaration == "COMPLEX :: c2"

    # Char lengths are not supported
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character :: l*4\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnsupportedFortranType)
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character(len=4) :: l\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnsupportedFortranType)

    # Test that CodeBlocks and refernces to variables initialised with a
    # CodeBlock are handled correctly
    reader = FortranStringReader(
        "INTEGER, PARAMETER :: happy=1, fbsp=sin(1), "
        " sad=fbsp")
    fparser2spec = Specification_Part(reader).content[0]
    # We change SIN for something that creates a CodeBlock
    fparser2spec.items[2].items[1].items[3].items[1].items[0].string = "CBLOCK"
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fbsym = fake_parent.symbol_table.lookup("fbsp")
    assert fbsym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert isinstance(fbsym.initial_value, CodeBlock)
    # The first parameter should have been handled correctly
    hsym = fake_parent.symbol_table.lookup("happy")
    assert hsym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert hsym.initial_value.value == "1"
    # As should the third
    ssym = fake_parent.symbol_table.lookup("sad")
    assert ssym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert isinstance(ssym.initial_value, Reference)
    assert ssym.initial_value.symbol.name == "fbsp"


@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_decln_initial_value(monkeypatch):
    ''' Check that an invalid constant value for a parameter is handled
    correctly. '''
    fake_parent = KernelSchedule("dummy_schedule")
    reader = FortranStringReader(
        "INTEGER, PRIVATE, PARAMETER :: happy=1, "
        "fbsp=SELECTED_REAL_KIND(6,37), sad=fbsp")
    fparser2spec = Specification_Part(reader).content[0]
    # This error condition is very difficult to trigger so we monkeypatch
    # the DataSymbol class itself with a setter that raises a ValueError
    # for anything other than a Literal.

    class BrokenDataSymbol(DataSymbol):
        ''' Sub-class of DataSymbol with `initial_value` setter patched
        so that it raises a ValueError for anything other than a Literal. '''
        @property
        def initial_value(self):
            return self._initial_value

        @initial_value.setter
        def initial_value(self, value):
            if isinstance(value, Literal):
                self._initial_value = value
            else:
                raise ValueError("")

    # At this point the fparser2 module will already have 'DataSymbol' in
    # its namespace (due to the imports at the top of this file) so we
    # monkeypatch that entry.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyir.frontend import fparser2
    monkeypatch.setattr(fparser2, "DataSymbol", BrokenDataSymbol)

    processor = Fparser2Reader()
    processor.process_declarations(fake_parent, [fparser2spec], [])
    hsym = fake_parent.symbol_table.lookup("happy")
    assert hsym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert hsym.initial_value.value == "1"
    fbsym = fake_parent.symbol_table.lookup("fbsp")
    assert isinstance(fbsym.datatype, UnsupportedFortranType)
    assert (fbsym.datatype.declaration == "INTEGER, PRIVATE, PARAMETER :: "
            "fbsp = SELECTED_REAL_KIND(6, 37)")
    sadsym = fake_parent.symbol_table.lookup("sad")
    assert isinstance(sadsym.datatype, UnsupportedFortranType)
    assert (sadsym.datatype.declaration == "INTEGER, PRIVATE, PARAMETER :: "
            "sad = fbsp")

    # Now do the same but the UnsupportedType constant_value is also the symbol
    # tagged as 'own_routine_symbol'. This is not recoverable.
    fake_parent = KernelSchedule("fbsp")
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("The fparser2 frontend does not support declarations where the "
            "routine name is of UnsupportedType, but found this case in "
            "'fbsp'." in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_decln_duplicate_symbol():
    ''' Check that we raise the expected error when an unsupported declaration
    of only one symbol clashes with an existing entry in the symbol table. '''
    fake_parent = KernelSchedule("dummy_schedule")
    fake_parent.symbol_table.add(Symbol("var"))
    processor = Fparser2Reader()
    # Note leading white space to ensure fparser doesn't identify a comment
    reader = FortranStringReader(" complex var")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An entry for symbol 'var' is already in the" in str(err.value)


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("precision", [1, 2, 4, 8, 16, 32])
@pytest.mark.parametrize("type_name,fort_name",
                         [(ScalarType.Intrinsic.INTEGER, "integer"),
                          (ScalarType.Intrinsic.REAL, "real"),
                          (ScalarType.Intrinsic.BOOLEAN, "logical")])
def test_process_declarations_precision(precision, type_name, fort_name):
    '''Test that process_declarations method of Fparser2Reader converts
    the fparser2 declarations with explicit precision of the form
    datatype*precision e.g. real*8, to symbols with the expected
    precision in the provided parent Kernel Schedule.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader(f"{fort_name}*{precision} :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = fake_parent.symbol_table.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == type_name
    assert l1_var.datatype.precision == precision
    assert l1_var.is_automatic


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_double_precision():
    '''Test that process_declarations method of Fparser2Reader converts
    the fparser2 declarations specified as double precision to symbols
    with the expected precision.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("double precision :: x")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    x_var = fake_parent.symbol_table.lookup("x")
    assert x_var.name == 'x'
    assert isinstance(x_var.datatype, ScalarType)
    assert x_var.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert x_var.datatype.precision == ScalarType.Precision.DOUBLE
    assert x_var.is_automatic


@pytest.mark.usefixtures("f2008_parser")
def test_process_array_declarations():
    ''' Test that Fparser2Reader.process_declarations() handles various forms
    of array declaration.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # RHS array specifications
    reader = FortranStringReader("integer :: l3(l1)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l3_var = fake_parent.symbol_table.lookup("l3")
    assert l3_var.name == 'l3'
    assert isinstance(l3_var.datatype, ArrayType)
    assert l3_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert len(l3_var.datatype.shape) == 1
    assert l3_var.datatype.precision == ScalarType.Precision.UNDEFINED

    reader = FortranStringReader("integer :: l4(l1, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l4_var = fake_parent.symbol_table.lookup("l4")
    assert l4_var.name == 'l4'
    assert isinstance(l4_var.datatype, ArrayType)
    assert l4_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert len(l4_var.datatype.shape) == 2
    assert l4_var.datatype.precision == ScalarType.Precision.UNDEFINED

    reader = FortranStringReader("integer :: l5(2), l6(3)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l5_datatype = fake_parent.symbol_table.lookup("l5").datatype
    assert len(l5_datatype.shape) == 1
    assert isinstance(l5_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l5_datatype.shape[0].lower, Literal)
    assert isinstance(l5_datatype.shape[0].upper, Literal)
    assert l5_datatype.shape[0].lower.value == '1'
    assert l5_datatype.shape[0].upper.value == '2'
    assert (l5_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l5_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    l6_datatype = fake_parent.symbol_table.lookup("l6").datatype
    assert len(l6_datatype.shape) == 1
    assert isinstance(l6_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l6_datatype.shape[0].lower, Literal)
    assert isinstance(l6_datatype.shape[0].upper, Literal)
    assert l6_datatype.shape[0].lower.value == '1'
    assert l6_datatype.shape[0].upper.value == '3'
    assert (l6_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l6_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)

    # Test that component-array-spec has priority over dimension attribute
    reader = FortranStringReader("integer, dimension(2) :: l7(3, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l7_datasymbol = fake_parent.symbol_table.lookup("l7")
    assert l7_datasymbol.name == 'l7'
    assert len(l7_datasymbol.shape) == 2
    l7_datatype = l7_datasymbol.datatype
    assert isinstance(l7_datatype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l7_datatype.shape[0].upper, Literal)
    assert l7_datatype.shape[0].upper.value == '3'
    assert (l7_datatype.shape[0].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l7_datatype.shape[0].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)
    assert isinstance(l7_datatype.shape[1], ArrayType.ArrayBounds)
    assert isinstance(l7_datatype.shape[1].upper, Literal)
    assert l7_datatype.shape[1].upper.value == '2'
    assert (l7_datatype.shape[1].upper.datatype.intrinsic ==
            ScalarType.Intrinsic.INTEGER)
    assert (l7_datatype.shape[1].upper.datatype.precision ==
            ScalarType.Precision.UNDEFINED)

    # Allocatable
    reader = FortranStringReader("integer, allocatable :: l8(:)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l8")
    assert symbol.name == "l8"
    assert symbol.datatype.precision == ScalarType.Precision.UNDEFINED
    assert symbol.shape == [ArrayType.Extent.DEFERRED]

    reader = FortranStringReader("integer, allocatable, dimension(:,:) :: l9")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l9")
    assert symbol.name == "l9"
    assert symbol.shape == [ArrayType.Extent.DEFERRED,
                            ArrayType.Extent.DEFERRED]

    # Unknown extents but not allocatable
    reader = FortranStringReader("integer :: l10(:, :)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l10")
    assert symbol.name == "l10"
    assert symbol.shape == [ArrayType.Extent.ATTRIBUTE,
                            ArrayType.Extent.ATTRIBUTE]

    # Extent given by variable with UnsupportedFortranType
    udim = DataSymbol("udim", UnsupportedFortranType("integer :: udim"),
                      interface=UnresolvedInterface())
    fake_parent.symbol_table.add(udim)
    reader = FortranStringReader("integer :: l11(udim)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l11")
    assert symbol.name == "l11"
    assert len(symbol.shape) == 1
    # Upper bound of extent should be the udim Symbol
    reference = symbol.shape[0].upper
    assert isinstance(reference, Reference)
    assert reference.name == "udim"
    assert reference.symbol is udim
    assert isinstance(reference.symbol.datatype, UnsupportedFortranType)

    # Extent given by variable with UnresolvedType
    ddim = DataSymbol("ddim", UnresolvedType(),
                      interface=UnresolvedInterface())
    fake_parent.symbol_table.add(ddim)
    reader = FortranStringReader("integer :: l12(ddim)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l12")
    assert symbol.name == "l12"
    assert len(symbol.shape) == 1
    assert isinstance(symbol.shape[0].lower, Literal)
    # Upper bound of extent should now be the ddim Symbol
    reference = symbol.shape[0].upper
    assert reference.name == "ddim"
    assert reference.symbol is ddim
    assert isinstance(reference.symbol.datatype, UnresolvedType)

    # Extent given by range
    reader = FortranStringReader("integer :: var(2:4)")
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("var")
    assert len(symbol.shape) == 1
    assert symbol.datatype.shape[0].lower.value == "2"
    assert symbol.datatype.shape[0].upper.value == "4"

    reader = FortranStringReader("integer :: var1(2:ddim)")
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("var1")
    assert len(symbol.shape) == 1
    assert isinstance(symbol.datatype.shape[0], ArrayType.ArrayBounds)
    assert symbol.datatype.shape[0].lower.value == "2"
    assert symbol.datatype.shape[0].upper.symbol is ddim

    reader = FortranStringReader("integer :: var2(4:)")
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("var2")
    assert len(symbol.shape) == 1
    # Shape should be an ArrayBounds with known lower bound and ATTRIBUTE
    # upper.
    assert isinstance(symbol.datatype.shape[0], ArrayType.ArrayBounds)
    assert symbol.datatype.shape[0].lower.value == "4"
    assert symbol.datatype.shape[0].upper == ArrayType.Extent.ATTRIBUTE


@pytest.mark.usefixtures("f2008_parser")
def test_process_not_supported_declarations():
    '''Test that process_declarations method raises the proper errors when
    declarations contain unsupported attributes.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, external :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("arg1").datatype,
                      UnsupportedFortranType)

    reader = FortranStringReader("real, allocatable :: p3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("p3").datatype,
                      UnsupportedFortranType)

    reader = FortranStringReader("class(my_type), intent(in) :: carg")
    # Set reader to free format (otherwise this is a comment in fixed format)
    reader.set_format(FortranFormat(True, True))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    sym = fake_parent.symbol_table.lookup("carg")
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert (sym.datatype.declaration.lower() ==
            "class(my_type), intent(in) :: carg")

    # Allocatable but with specified extent. This is invalid Fortran but
    # fparser2 doesn't spot it (see fparser/#229).
    reader = FortranStringReader("integer, allocatable :: l10(5)")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An array with defined extent cannot have the ALLOCATABLE" \
        in str(err.value)

    reader = FortranStringReader("integer, allocatable, dimension(n) :: l10")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An array with defined extent cannot have the ALLOCATABLE" \
        in str(err.value)

    reader = FortranStringReader("integer :: l11")
    fparser2spec = Specification_Part(reader).content[0]
    # Break the parse tree
    fparser2spec.items = ("hello", fparser2spec.items[1],
                          fparser2spec.items[2])
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l11sym = fake_parent.symbol_table.lookup("l11")
    assert isinstance(l11sym.datatype, UnsupportedFortranType)

    # Assumed-size array with specified upper bound. fparser2 does spot that
    # this is invalid so we have to break the parse tree it produces for
    # a valid case.
    reader = FortranStringReader("integer :: l12(:)")
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    # Break the parse tree
    aspec = walk(fparser2spec, Fortran2003.Assumed_Shape_Spec)[0]
    aspec.items = (None, Fortran2003.Int_Literal_Constant('4', None))
    with pytest.raises(GenerationError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("assumed-shape array declaration with only an upper bound (: 4). "
            "This is not valid Fortran" in str(err.value))


def test_process_save_attribute_declarations(parser):
    ''' Test that the SAVE attribute in a declaration is supported. '''

    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Test with no context about where the declaration. Not even that is
    # in the Specification_Part.
    reader = FortranStringReader("integer, save :: var1")
    fparser2spec = Type_Declaration_Stmt(reader)
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("var1").datatype,
                      ScalarType)
    assert isinstance(fake_parent.symbol_table.lookup("var1").interface,
                      StaticInterface)

    # Test with no context about where the declaration is.
    reader = FortranStringReader("integer, save :: var2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("var2").datatype,
                      ScalarType)
    assert isinstance(fake_parent.symbol_table.lookup("var2").interface,
                      StaticInterface)

    # Test with a subroutine.
    reader = FortranStringReader(
        "subroutine name()\n"
        "integer, save :: var3\n"
        "end subroutine name")
    fparser2spec = parser(reader).content[0].content[1]
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    assert isinstance(fake_parent.symbol_table.lookup("var3").datatype,
                      ScalarType)
    assert isinstance(fake_parent.symbol_table.lookup("var3").interface,
                      StaticInterface)

    # Test with a module.
    reader = FortranStringReader(
        "module modulename\n"
        "integer, save :: var4\n"
        "end module modulename")
    fparser2spec = parser(reader).content[0].content[1]
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    var4 = fake_parent.symbol_table.lookup("var4")
    assert var4.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert isinstance(fake_parent.symbol_table.lookup("var4").interface,
                      StaticInterface)

    # Test that when it is part of an UnsupportedType (target attribute in
    # this case) it becomes an UnknownInterface.
    reader = FortranStringReader("integer, target :: var5")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("var5").datatype,
                      UnsupportedFortranType)
    assert isinstance(fake_parent.symbol_table.lookup("var5").interface,
                      UnknownInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_intent():
    '''Test that process_declarations method handles various different
    specifications of variable attributes.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, intent(in) :: arg1, arg1a")
    fparser2spec = Specification_Part(reader).content[0]
    arg_list = [Fortran2003.Name("arg1"), Fortran2003.Name("arg1a")]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    # Check that the interface is correct and distinct for each symbol.
    interface1 = fake_parent.symbol_table.lookup("arg1").interface
    assert interface1.access == ArgumentInterface.Access.READ
    interface1a = fake_parent.symbol_table.lookup("arg1a").interface
    assert interface1a.access == ArgumentInterface.Access.READ
    assert interface1a is not interface1

    reader = FortranStringReader("integer, intent( IN ) :: arg2")
    arg_list.append(Fortran2003.Name("arg2"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg2").interface.access == \
        ArgumentInterface.Access.READ

    reader = FortranStringReader("integer, intent( Out ) :: arg3")
    arg_list.append(Fortran2003.Name("arg3"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg3").interface.access == \
        ArgumentInterface.Access.WRITE

    reader = FortranStringReader("integer, intent ( InOut ) :: arg4")
    arg_list.append(Fortran2003.Name("arg4"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg4").interface.access is \
        ArgumentInterface.Access.READWRITE

    reader = FortranStringReader("integer, intent ( invalid ) :: arg5")
    arg_list.append(Fortran2003.Name("arg5"))
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(
            fake_parent, [fparser2spec], arg_list, {})
    del arg_list[-1]
    assert "Could not process " in str(err.value)
    assert "Unexpected intent attribute " in str(err.value)

    # Test that argument intent is left as UNKNOWN when not available in the
    # declaration
    reader = FortranStringReader("integer :: arg6")
    arg_list.append(Fortran2003.Name("arg6"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg6").interface.access is \
        ArgumentInterface.Access.UNKNOWN


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_stmt_functions():
    '''Test that process_declarations method handles statement functions
    appropriately.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # If 'a' is not declared it could be a statement function, which are
    # unsupported and produce a NotImplementedError.
    reader = FortranStringReader("a(x) = 1")
    fparser2spec = Stmt_Function_Stmt(reader)
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process '" in str(error.value)
    assert "'. Statement Function declarations are not supported." \
        in str(error.value)

    # If 'a' is declared in the symbol table as an array, it is an array
    # assignment which belongs in the execution part.
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE])
    fake_parent.symbol_table.add(
        DataSymbol('a', array_type))
    fake_parent.symbol_table.add(DataSymbol('x', REAL_TYPE))
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert len(fake_parent.children) == 1
    array = fake_parent.children[0].children[0]
    assert isinstance(array, ArrayReference)
    assert array.name == "a"

    # Test that it works with multi-dimensional arrays
    fake_parent = KernelSchedule("dummy_schedule")
    reader = FortranStringReader("b(x, y) = 1")
    fparser2spec = Stmt_Function_Stmt(reader)
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                       ArrayType.Extent.ATTRIBUTE])
    fake_parent.symbol_table.add(DataSymbol('b', array_type))
    fake_parent.symbol_table.add(DataSymbol('x', INTEGER_TYPE))
    fake_parent.symbol_table.add(DataSymbol('y', INTEGER_TYPE))
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert len(fake_parent.children) == 1
    array = fake_parent.children[0].children[0]
    assert isinstance(array, ArrayReference)
    assert array.name == "b"

    # Test that if symbol is not an array, it raises InternalError
    fake_parent.symbol_table.lookup('b').datatype = INTEGER_TYPE
    with pytest.raises(InternalError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process '" in str(error.value)
    assert "'. Symbol 'b' is in the SymbolTable but it is not an array as " \
        "expected, so it can not be recovered as an array assignment." \
        in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_unsupported_node():
    ''' Check that process_declarations raises the expected error if it
    encounters an unsupported fparser2 node. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("integer, parameter :: r_def = KIND(1.0D0)\n"
                                 "real(kind=r_def) :: var2")
    fparser2spec = Specification_Part(reader)
    # Append an fparser2 node that is not a valid/supported declaration
    fparser2spec.content.append(Fortran2003.Name("wrong"))
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(
            fake_parent, fparser2spec.content, [], {})
    assert "fparser2 node of type 'Name' not supported" in str(err.value)


@pytest.mark.usefixtures("f2008_parser")
def test_parse_array_dimensions_attributes():
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''

    sym_table = SymbolTable()
    reader = FortranStringReader("dimension(:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert shape == [None]

    reader = FortranStringReader("dimension(:,:,:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert shape == [None, None, None]

    reader = FortranStringReader("dimension(3,5)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 2
    assert shape[0][0].value == "1"
    assert shape[0][1].value == "3"
    assert shape[1][0].value == "1"
    assert shape[1][1].value == "5"

    sym_table.add(DataSymbol('var1', INTEGER_TYPE))
    sym_table.add(DataSymbol('var1_upper', INTEGER_TYPE))

    reader = FortranStringReader("dimension(var1)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 1
    assert shape[0][0].value == "1"
    assert shape[0][1].symbol == sym_table.lookup('var1')

    reader = FortranStringReader("dimension(0:3,var1)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    # First dim is specified with both lower and upper bounds so should
    # have a tuple
    assert isinstance(shape[0], tuple)
    assert len(shape[0]) == 2
    assert shape[0][0].value == "0"
    assert shape[0][1].value == "3"
    assert shape[1][0].value == "1"
    assert shape[1][1].symbol is sym_table.lookup('var1')

    reader = FortranStringReader("dimension(0:3,var1:var1_upper)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert isinstance(shape[0], tuple)
    assert len(shape[0]) == 2
    assert shape[0][0].value == "0"
    assert shape[0][1].value == "3"
    assert isinstance(shape[1], tuple)
    assert len(shape[1]) == 2
    assert shape[1][0].symbol is sym_table.lookup('var1')
    assert shape[1][1].symbol is sym_table.lookup('var1_upper')

    # Assumed size arrays not supported
    reader = FortranStringReader("dimension(*)")
    fparser2spec = Dimension_Attr_Spec(reader)
    with pytest.raises(NotImplementedError) as error:
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert "Assumed-size arrays are not supported." in str(error.value)

    # Explicit shape symbols must be integer
    reader = FortranStringReader("dimension(var2)")
    fparser2spec = Dimension_Attr_Spec(reader)
    with pytest.raises(NotImplementedError) as error:
        sym_table.add(DataSymbol("var2", REAL_TYPE))
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert ("Only scalar integer literals or symbols are supported for "
            "explicit-shape array declarations.") in str(error.value)

    # Explicit shape symbols can only be Literal or Symbol
    with pytest.raises(NotImplementedError) as error:
        class UnrecognizedType():
            '''Type guaranteed to not be part of the _parse_dimensions
            conditional type handler.'''
        fparser2spec.items[1].items[0].items[1].__class__ = UnrecognizedType
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert ("Only scalar integer literals or symbols are supported for "
            "explicit-shape array declarations.") in str(error.value)

    # Shape specified by an unknown Symbol
    reader = FortranStringReader("dimension(var3)")
    fparser2spec = Dimension_Attr_Spec(reader)
    csym = sym_table.new_symbol("some_mod", symbol_type=ContainerSymbol)
    vsym = sym_table.new_symbol("var3", interface=ImportInterface(csym))
    # pylint: disable=unidiomatic-typecheck
    assert type(vsym) is Symbol
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 1
    assert shape[0][0].value == "1"
    assert isinstance(shape[0][1], Reference)
    # Symbol is the same object but is now a DataSymbol
    assert shape[0][1].symbol is vsym
    assert isinstance(shape[0][1].symbol, DataSymbol)
    assert shape[0][1].symbol.name == "var3"
    assert isinstance(shape[0][1].symbol.interface, ImportInterface)

    # Test dimension and intent arguments together
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, intent(in), dimension(:) :: array3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec],
                                   [Name("array3")])
    array3 = fake_parent.symbol_table.lookup("array3")
    assert array3.name == "array3"
    assert array3.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert array3.shape == [ArrayType.Extent.ATTRIBUTE]
    assert array3.interface.access is ArgumentInterface.Access.READ


@pytest.mark.usefixtures("f2008_parser")
def test_deferred_array_size():
    ''' Check that we handle the case of an array being declared with an
    extent specified by a variable that is declared after it. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, intent(in), dimension(n) :: array3\n"
                                 "integer, intent(in) :: n")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec,
                                   [Name("array3"), Name("n")])
    dim_sym = fake_parent.symbol_table.lookup("n")
    assert isinstance(dim_sym.interface, ArgumentInterface)
    assert dim_sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


@pytest.mark.usefixtures("f2008_parser")
def test_unresolved_array_size():
    ''' Check that we handle the case where we do not find an explicit
    declaration of a symbol used in the definition of an array extent. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, dimension(n) :: array3")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec, [])
    dim_sym = fake_parent.symbol_table.lookup("n")
    assert isinstance(dim_sym.interface, UnresolvedInterface)
    assert dim_sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    # Check that the lookup of the dimensioning symbol is not case sensitive
    reader = FortranStringReader("real, dimension(N) :: array4")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec, [])
    assert (fake_parent.symbol_table.lookup("array4").shape[0].upper.symbol is
            dim_sym)


@pytest.mark.usefixtures("f2008_parser")
def test_process_use_stmts_with_default_visibility():
    ''' Check that SymbolTable entries are correctly created from
    module use statements. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod, only: some_var\n"
                                 "use this_mod\n"
                                 "use other_mod, only: var1, var2\n")
    fparser2spec = Specification_Part(reader)
    processor._process_use_stmts(fake_parent, fparser2spec.content)

    symtab = fake_parent.symbol_table

    for module_name in ["my_mod", "this_mod", "other_mod"]:
        container = symtab.lookup(module_name)
        assert isinstance(container, ContainerSymbol)
        assert container.name == module_name
        assert not container._reference  # It is not evaluated explicitly told

    for var in ["some_var", "var1", "var2"]:
        assert symtab.lookup(var).name == var

    assert symtab.lookup("some_var").interface.container_symbol \
        == symtab.lookup("my_mod")
    assert symtab.lookup("var2").interface.container_symbol \
        == symtab.lookup("other_mod")

    assert symtab.lookup("this_mod").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("var1").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("other_mod").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("var2").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("my_mod").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("some_var").visibility == Symbol.Visibility.PUBLIC


def test_process_use_stmts_with_accessibility_statements(parser):
    ''' Same as the previous test, but now from a module with a Fortran
    accessibility statement. This will provide a visibility map to the
    use statement processor so the imported symbols and modules end up with
    the appropriate visibility attributes. '''
    processor = Fparser2Reader()
    reader = FortranStringReader('''
        module test
            use my_mod, only: some_var
            use this_mod
            use other_mod, only: var1, var2
            private this_mod, var1
        end module test
    ''')
    parse_tree = parser(reader)
    module = parse_tree.children[0]
    psyir = processor._module_handler(module, None)

    symtab = psyir.symbol_table

    assert symtab.lookup("this_mod").visibility == Symbol.Visibility.PRIVATE
    assert symtab.lookup("var1").visibility == Symbol.Visibility.PRIVATE
    assert symtab.lookup("other_mod").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("var2").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("my_mod").visibility == Symbol.Visibility.PUBLIC
    assert symtab.lookup("some_var").visibility == Symbol.Visibility.PUBLIC


@pytest.mark.usefixtures("f2008_parser")
def test_use_stmt_error(monkeypatch):
    ''' Check that we raise the expected error if the parse tree representing
    a USE statement doesn't have the expected structure. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod, only: some_var\n"
                                 "use this_mod\n"
                                 "use other_mod, only: var1, var2\n")
    fparser2spec = Specification_Part(reader)
    monkeypatch.setattr(fparser2spec.content[0], "items",
                        [None, "hello", None])
    with pytest.raises(GenerationError) as err:
        processor.process_declarations(
            fake_parent, fparser2spec.content, [], {})
    assert ("Expected the parse tree for a USE statement to contain 5 items "
            "but found 3 for 'hello'" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_unrecognised_attribute():
    ''' Check that a declaration with an unrecognised attribute results in
    a symbol with UnsupportedFortranType and the correct visibility. '''
    fake_parent = KernelSchedule("dummy")
    processor = Fparser2Reader()
    reader = FortranStringReader("integer, private, target :: idx1\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    sym = fake_parent.symbol_table.lookup("idx1")
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert sym.visibility == Symbol.Visibility.PRIVATE
    # No access statement so should be public (the default in Fortran)
    reader = FortranStringReader("integer, target :: idx2\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    sym = fake_parent.symbol_table.lookup("idx2")
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert sym.visibility == Symbol.Visibility.PUBLIC
    # No access statement so should pick up the default visibility supplied
    # to the symbol table.
    fake_parent.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    reader = FortranStringReader("integer, target :: idx3\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(
        fake_parent, fparser2spec.children, [], {})
    sym = fake_parent.symbol_table.lookup("idx3")
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert sym.visibility == Symbol.Visibility.PRIVATE
    # No access statement but visibility provided in visibility_map argument
    # to process_declarations()
    reader = FortranStringReader("integer, target :: idx4\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(
        fake_parent, fparser2spec.children, [],
        {"idx4": Symbol.Visibility.PUBLIC})
    sym = fake_parent.symbol_table.lookup("idx4")
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert sym.visibility == Symbol.Visibility.PUBLIC


@pytest.mark.usefixtures("f2008_parser")
def test_parse_array_dimensions_unhandled(monkeypatch):
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''

    def walk_ast_return(_1, _2, _3=None, _4=None):
        '''Function that returns a unique object that will not be part
        of the implemented handling in the walk method caller.'''
        class Invalid():
            '''Class that would be invalid to return from an fparser2 parse
            tree.'''
        newobject = Invalid()
        return [newobject]

    monkeypatch.setattr(fparser.two.utils, 'walk', walk_ast_return)

    reader = FortranStringReader("dimension(:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    with pytest.raises(InternalError) as error:
        _ = Fparser2Reader._parse_dimensions(fparser2spec, None)
    assert "Reached end of loop body and array-shape specification" \
        in str(error.value)
    assert " has not been handled." in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_assignment_stmt():
    ''' Test that fparser2 Assignment_Stmt is converted to the expected PSyIR
    tree structure.

    '''
    reader = FortranStringReader("x=1")
    fparser2assignment = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("x", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2assignment])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Assignment)
    assert len(new_node.children) == 2


@pytest.mark.usefixtures("f2008_parser")
def test_handling_labelled_assignment_stmt():
    ''' Test that a labelled assignment is represented by a CodeBlock. '''
    reader = FortranStringReader("111 x=1")
    fparser2assignment = Execution_Part.match(reader)[0][0]
    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("x", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2assignment])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_name():
    ''' Test that fparser2 Name is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x=1")
    fparser2name = Execution_Part.match(reader)[0][0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    fake_parent.symbol_table.add(DataSymbol('x', INTEGER_TYPE))
    processor.process_nodes(fake_parent, [fparser2name])
    assert len(fake_parent.children) == 1
    assignment = fake_parent.children[0]
    assert len(assignment.children) == 2
    new_ref = assignment.children[0]
    assert isinstance(new_ref, Reference)
    assert new_ref.name == "x"


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_parenthesis():
    ''' Test that fparser2 Parenthesis is converted to the expected PSyIR
    tree structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader("x=(x+1)")
    fparser2parenthesis = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2parenthesis])
    # Check that a new node was generated, parenthesis are ignored and
    # the new node is connected directly to parent
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, BinaryOperation)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_array_section():
    ''' Check that we correctly handle an array section.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''

    def _array_create(code):
        '''Utility function that takes the supplied Fortran code and returns
        its PSyIR representation.

        :param str code: the executable code as a string.

        :returns: the executable code as PSyIR nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        processor = Fparser2Reader()
        fake_parent = Schedule()
        reader = FortranStringReader(code)
        fp2node = Execution_Part.match(reader)[0][0]
        processor.process_nodes(fake_parent, [fp2node])
        return fake_parent.children[0].children[0]

    def _check_array(node, ndims):
        '''Utility function that checks that the supplied node is an array and
        has the expected number of dimensions.

        :param node: the node to check.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayReference`
        :param int ndims: the number of expected array dimensions.

        '''
        assert isinstance(node, ArrayReference)
        assert len(node.children) == ndims

    def _check_range(array, dim):
        '''Utility function that checks that the "dim" index of the supplied
        array contains a range node. Assumes that the supplied
        argument "array" is an array.

        :param array: the node to check.
        :type array: :py:class:`psyclone.psyir.nodes.ArrayReference`
        :param int dim: the array dimension index to check.

        '''
        # Note, in Fortran the 1st dimension is 1, second is 2
        # etc. Therefore to obtain the correct child index we need to
        # subtract 1.
        range_node = array.children[dim-1]
        assert isinstance(range_node, Range)

    def _check_reference(node, dim, index, name):
        '''Utility function to check that the supplied array has a reference
        at dimension index "dim" and range index "index" with name
        "name".

        Assumes that the node argument is an array and that the
        supplied dimension index is a Range node and that the supplied
        range index is valid.

        :param array: the node to check.
        :type array: :py:class:`pysclone.psyir.node.ArrayReference`
        :param int dim: the dimension index to check.
        :param int index: the index of the range to check (0 is the \
            lower bound, 1 is the upper bound).
        :param str name: the expected name of the reference.

        '''
        # Note, in Fortran the 1st dimension is 1, second is 2
        # etc. Therefore to obtain the correct child index we need to
        # subtract 1.
        reference = node.children[dim-1].children[index]
        assert isinstance(reference, Reference)
        assert reference.name == name

    # Simple one-dimensional
    for code in ["a(:) = 0.0", "a(::) = 0.0"]:
        array_reference = _array_create(code)
        _check_array(array_reference, ndims=1)
        _check_range(array_reference, dim=1)
        assert _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.LBOUND)
        assert _is_bound_full_extent(array_reference, 1,
                                     IntrinsicCall.Intrinsic.UBOUND)
        assert _is_array_range_literal(
            array_reference, dim=1, index=2, value=1)
    # Simple multi-dimensional
    for code in ["a(:,:,:) = 0.0", "a(::,::,::) = 0.0"]:
        array_reference = _array_create(code)
        _check_array(array_reference, ndims=3)
        for dim in range(1, 4):
            # Check each of the 3 dimensions (1, 2, 3)
            _check_range(array_reference, dim=dim)
            assert _is_bound_full_extent(
                array_reference, dim,
                IntrinsicCall.Intrinsic.LBOUND)
            assert _is_bound_full_extent(
                array_reference, dim,
                IntrinsicCall.Intrinsic.UBOUND)
            assert _is_array_range_literal(
                array_reference, dim=dim, index=2, value=1)
    # Simple values
    code = "a(1:, 1:2, 1:2:3, :2, :2:3, ::3, 1::3) = 0.0"
    array_reference = _array_create(code)
    _check_array(array_reference, ndims=7)
    # dim 1
    _check_range(array_reference, dim=1)
    assert _is_array_range_literal(array_reference, dim=1, index=0, value=1)
    assert _is_bound_full_extent(array_reference, 1,
                                 IntrinsicCall.Intrinsic.UBOUND)
    assert _is_array_range_literal(array_reference, dim=1, index=2, value=1)
    # dim 2
    _check_range(array_reference, dim=2)
    assert _is_array_range_literal(array_reference, dim=2, index=0, value=1)
    assert _is_array_range_literal(array_reference, dim=2, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=2, index=2, value=1)
    # dim 3
    _check_range(array_reference, dim=3)
    assert _is_array_range_literal(array_reference, dim=3, index=0, value=1)
    assert _is_array_range_literal(array_reference, dim=3, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=3, index=2, value=3)
    # dim 4
    _check_range(array_reference, dim=4)
    assert _is_bound_full_extent(array_reference, 4,
                                 IntrinsicCall.Intrinsic.LBOUND)
    assert _is_array_range_literal(array_reference, dim=4, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=4, index=2, value=1)
    # dim 5
    _check_range(array_reference, dim=5)
    assert _is_bound_full_extent(array_reference, 5,
                                 IntrinsicCall.Intrinsic.LBOUND)
    assert _is_array_range_literal(array_reference, dim=5, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=5, index=2, value=3)
    # dim 6
    _check_range(array_reference, dim=6)
    assert _is_bound_full_extent(array_reference, 6,
                                 IntrinsicCall.Intrinsic.LBOUND)
    assert _is_bound_full_extent(array_reference, 6,
                                 IntrinsicCall.Intrinsic.UBOUND)
    assert _is_array_range_literal(array_reference, dim=6, index=2, value=3)
    # dim 7
    _check_range(array_reference, dim=7)
    assert _is_array_range_literal(array_reference, dim=7, index=0, value=1)
    assert _is_bound_full_extent(array_reference, 7,
                                 IntrinsicCall.Intrinsic.UBOUND)
    assert _is_array_range_literal(array_reference, dim=7, index=2, value=3)

    # Simple variables
    code = "a(b:, b:c, b:c:d) = 0.0"
    array_reference = _array_create(code)
    _check_array(array_reference, ndims=3)
    # dim 1
    _check_range(array_reference, dim=1)
    _check_reference(array_reference, dim=1, index=0, name="b")
    assert _is_bound_full_extent(array_reference, 1,
                                 IntrinsicCall.Intrinsic.UBOUND)
    assert _is_array_range_literal(array_reference, dim=1, index=2, value=1)
    # dim 2
    _check_range(array_reference, dim=2)
    _check_reference(array_reference, dim=2, index=0, name="b")
    _check_reference(array_reference, dim=2, index=1, name="c")
    assert _is_array_range_literal(array_reference, dim=2, index=2, value=1)
    # dim 3
    _check_range(array_reference, dim=3)
    _check_reference(array_reference, dim=3, index=0, name="b")
    _check_reference(array_reference, dim=3, index=1, name="c")
    _check_reference(array_reference, dim=3, index=2, name="d")

    # Expressions
    code = "a(b*c:b+c:b/c) = 0.0"
    array_reference = _array_create(code)
    _check_array(array_reference, ndims=1)
    _check_range(array_reference, dim=1)
    my_range = array_reference.children[0]
    assert isinstance(my_range.children[0], BinaryOperation)
    assert my_range.children[0].operator == BinaryOperation.Operator.MUL
    assert isinstance(my_range.children[1], BinaryOperation)
    assert my_range.children[1].operator == BinaryOperation.Operator.ADD
    assert isinstance(my_range.children[2], BinaryOperation)
    assert my_range.children[2].operator == BinaryOperation.Operator.DIV


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_array_product():
    ''' Check that we correctly handle array products.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader(
        "ze_z(:,:) = e1t(:,:) * e2t(:,:) * zav_tide(:,:,jk)")
    fp2node = Execution_Part.match(reader)
    processor.process_nodes(fake_parent, [fp2node[0][0]])
    assert not fake_parent.walk(CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_if_stmt():
    ''' Test that fparser2 If_Stmt is converted to the expected PSyIR
    tree structure.

    '''
    reader = FortranStringReader("if(x==1)y=1")
    fparser2if_stmt = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("x", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("y", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_stmt])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, IfBlock)
    assert len(new_node.children) == 2


@pytest.mark.usefixtures("f2008_parser")
def test_handling_labelled_if_stmt():
    ''' Test that a labelled fparser2 If_Stmt is converted to a CodeBlock.

    '''
    reader = FortranStringReader("200 if(x==1)y=1")
    fparser2if_stmt = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("x", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("y", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_stmt])
    # Check that a CodeBlock was created
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0], CodeBlock)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_if_construct():
    ''' Test that fparser2 If_Construct is converted to the expected PSyIR
    tree structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''if (condition1 == 1) then
            branch1 = 1
            branch1 = 2
        elseif (condition2 == 2) then
            branch2 = 1
        else
            branch3 = 1
        endif''')
    fparser2if_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_construct])

    # Check a new node was properly generated and connected to parent
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert ifnode.ast is fparser2if_construct
    assert 'was_elseif' not in ifnode.annotations

    # First level contains: condition1, branch1 and elsebody
    assert len(ifnode.children) == 3
    assert ifnode.condition.children[0].name == 'condition1'
    assert isinstance(ifnode.children[1], Schedule)
    assert ifnode.children[1].parent is ifnode
    assert ifnode.children[1].ast is fparser2if_construct.content[1]
    assert ifnode.children[1].ast_end is fparser2if_construct.content[2]
    assert ifnode.if_body[0].children[0].name == 'branch1'
    assert isinstance(ifnode.children[2], Schedule)
    assert ifnode.children[2].parent is ifnode
    assert ifnode.children[2].ast is fparser2if_construct.content[3]

    # Second level contains condition2, branch2, elsebody
    ifnode = ifnode.else_body[0]
    assert 'was_elseif' in ifnode.annotations
    assert ifnode.condition.children[0].name == 'condition2'
    assert isinstance(ifnode.children[1], Schedule)
    assert ifnode.children[1].parent is ifnode
    assert ifnode.if_body[0].children[0].name == 'branch2'
    assert isinstance(ifnode.children[2], Schedule)
    assert ifnode.children[2].parent is ifnode

    # Third level is just branch3
    elsebody = ifnode.else_body[0]
    assert elsebody.children[0].name == 'branch3'
    assert elsebody.ast is fparser2if_construct.content[6]


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_if_construct_errors():
    ''' Test that unsupported If_Construct structures raise the proper
    errors.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        endif''')

    fake_parent = Schedule()
    processor = Fparser2Reader()

    # Test with no opening If_Then_Stmt
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    del fparser2if_construct.content[0]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct])
    assert "Failed to find opening if then statement in:" in str(error.value)

    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        endif''')

    # Test with no closing End_If_Stmt
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    del fparser2if_construct.content[-1]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct])
    assert "Failed to find closing end if statement in:" in str(error.value)

    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        else
        endif''')

    # Test with else clause before and elseif clause
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    children = fparser2if_construct.content
    children[1], children[2] = children[2], children[1]  # Swap clauses
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct])
    assert ("Else clause should only be found next to last clause, but "
            "found") in str(error.value)

    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        else
        endif''')

    # Test with unexpected clause
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    children = fparser2if_construct.content
    children[1] = children[-1]  # Add extra End_If_Stmt
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct])
    assert ("Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt are "
            "expected, but found") in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_labelled_if_construct():
    ''' Test that a labelled if construct is captured as a CodeBlock.

    '''
    reader = FortranStringReader(
        '''181 if (condition1) then
        elseif (condition2) then
        endif''')

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("condition1", symbol_type=DataSymbol,
                                        datatype=BOOLEAN_TYPE)
    fake_parent.symbol_table.new_symbol("condition2", symbol_type=DataSymbol,
                                        datatype=BOOLEAN_TYPE)
    processor = Fparser2Reader()
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fparser2if_construct])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0], CodeBlock)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_complex_if_construct():
    ''' Test that nested If_Construct structures and empty bodies are
    handled properly.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
            if (condition3) then
            elseif (condition4) then
                if (condition6) found = 1
            elseif (condition5) then
            else
            endif
        else
        endif''')
    fparser2if_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_construct])

    elseif = fake_parent.children[0].children[2].children[0]
    assert 'was_elseif' in elseif.annotations
    nested_if = elseif.children[1].children[0]
    assert 'was_elseif' not in nested_if.annotations  # Was manually nested
    elseif2 = nested_if.children[2].children[0]
    assert 'was_elseif' in elseif2.annotations
    nested_if2 = elseif2.children[1].children[0]
    assert nested_if2.children[1].children[0].children[0].name == 'found'


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_binaryopbase():
    ''' Test that fparser2 BinaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x=1+4")
    fp2binaryop = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fp2binaryop])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, BinaryOperation)
    assert len(new_node.children) == 2
    assert new_node._operator == BinaryOperation.Operator.ADD

    # Test parsing all supported arithmetic binary operators.
    testlist = (('+', BinaryOperation.Operator.ADD),
                ('-', BinaryOperation.Operator.SUB),
                ('*', BinaryOperation.Operator.MUL),
                ('/', BinaryOperation.Operator.DIV),
                ('**', BinaryOperation.Operator.POW),
                ('==', BinaryOperation.Operator.EQ),
                ('.eq.', BinaryOperation.Operator.EQ),
                ('.EQ.', BinaryOperation.Operator.EQ),
                ('/=', BinaryOperation.Operator.NE),
                ('.ne.', BinaryOperation.Operator.NE),
                ('>', BinaryOperation.Operator.GT),
                ('.GT.', BinaryOperation.Operator.GT),
                ('<', BinaryOperation.Operator.LT),
                ('.lt.', BinaryOperation.Operator.LT),
                ('>=', BinaryOperation.Operator.GE),
                ('.ge.', BinaryOperation.Operator.GE),
                ('<=', BinaryOperation.Operator.LE),
                ('.LE.', BinaryOperation.Operator.LE))

    for opstring, expected in testlist:
        # Manipulate the fparser2 ParseTree so that it contains the operator
        # under test
        reader = FortranStringReader("x=1" + opstring + "4")
        fp2binaryop = Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [fp2binaryop])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, BinaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test parsing all supported logical binary operators.
    testlist = (('.and.', BinaryOperation.Operator.AND),
                ('.eqv.', BinaryOperation.Operator.EQV),
                ('.neqv.', BinaryOperation.Operator.NEQV),
                ('.or.', BinaryOperation.Operator.OR))
    for opstring, expected in testlist:
        # Manipulate the fparser2 ParseTree so that it contains the operator
        # under test
        reader = FortranStringReader("x=a" + opstring + ".true.")
        fp2binaryop = Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [fp2binaryop])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, BinaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported binary operator creates a CodeBlock
    fake_parent = Schedule()
    fp2binaryop.items = (fp2binaryop.items[0], fp2binaryop.items[1],
                         (fp2binaryop.items[2].items[0], 'unsupported',
                          fp2binaryop.items[2].items[2]))
    processor.process_nodes(fake_parent, [fp2binaryop])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0].rhs, CodeBlock)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_unaryopbase():
    ''' Test that fparser2 UnaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x=-4")
    fp2unaryop = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fp2unaryop])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, UnaryOperation)
    assert len(new_node.children) == 1
    assert new_node._operator == UnaryOperation.Operator.MINUS

    # Test parsing all supported unary operators.
    testlist = (('+', UnaryOperation.Operator.PLUS),
                ('-', UnaryOperation.Operator.MINUS),
                ('.not.', UnaryOperation.Operator.NOT),
                ('.NOT.', UnaryOperation.Operator.NOT))

    for opstring, expected in testlist:
        # Manipulate the fparser2 ParseTree so that it contains the operator
        # under test
        reader = FortranStringReader("x=" + opstring + "4")
        fp2unaryop = Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [fp2unaryop])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, UnaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported unary operator creates a CodeBlock
    fp2unaryop.items = (fp2unaryop.items[0], fp2unaryop.items[1],
                        ('unsupported', fp2unaryop.items[2].items[1]))
    fake_parent = Schedule()
    processor.process_nodes(fake_parent, [fp2unaryop])

    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_return_stmt():
    ''' Test that fparser2 Return_Stmt is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("return")
    return_stmt = Execution_Part.match(reader)[0][0]
    assert isinstance(return_stmt, Return_Stmt)

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [return_stmt])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Return)
    assert not new_node.children


@pytest.mark.usefixtures("f2008_parser")
def test_handling_labelled_return_stmt():
    ''' Test that a labelled fparser2 Return_Stmt is converted to a CodeBlock.
    '''
    reader = FortranStringReader("999 return")
    return_stmt = Execution_Part.match(reader)[0][0]
    assert isinstance(return_stmt, Return_Stmt)

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [return_stmt])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_end_subroutine_stmt():
    ''' Test that fparser2 End_Subroutine_Stmt are ignored.'''
    reader = FortranStringReader('''
        subroutine dummy_code()
        end subroutine dummy_code
        ''')
    fparser2endsub = Subroutine_Subprogram.match(reader)[0][-1]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2endsub])
    assert not fake_parent.children  # No new children created


# (1/3) fparser2reader::nodes_to_code_block
def test_nodes_to_code_block_1(f2008_parser):
    '''Check that a statement codeblock that is at the "top level" in the
    PSyIR has the structure property set to statement (as it has a
    schedule as parent).

    '''
    reader = FortranStringReader('''
        program test
        loop: do i = 0, 9
            do j = i, 9
                if (j == 5) exit loop
            end do
        end do loop
        end program test
        ''')
    prog = f2008_parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    assert isinstance(schedule[0], CodeBlock)
    assert schedule[0].structure == CodeBlock.Structure.STATEMENT


# (2/3) fparser2reader::nodes_to_code_block
def test_nodes_to_code_block_2(f2008_parser):
    '''Check that a statement codeblock that is within another statement
    in the PSyIR has the structure property set to statement (as it
    has a schedule as parent).

    '''
    reader = FortranStringReader('''
        program test
        if (.true.) then
            loop: do i = 0, 9
                do j = i, 9
                    if (j == 5) exit loop
                end do
            end do loop
        end if
        end program test
        ''')
    prog = f2008_parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    assert isinstance(schedule[0].if_body[0], CodeBlock)
    assert schedule[0].if_body[0].structure == CodeBlock.Structure.STATEMENT


# (3/3) fparser2reader::nodes_to_code_block
@pytest.mark.usefixtures("f2008_parser")
def test_nodes_to_code_block_3():
    '''Check that a codeblock that has a directive as a parent causes the
    expected exception.
    All directives are now StandaloneDirective or RegionDirective children.
    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Fparser2Reader.nodes_to_code_block(StandaloneDirective(), "hello")
    assert ("A CodeBlock with a Directive as parent is not yet supported."
            in str(excinfo.value))

    with pytest.raises(InternalError) as excinfo:
        _ = Fparser2Reader.nodes_to_code_block(RegionDirective(), "hello")
    assert ("A CodeBlock with a Directive as parent is not yet supported."
            in str(excinfo.value))


def test_named_and_wildcard_use_var(f2008_parser):
    ''' Check that we handle the case where a variable is accessed first by
    a wildcard import and then by a named import. '''
    reader = FortranStringReader('''
        module test_mod
          use some_mod
        contains
          subroutine test_sub1()
            ! a_var here must be being brought into scope by the
            ! `use some_mod` in the module.
            a_var = 1.0
          end subroutine test_sub1
          subroutine test_sub2()
            use some_mod, only: a_var
            a_var = 2.0
          end subroutine test_sub2
        end module test_mod
        ''')
    prog = f2008_parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    # We should not have an entry for "a_var" in the Container symbol
    # table as we don't know whether the access in "test_sub1" comes
    # from the wildcard import ("some_mod"). The Container is the
    # first child of the FileContainer node.
    container = psy.container.children[0]
    assert "a_var" not in container.symbol_table
    # There should be an entry for "a_var" in the symbol table for the
    # "test_sub1" routine as we do not yet know where it is declared.
    routine = container.children[0]
    avar1 = routine.symbol_table.lookup("a_var")
    # It must be a generic Symbol since we don't know anything about it
    # pylint: disable=unidiomatic-typecheck
    assert type(avar1) is Symbol

    # There should be another, distinct entry for "a_var" in the symbol table
    # for "test_sub2" as it has a use statement that imports it.
    routine = container.children[1]
    avar2 = routine.symbol_table.lookup("a_var")
    assert type(avar2) is Symbol
    assert avar2 is not avar1


# _process_args tests

def test_intrinsic_names_error(fortran_reader):
    '''In the _process_args method, check that the expected exception is
    raised if the intrinsic arguments do not follow the rule that all
    named arguments follow all positional arguments. Use the sum
    intrinsic as an example.

    '''
    code = '''
subroutine test_intrinsic(var1, var2, dim, mask)
  real, intent(in) :: var1(:)
  integer, intent(in) :: dim
  logical, intent(in) :: mask
  real, intent(out) :: var2
  var2 = sum(var1, dim=dim, mask)
end subroutine test_intrinsic
'''
    with pytest.raises(GenerationError) as info:
        _ = fortran_reader.psyir_from_source(code)
    assert ("In Fortran, all named arguments should follow all positional "
            "arguments, but found 'SUM(var1, dim = dim, mask)'."
            in str(info.value))


@pytest.mark.parametrize("args,arg_names", [
    ("1.0, a, (a+b)*2.0", [None, None, None]),
    ("1.0, arg2=a, arg3=(a+b)*2.0", [None, "arg2", "arg3"])])
def test_call_args(f2008_parser, args, arg_names):
    '''Test that fparser2reader _process_args method transforms the
    arguments of a Fortran subroutine call with into the equivalent
    PSyIR arguments. Test with and without named arguments.

    '''
    test_code = (
        f"subroutine test()\n"
        f"use my_mod, only : kernel\n"
        f"real :: a,b\n"
        f"  call kernel({args})\n"
        f"end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    # The fparser2 call hierarchy to find Call_Stmt is the following:
    # Program->Subroutine_SubProgram->Execution_Part->Call_Stmt
    fparser2_call_node = ptree.children[0].children[2].children[0]
    processor = Fparser2Reader()
    psyir = processor.generate_psyir(ptree)

    call_node = psyir.walk(Call)[0]
    assert isinstance(call_node, Call)
    assert call_node.ast == fparser2_call_node
    assert len(call_node._argument_names) == len(call_node.children)
    for idx, child in enumerate(call_node.children):
        assert call_node._argument_names[idx] == (id(child), arg_names[idx])
    assert call_node.argument_names == arg_names
    assert len(call_node.children) == 3
    assert isinstance(call_node.children[0], Literal)
    assert call_node.children[0].value == "1.0"
    assert isinstance(call_node.children[1], Reference)
    assert call_node.children[1].name == "a"
    assert isinstance(call_node.children[2], BinaryOperation)


def test_intrinsiccall_args(f2008_parser):
    '''Test that fparser2reader _process_args method transforms the
    arguments of a Fortran subroutine call with into the equivalent
    PSyIR arguments. Test with and without named arguments.

    '''
    test_code = (
        "subroutine test(a, d, m, result)\n"
        "  real, intent(in) :: a(:)\n"
        "  integer, intent(in) :: d\n"
        "  logical, intent(in) :: m\n"
        "  real, intent(out) :: result\n"
        "  result = minval(a, d, m)\n"
        "end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    psyir = processor.generate_psyir(ptree)

    intrinsic_node = psyir.walk(IntrinsicCall)[0]
    assert isinstance(intrinsic_node, IntrinsicCall)
    assert len(intrinsic_node._argument_names) == len(intrinsic_node.children)
    arg_names = [None, "dim", "mask"]
    for idx, child in enumerate(intrinsic_node.children):
        assert intrinsic_node._argument_names[idx] == (
            id(child), arg_names[idx])
    assert intrinsic_node.argument_names == arg_names
    assert len(intrinsic_node.children) == 3
    assert isinstance(intrinsic_node.children[0], Reference)
    assert intrinsic_node.children[0].name == "a"
    assert isinstance(intrinsic_node.children[1], Reference)
    assert intrinsic_node.children[1].name == "d"
    assert isinstance(intrinsic_node.children[2], Reference)
    assert intrinsic_node.children[2].name == "m"


def test_call_codeblock_args(fortran_reader):
    '''Test that we get one CodeBlock for each (unrecognised) argument
    when _process_args() calls process_nodes(), rather than a single
    CodeBlock containing all of them.

    '''
    test_code = (
        "subroutine test()\n"
        "  use my_mod, only : kernel\n"
        "  real :: a, b\n"
        "  call kernel(a, 'not'//'nice', 'at'//'all', b)\n"
        "end subroutine")
    psyir = fortran_reader.psyir_from_source(test_code)
    call_node = psyir.walk(Call)[0]
    assert isinstance(call_node, Call)
    assert len(call_node.children) == 4
    assert isinstance(call_node.children[0], Reference)
    assert call_node.children[0].name == "a"
    assert isinstance(call_node.children[1], CodeBlock)
    assert isinstance(call_node.children[2], CodeBlock)
    assert isinstance(call_node.children[3], Reference)
    assert call_node.children[3].name == "b"


def test_declarations_with_initialisations_errors(parser):
    ''' Test that Fparser2Reader propagates uncaught errors from
    declaration initialisations.
    '''

    def raise_value_error(_1, _2):
        raise ValueError("error to propagate")

    # We create a new parser(we don't want to pollute the cached one) and we
    # patch the integer literal handler (we need to do this by updating the
    # handlers map instead of monkeypatching the function)
    processor = Fparser2Reader()
    processor.handlers[Fortran2003.Int_Literal_Constant] = raise_value_error

    reader = FortranStringReader("""
    module test_mod
        integer, parameter :: b = 1
        contains
        subroutine a()
            integer :: var1
            integer, parameter :: b = 1
        end subroutine a
    end module test_mod
    """)
    ast = parser(reader)

    with pytest.raises(ValueError) as err:
        _ = processor.get_routine_schedules("a", ast)
    assert "error to propagate" in str(err.value)


def test_structures(fortran_reader, fortran_writer):
    '''Test that Fparser2Reader parses Fortran types correctly when there
    is a type declaration with one of the members being initialised,
    when there is a type declaration with an additional attribute
    (e.g. one that extends an existing type), when there is a type
    declaration that contains procedures and when there is a type
    declaration that both has an additional attribute and contains
    procedures.

    '''
    # derived-type with initial value (StructureType)
    test_code = (
        "module test_mod\n"
        "    type, private :: my_type\n"
        "      integer :: i = 1\n"
        "      integer :: j\n"
        "    end type my_type\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, StructureType)
    result = fortran_writer(psyir)
    assert (
        "  type, private :: my_type\n"
        "    integer, public :: i = 1\n"
        "    integer, public :: j\n"
        "  end type my_type\n" in result)

    # type that extends another type (UnsupportedFortranType)
    test_code = (
        "module test_mod\n"
        "    use kernel_mod, only : kernel_type\n"
        "    type, extends(kernel_type) :: my_type\n"
        "      integer :: i = 1\n"
        "    end type my_type\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    result = fortran_writer(psyir)
    assert (
        "  type, extends(kernel_type), public :: my_type\n"
        "  INTEGER :: i = 1\n"
        "END TYPE my_type\n" in result)

    # type that contains a procedure (UnsupportedFortranType)
    test_code = (
        "module test_mod\n"
        "    type :: test_type\n"
        "      integer :: i = 1\n"
        "      contains\n"
        "      procedure, nopass :: test_code\n"
        "    end type test_type\n"
        "    contains\n"
        "    subroutine test_code()\n"
        "    end subroutine\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    symbol = sym_table.lookup("test_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    result = fortran_writer(psyir)
    assert (
        "  type, public :: test_type\n"
        "  INTEGER :: i = 1\n"
        "  CONTAINS\n"
        "  PROCEDURE, NOPASS :: test_code\n"
        "END TYPE test_type\n" in result)

    # type that creates an abstract type and contains a procedure
    # (UnsupportedFortranType)
    test_code = (
        "module test_mod\n"
        "    type, abstract, private :: test_type\n"
        "      integer :: i = 1\n"
        "      contains\n"
        "      procedure, nopass :: test_code\n"
        "    end type test_type\n"
        "    contains\n"
        "    subroutine test_code()\n"
        "    end subroutine\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    symbol = sym_table.lookup("test_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    result = fortran_writer(psyir)
    assert (
        "  type, abstract, private :: test_type\n"
        "  INTEGER :: i = 1\n"
        "  CONTAINS\n"
        "  PROCEDURE, NOPASS :: test_code\n"
        "END TYPE test_type\n" in result)


def test_structures_constants(fortran_reader, fortran_writer):
    '''Test that Fparser2Reader parses Fortran types correctly when there
    is a type declaration with one of the members being initialised
    with constants that are declared outside of the type.

    '''
    test_code = (
        "module test_mod\n"
        "    integer, parameter :: N = 1, M = 2\n"
        "    type, private :: my_type\n"
        "      integer :: i = N + M\n"
        "      integer :: j\n"
        "    end type my_type\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    n_symbol = sym_table.lookup("n")
    assert isinstance(n_symbol.interface, StaticInterface)
    m_symbol = sym_table.lookup("m")
    assert isinstance(m_symbol.interface, StaticInterface)
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, StructureType)
    i_symbol = symbol.datatype.lookup("i")
    n_reference = i_symbol.initial_value.children[0]
    assert n_reference.symbol is n_symbol
    m_reference = i_symbol.initial_value.children[1]
    assert m_reference.symbol is m_symbol
    result = fortran_writer(psyir)
    assert ("  integer, parameter, public :: N = 1\n"
            "  integer, parameter, public :: M = 2\n"
            "  type, private :: my_type\n"
            "    integer, public :: i = N + M\n"
            "    integer, public :: j\n"
            "  end type my_type\n" in result)


def test_structures_constant_scope(fortran_reader, fortran_writer):
    '''Test that Fparser2Reader parses Fortran types correctly when there
    is a type declaration with one of the members being initialised
    with constants that are declared outside of the type within a
    different symbol table.

    '''
    test_code = (
        "module test_mod\n"
        "  integer, parameter :: N = 1, M = 2\n"
        "  contains\n"
        "  subroutine test_code()\n"
        "    type, private :: my_type\n"
        "      integer :: i = N + M\n"
        "      integer :: j\n"
        "    end type my_type\n"
        "  end subroutine\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    n_symbol = sym_table.lookup("n")
    assert isinstance(n_symbol.interface, StaticInterface)
    m_symbol = sym_table.lookup("m")
    assert isinstance(m_symbol.interface, StaticInterface)
    sym_table = psyir.children[0].children[0].symbol_table
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, StructureType)
    i_symbol = symbol.datatype.lookup("i")
    n_reference = i_symbol.initial_value.children[0]
    assert n_reference.symbol is n_symbol
    m_reference = i_symbol.initial_value.children[1]
    assert m_reference.symbol is m_symbol
    result = fortran_writer(psyir)
    assert (
        "module test_mod\n"
        "  implicit none\n"
        "  integer, parameter, public :: n = 1\n"
        "  integer, parameter, public :: m = 2\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine test_code()\n"
        "    type :: my_type\n"
        "      integer :: i = n + m\n"
        "      integer :: j\n"
        "    end type my_type\n\n\n"
        "  end subroutine test_code\n\n"
        "end module test_mod" in result)


def test_structures_constant_use(fortran_reader, fortran_writer):
    '''Test that Fparser2Reader parses Fortran types correctly when there
    is a type declaration with one of the members being initialised
    with constants that are declared outside of the type within a
    different symbol table and there is a wildcard use statement.

    '''
    test_code = (
        "module test_mod\n"
        "  use wildcard\n"
        "  contains\n"
        "  subroutine test_code()\n"
        "    integer, parameter :: N = 1, M = 2\n"
        "    type :: my_type\n"
        "      integer :: i = N + M\n"
        "      integer :: j\n"
        "    end type my_type\n"
        "  end subroutine\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].children[0].symbol_table
    n_symbol = sym_table.lookup("n")
    assert isinstance(n_symbol.interface, StaticInterface)
    m_symbol = sym_table.lookup("m")
    assert isinstance(m_symbol.interface, StaticInterface)
    symbol = sym_table.lookup("my_type")
    assert isinstance(symbol, DataTypeSymbol)
    assert isinstance(symbol.datatype, StructureType)
    i_symbol = symbol.datatype.lookup("i")
    n_reference = i_symbol.initial_value.children[0]
    assert n_reference.symbol is n_symbol
    m_reference = i_symbol.initial_value.children[1]
    assert m_reference.symbol is m_symbol
    result = fortran_writer(psyir)
    assert (
        "    integer, parameter :: N = 1\n"
        "    integer, parameter :: M = 2\n"
        "    type :: my_type\n"
        "      integer :: i = N + M\n"
        "      integer :: j\n"
        "    end type my_type\n" in result)
