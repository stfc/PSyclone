# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end '''

from __future__ import absolute_import
import pytest
import fparser
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.Fortran2003 import Specification_Part, \
    Type_Declaration_Stmt, Execution_Part, Name, Stmt_Function_Stmt, \
    Dimension_Attr_Spec, Assignment_Stmt, Return_Stmt, Subroutine_Subprogram
from psyclone.psyir.nodes import Schedule, CodeBlock, Assignment, Return, \
    UnaryOperation, BinaryOperation, NaryOperation, IfBlock, Reference, \
    ArrayReference, Container, Literal, Range, KernelSchedule, Directive
from psyclone.psyGen import PSyFactory
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.symbols import (
    DataSymbol, ContainerSymbol, SymbolTable, RoutineSymbol,
    ArgumentInterface, SymbolError, ScalarType, ArrayType, INTEGER_TYPE,
    REAL_TYPE, UnknownFortranType, DeferredType, Symbol, UnresolvedInterface,
    ImportInterface, BOOLEAN_TYPE)
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    _is_array_range_literal, _is_bound_full_extent, \
    _is_range_full_extent, _check_args, default_precision, \
    default_integer_type, default_real_type, _kind_symbol_from_name, \
    _first_type_match


def process_declarations(code):
    '''
    Utility routine to create PSyIR for Fortran variable declarations.

    :param str code: Fortran declaration statement(s)

    :returns: a 2-tuple consisting of a KernelSchedule with populated Symbol \
              Table and the parse tree for the specification part.
    :rtype: (:py:class:`psyclone.psyir.nodes.KernelSchedule`, \
             :py:class:`fparser.two.Fortran2003.Specification_Part`)
    '''
    sched = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(sched, fparser2spec, [])
    return sched, fparser2spec


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
    assert ("'array' argument should be an ArrayReference type but found "
            "'NoneType'." in str(excinfo.value))

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
    assert ("'array' argument should be an ArrayReference type but found "
            "'NoneType'." in str(excinfo.value))

    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [20])
    symbol = DataSymbol('a', array_type)
    my_range = Range.create(one.copy(), one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    with pytest.raises(TypeError) as excinfo:
        _is_bound_full_extent(array_reference, 1, None)
    assert ("'operator' argument  expected to be LBOUND or UBOUND but found "
            "'NoneType'" in str(excinfo.value))

    # Expecting BinaryOperation but found Literal
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.UBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND, one.copy(), one.copy())
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting operator to be Operator.LBOUND, but found
    # Operator.UBOUND
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.LBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND, one.copy(), one.copy())
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting Reference but found Literal
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.LBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(DataSymbol("x", INTEGER_TYPE)), one.copy())
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting Reference symbol x to be the same as array symbol a
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.LBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("1.0", REAL_TYPE))
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting integer but found real
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.LBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("2", INTEGER_TYPE))
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # Expecting literal value 2 to be the same as the current array
    # dimension 1
    assert not _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.LBOUND)

    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))
    my_range = Range.create(operator, one.copy())
    array_reference = ArrayReference.create(symbol, [my_range])

    # valid
    assert _is_bound_full_extent(array_reference, 1,
                                 BinaryOperation.Operator.LBOUND)


def test_is_array_range_literal():
    ''' Test the _is_array_range_literal function.'''

    # Check that _is_array_range_literal calls the _check_args function.
    with pytest.raises(TypeError) as excinfo:
        _is_array_range_literal(None, None, None, None)
    assert ("'array' argument should be an ArrayReference type but found "
            "'NoneType'." in str(excinfo.value))

    one = Literal("1", INTEGER_TYPE)
    array_type = ArrayType(REAL_TYPE, [20])
    symbol = DataSymbol('a', array_type)
    operator = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))
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
    lbound_op = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))
    ubound_op = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))

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


# Class Fparser2Reader


def test_array_notation_rank():
    '''Test the static method _array_notation_rank in the fparser2reader
    class

    '''
    # An array with no dimensions raises an exception
    array_type = ArrayType(REAL_TYPE, [10])
    symbol = DataSymbol("a", array_type)
    array = ArrayReference(symbol, [])
    with pytest.raises(NotImplementedError) as excinfo:
        Fparser2Reader._array_notation_rank(array)
    assert ("An Array reference in the PSyIR must have at least one child but "
            "'a' has none" in str(excinfo.value))

    # If array syntax notation is found, it must be for all elements
    # in that dimension
    array_type = ArrayType(REAL_TYPE, [10, 10, 10])
    symbol = DataSymbol("a", array_type)
    lbound_op1 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))
    ubound_op1 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol), Literal("1", INTEGER_TYPE))
    lbound_op3 = BinaryOperation.create(
        BinaryOperation.Operator.LBOUND,
        Reference(symbol), Literal("3", INTEGER_TYPE))
    ubound_op3 = BinaryOperation.create(
        BinaryOperation.Operator.UBOUND,
        Reference(symbol), Literal("3", INTEGER_TYPE))

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


def test_generate_schedule_empty_subroutine(parser):
    ''' Tests the fp2Reader generate_schedule method with an empty
    subroutine.
    '''
    reader = FortranStringReader(FAKE_KERNEL_METADATA)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test properly formed but empty kernel schedule
    schedule = processor.generate_schedule("dummy_code", ast)
    assert isinstance(schedule, KernelSchedule)

    # Test that the container is created correctly
    assert isinstance(schedule.parent, Container)
    container = schedule.parent
    assert len(container.children) == 1
    assert container.children[0] is schedule
    assert container.name == "dummy_mod"
    rsym = container.symbol_table.lookup("dummy_code")
    assert isinstance(rsym, RoutineSymbol)

    # Test that we get an error for a nonexistent subroutine name
    with pytest.raises(GenerationError) as error:
        schedule = processor.generate_schedule("nonexistent_code", ast)
    assert "Unexpected kernel AST. Could not find " \
           "subroutine: nonexistent_code" in str(error.value)

    # Test corrupting ast by deleting subroutine
    del ast.content[0].content[2]
    with pytest.raises(GenerationError) as error:
        schedule = processor.generate_schedule("dummy_code", ast)
    assert "Unexpected kernel AST. Could not find " \
           "subroutine: dummy_code" in str(error.value)


def test_generate_schedule_module_decls(parser):
    '''Test that the generate_schedule method in the Fparser2Reader class
    stores module variables in the generated container's symbol table.

    '''
    input_code = FAKE_KERNEL_METADATA.replace(
        "  end type dummy_type\n",
        "  end type dummy_type\n"
        "  real :: scalar1\n"
        "  real :: array1(10,10,10)\n")
    reader = FortranStringReader(input_code)
    ast = parser(reader)
    processor = Fparser2Reader()
    schedule = processor.generate_schedule("dummy_code", ast)
    symbol_table = schedule.parent.symbol_table
    assert isinstance(symbol_table, SymbolTable)
    assert isinstance(symbol_table.lookup("scalar1"), DataSymbol)
    assert isinstance(symbol_table.lookup("array1"), DataSymbol)
    assert isinstance(symbol_table.lookup("dummy_code"), RoutineSymbol)


def test_generate_schedule_dummy_subroutine(parser):
    ''' Tests the fparser2Reader generate_schedule method with a simple
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
    schedule = processor.generate_schedule("dummy_code", ast)
    assert isinstance(schedule, KernelSchedule)

    # Test that a kernel subroutine without Execution_Part still creates a
    # valid KernelSchedule
    del ast.content[0].content[2].content[1].content[2]
    schedule = processor.generate_schedule("dummy_code", ast)
    assert isinstance(schedule, KernelSchedule)
    assert not schedule.children


def test_generate_schedule_no_args_subroutine(parser):
    ''' Tests the fparser2Reader generate_schedule method with a simple
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
    schedule = processor.generate_schedule("dummy_code", ast)
    assert isinstance(schedule, KernelSchedule)
    # TODO: In the future we could validate that metadata matches
    # the kernel arguments, then this test would fail. Issue #288


def test_generate_schedule_unmatching_arguments(parser):
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
        _ = processor.generate_schedule("dummy_code", ast)
    assert "The kernel argument list" in str(error.value)
    assert "does not match the variable declarations for fparser nodes" \
        in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations():
    '''Test that process_declarations method of Fparser2Reader
    converts the fparser2 declarations to symbols in the provided
    parent Kernel Schedule.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader("integer :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = fake_parent.symbol_table.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert l1_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l1_var.is_local

    reader = FortranStringReader("Real      ::      l2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l2_var = fake_parent.symbol_table.lookup("l2")
    assert l2_var.name == "l2"
    assert isinstance(l2_var.datatype, ScalarType)
    assert l2_var.datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert l2_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert l2_var.is_local

    reader = FortranStringReader("LOGICAL      ::      b")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    b_var = fake_parent.symbol_table.lookup("b")
    assert b_var.name == "b"
    # Symbol should be public by default
    assert b_var.visibility == Symbol.Visibility.PUBLIC
    assert isinstance(b_var.datatype, ScalarType)
    assert b_var.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN
    assert b_var.datatype.precision == ScalarType.Precision.UNDEFINED
    assert b_var.is_local

    # public/private attribute
    reader = FortranStringReader("real, public :: p2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert (fake_parent.symbol_table.lookup("p2").visibility ==
            Symbol.Visibility.PUBLIC)
    reader = FortranStringReader("real, private :: p3, p4")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert (fake_parent.symbol_table.lookup("p3").visibility ==
            Symbol.Visibility.PRIVATE)
    assert (fake_parent.symbol_table.lookup("p4").visibility ==
            Symbol.Visibility.PRIVATE)

    # Initialisations of static constant values (parameters)
    reader = FortranStringReader("integer, parameter :: i1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    newsymbol = fake_parent.symbol_table.lookup("i1")
    assert newsymbol.is_constant
    assert isinstance(newsymbol.constant_value, Literal)
    assert newsymbol.constant_value.value == "1"

    reader = FortranStringReader("real, parameter :: i2 = 2.2, i3 = 3.3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("i2").constant_value.value == "2.2"
    assert fake_parent.symbol_table.lookup("i3").constant_value.value == "3.3"

    # Initialisation with constant expressions
    reader = FortranStringReader("real, parameter :: i4 = 1.1, i5 = i4 * 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("i4").constant_value.value == "1.1"
    assert isinstance(fake_parent.symbol_table.lookup("i5").constant_value,
                      BinaryOperation)

    # Initialisation with a constant expression (1) and with a symbol (val1)
    reader = FortranStringReader("integer, parameter :: val1 = 1, val2 = val1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("val1").constant_value.value == "1"
    assert isinstance(
        fake_parent.symbol_table.lookup("val2").constant_value, Reference)
    assert fake_parent.symbol_table.lookup("val2").constant_value.symbol == \
        fake_parent.symbol_table.lookup("val1")

    # Initialisation with a complex constant expression
    reader = FortranStringReader(
        "integer, parameter :: val3 = 2 * (val1 + val2) + 2_precisionkind")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    # Val3 has been given a constant expression
    assert fake_parent.symbol_table.lookup("val3").constant_value
    # The new symbol (precisionkind) has been added to the parent Symbol Table
    assert fake_parent.symbol_table.lookup("precisionkind")

    # Check we catch duplicated symbols
    reader = FortranStringReader("integer :: i2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Symbol 'i2' already present in SymbolTable with a defined "
            "interface" in str(error.value))


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
def test_process_unsupported_declarations(fortran_reader):
    ''' Check that the frontend handles unsupported declarations by
    creating symbols of UnknownFortranType. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Initial values for variables are not supported so we should get a symbol
    # with unknown type.
    reader = FortranStringReader("real:: a = 1.1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    asym = fake_parent.symbol_table.lookup("a")
    assert isinstance(asym.datatype, UnknownFortranType)
    assert asym.datatype.declaration == "REAL :: a = 1.1"

    reader = FortranStringReader("real:: b = 1.1, c = 2.2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    bsym = fake_parent.symbol_table.lookup("b")
    assert isinstance(bsym.datatype, UnknownFortranType)
    assert bsym.datatype.declaration == "REAL :: b = 1.1"
    csym = fake_parent.symbol_table.lookup("c")
    assert isinstance(csym.datatype, UnknownFortranType)
    assert csym.datatype.declaration == "REAL :: c = 2.2"

    # Multiple symbols with a single attribute
    reader = FortranStringReader("integer, private :: d = 1, e = 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    dsym = fake_parent.symbol_table.lookup("d")
    assert isinstance(dsym.datatype, UnknownFortranType)
    assert dsym.datatype.declaration == "INTEGER, PRIVATE :: d = 1"
    esym = fake_parent.symbol_table.lookup("e")
    assert isinstance(esym.datatype, UnknownFortranType)
    assert esym.datatype.declaration == "INTEGER, PRIVATE :: e = 2"

    # Multiple attributes
    reader = FortranStringReader(
        "INTEGER, PRIVATE, DIMENSION(3) :: f = 2, g = 3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fsym = fake_parent.symbol_table.lookup("f")
    assert isinstance(fsym.datatype, UnknownFortranType)
    assert (fsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3) :: f = 2")
    gsym = fake_parent.symbol_table.lookup("g")
    assert isinstance(gsym.datatype, UnknownFortranType)
    assert (gsym.datatype.declaration ==
            "INTEGER, PRIVATE, DIMENSION(3) :: g = 3")

    # Test with unsupported intrinsic type. Note the space before complex
    # below which stops the line being treated as a comment.
    reader = FortranStringReader(" complex     ::      c2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    c2sym = fake_parent.symbol_table.lookup("c2")
    assert isinstance(c2sym.datatype, UnknownFortranType)
    assert c2sym.datatype.declaration == "COMPLEX :: c2"

    # Char lengths are not supported
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character :: l*4\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnknownFortranType)
    psyir = fortran_reader.psyir_from_source("program dummy\n"
                                             "character(len=4) :: l\n"
                                             "end program")
    assert isinstance(psyir.children[0].symbol_table.lookup("l").datatype,
                      UnknownFortranType)

    # Unsupported initialisation of a parameter which comes after a valid
    # initialisation
    reader = FortranStringReader(
        "INTEGER, PARAMETER :: happy=1, fbsp = SELECTED_REAL_KIND( 6, 37)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    fbsym = fake_parent.symbol_table.lookup("fbsp")
    assert isinstance(fbsym.datatype, UnknownFortranType)
    assert (fbsym.datatype.declaration ==
            "INTEGER, PARAMETER :: fbsp = SELECTED_REAL_KIND(6, 37)")
    # The first parameter should have been handled correctly
    hsym = fake_parent.symbol_table.lookup("happy")
    assert hsym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert hsym.constant_value.value == "1"
    # The fparser2 parse tree should still be intact
    assert ("INTEGER, PARAMETER :: happy = 1, fbsp = SELECTED_REAL_KIND(6, 37)"
            in str(fparser2spec))


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

    reader = FortranStringReader("{0}*{1} :: l1".format(fort_name, precision))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = fake_parent.symbol_table.lookup("l1")
    assert l1_var.name == 'l1'
    assert isinstance(l1_var.datatype, ScalarType)
    assert l1_var.datatype.intrinsic == type_name
    assert l1_var.datatype.precision == precision
    assert l1_var.is_local


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
    assert x_var.is_local


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

    # Extent given by variable with UnknownFortranType
    udim = DataSymbol("udim", UnknownFortranType("integer :: udim"),
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
    assert isinstance(reference.symbol.datatype, UnknownFortranType)

    # Extent given by variable with DeferredType
    ddim = DataSymbol("ddim", DeferredType(),
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
    assert isinstance(reference.symbol.datatype, DeferredType)


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
                      UnknownFortranType)

    reader = FortranStringReader("real, allocatable :: p3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("p3").datatype,
                      UnknownFortranType)

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
    assert isinstance(l11sym.datatype, UnknownFortranType)


def test_module_function_symbol(parser):
    ''' Check that the frontend correctly creates a new, local symbol for
    a Function's return value. '''
    dummy_module = '''
    module dummy_mod
        use mod1
    contains
        subroutine dummy_code(f1, f2)
            real(wp), dimension(:,:), intent(in)  :: f1
            real(wp), dimension(:,:), intent(out)  :: f2
            f2 = f1 + modvar1(1)
        end subroutine dummy_code
        function modvar1(arg)
            integer :: arg
            real :: modvar1
            modvar1 = 0.0
        end function modvar1
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_module)
    ast = parser(reader)
    processor = Fparser2Reader()
    container = processor.generate_container(ast)
    # This will result in a symbol named "modvar1" being put into
    # the Container
    _ = processor.generate_schedule("dummy_code", ast, container)
    sym = container.symbol_table.lookup("modvar1")
    assert isinstance(sym, Symbol)
    # This should result in a new, *local* symbol named "modvar1"
    sched = processor.generate_schedule("modvar1", ast, container)
    # Check that the resulting Schedule has a local symbol
    sym = sched.scope.symbol_table.lookup("modvar1", scope_limit=sched)
    assert isinstance(sym, DataSymbol)
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.REAL


def test_process_save_attribute_declarations(parser):
    ''' Test that the SAVE attribute in a declaration is supported when
    found in the specification part of a module or main_program, otherwise
    it raises an error.'''

    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Test with no context about where the declaration. Not even that is
    # in the Specification_Part.
    reader = FortranStringReader("integer, save :: var1")
    fparser2spec = Type_Declaration_Stmt(reader)
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("var1").datatype,
                      UnknownFortranType)

    # Test with no context about where the declaration is.
    reader = FortranStringReader("integer, save :: var2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert isinstance(fake_parent.symbol_table.lookup("var2").datatype,
                      UnknownFortranType)

    # Test with a subroutine.
    reader = FortranStringReader(
        "subroutine name()\n"
        "integer, save :: var3\n"
        "end subroutine name")
    fparser2spec = parser(reader).content[0].content[1]
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    assert isinstance(fake_parent.symbol_table.lookup("var3").datatype,
                      UnknownFortranType)

    # Test with a module.
    reader = FortranStringReader(
        "module modulename\n"
        "integer, save :: var4\n"
        "end module modulename")
    fparser2spec = parser(reader).content[0].content[1]
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    var4 = fake_parent.symbol_table.lookup("var4")
    assert var4.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_intent():
    '''Test that process_declarations method handles various different
    specifications of variable attributes.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, intent(in) :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    arg_list = [Fortran2003.Name("arg1")]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg1").interface.access == \
        ArgumentInterface.Access.READ

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
def test_process_declarations_kind_new_param():
    ''' Test that process_declarations handles variables declared with
    an explicit KIND parameter that has not already been declared. Also
    check that the matching on the variable name is not case sensitive.

    '''
    fake_parent, fp2spec = process_declarations("real(kind=wp) :: var1\n"
                                                "real(kind=Wp) :: var2\n")
    var1_var = fake_parent.symbol_table.lookup("var1")
    assert isinstance(var1_var.datatype.precision, DataSymbol)
    # Check that this has resulted in the creation of a new 'wp' symbol
    wp_var = fake_parent.symbol_table.lookup("wp")
    assert wp_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert var1_var.datatype.precision is wp_var
    # Check that, despite the difference in case, the second variable
    # references the same 'wp' symbol.
    var2_var = fake_parent.symbol_table.lookup("var2")
    assert var2_var.datatype.precision is wp_var
    # Check that we get a symbol of unknown type if the KIND expression has
    # an unexpected structure
    # Break the parse tree by changing Name('wp') into a str
    fp2spec[0].items[0].items[1].items = ("(", "blah", ")")
    # Change the variable name too to prevent a clash
    fp2spec[0].children[2].children[0].items[0].string = "var3"
    processor = Fparser2Reader()
    processor.process_declarations(fake_parent, [fp2spec[0]], [])
    sym = fake_parent.symbol_table.lookup("var3")
    assert isinstance(sym, DataSymbol)
    assert isinstance(sym.datatype, UnknownFortranType)


@pytest.mark.xfail(reason="Kind parameter declarations not supported - #569")
@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_param():
    ''' Test that process_declarations handles the kind attribute when
    it specifies a previously-declared symbol.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("integer, parameter :: r_def = KIND(1.0D0)\n"
                                 "real(kind=r_def) :: var2")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert isinstance(fake_parent.symbol_table.lookup("var2").precision,
                      DataSymbol)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_use():
    ''' Test that process_declarations handles the kind attribute when
    it specifies a symbol accessed via a USE.

    '''
    fake_parent, _ = process_declarations("use kind_mod, only: r_def\n"
                                          "real(kind=r_def) :: var2")
    var2_var = fake_parent.symbol_table.lookup("var2")
    assert isinstance(var2_var.datatype.precision, DataSymbol)
    assert fake_parent.symbol_table.lookup("r_def") is \
        var2_var.datatype.precision


@pytest.mark.usefixtures("f2008_parser")
def test_wrong_type_kind_param():
    ''' Check that we raise the expected error if a variable used as a KIND
    specifier is not a DataSymbol or has already been declared with non-integer
    type.

    '''
    fake_parent, _ = process_declarations("integer :: r_def\n"
                                          "real(kind=r_def) :: var2")
    r_def = fake_parent.symbol_table.lookup("r_def")
    # Monkeypatch this DataSymbol so that it appears to be a RoutineSymbol
    r_def.__class__ = RoutineSymbol
    with pytest.raises(TypeError) as err:
        _kind_symbol_from_name("r_def", fake_parent.symbol_table)
    assert ("found an entry of type 'RoutineSymbol' for variable 'r_def'" in
            str(err.value))
    # Repeat but declare r_def as real
    with pytest.raises(TypeError) as err:
        process_declarations("real :: r_def\n"
                             "real(kind=r_def) :: var2")
    assert ("already contains a DataSymbol for variable 'r_def'" in
            str(err.value))


@pytest.mark.parametrize("vartype, kind, precision",
                         [("real", "1.0d0", ScalarType.Precision.DOUBLE),
                          ("real", "1.0D7", ScalarType.Precision.DOUBLE),
                          ("real", "1_t_def", None),
                          ("real", "1.0", ScalarType.Precision.UNDEFINED),
                          ("real", "1.0E3", ScalarType.Precision.SINGLE),
                          # 32-bit integer
                          ("integer", "1", ScalarType.Precision.UNDEFINED),
                          # 64-bit integer
                          ("integer", str(1 << 31 + 4)+"_t_def", None)])
@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_literals(vartype, kind, precision):
    ''' Test that process_declarations handles variables declared with
    an explicit KIND specified using a literal constant.

    '''
    fake_parent, _ = process_declarations("{0}(kind=KIND({1})) :: var".
                                          format(vartype, kind))
    if not precision:
        assert fake_parent.symbol_table.lookup("var").datatype.precision is \
            fake_parent.symbol_table.lookup("t_def")
    else:
        assert (fake_parent.symbol_table.lookup("var").datatype.precision ==
                precision)


@pytest.mark.parametrize("vartype, kind",
                         [("logical", ".false."),
                          ("real", "-1.0D7"),
                          ("real", "kvar"),
                          ("real", "kvar(1)")])
@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_kind(vartype, kind):
    ''' Check that we get an unknown type for an unsupported kind specifier.
        TODO #569 - add support for some/all of these.

    '''
    sched, _ = process_declarations("{0}(kind=KIND({1})) :: var".format(
        vartype, kind))
    assert isinstance(sched.symbol_table.lookup("var").datatype,
                      UnknownFortranType)


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

    # Test that if symbol is not an array, it raises GenerationError
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
        class UnrecognizedType(object):
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
    assert type(vsym) == Symbol
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
def test_use_stmt():
    ''' Check that SymbolTable entries are correctly created from
    module use statements. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod, only: some_var\n"
                                 "use this_mod\n"
                                 "use other_mod, only: var1, var2\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])

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
    a symbol with UnknownFortranType and the correct visibility. '''
    fake_parent = KernelSchedule("dummy")
    processor = Fparser2Reader()
    reader = FortranStringReader("integer, private, target :: idx1\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    sym = fake_parent.symbol_table.lookup("idx1")
    assert isinstance(sym.datatype, UnknownFortranType)
    assert sym.visibility == Symbol.Visibility.PRIVATE
    # No access statement so should be public (the default in Fortran)
    reader = FortranStringReader("integer, target :: idx2\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.children, [])
    sym = fake_parent.symbol_table.lookup("idx2")
    assert isinstance(sym.datatype, UnknownFortranType)
    assert sym.visibility == Symbol.Visibility.PUBLIC
    # No access statement so should pick up the default visibility supplied
    # to the symbol table.
    fake_parent.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    reader = FortranStringReader("integer, target :: idx3\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(
        fake_parent, fparser2spec.children, [], {})
    sym = fake_parent.symbol_table.lookup("idx3")
    assert isinstance(sym.datatype, UnknownFortranType)
    assert sym.visibility == Symbol.Visibility.PRIVATE
    # No access statement but visibility provided in visibility_map argument
    # to process_declarations()
    reader = FortranStringReader("integer, target :: idx4\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(
        fake_parent, fparser2spec.children, [],
        {"idx4": Symbol.Visibility.PUBLIC})
    sym = fake_parent.symbol_table.lookup("idx4")
    assert isinstance(sym.datatype, UnknownFortranType)
    assert sym.visibility == Symbol.Visibility.PUBLIC


@pytest.mark.usefixtures("f2008_parser")
def test_parse_array_dimensions_unhandled(monkeypatch):
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''

    def walk_ast_return(_1, _2, _3=None, _4=None):
        '''Function that returns a unique object that will not be part
        of the implemented handling in the walk method caller.'''
        class Invalid(object):
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

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2name])
    assert "No Symbol found for name 'x'." in str(error.value)

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


@pytest.mark.usefixtures("f2008_parser")
def test_handling_part_ref():
    ''' Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x(2)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2part_ref])
    assert "No Symbol found for name 'x'." in str(error.value)

    fake_parent.symbol_table.add(DataSymbol('x', INTEGER_TYPE))
    processor.process_nodes(fake_parent, [fparser2part_ref])
    assert len(fake_parent.children) == 1
    assignment = fake_parent.children[0]
    assert len(assignment.children) == 2
    new_node = assignment.children[0]
    assert isinstance(new_node, ArrayReference)
    assert new_node.name == "x"
    assert len(new_node.children) == 1  # Array dimensions

    # Parse a complex array expression
    reader = FortranStringReader("x(i+3,j-4,(z*5)+1)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0]

    fake_parent = KernelSchedule('assign')
    array_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    fake_parent.symbol_table.add(DataSymbol('x', array_type))
    fake_parent.symbol_table.add(DataSymbol('i', INTEGER_TYPE))
    fake_parent.symbol_table.add(DataSymbol('j', INTEGER_TYPE))
    fake_parent.symbol_table.add(DataSymbol('z', INTEGER_TYPE))
    processor.process_nodes(fake_parent, [fparser2part_ref])
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].lhs
    assert isinstance(new_node, ArrayReference)
    assert new_node.name == "x"
    assert len(new_node.children) == 3  # Array dimensions


@pytest.fixture(scope="module", name="symbol_table")
def make_symbol_table():
    '''
    pytest fixture to create and populate a symbol table for the
    'test_handling_intrinsics' test below.

    '''
    symbol_table = SymbolTable()
    symbol_table.new_symbol("x", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("a", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("b", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("c", symbol_type=DataSymbol,
                            datatype=REAL_TYPE)
    symbol_table.new_symbol("idx", symbol_type=DataSymbol,
                            datatype=INTEGER_TYPE)
    symbol_table.new_symbol("mask", symbol_type=DataSymbol,
                            datatype=ArrayType(INTEGER_TYPE, [10]))
    return symbol_table


@pytest.mark.parametrize(
    "code, expected_type, expected_op",
    [('x = exp(a)', UnaryOperation, UnaryOperation.Operator.EXP),
     ('x = sin(a)', UnaryOperation, UnaryOperation.Operator.SIN),
     ('x = asin(a)', UnaryOperation, UnaryOperation.Operator.ASIN),
     ('idx = ceiling(a)', UnaryOperation, UnaryOperation.Operator.CEIL),
     ('x = abs(a)', UnaryOperation, UnaryOperation.Operator.ABS),
     ('x = cos(a)', UnaryOperation, UnaryOperation.Operator.COS),
     ('x = acos(a)', UnaryOperation, UnaryOperation.Operator.ACOS),
     ('x = tan(a)', UnaryOperation, UnaryOperation.Operator.TAN),
     ('x = atan(a)', UnaryOperation, UnaryOperation.Operator.ATAN),
     ('x = real(a)', UnaryOperation, UnaryOperation.Operator.REAL),
     ('x = real(a, 8)', BinaryOperation, BinaryOperation.Operator.REAL),
     ('x = int(a)', UnaryOperation, UnaryOperation.Operator.INT),
     ('x = int(a, 8)', BinaryOperation, BinaryOperation.Operator.INT),
     ('x = log(a)', UnaryOperation, UnaryOperation.Operator.LOG),
     ('x = log10(a)', UnaryOperation, UnaryOperation.Operator.LOG10),
     ('x = mod(a, b)', BinaryOperation, BinaryOperation.Operator.REM),
     ('x = matmul(a, b)', BinaryOperation,
      BinaryOperation.Operator.MATMUL),
     ('x = max(a, b)', BinaryOperation, BinaryOperation.Operator.MAX),
     ('x = mAx(a, b, c)', NaryOperation, NaryOperation.Operator.MAX),
     ('x = min(a, b)', BinaryOperation, BinaryOperation.Operator.MIN),
     ('x = min(a, b, c)', NaryOperation, NaryOperation.Operator.MIN),
     ('x = sign(a, b)', BinaryOperation, BinaryOperation.Operator.SIGN),
     ('x = sqrt(a)', UnaryOperation, UnaryOperation.Operator.SQRT),
     ('x = sum(a)', UnaryOperation, UnaryOperation.Operator.SUM),
     ('x = sum(a, idx)', BinaryOperation, BinaryOperation.Operator.SUM),
     ('x = suM(a, idx, mask)', NaryOperation, NaryOperation.Operator.SUM),
     # Check that we get a CodeBlock for an unsupported N-ary operation
     ('x = reshape(a, b, c)', CodeBlock, None)])
@pytest.mark.usefixtures("f2008_parser")
def test_handling_intrinsics(code, expected_type, expected_op, symbol_table):
    ''' Test that fparser2 Intrinsic_Function_Reference nodes are
    handled appropriately.

    '''
    processor = Fparser2Reader()
    fake_parent = Schedule(symbol_table=symbol_table)
    reader = FortranStringReader(code)
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    assign = fake_parent.children[0]
    assert isinstance(assign, Assignment)
    assert isinstance(assign.rhs, expected_type), \
        "Fails when parsing '" + code + "'"
    if expected_type is not CodeBlock:
        assert assign.rhs._operator == expected_op, \
            "Fails when parsing '" + code + "'"


@pytest.mark.usefixtures("f2008_parser")
def test_intrinsic_no_args():
    ''' Check that an intrinsic with no arguments results in a
    NotImplementedError. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = SUM(a, b)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Manually remove the arguments
    fp2node.items = (fp2node.items[0],)
    with pytest.raises(NotImplementedError) as err:
        processor._intrinsic_handler(fp2node, fake_parent)
    assert "SUM" in str(err.value)


@pytest.mark.usefixtures("f2008_parser")
def test_unary_op_handler_error():
    ''' Check that the unary op handler raises the expected error if the
    parse tree has an unexpected structure. This is a hard error to
    provoke since fparser checks that the number of arguments is correct. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = exp(a)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Create an fparser node for a binary operation so that we can steal
    # its operands
    reader = FortranStringReader("x = max(a, b)")
    maxnode = Execution_Part.match(reader)[0][0].items[2]
    # Break the number of arguments in the fparser node by using those
    # from the binary operation
    fp2node.items = (fp2node.items[0], maxnode.items[1])
    with pytest.raises(InternalError) as err:
        processor._unary_op_handler(fp2node, fake_parent)
    assert ("Operation 'EXP(a, b)' has more than one argument and is "
            "therefore not unary" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_binary_op_handler_error():
    ''' Check that the binary op handler raises the expected errors if the
    parse tree has an unexpected structure. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = SUM(a, b)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Break the number of arguments in the fparser node
    fp2node.items[1].items = (Name('a'),)
    with pytest.raises(InternalError) as err:
        processor._binary_op_handler(fp2node, fake_parent)
    assert ("Binary operator should have exactly two arguments but found 1 "
            "for 'SUM(a)'." in str(err.value))
    # Now break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._binary_op_handler(fp2node, fake_parent)
    assert ("binary intrinsic operation 'SUM(dummy)'. Expected second child "
            "to be Actual_Arg_Spec_List" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_nary_op_handler_error():
    ''' Check that the Nary op handler raises the expected error if the parse
    tree has an unexpected structure. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x = SUM(a, b, mask)")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    # Give the node an incorrect number of arguments for the Nary handler
    fp2node.items[1].items = (Name('a'),)
    with pytest.raises(InternalError) as err:
        processor._nary_op_handler(fp2node, fake_parent)
    assert ("An N-ary operation must have more than two arguments but found 1 "
            "for 'SUM(a)'" in str(err.value))
    # Break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._nary_op_handler(fp2node, fake_parent)
    assert ("Expected second 'item' of N-ary intrinsic 'SUM(dummy)' in fparser"
            " parse tree to be an Actual_Arg_Spec_List" in str(err.value))


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_nested_intrinsic():
    ''' Check that we correctly handle nested intrinsic functions.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader(
        "ze_z = SUM( e1t(:,:) * e2t(:,:) * zav_tide(:,:,jk) * "
        "tmask_i(:,:) ) &\n"
        "   &  / MAX( 1.e-20, SUM( e1t(:,:) * e2t(:,:) * wmask (:,:,jk) * "
        "tmask_i(:,:) ) )")
    fp2node = Execution_Part.match(reader)[0][0]
    processor.process_nodes(fake_parent, [fp2node])
    array_refs = fake_parent.walk(Reference)
    assert "sum" not in [str(ref.name) for ref in array_refs]
    reader = FortranStringReader(
        "zccc = SQRT(MAX(zbbb * zbbb - 4._wp * rcpi * rLfus * ztmelts, 0.0))")
    fp2node = Execution_Part(reader)
    # Check that the frontend does not produce any CodeBlocks
    processor.process_nodes(fake_parent, fp2node.content)
    cblocks = fake_parent.children[1].walk(CodeBlock)
    assert not cblocks


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
                                     BinaryOperation.Operator.LBOUND)
        assert _is_bound_full_extent(array_reference, 1,
                                     BinaryOperation.Operator.UBOUND)
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
                BinaryOperation.Operator.LBOUND)
            assert _is_bound_full_extent(
                array_reference, dim,
                BinaryOperation.Operator.UBOUND)
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
                                 BinaryOperation.Operator.UBOUND)
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
                                 BinaryOperation.Operator.LBOUND)
    assert _is_array_range_literal(array_reference, dim=4, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=4, index=2, value=1)
    # dim 5
    _check_range(array_reference, dim=5)
    assert _is_bound_full_extent(array_reference, 5,
                                 BinaryOperation.Operator.LBOUND)
    assert _is_array_range_literal(array_reference, dim=5, index=1, value=2)
    assert _is_array_range_literal(array_reference, dim=5, index=2, value=3)
    # dim 6
    _check_range(array_reference, dim=6)
    assert _is_bound_full_extent(array_reference, 6,
                                 BinaryOperation.Operator.LBOUND)
    assert _is_bound_full_extent(array_reference, 6,
                                 BinaryOperation.Operator.UBOUND)
    assert _is_array_range_literal(array_reference, dim=6, index=2, value=3)
    # dim 7
    _check_range(array_reference, dim=7)
    assert _is_array_range_literal(array_reference, dim=7, index=0, value=1)
    assert _is_bound_full_extent(array_reference, 7,
                                 BinaryOperation.Operator.UBOUND)
    assert _is_array_range_literal(array_reference, dim=7, index=2, value=3)

    # Simple variables
    code = "a(b:, b:c, b:c:d) = 0.0"
    array_reference = _array_create(code)
    _check_array(array_reference, ndims=3)
    # dim 1
    _check_range(array_reference, dim=1)
    _check_reference(array_reference, dim=1, index=0, name="b")
    assert _is_bound_full_extent(array_reference, 1,
                                 BinaryOperation.Operator.UBOUND)
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
def test_handling_case_construct():
    ''' Test that fparser2 Case_Construct is converted to the expected PSyIR
    tree structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])

    # Check a new node was properly generated and connected to parent
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert ifnode.if_body.ast is fparser2case_construct.content[2]
    assert ifnode.if_body.ast_end is fparser2case_construct.content[2]
    assert 'was_case' in ifnode.annotations
    assert ifnode.condition.children[0].name == 'selector'
    assert ifnode.condition.children[1].name == 'label1'
    assert ifnode.if_body[0].children[0].name == 'branch1'
    assert isinstance(ifnode.else_body[0], IfBlock)
    assert ifnode.else_body[0].condition.children[1].name == 'label2'
    assert ifnode.else_body[0].if_body[0].children[0].name == 'branch2'
    assert ifnode.else_body[0].ast is \
        fparser2case_construct.content[4]
    assert ifnode.else_body[0].children[1].ast is \
        fparser2case_construct.content[4]
    assert ifnode.else_body[0].children[1].ast_end is \
        fparser2case_construct.content[4]
    assert len(ifnode.else_body[0].children) == 2  # SELECT CASE ends here


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_case_default():
    ''' Check that the fparser2Reader handles SELECT blocks with
    a default clause.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    case_clauses = ["CASE default\nbranch3 = 1\nbranch3 = branch3 * 2\n",
                    "CASE (label1)\nbranch1 = 1\n",
                    "CASE (label2)\nbranch2 = 1\n"]
    # Loop over the 3 possible locations for the 'default' clause
    for idx1, idx2, idx3 in [(0, 1, 2), (1, 0, 2), (1, 2, 0)]:
        fortran_text = (
            "SELECT CASE (selector)\n"
            "{0}{1}{2}"
            "END SELECT\n".format(case_clauses[idx1], case_clauses[idx2],
                                  case_clauses[idx3]))
        reader = FortranStringReader(fortran_text)
        fparser2case_construct = Execution_Part.match(reader)[0][0]

        fake_parent = Schedule()
        processor = Fparser2Reader()
        processor.process_nodes(fake_parent, [fparser2case_construct])
        assigns = fake_parent.walk(Assignment)
        # Check that the assignment to 'branch 3' (in the default clause) is
        # the deepest in the tree
        assert "branch3" in str(assigns[2])
        assert isinstance(assigns[2].ast, Assignment_Stmt)
        assert isinstance(assigns[2].parent, Schedule)
        assert isinstance(assigns[2].parent.ast, Assignment_Stmt)
        assert "branch3 * 2" in str(assigns[2].parent.ast_end)
        assert isinstance(assigns[2].parent.parent, IfBlock)
        # Check that the if-body of the parent IfBlock also contains
        # an Assignment
        assert isinstance(assigns[2].parent.parent.children[1], Schedule)
        assert isinstance(assigns[2].parent.parent.children[1].children[0],
                          Assignment)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_list():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a list of conditions.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label2, label3)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.condition, BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.OR
    eqnode = ifnode.condition.children[0]
    assert eqnode.operator == BinaryOperation.Operator.EQ
    assert "my_var" in str(eqnode.children[0])
    assert "label2" in str(eqnode.children[1])
    eqnode = ifnode.children[0].children[1]
    assert eqnode.operator == BinaryOperation.Operator.EQ
    assert "my_var" in str(eqnode.children[0])
    assert "label3" in str(eqnode.children[1])

    assert "Reference[name:'branch2']" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_range():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a range.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label4:label5)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.children[0], BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.AND
    assert ifnode.condition.children[0].operator == BinaryOperation.Operator.GE
    assert ifnode.condition.children[1].operator == BinaryOperation.Operator.LE
    assert "branch3" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_range_list():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a list of ranges.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (:label1, label5:, label6)
                branch4 = 1
            END SELECT''')
    # We should end up with:
    #    my_var <= label1 OR my_var >= label5 OR my_var == label6
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.condition, BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.OR
    assert ifnode.condition.children[0].operator == BinaryOperation.Operator.LE
    assert "label1" in str(ifnode.condition.children[0].children[1])
    orop = ifnode.condition.children[1]
    assert orop.operator == BinaryOperation.Operator.OR
    assert orop.children[0].operator == BinaryOperation.Operator.GE
    assert "label5" in str(orop.children[0].children[1])
    assert orop.children[1].operator == BinaryOperation.Operator.EQ
    assert "label6" in str(orop.children[1].children[1])
    assert "branch4" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_invalid_case_construct():
    ''' Test that the Case_Construct handler raises the proper errors when
    it parses invalid or unsupported fparser2 trees.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    # CASE (default) is just a regular symbol named default
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (default)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert isinstance(fake_parent.children[0], IfBlock)

    # Test with no opening Select_Case_Stmt
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    del fparser2case_construct.content[0]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "Failed to find opening case statement in:" in str(error.value)

    # Test with no closing End_Select_Stmt
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    del fparser2case_construct.content[-1]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "Failed to find closing case statement in:" in str(error.value)

    # Test when one clause is not of the expected type
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    fparser2case_construct.content[1].items = (Name("Fake"), None)
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "to be a Case_Selector but got" in str(error.value)


@pytest.mark.usefixtures("parser")
def test_handling_labelled_case_construct():
    ''' Test that a labelled case construct results in a CodeBlock. '''
    reader = FortranStringReader(
        '''999 SELECT CASE (selector)
            CASE (pick_me)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("selector", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("pick_me", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("branch3", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_case_default_only():
    ''' Check that we handle a select case that contains only a
    default clause and is thus redundant. The PSyIR should represent
    only the code that is within the default case.

    '''
    fake_parent = Schedule()
    fake_parent.symbol_table.add(Symbol("a"))
    processor = Fparser2Reader()
    reader = FortranStringReader(
        '''SELECT CASE ( jprstlib )
           CASE DEFAULT
             WRITE(numout,*) 'open ice restart NetCDF file: '
             a = 1
           END SELECT''')
    exe_part = Execution_Part.match(reader)
    processor.process_nodes(fake_parent, exe_part[0])
    # We should have no IfBlock in the resulting PSyIR
    assert len(fake_parent.children) == 2
    assert isinstance(fake_parent.children[0], CodeBlock)
    assert isinstance(fake_parent.children[1], Assignment)


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

    # Test parsing all supported binary operators.
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
                ('.LE.', BinaryOperation.Operator.LE),
                ('.and.', BinaryOperation.Operator.AND),
                ('.or.', BinaryOperation.Operator.OR))

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
        do while(a .gt. b)
            c = c + 1
        end do
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
            do while(a .gt. b)
                c = c + 1
            end do
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

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Fparser2Reader.nodes_to_code_block(Directive(), "hello")
    assert ("A CodeBlock with a Directive as parent is not yet supported."
            in str(excinfo.value))


def test_loop_var_exception(parser):
    '''Checks that the expected exception is raised in class
    Fparser2Reader method generate_schedule if a loop variable is not
    declared and there is no unqualified use statement.

    '''
    code = ('''
      subroutine test()
        do i=1,10
        end do
      end subroutine test
    ''')
    reader = FortranStringReader(code)
    fparser_tree = parser(reader)
    fparser2psyir = Fparser2Reader()
    with pytest.raises(InternalError) as excinfo:
        _ = fparser2psyir.generate_schedule("test", fparser_tree)
    assert (
        "Loop-variable name 'i' is not declared and there are no unqualified "
        "use statements. This is currently unsupported."
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
    # We should have an entry for "a_var" in the Container symbol table
    # due to the access in "test_sub1".
    container = psy.invokes.container
    avar1 = container.symbol_table.lookup("a_var")
    # It must be a generic Symbol since we don't know anything about it
    # pylint: disable=unidiomatic-typecheck
    assert type(avar1) == Symbol
    # There should be no entry for "a_var" in the symbol table for the
    # "test_sub1" routine as it is not declared there.
    schedule = psy.invokes.invoke_list[0].schedule
    assert "a_var" not in schedule.symbol_table
    # There should be another, distinct entry for "a_var" in the symbol table
    # for "test_sub2" as it has a use statement that imports it.
    schedule = psy.invokes.invoke_list[1].schedule
    avar2 = schedule.symbol_table.lookup("a_var")
    assert type(avar2) == Symbol
    assert avar2 is not avar1
