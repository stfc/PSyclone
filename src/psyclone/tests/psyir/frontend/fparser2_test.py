# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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

from psyclone.configuration import Config
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, default_precision, default_integer_type,
    default_real_type, _first_type_match, _get_arg_names)
from psyclone.psyir.nodes import (
    Schedule, CodeBlock, Assignment, Return, UnaryOperation, BinaryOperation,
    IfBlock, Reference, ArrayReference, Literal, KernelSchedule,
    RegionDirective, Routine, StandaloneDirective,
    Call, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, ContainerSymbol, ArgumentInterface, ArrayType,
    SymbolError, ScalarType, INTEGER_TYPE, REAL_TYPE, RoutineSymbol,
    UnsupportedFortranType, UnresolvedType, Symbol, UnresolvedInterface,
    ImportInterface, BOOLEAN_TYPE, StaticInterface, UnknownInterface,
    StructureType, DataTypeSymbol)

# pylint: disable=too-many-statements


# Tests


def test_constructor():
    ''' Test the constructor and its arguments '''
    processor = Fparser2Reader()

    # By default it will not resolve external modules
    assert processor._resolve_all_modules is False
    assert processor._modules_to_resolve == []

    # But it can be set to true or a list of module names
    processor = Fparser2Reader(resolve_modules=True)
    assert processor._resolve_all_modules is True
    assert processor._modules_to_resolve == []

    processor = Fparser2Reader(resolve_modules=['module1'])
    assert processor._resolve_all_modules is False
    assert "module1" in processor._modules_to_resolve

    # Anything else is invalid
    with pytest.raises(TypeError) as err:
        processor = Fparser2Reader(resolve_modules=[123])
    assert ("The 'resolve_modules' argument must be a boolean or an "
            "Iterable[str] but found '[123]'." in str(err.value))

    with pytest.raises(TypeError) as err:
        processor = Fparser2Reader(resolve_modules=456)
    assert ("The 'resolve_modules' argument must be a boolean or an "
            "Iterable[str] but found '456'." in str(err.value))


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


@pytest.mark.usefixtures("f2008_parser")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
    processor = Fparser2Reader()
    st = fake_parent.symbol_table

    # Entry in symbol table with unmodified properties.
    reader = FortranStringReader("integer :: l1=2")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, st, {})
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
    datatype, init = processor._get_partial_datatype(node, fake_parent, st, {})
    assert isinstance(datatype, ScalarType)
    assert isinstance(init, IntrinsicCall)
    assert init.parent is None
    assert datatype.intrinsic is ScalarType.Intrinsic.INTEGER
    # Check fparser2 tree is unmodified
    assert ids == [id(entry) for entry in walk(node)]

    # Entry in symbol table with partial information. Example has one
    # unsupported attribute and one supported attribute.
    reader = FortranStringReader("real*4, target, dimension(10,20) :: l1")
    node = Specification_Part(reader).content[0]
    ids = [id(entry) for entry in walk(node)]
    datatype, init = processor._get_partial_datatype(node, fake_parent, st, {})
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
    dtype, init = processor._get_partial_datatype(node, fake_parent, st, {})
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
    datatype, init = processor._get_partial_datatype(node, fake_parent, st, {})
    assert isinstance(datatype, ScalarType)
    assert isinstance(init, IntrinsicCall)
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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

    # Initialisation with constant expressions, including where the expression
    # references the symbol being declared.
    reader = FortranStringReader("real, parameter :: i4 = HUGE(i4), "
                                 "i5 = i4 * 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    i4sym = symtab.lookup("i4")
    assert isinstance(i4sym.initial_value, IntrinsicCall)
    # The initial value of i4sym should contain a reference to i4sym.
    assert i4sym.initial_value.arguments[0].symbol is i4sym
    i5sym = symtab.lookup("i5")
    assert isinstance(i5sym.initial_value, BinaryOperation)
    assert i5sym.initial_value.operands[0].symbol is i4sym

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
    assert isinstance(ptr_sym.initial_value, IntrinsicCall)


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("decln_text",
                         ["integer, pointer :: l1 => null(), l2 => null()",
                          "integer, intent(in), optional :: l1, l2",
                          "integer, target :: l1, l2"])
def test_process_declarations_unsupportedfortrantype(decln_text):
    '''Test that process_declarations method of Fparser2Reader adds
    datatype information to an UnsupportedFortranType by
    calling the get_partial_datatype method, also from Fparser2Reader.

    '''
    fake_parent = KernelSchedule.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(decln_text)
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    sched = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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

    # Test that CodeBlocks and references to variables initialised with a
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


def test_unsupported_decln(fortran_reader):
    '''
    Check that the frontend raises the expected error if it hits trouble
    while creating a DataSymbol.

    '''
    code = '''
    module my_mod
    contains
    function problem()
      ! Deliberately broken Fortran - a PARAMETER without an initial value.
      real, parameter :: problem
    end function problem
    end module my_mod
    '''
    with pytest.raises(InternalError) as err:
        _ = fortran_reader.psyir_from_source(code)
    assert ("Invalid variable declaration found in _process_decln for "
            "'problem'" in str(err.value))

    reader = FortranStringReader("integer, intent(in) :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    # Break the attribute-spec list for this declaration.
    attr_specs = fparser2spec.items[1]
    attr_specs.items = (attr_specs.items[0], "rubbish")
    fake_parent = KernelSchedule.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    with pytest.raises(NotImplementedError) as error:
        processor._process_decln(fake_parent, symtab, fparser2spec, {})
    assert "Unrecognised attribute type 'str'" in str(error.value)


def test_unsupported_decln_structure_type(fortran_reader):
    '''
    Check that the frontend generated code for unsupported values when
    creating a DataSymbol.
    '''
    code = '''
    module my_mod
    use some_other_mod
    contains
    subroutine my_sub
       type(some_type), parameter :: x = func()
    end subroutine my_sub
    end module my_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine.symbol_table.lookup('x').datatype,
                      UnsupportedFortranType)
    assert (routine.symbol_table.lookup('x').datatype.declaration ==
            "TYPE(some_type), PARAMETER :: x = func()")


@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_decln_duplicate_symbol():
    ''' Check that we raise the expected error when an unsupported declaration
    of only one symbol clashes with an existing entry in the symbol table. '''
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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

    # When lower bound is default (i.e. 1)
    reader = FortranStringReader("integer :: var3(1:)")
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("var3")
    assert len(symbol.shape) == 1
    # Shape should be an ArrayBounds with known lower bound and ATTRIBUTE
    # upper.
    assert isinstance(symbol.datatype.shape[0], ArrayType.ArrayBounds)
    assert symbol.datatype.shape[0].lower.value == "1"
    assert symbol.datatype.shape[0].upper == ArrayType.Extent.ATTRIBUTE


@pytest.mark.usefixtures("f2008_parser")
def test_process_array_declarations_bound_expressions():
    ''' Test that Fparser2Reader.process_declarations() handles
    array declarations that use expressions to specify the bounds.
    '''
    fake_parent = KernelSchedule.create("dummy_schedule")
    processor = Fparser2Reader()

    # Simple expression for upper bound
    reader = FortranStringReader("integer :: l3(l1+1)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l3_var = fake_parent.symbol_table.lookup("l3")
    dtype = l3_var.datatype
    assert isinstance(dtype, ArrayType)
    assert isinstance(dtype.shape[0], ArrayType.ArrayBounds)
    assert dtype.shape[0].lower.value == "1"
    assert isinstance(dtype.shape[0].upper, BinaryOperation)
    # Complicated expressions using intrinsics.
    reader = FortranStringReader(
        "integer :: l5(nint(minval(l4)):nint(maxval(l4)))")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l5_var = fake_parent.symbol_table.lookup("l5")
    l5dtype = l5_var.datatype
    assert isinstance(l5dtype, ArrayType)
    assert isinstance(l5dtype.shape[0], ArrayType.ArrayBounds)
    assert isinstance(l5dtype.shape[0].lower, IntrinsicCall)
    assert l5dtype.shape[0].lower.intrinsic is IntrinsicCall.Intrinsic.NINT
    assert isinstance(l5dtype.shape[0].upper, IntrinsicCall)
    assert l5dtype.shape[0].upper.intrinsic is IntrinsicCall.Intrinsic.NINT
    assert l5dtype.shape[0].upper.debug_string() == "NINT(MAXVAL(l4))"


@pytest.mark.usefixtures("f2008_parser")
def test_process_not_supported_declarations():
    '''Test that process_declarations method raises the proper errors when
    declarations contain unsupported attributes.
    '''
    fake_parent = KernelSchedule.create("dummy_schedule")
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

    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    processor = Fparser2Reader()
    sched = KernelSchedule.create("a_test")
    sym_table = sched.symbol_table
    reader = FortranStringReader("dimension(:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = processor._parse_dimensions(fparser2spec, sym_table)
    assert shape == [None]

    reader = FortranStringReader("dimension(:,:,:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = processor._parse_dimensions(fparser2spec, sym_table)
    assert shape == [None, None, None]

    reader = FortranStringReader("dimension(3,5)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = processor._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 2
    assert shape[0][0].value == "1"
    assert shape[0][1].value == "3"
    assert shape[1][0].value == "1"
    assert shape[1][1].value == "5"

    sym_table.add(DataSymbol('var1', INTEGER_TYPE))
    sym_table.add(DataSymbol('var1_upper', INTEGER_TYPE))

    reader = FortranStringReader("dimension(var1)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = processor._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 1
    assert shape[0][0].value == "1"
    assert shape[0][1].symbol == sym_table.lookup('var1')

    reader = FortranStringReader("dimension(0:3,var1)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = processor._parse_dimensions(fparser2spec, sym_table)
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
    shape = processor._parse_dimensions(fparser2spec, sym_table)
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
        _ = processor._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert "Assumed-size arrays are not supported." in str(error.value)

    # Shape specified by an unknown Symbol
    reader = FortranStringReader("dimension(var3)")
    fparser2spec = Dimension_Attr_Spec(reader)
    csym = sym_table.new_symbol("some_mod", symbol_type=ContainerSymbol)
    vsym = sym_table.new_symbol("var3", interface=ImportInterface(csym))
    # pylint: disable=unidiomatic-typecheck
    assert type(vsym) is Symbol
    shape = processor._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 1
    assert shape[0][0].value == "1"
    assert isinstance(shape[0][1], Reference)
    # Symbol is the same object.
    assert shape[0][1].symbol is vsym
    assert shape[0][1].symbol.name == "var3"
    assert isinstance(shape[0][1].symbol.interface, ImportInterface)

    # Test dimension and intent arguments together
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, dimension(n) :: array3")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec, [])
    dim_sym = fake_parent.symbol_table.lookup("n")
    assert isinstance(dim_sym.interface, UnresolvedInterface)
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
    fake_parent = KernelSchedule.create("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod, only: some_var\n"
                                 "use this_mod\n"
                                 "use other_mod, only: var1, var2, sub1\n")
    fparser2spec = Specification_Part(reader)

    # In some cases we might already know that one of the symbols being
    # brought into scope is a Routine so include this situation.
    table = fake_parent.symbol_table
    table.add(RoutineSymbol("sub1"))

    processor._process_use_stmts(fake_parent, fparser2spec.content)

    symtab = fake_parent.symbol_table

    for module_name in ["my_mod", "this_mod", "other_mod"]:
        container = symtab.lookup(module_name)
        assert isinstance(container, ContainerSymbol)
        assert container.name == module_name
        # It is not evaluated unless explicitly requested
        assert not container._reference

    for var in ["some_var", "var1", "var2"]:
        assert symtab.lookup(var).name == var

    assert (symtab.lookup("some_var").interface.container_symbol ==
            symtab.lookup("my_mod"))
    assert (symtab.lookup("var2").interface.container_symbol ==
            symtab.lookup("other_mod"))
    # The existing RoutineSymbol should have had its interface updated.
    assert (symtab.lookup("sub1").interface.container_symbol ==
            symtab.lookup("other_mod"))

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


@pytest.mark.parametrize("value",
                         [True,                  # All enabled
                          ["other1", "other2"],  # Precise name enabled
                          False])                # Disabled
def test_process_use_stmts_resolving_external_imports(
        parser, tmp_path, monkeypatch, value):
    ''' Test that if the Fparser2Reader if provided with a list of
    modules_to_import this are used to resolve external symbol information
    by the frontend.'''

    # Write a first module into a tmp file
    other1 = tmp_path / "other1.f90"
    with open(other1, "w", encoding='utf-8') as my_file:
        my_file.write('''
    module other1
        integer, parameter :: N = 10
        integer, dimension(N) :: unused_array
        integer, dimension(N), private :: private_array
    contains
        function a_func(i)
            integer :: a_func
            integer, intent(in) :: i
            a_func = 3
        end function
    end module other1
    ''')

    # Write a second module to a tmp file
    other2 = tmp_path / "other2.F90"
    with open(other2, "w", encoding='utf-8') as my_file:
        my_file.write('''
    module other2
        integer, dimension(10) :: an_array
        integer, dimension(10) :: other_array
    end module other2
    ''')

    # Add the path to the include_path and set up a frontend instance
    # with the module_to_resolve names
    monkeypatch.setattr(Config.get(), '_include_paths', [tmp_path])
    processor = Fparser2Reader(resolve_modules=value)
    reader = FortranStringReader('''
    module test
        use other1
        private
        public a_func
    contains
        subroutine test_function()
            use other2, only : an_array
            integer :: a
            a = an_array(1) + a_func(2)
        end subroutine
    end module
    ''')
    parse_tree = parser(reader)
    module = parse_tree.children[0]
    psyir = processor._module_handler(module, None)

    symtab = psyir.symbol_table

    if value is False:
        # If value is false, the symbol information is not resolved, e.g.
        # unused public symbols will not be present
        assert "unused_array" not in symtab
        return  # The rest of the asserts require this information

    # The container, and all its public symbols are now in the table with
    # the right symbol kind and datatype
    assert isinstance(symtab.lookup("other1"), ContainerSymbol)
    assert isinstance(symtab.lookup("a_func"), RoutineSymbol)
    assert isinstance(symtab.lookup("N"), DataSymbol)
    assert isinstance(symtab.lookup("unused_array"), DataSymbol)
    assert symtab.lookup("n").datatype == INTEGER_TYPE
    # But not the private symbols
    assert "private_array" not in symtab
    # The local symbols respect the local visibility statements
    assert symtab.lookup("other1").visibility == Symbol.Visibility.PRIVATE
    assert symtab.lookup("N").visibility == Symbol.Visibility.PRIVATE
    assert symtab.lookup("a_func").visibility == Symbol.Visibility.PUBLIC

    routine = psyir.children[0]
    innersymtab = routine.symbol_table
    # The container, and all its 'only'-listed symbols are now in the
    # routine symbol table
    assert isinstance(innersymtab.lookup("other2"), ContainerSymbol)
    assert isinstance(innersymtab.lookup("an_array"), DataSymbol)
    assert isinstance(innersymtab.lookup("an_array").datatype, ArrayType)
    # But not the other public symbols, nor in the container symbol table.
    assert "other_array" not in innersymtab
    assert "an_array" not in symtab

    # The provided info allows the reader to differentiate between function
    # calls and Array accesses :)
    stmt_rhs = routine[0].rhs
    assert isinstance(stmt_rhs.children[0], Reference)
    assert isinstance(stmt_rhs.children[1], Call)


def test_process_resolving_modules_give_correct_types(
        parser, tmp_path, monkeypatch):
    ''' Test that if the Fparser2Reader is provided with a list of
    modules_to_import these are used to resolve external symbol information
    by the frontend.'''

    # Write a first module into a tmp file
    other1 = tmp_path / "other.f90"
    with open(other1, "w", encoding='utf-8') as my_file:
        my_file.write('''
    module other
        implicit none
        integer, dimension(10) :: supported_array
        integer, dimension(10), target :: unsupported_array
    contains
        pure elemental function pure_func(i)
            integer :: pure_func
            integer, intent(in) :: i
            pure_func = 3
        end function
    end module
    ''')
    reader = FortranStringReader('''
    module test
        use other
    contains
        subroutine test_function()
            integer :: a
            a = supported_array(3)
            a = unsupported_array(3)
            a = pure_func(3)
        end subroutine
    end module
    ''')
    parse_tree = parser(reader)
    module = parse_tree.children[0]

    # By default this will all be parsed as Calls with unknown
    # is_elemental/is_pure attributes
    processor = Fparser2Reader()
    psyir = processor._module_handler(module, None)
    assigns = psyir.walk(Assignment)
    assert isinstance(assigns[0].rhs, Call)
    assert isinstance(assigns[1].rhs, Call)
    assert isinstance(assigns[2].rhs, Call)
    assert assigns[2].rhs.is_elemental is None
    assert assigns[2].rhs.is_pure is None

    # If we populate the module_to_resolve and add the include_path
    # then we know if they are arrays and pure/elemental
    processor = Fparser2Reader(resolve_modules=["other"])
    monkeypatch.setattr(Config.get(), '_include_paths', [tmp_path])
    psyir = processor._module_handler(module, None)
    assigns = psyir.walk(Assignment)
    assert isinstance(assigns[0].rhs, ArrayReference)
    assert isinstance(assigns[1].rhs, ArrayReference)
    assert isinstance(assigns[2].rhs, Call)
    assert assigns[2].rhs.is_elemental
    assert assigns[2].rhs.is_pure


def test_intrinsic_use_stmt(parser):
    ''' Tests that intrinsic value is set correctly for an intrinsic module
    use statement.'''
    processor = Fparser2Reader()
    reader = FortranStringReader('''
        module test
            use, intrinsic :: ieee_arithmetic, only: isnan =>ieee_is_nan
            use mymod
        end module test
    ''')
    parse_tree = parser(reader)
    module = parse_tree.children[0]
    psyir = processor._module_handler(module, None)
    symtab = psyir.symbol_table
    assert symtab.lookup("ieee_arithmetic").is_intrinsic
    assert not symtab.lookup("mymod").is_intrinsic

    processor = Fparser2Reader()
    reader = FortranStringReader('''
        module test
            use, non_intrinsic :: ieee_arithmetic, only: isnan =>ieee_is_nan
            use mymod
        end module test
    ''')
    parse_tree = parser(reader)
    module = parse_tree.children[0]
    psyir = processor._module_handler(module, None)
    symtab = psyir.symbol_table
    assert not symtab.lookup("ieee_arithmetic").is_intrinsic


@pytest.mark.usefixtures("f2008_parser")
def test_use_stmt_error(monkeypatch):
    ''' Check that we raise the expected error if the parse tree representing
    a USE statement doesn't have the expected structure. '''
    fake_parent = KernelSchedule.create("dummy_schedule")
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
    fake_parent = KernelSchedule.create("dummy")
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
        _ = Fparser2Reader()._parse_dimensions(fparser2spec, None)
    assert ("Reached end of loop body and array-shape specification"
            in str(error.value))
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

    fake_parent = KernelSchedule.create('kernel')
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
def test_handling_parenthesis_over_binary_op():
    ''' Test that fparser2 parenthesis are recorded in the appropriate
    attribute when they are explicit over BinaryOperations.
    '''
    reader = FortranStringReader("a = (a + b) + (c + d)")
    fparser2parenthesis = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2parenthesis])

    bop1 = fake_parent[0].rhs
    bop2 = bop1.children[0]
    bop3 = bop1.children[1]

    # The parent addition does not have explicit parenthesis
    assert not bop1.has_explicit_grouping
    # But the two inner ones have explicit parenthesis syntax
    assert bop2.has_explicit_grouping
    assert bop3.has_explicit_grouping


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
def test_handling_unaryopbase():
    ''' Test that fparser2 UnaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    reader = FortranStringReader("x=-4")
    assign_stmt = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [assign_stmt])
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
        assign_stmt = Execution_Part.match(reader)[0][0]
        # And then translate it to PSyIR again.
        fake_parent = Schedule()
        processor.process_nodes(fake_parent, [assign_stmt])
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent[0].rhs, UnaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent[0].rhs._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported unary operator creates a CodeBlock
    fp2unaryop = assign_stmt.children[2]
    fp2unaryop.items = ('unsupported', fp2unaryop.children[1])
    fake_parent = Schedule()
    processor.process_nodes(fake_parent, [assign_stmt])

    assert len(fake_parent.children) == 1
    new_node = fake_parent[0].rhs
    assert isinstance(new_node, CodeBlock)


def test_handling_return_stmt(fortran_reader):
    ''' Test that fparser2 Return_Stmt is converted to the expected PSyIR
    tree structure.
    '''
    test_code = '''
        module test_mod
        contains
          subroutine single_exit(a)
              integer, intent(inout) :: a
              a = 3
              return
          end subroutine
          subroutine multiple_exit(a)
              integer, intent(inout) :: a
              if (a .eq. 4) then
                  return
              end if
              a  = 3
              return 4
          end subroutine
        end module test_mod
    '''
    psyir = fortran_reader.psyir_from_source(test_code)
    routines = psyir.walk(Routine)
    # First subroutine return is redundant
    assert len(routines[0].walk(Return)) == 0
    # Second subroutine has a multi-exit return and an unsupported
    # alternate return
    assert len(routines[1].walk(Return)) == 1
    assert len(routines[1].walk(CodeBlock)) == 1
    assert ("Fortran alternate returns are not supported." in
            routines[1].walk(CodeBlock)[0].preceding_comment)


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
    processor = Fparser2Reader()
    schedule = processor.generate_psyir(prog).walk(Routine)[0]
    assert isinstance(schedule[0], CodeBlock)
    assert schedule[0].structure == CodeBlock.Structure.STATEMENT
    # Check that the error message that generated the codeblock has been
    # added as a preceding comment by nodes_to_code_block
    assert (schedule[0].preceding_comment ==
            "PSyclone CodeBlock (unsupported code) reason:\n"
            " - Unsupported label reference within DO")


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
    processor = Fparser2Reader()
    schedule = processor.generate_psyir(prog).walk(Routine)[0]
    assert isinstance(schedule[0].if_body[0], CodeBlock)
    assert schedule[0].if_body[0].structure == CodeBlock.Structure.STATEMENT
    # Check that the error message that generated the codeblock has been
    # added as a preceding comment by nodes_to_code_block
    assert (schedule[0].if_body[0].preceding_comment ==
            "PSyclone CodeBlock (unsupported code) reason:\n"
            " - Unsupported label reference within DO")


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
    processor = Fparser2Reader()
    psyir = processor.generate_psyir(prog)
    # We should not have an entry for "a_var" in the Container symbol
    # table as we don't know whether the access in "test_sub1" comes
    # from the wildcard import ("some_mod"). The Container is the
    # first child of the FileContainer node.
    container = psyir.children[0]
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
    assert len(call_node._argument_names) == len(call_node.arguments)
    for idx, child in enumerate(call_node.arguments):
        assert call_node._argument_names[idx] == (id(child), arg_names[idx])
    assert call_node.argument_names == arg_names
    assert len(call_node.arguments) == 3
    assert isinstance(call_node.arguments[0], Literal)
    assert call_node.arguments[0].value == "1.0"
    assert isinstance(call_node.arguments[1], Reference)
    assert call_node.arguments[1].name == "a"
    assert isinstance(call_node.arguments[2], BinaryOperation)


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
    assert len(intrinsic_node._argument_names) == len(intrinsic_node.arguments)
    arg_names = ["array", "dim", "mask"]
    for idx, child in enumerate(intrinsic_node.arguments):
        assert intrinsic_node._argument_names[idx] == (
            id(child), arg_names[idx])
    assert intrinsic_node.argument_names == arg_names
    assert len(intrinsic_node.arguments) == 3
    assert isinstance(intrinsic_node.arguments[0], Reference)
    assert intrinsic_node.arguments[0].name == "a"
    assert isinstance(intrinsic_node.arguments[1], Reference)
    assert intrinsic_node.arguments[1].name == "d"
    assert isinstance(intrinsic_node.arguments[2], Reference)
    assert intrinsic_node.arguments[2].name == "m"


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
    assert len(call_node.arguments) == 4
    assert isinstance(call_node.arguments[0], Reference)
    assert call_node.arguments[0].name == "a"
    assert isinstance(call_node.arguments[1], CodeBlock)
    assert isinstance(call_node.arguments[2], CodeBlock)
    assert isinstance(call_node.arguments[3], Reference)
    assert call_node.arguments[3].name == "b"


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
    # derived-type with initial value (StructureType) and in-line visibility
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

    # Repeat but have visibility of symbol specified separately.
    test_code = (
        "module test_mod\n"
        "    private :: my_type\n"
        "    type :: my_type\n"
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


def test_structures_duplicate_name(f2008_parser):
    '''
    Check that the datatype of a structure member correctly refers to
    a DataTypeSymbol in the parent scope.
    '''
    test_code = '''\
    subroutine test()
      integer, parameter :: nelem = 10
      type :: y
        integer, dimension(3) :: jp
      end type
      type :: x
        type(y), dimension(nelem) :: y
      end type
    end subroutine'''
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    psyir = processor.generate_psyir(ptree)
    routine = psyir.walk(Routine)[0]
    table = routine.symbol_table
    xsym = table.lookup("x")
    ysym = table.lookup("y")
    nelem = table.lookup("nelem")
    assert isinstance(xsym, DataTypeSymbol)
    dtype = xsym.datatype
    assert isinstance(dtype, StructureType)
    ycompt = dtype.components["y"]
    # The datatype of the member 'y' must be the 'y' DataTypeSymbol.
    assert isinstance(ycompt.datatype, ArrayType)
    assert ycompt.datatype.intrinsic is ysym
    # Its shape must refer to "nelem" in the table of the Routine.
    assert isinstance(ycompt.datatype.shape[0].upper, Reference)
    assert ycompt.datatype.shape[0].upper.symbol is nelem


def test_structuretype_used_before_def(fortran_reader):
    '''
    Test that an existing Symbol of unresolved type is specialised to
    a DataTypeSymbol.

    '''
    test_code = '''\
        module test_mod
          use some_mod, only: my_type
        contains
          subroutine test_code()
            type(my_type) :: var
          end subroutine
        end module test_mod'''
    psyir = fortran_reader.psyir_from_source(test_code)
    sym_table = psyir.children[0].symbol_table
    mytype = sym_table.lookup("my_type")
    assert isinstance(mytype, DataTypeSymbol)
    assert mytype.is_import
