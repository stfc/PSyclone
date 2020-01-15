# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyGen import PSyFactory, Node, Directive, Schedule, \
    CodeBlock, Assignment, Return, UnaryOperation, BinaryOperation, \
    NaryOperation, IfBlock, Reference, Array, KernelSchedule, \
    Container, InternalError, GenerationError, Literal
from psyclone.psyir.symbols import DataSymbol, ContainerSymbol, SymbolTable, \
    ArgumentInterface, SymbolError, DataType
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


def process_declarations(code):
    '''
    Utility routine to create PSyIR for Fortran variable declarations.

    :param str code: Fortran declaration statement(s)

    :returns: a 2-tuple consisting of a KernelSchedule with populated Symbol \
              Table and the parse tree for the specification part.
    :rtype: (:py:class:`psyclone.psyGen.KernelSchedule`, \
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
  type, extends(kernel_type) :: dummy_type
     type(arg_type) meta_args(3) =                     &
          (/ arg_type(gh_field, gh_write,     w3),     &
             arg_type(gh_field, gh_readwrite, wtheta), &
             arg_type(gh_field, gh_inc,       w1)      &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


# Class Fparser2Reader


# Method generate_container
def test_generate_container(parser):
    ''' Test that generate_container creates a PSyIR container with the
    contents of the given fparser2 fortran module.'''
    dummy_module = '''
    module dummy_mod
        use mod1
        use mod2, only: var1
        real :: modvar1
    contains
        subroutine dummy_code(f1, f2, f3)
            real(wp), dimension(:,:), intent(in)  :: f1
            real(wp), dimension(:,:), intent(out)  :: f2
            real(wp), dimension(:,:) :: f3
            f2 = f1 + 1
        end subroutine dummy_code
    end module dummy_mod
    '''
    reader = FortranStringReader(dummy_module)
    ast = parser(reader)
    processor = Fparser2Reader()
    container = processor.generate_container(ast)
    assert isinstance(container, Container)
    assert not container.children
    assert container.symbol_table
    assert container.symbol_table.lookup("modvar1")
    assert container.symbol_table.lookup("var1")
    assert container.symbol_table.lookup("mod1")
    assert container.symbol_table.lookup("mod2")


def test_generate_container_two_modules(parser):
    ''' Tests the fparser2Reader generate_container method raises an exception
    when more than one fparser2 module node is provided.
    '''
    reader = FortranStringReader(FAKE_KERNEL_METADATA*2)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test kernel with two modules
    with pytest.raises(GenerationError) as error:
        _ = processor.generate_container(ast)
    assert "Could not process" in str(error.value)
    assert "Just one module definition per file supported." in str(error.value)


# Method generate_schedule
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
    assert not container.symbol_table.symbols

    # Test that we get an error for a nonexistant subroutine name
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
    assert len(symbol_table.symbols) == 2
    assert symbol_table.lookup("scalar1")
    assert symbol_table.lookup("array1")


def test_generate_schedule_dummy_subroutine(parser):
    ''' Tests the fparser2Reader generate_schedule method with a simple
    subroutine.
    '''
    dummy_kernel_metadata = '''
    module dummy_mod
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                     &
              (/ arg_type(gh_field, gh_write,     w3),     &
                 arg_type(gh_field, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_inc,       w1)      &
               /)
         integer :: iterates_over = cells
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

    # Test argument intent is inferred when not available in the declaration
    assert schedule.symbol_table.lookup('f3').interface.access is \
        ArgumentInterface.Access.READWRITE

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
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                      &
              (/ arg_type(gh_field, gh_write,     w3),     &
                 arg_type(gh_field, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_inc,       w1)      &
               /)
         integer :: iterates_over = cells
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
      type, extends(kernel_type) :: dummy_type
         type(arg_type) meta_args(3) =                     &
              (/ arg_type(gh_field, gh_write,     w3),     &
                 arg_type(gh_field, gh_readwrite, wtheta), &
                 arg_type(gh_field, gh_inc,       w1)      &
               /)
         integer :: iterates_over = cells
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


def test_process_declarations(f2008_parser):
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
    assert fake_parent.symbol_table.lookup("l1").name == 'l1'
    assert fake_parent.symbol_table.lookup("l1").datatype == DataType.INTEGER
    assert fake_parent.symbol_table.lookup("l1").shape == []
    assert fake_parent.symbol_table.lookup("l1").is_local
    assert not fake_parent.symbol_table.lookup("l1").precision

    reader = FortranStringReader("Real      ::      l2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l2").name == "l2"
    assert fake_parent.symbol_table.lookup("l2").datatype == DataType.REAL
    assert not fake_parent.symbol_table.lookup("l2").precision

    reader = FortranStringReader("LOGICAL      ::      b")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("b").name == "b"
    assert fake_parent.symbol_table.lookup("b").datatype == DataType.BOOLEAN
    assert not fake_parent.symbol_table.lookup("b").precision

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

    # Initialisation with constant expresions
    reader = FortranStringReader("real, parameter :: i4 = 1.1, i5 = i4 * 2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("i4").constant_value.value == "1.1"
    assert isinstance(fake_parent.symbol_table.lookup("i5").constant_value,
                      BinaryOperation)

    # Initial values for variables are not supported
    reader = FortranStringReader("real:: a = 1.1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert "Initialisations on the declaration statements are only " \
           "supported for parameter declarations." in str(error.value)

    # Check we catch duplicated symbols
    reader = FortranStringReader("integer :: i2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(SymbolError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Symbol 'i2' already present in SymbolTable with a defined "
            "interface" in str(error.value))

    # Test with unsupported data type
    reader = FortranStringReader("doubleprecision     ::      c2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert (". Only 'real', 'integer', 'logical' and 'character' intrinsic "
            "types are supported.") in str(error.value)

    # Test with unsupported attribute
    reader = FortranStringReader("real, public :: p2")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert "Unrecognized attribute type " in str(error.value)

    # Char lengths are not supported
    # TODO: It would be simpler to do just a Specification_Part(reader) instead
    # of parsing a full program, but fparser/169 needs to be fixed first.
    reader = FortranStringReader("program dummy\ncharacter :: l*4"
                                 "\nend program")
    program = f2008_parser(reader)
    fparser2spec = program.content[0].content[1].content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Character length specifications are not "
            "supported.") in str(error.value)


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
    assert fake_parent.symbol_table.lookup("l3").name == 'l3'
    assert fake_parent.symbol_table.lookup("l3").datatype == DataType.INTEGER
    assert len(fake_parent.symbol_table.lookup("l3").shape) == 1
    assert not fake_parent.symbol_table.lookup("l3").precision

    reader = FortranStringReader("integer :: l4(l1, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l4").name == 'l4'
    assert fake_parent.symbol_table.lookup("l4").datatype == DataType.INTEGER
    assert len(fake_parent.symbol_table.lookup("l4").shape) == 2
    assert not fake_parent.symbol_table.lookup("l4").precision

    reader = FortranStringReader("integer :: l5(2), l6(3)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l5").shape == [2]
    assert fake_parent.symbol_table.lookup("l6").shape == [3]

    # Test that component-array-spec has priority over dimension attribute
    reader = FortranStringReader("integer, dimension(2) :: l7(3, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l7").name == 'l7'
    assert fake_parent.symbol_table.lookup("l7").shape == [3, 2]

    # Allocatable
    reader = FortranStringReader("integer, allocatable :: l8(:)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l8")
    assert symbol.name == "l8"
    assert symbol.shape == [DataSymbol.Extent.DEFERRED]

    # Unknown extents but not allocatable
    reader = FortranStringReader("integer :: l9(:, :)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    symbol = fake_parent.symbol_table.lookup("l9")
    assert symbol.name == "l9"
    assert symbol.shape == [DataSymbol.Extent.ATTRIBUTE,
                            DataSymbol.Extent.ATTRIBUTE]


@pytest.mark.usefixtures("f2008_parser")
def test_process_not_supported_declarations():
    '''Test that process_declarations method raises the proper errors when
    declarations contain unsupported attributes.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, external :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert ". Unrecognized attribute " in str(error.value)

    reader = FortranStringReader("integer, save :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert ". Unrecognized attribute " in str(error.value)

    reader = FortranStringReader("real, allocatable :: p3")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process " in str(error.value)
    assert ("'allocatable' attribute is only supported on array "
            "declarations" in str(error.value))

    # Allocatable but with specified extent. This is invalid Fortran but
    # fparser2 doesn't spot it (see fparser/#229).
    reader = FortranStringReader("integer, allocatable :: l10(5)")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(InternalError) as err:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "An array with defined extent cannot have the ALLOCATABLE" \
        in str(err.value)


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


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_new_param():
    ''' Test that process_declarations handles variables declared with
    an explicit KIND parameter that has not already been declared. Also
    check that the matching on the variable name is not case sensitive.

    '''
    fake_parent, fp2spec = process_declarations("real(kind=wp) :: var1\n"
                                                "real(kind=Wp) :: var2\n")
    assert isinstance(fake_parent.symbol_table.lookup("var1").precision,
                      DataSymbol)
    # Check that this has resulted in the creation of a new 'wp' symbol
    wp_var = fake_parent.symbol_table.lookup("wp")
    assert wp_var.datatype == DataType.INTEGER
    assert fake_parent.symbol_table.lookup("var1").precision is wp_var
    # Check that, despite the difference in case, the second variable
    # references the same 'wp' symbol.
    assert fake_parent.symbol_table.lookup("var2").precision is wp_var
    # Check that we raise an error if the KIND expression has an unexpected
    # structure
    # Break the parse tree by changing Name('wp') into a str
    fp2spec[0].items[0].items[1].items = ("(", "blah", ")")
    processor = Fparser2Reader()
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(fake_parent, fp2spec, [])
    assert ("Failed to find valid Name in Fortran Kind Selector: "
            "'(KIND = blah)'" in str(err.value))


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
    assert isinstance(fake_parent.symbol_table.lookup("var2").precision,
                      DataSymbol)
    assert fake_parent.symbol_table.lookup("r_def") is \
        fake_parent.symbol_table.lookup("var2").precision


@pytest.mark.usefixtures("f2008_parser")
def test_wrong_type_kind_param():
    ''' Check that we raise the expected error if a variable used as a KIND
    specifier has already been declared with non-integer type.

    '''
    with pytest.raises(TypeError) as err:
        process_declarations("real :: r_def\n"
                             "real(kind=r_def) :: var2")
    assert "already contains an entry for variable 'r_def'" in str(err.value)


@pytest.mark.parametrize("vartype, kind, precision",
                         [("real", "1.0d0", DataSymbol.Precision.DOUBLE),
                          ("real", "1.0D7", DataSymbol.Precision.DOUBLE),
                          ("real", "1_t_def", None),
                          ("real", "1.0", DataSymbol.Precision.SINGLE),
                          ("real", "1.0E3", DataSymbol.Precision.SINGLE),
                          # 32-bit integer
                          ("integer", "1", DataSymbol.Precision.SINGLE),
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
        assert fake_parent.symbol_table.lookup("var").precision is \
            fake_parent.symbol_table.lookup("t_def")
    else:
        assert fake_parent.symbol_table.lookup("var").precision == precision


@pytest.mark.parametrize("vartype, kind",
                         [("logical", ".false."),
                          ("real", "-1.0D7"),
                          ("real", "kvar"),
                          ("real", "kvar(1)")])
@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_kind(vartype, kind):
    ''' Check that we raise an error for an unsupported kind specifier.
        TODO #569 - add support for some/all of these.

    '''
    with pytest.raises(NotImplementedError) as err:
        process_declarations("{0}(kind=KIND({1})) :: var".format(vartype,
                                                                 kind))
    assert ("Only real and integer literals are supported as arguments to"
            in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_stmt_functions():
    '''Test that process_declarations method handles statement functions
    appropriately.
    '''
    from fparser.two.Fortran2003 import Stmt_Function_Stmt
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
    fake_parent.symbol_table.add(
        DataSymbol('a', DataType.REAL, shape=[DataSymbol.Extent.ATTRIBUTE]))
    fake_parent.symbol_table.add(DataSymbol('x', DataType.REAL, shape=[]))
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert len(fake_parent.children) == 1
    array = fake_parent.children[0].children[0]
    assert isinstance(array, Array)
    assert array.name == "a"

    # Test that it works with multi-dimensional arrays
    fake_parent = KernelSchedule("dummy_schedule")
    reader = FortranStringReader("b(x, y) = 1")
    fparser2spec = Stmt_Function_Stmt(reader)
    fake_parent.symbol_table.add(
        DataSymbol('b', DataType.REAL, shape=[DataSymbol.Extent.ATTRIBUTE,
                                              DataSymbol.Extent.ATTRIBUTE]))
    fake_parent.symbol_table.add(DataSymbol('x', DataType.INTEGER, shape=[]))
    fake_parent.symbol_table.add(DataSymbol('y', DataType.INTEGER, shape=[]))
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert len(fake_parent.children) == 1
    array = fake_parent.children[0].children[0]
    assert isinstance(array, Array)
    assert array.name == "b"

    # Test that if symbol is not an array, it raises GenerationError
    fake_parent.symbol_table.lookup('b')._shape = []
    with pytest.raises(InternalError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process '" in str(error.value)
    assert "'. Symbol 'b' is in the SymbolTable but it is not an array as " \
        "expected, so it can not be recovered as an array assignment." \
        in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_parse_array_dimensions_attributes():
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''
    from fparser.two.Fortran2003 import Name, Dimension_Attr_Spec

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
    assert shape == [3, 5]

    sym_table.add(DataSymbol('var1', DataType.INTEGER, []))
    reader = FortranStringReader("dimension(var1)")
    fparser2spec = Dimension_Attr_Spec(reader)
    shape = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert len(shape) == 1
    assert shape[0] == sym_table.lookup('var1')

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
        sym_table.add(DataSymbol("var2", DataType.REAL, []))
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert ("Only scalar integer literals or symbols are supported for "
            "explicit shape array declarations.") in str(error.value)

    # Explicit shape symbols can only be Literal or Symbol
    with pytest.raises(NotImplementedError) as error:
        class UnrecognizedType(object):
            '''Type guaranteed to not be part of the _parse_dimensions
            conditional type handler.'''
        fparser2spec.items[1].items[0].items[1].__class__ = UnrecognizedType
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert ("Only scalar integer literals or symbols are supported for "
            "explicit shape array declarations.") in str(error.value)

    # Test dimension and intent arguments together
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, intent(in), dimension(:) :: array3")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec],
                                   [Name("array3")])
    assert fake_parent.symbol_table.lookup("array3").name == "array3"
    assert fake_parent.symbol_table.lookup("array3").datatype == DataType.REAL
    assert fake_parent.symbol_table.lookup("array3").shape == \
        [DataSymbol.Extent.ATTRIBUTE]
    assert fake_parent.symbol_table.lookup("array3").interface.access is \
        ArgumentInterface.Access.READ


@pytest.mark.usefixtures("f2008_parser")
def test_deferred_array_size():
    ''' Check that we handle the case of an array being declared with an
    extent specified by a variable that is declared after it. '''
    from fparser.two.Fortran2003 import Name
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, intent(in), dimension(n) :: array3\n"
                                 "integer, intent(in) :: n")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec,
                                   [Name("array3"), Name("n")])
    dim_sym = fake_parent.symbol_table.lookup("n")
    assert isinstance(dim_sym.interface, ArgumentInterface)
    assert dim_sym.datatype == DataType.INTEGER


@pytest.mark.usefixtures("f2008_parser")
def test_unresolved_array_size():
    ''' Check that we handle the case where we do not find an explicit
    declaration of a symbol used in the definition of an array extent. '''
    from psyclone.psyir.symbols import UnresolvedInterface
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("real, dimension(n) :: array3")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec, [])
    dim_sym = fake_parent.symbol_table.lookup("n")
    assert isinstance(dim_sym.interface, UnresolvedInterface)
    assert dim_sym.datatype == DataType.INTEGER
    # Check that the lookup of the dimensioning symbol is not case sensitive
    reader = FortranStringReader("real, dimension(N) :: array4")
    fparser2spec = Specification_Part(reader).content
    processor.process_declarations(fake_parent, fparser2spec, [])
    assert fake_parent.symbol_table.lookup("array4").shape[0] is dim_sym


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
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert ("Expected the parse tree for a USE statement to contain 5 items "
            "but found 3 for 'hello'" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_parse_array_dimensions_unhandled(monkeypatch):
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''
    from fparser.two.Fortran2003 import Dimension_Attr_Spec
    import fparser

    def walk_ast_return(_1, _2, _3=None, _4=None):
        '''Function that returns a unique object that will not be part
        of the implemented handling in the walk_ast method caller.'''
        class Invalid(object):
            '''Class that would be invalid to return from an fparser2 parse
            tree.'''
        newobject = Invalid()
        return [newobject]

    monkeypatch.setattr(fparser.two.utils, 'walk_ast', walk_ast_return)

    reader = FortranStringReader("dimension(:)")
    fparser2spec = Dimension_Attr_Spec(reader)
    with pytest.raises(InternalError) as error:
        _ = Fparser2Reader._parse_dimensions(fparser2spec, None)
    assert "Reached end of loop body and array-shape specification" \
        in str(error.value)
    assert " has not been handled." in str(error.value)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_assignment_stmt():
    ''' Test that fparser2 Assignment_Stmt is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x=1")
    fparser2assignment = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2assignment], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Assignment)
    assert len(new_node.children) == 2


@pytest.mark.usefixtures("f2008_parser")
def test_handling_name():
    ''' Test that fparser2 Name is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x=1")
    fparser2name = Execution_Part.match(reader)[0][0].items[0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2name], None)
    assert "Undeclared reference 'x' found." in str(error.value)

    fake_parent.symbol_table.add(DataSymbol('x', DataType.INTEGER))
    processor.process_nodes(fake_parent, [fparser2name], None)
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Reference)
    assert new_node._reference == "x"


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_parenthesis():
    ''' Test that fparser2 Parenthesis is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x=(x+1)")
    fparser2parenthesis = Execution_Part.match(reader)[0][0].items[2]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2parenthesis], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    # Check parenthesis are ignored and process_nodes uses its child
    assert isinstance(new_node, BinaryOperation)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_part_ref():
    ''' Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x(2)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0].items[0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2part_ref], None)
    assert "Undeclared reference 'x' found." in str(error.value)

    fake_parent.symbol_table.add(DataSymbol('x', DataType.INTEGER))
    processor.process_nodes(fake_parent, [fparser2part_ref], None)
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Array)
    assert new_node._reference == "x"
    assert len(new_node.children) == 1  # Array dimensions

    # Parse a complex array expression
    reader = FortranStringReader("x(i+3,j-4,(z*5)+1)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0].items[0]

    fake_parent = Node()
    processor.process_nodes(fake_parent, [fparser2part_ref], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Array)
    assert new_node._reference == "x"
    assert len(new_node.children) == 3  # Array dimensions


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_intrinsics():
    ''' Test that fparser2 Intrinsic_Function_Reference nodes are
    handled appropriately.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    processor = Fparser2Reader()

    # Test parsing all supported binary operators.
    testlist = (
        ('x = exp(a)', UnaryOperation, UnaryOperation.Operator.EXP),
        ('x = sin(a)', UnaryOperation, UnaryOperation.Operator.SIN),
        ('x = asin(a)', UnaryOperation, UnaryOperation.Operator.ASIN),
        ('ix = ceiling(a)', UnaryOperation, UnaryOperation.Operator.CEIL),
        ('x = abs(a)', UnaryOperation, UnaryOperation.Operator.ABS),
        ('x = cos(a)', UnaryOperation, UnaryOperation.Operator.COS),
        ('x = acos(a)', UnaryOperation, UnaryOperation.Operator.ACOS),
        ('x = tan(a)', UnaryOperation, UnaryOperation.Operator.TAN),
        ('x = atan(a)', UnaryOperation, UnaryOperation.Operator.ATAN),
        ('x = real(a)', UnaryOperation, UnaryOperation.Operator.REAL),
        ('x = real(a, 8)', CodeBlock, None),
        ('x = int(a)', UnaryOperation, UnaryOperation.Operator.INT),
        ('x = int(a, 8)', CodeBlock, None),
        ('x = log(a)', UnaryOperation, UnaryOperation.Operator.LOG),
        ('x = log10(a)', UnaryOperation, UnaryOperation.Operator.LOG10),
        ('x = mod(a, b)', BinaryOperation, BinaryOperation.Operator.REM),
        ('x = max(a, b)', BinaryOperation, BinaryOperation.Operator.MAX),
        ('x = mAx(a, b, c)', NaryOperation, NaryOperation.Operator.MAX),
        ('x = min(a, b)', BinaryOperation, BinaryOperation.Operator.MIN),
        ('x = min(a, b, c)', NaryOperation, NaryOperation.Operator.MIN),
        ('x = sign(a, b)', BinaryOperation, BinaryOperation.Operator.SIGN),
        ('x = sqrt(a)', UnaryOperation, UnaryOperation.Operator.SQRT),
        ('x = sum(a, idim)', BinaryOperation, BinaryOperation.Operator.SUM),
        ('x = suM(a, idim, mask)', NaryOperation, NaryOperation.Operator.SUM),
        # Check that we get a CodeBlock for an unsupported N-ary operation
        ('x = reshape(a, b, c)', CodeBlock, None),
    )

    for code, expected_type, expected_op in testlist:
        fake_parent = Node()
        reader = FortranStringReader(code)
        fp2node = Execution_Part.match(reader)[0][0].items[2]
        processor.process_nodes(fake_parent, [fp2node], None)
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent.children[0], expected_type), \
            "Fails when parsing '" + code + "'"
        if expected_type is not CodeBlock:
            assert fake_parent.children[0]._operator == expected_op, \
                "Fails when parsing '" + code + "'"


@pytest.mark.usefixtures("f2008_parser")
def test_intrinsic_no_args():
    ''' Check that an intrinsic with no arguments results in a
    NotImplementedError. '''
    from fparser.two.Fortran2003 import Execution_Part
    processor = Fparser2Reader()
    fake_parent = Node()
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
    from fparser.two.Fortran2003 import Execution_Part
    processor = Fparser2Reader()
    fake_parent = Node()
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
    from fparser.two.Fortran2003 import Execution_Part, Name
    processor = Fparser2Reader()
    fake_parent = Node()
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
    from fparser.two.Fortran2003 import Execution_Part, Name
    processor = Fparser2Reader()
    fake_parent = Node()
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
    ''' Check that we correctly handle nested intrinsic functions. '''
    from fparser.two.Fortran2003 import Execution_Part
    processor = Fparser2Reader()
    fake_parent = Node()
    reader = FortranStringReader(
        "ze_z = SUM( e1t(:,:) * e2t(:,:) * zav_tide(:,:,jk) * "
        "tmask_i(:,:) ) &\n"
        "   &  / MAX( 1.e-20, SUM( e1t(:,:) * e2t(:,:) * wmask (:,:,jk) * "
        "tmask_i(:,:) ) )")
    fp2node = Execution_Part.match(reader)[0][0].items[2]
    processor.process_nodes(fake_parent, [fp2node], None)
    array_refs = fake_parent.walk(Reference)
    assert "sum" not in [str(ref.name) for ref in array_refs]
    reader = FortranStringReader(
        "zccc = SQRT(MAX(zbbb * zbbb - 4._wp * rcpi * rLfus * ztmelts, 0.0))")
    fp2node = Execution_Part(reader)
    # Check that the frontend does not produce any CodeBlocks
    processor.process_nodes(fake_parent, fp2node.content, None)
    cblocks = fake_parent.children[1].walk(CodeBlock)
    assert not cblocks


@pytest.mark.xfail(reason="#412 Fortran array notation not yet handled in "
                   "non-NEMO PSyIR")
@pytest.mark.usefixtures("f2008_parser")
def test_handling_array_product():
    ''' Check that we correctly handle array products. '''
    from fparser.two.Fortran2003 import Execution_Part
    processor = Fparser2Reader()
    fake_parent = Node()
    reader = FortranStringReader(
        "ze_z(:,:) = e1t(:,:) * e2t(:,:) * zav_tide(:,:,jk)")
    fp2node = Execution_Part.match(reader)
    processor.process_nodes(fake_parent, [fp2node[0][0]], None)
    fake_parent.children[0].view()
    assert not fake_parent.walk(CodeBlock)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_if_stmt():
    ''' Test that fparser2 If_Stmt is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("if(x==1)y=1")
    fparser2if_stmt = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_stmt], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, IfBlock)
    assert len(new_node.children) == 2


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_if_construct():
    ''' Test that fparser2 If_Construct is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
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

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_construct], None)

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
    assert ifnode.children[1].ast is fparser2if_construct.content[1]
    assert ifnode.children[1].ast_end is fparser2if_construct.content[2]
    assert ifnode.if_body[0].children[0].name == 'branch1'
    assert isinstance(ifnode.children[2], Schedule)
    assert ifnode.children[2].ast is fparser2if_construct.content[3]

    # Second level contains condition2, branch2, elsebody
    ifnode = ifnode.else_body[0]
    assert 'was_elseif' in ifnode.annotations
    assert ifnode.condition.children[0].name == 'condition2'
    assert isinstance(ifnode.children[1], Schedule)
    assert ifnode.if_body[0].children[0].name == 'branch2'
    assert isinstance(ifnode.children[2], Schedule)

    # Third level is just branch3
    elsebody = ifnode.else_body[0]
    assert elsebody.children[0].name == 'branch3'
    assert elsebody.ast is fparser2if_construct.content[6]


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_if_construct_errors():
    ''' Test that unsupported If_Construct structures raise the proper
    errors.
    '''
    from fparser.two.Fortran2003 import Execution_Part

    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        endif''')

    fake_parent = Node()
    processor = Fparser2Reader()

    # Test with no opening If_Then_Stmt
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    del fparser2if_construct.content[0]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct], None)
    assert "Failed to find opening if then statement in:" in str(error.value)

    reader = FortranStringReader(
        '''if (condition1) then
        elseif (condition2) then
        endif''')

    # Test with no closing End_If_Stmt
    fparser2if_construct = Execution_Part.match(reader)[0][0]
    del fparser2if_construct.content[-1]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2if_construct], None)
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
        processor.process_nodes(fake_parent, [fparser2if_construct], None)
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
        processor.process_nodes(fake_parent, [fparser2if_construct], None)
    assert ("Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt are "
            "expected, but found") in str(error.value)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_complex_if_construct():
    ''' Test that nested If_Construct structures and empty bodies are
    handled properly.
    '''
    from fparser.two.Fortran2003 import Execution_Part
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

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2if_construct], None)

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
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct], None)

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
    a default clause. '''
    from fparser.two.Fortran2003 import Execution_Part, Assignment_Stmt
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

        fake_parent = Node()
        processor = Fparser2Reader()
        processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
    statements involving a list of conditions. '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label2, label3)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
    statements involving a range. '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label4:label5)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
    statements involving a list of ranges. '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (:label1, label5:, label6)
                branch4 = 1
            END SELECT''')
    # We should end up with:
    #    my_var <= label1 OR my_var >= label5 OR my_var == label6
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
    '''
    from fparser.two.Fortran2003 import Execution_Part, Name
    # CASE (default) is just a regular symbol named default
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (default)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
        processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
        processor.process_nodes(fake_parent, [fparser2case_construct], None)
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
        processor.process_nodes(fake_parent, [fparser2case_construct], None)
    assert "to be a Case_Selector but got" in str(error.value)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_binaryopbase():
    ''' Test that fparser2 BinaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x=1+4")
    fp2binaryop = Execution_Part.match(reader)[0][0].items[2]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fp2binaryop], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
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
        fp2binaryop.items = (fp2binaryop.items[0], opstring,
                             fp2binaryop.items[2])
        # And then translate it to PSyIR again.
        fake_parent = Node()
        processor.process_nodes(fake_parent, [fp2binaryop], None)
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent.children[0], BinaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent.children[0]._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported binary operator creates a CodeBlock
    fake_parent = Node()
    fp2binaryop.items = (fp2binaryop.items[0], 'unsupported',
                         fp2binaryop.items[2])
    processor.process_nodes(fake_parent, [fp2binaryop], None)
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_unaryopbase():
    ''' Test that fparser2 UnaryOpBase is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part, UnaryOpBase
    reader = FortranStringReader("x=-4")
    fp2unaryop = Execution_Part.match(reader)[0][0].items[2]
    assert isinstance(fp2unaryop, UnaryOpBase)

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fp2unaryop], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
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
        fp2unaryop.items = (opstring, fp2unaryop.items[1])
        # And then translate it to PSyIR again.
        fake_parent = Node()
        processor.process_nodes(fake_parent, [fp2unaryop], None)
        assert len(fake_parent.children) == 1
        assert isinstance(fake_parent.children[0], UnaryOperation), \
            "Fails when parsing '" + opstring + "'"
        assert fake_parent.children[0]._operator == expected, \
            "Fails when parsing '" + opstring + "'"

    # Test that an unsupported unary operator creates a CodeBlock
    fp2unaryop.items = ('unsupported', fp2unaryop.items[1])
    fake_parent = Node()
    processor.process_nodes(fake_parent, [fp2unaryop], None)

    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_handling_return_stmt():
    ''' Test that fparser2 Return_Stmt is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part, Return_Stmt
    reader = FortranStringReader("return")
    return_stmt = Execution_Part.match(reader)[0][0]
    assert isinstance(return_stmt, Return_Stmt)

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [return_stmt], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Return)
    assert not new_node.children


@pytest.mark.usefixtures("f2008_parser")
def test_handling_end_do_stmt():
    ''' Test that fparser2 End_Do_Stmt are ignored.'''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader('''
        do i=1,10
            a=a+1
        end do
        ''')
    fparser2enddo = Execution_Part.match(reader)[0][0].content[-1]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2enddo], None)
    assert not fake_parent.children  # No new children created


@pytest.mark.usefixtures("f2008_parser")
def test_handling_end_subroutine_stmt():
    ''' Test that fparser2 End_Subroutine_Stmt are ignored.'''
    from fparser.two.Fortran2003 import Subroutine_Subprogram
    reader = FortranStringReader('''
        subroutine dummy_code()
        end subroutine dummy_code
        ''')
    fparser2endsub = Subroutine_Subprogram.match(reader)[0][-1]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2endsub], None)
    assert not fake_parent.children  # No new children created


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_do_construct():
    ''' Check that do loop constructs are converted to the expected
    PSyIR node'''
    from fparser.two.Fortran2003 import Execution_Part
    from psyclone.psyGen import Loop
    reader = FortranStringReader('''
        do i = 1, 10 , 2\n
            sum = sum + i\n
        end do\n
        ''')
    fparser2do = Execution_Part.match(reader)[0][0]
    processor = Fparser2Reader()
    fake_parent = Node()
    processor.process_nodes(fake_parent, [fparser2do], None)
    assert fake_parent.children[0]
    new_loop = fake_parent.children[0]
    assert isinstance(new_loop, Loop)
    assert new_loop.variable_name == "i"
    assert new_loop.start_expr.value == "1"
    assert new_loop.stop_expr.value == "10"
    assert new_loop.step_expr.value == "2"
    assert len(new_loop.loop_body.children) == 1
    assert isinstance(new_loop.loop_body[0], Assignment)


@pytest.mark.usefixtures("f2008_parser")
def test_do_construct_while():
    ''' Check that do while constructs are placed in Codeblocks '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader('''
        do while(a .gt. b)\n
            c = c + 1\n
        end do\n
        ''')
    fparser2while = Execution_Part.match(reader)[0][0]
    processor = Fparser2Reader()
    fake_parent = Node()
    processor.process_nodes(fake_parent, [fparser2while], None)
    assert isinstance(fake_parent.children[0], CodeBlock)


# (1/4) fparser2reader::nodes_to_code_block
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


# (2/4) fparser2reader::nodes_to_code_block
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


# (3/4) fparser2reader::nodes_to_code_block
@pytest.mark.usefixtures("disable_declaration_check")
def test_nodes_to_code_block_3(f2008_parser):
    '''Check that a codeblock that contains an expression has the
    structure property set to expression.

    '''
    # The derived-type reference is currently a code block in the PSyIR
    reader = FortranStringReader('''
        program test
        if (a%text == "HELLO") then
        end if
        end program test
        ''')
    prog = f2008_parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    code_block = schedule[0].condition.children[0]
    assert isinstance(code_block, CodeBlock)
    assert code_block.structure == CodeBlock.Structure.EXPRESSION


# (4/4) fparser2reader::nodes_to_code_block
@pytest.mark.usefixtures("f2008_parser")
def test_nodes_to_code_block_4():
    '''Check that a codeblock that has a directive as a parent causes the
    expected exception.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Fparser2Reader.nodes_to_code_block(Directive(), "hello")
    assert ("A CodeBlock with a Directive as parent is not yet supported."
            in str(excinfo.value))


@pytest.mark.usefixtures("f2008_parser")
def test_missing_loop_control(monkeypatch):
    ''' Check that encountering a loop in the fparser parse tree that is
    missing a Loop_Control element raises an InternalError. '''
    from fparser.two.utils import walk_ast
    reader = FortranStringReader('''
        do while(a .gt. b)\n
            c = c + 1\n
        end do\n
        ''')
    fparser2while = Fortran2003.Execution_Part.match(reader)[0][0]
    processor = Fparser2Reader()

    # We have to break the fparser2 parse tree in order to trigger the
    # internal error
    ctrl = walk_ast(fparser2while.content[0].items, [Fortran2003.Loop_Control])
    # 'items' is a tuple and therefore immutable so make a new list
    item_list = list(fparser2while.content[0].items)
    # Create a new tuple for the items member without the Loop_Control
    item_list.remove(ctrl[0])
    fparser2while.content[0].items = tuple(item_list)
    monkeypatch.setattr(fparser2while, "tostr", lambda: "<fparser2while>")

    fake_parent = Node()
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2while], None)
    assert "Unrecognised form of DO loop - failed to find Loop_Control " \
        "element in the node '<fparser2while>'." in str(err.value)
