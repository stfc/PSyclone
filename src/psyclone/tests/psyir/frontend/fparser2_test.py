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

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, Node, Directive, Schedule, \
    CodeBlock, Assignment, Return, UnaryOperation, BinaryOperation, \
    NaryOperation, Literal, IfBlock, Reference, Array, KernelSchedule, \
    Symbol, SymbolTable, Container, \
    InternalError, GenerationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader

# Fixtures


@pytest.fixture(scope="module")
def f2008_parser():
    '''Initialise fparser2 with Fortran2008 standard'''
    from fparser.two.parser import ParserFactory
    return ParserFactory().create(std="f2008")


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


def test_generate_schedule_two_modules(parser):
    ''' Tests the fparser2Reader generate_schedule method raises an exception
    when more than one fparser2 module node is provided.
    '''
    reader = FortranStringReader(FAKE_KERNEL_METADATA*2)
    ast = parser(reader)
    processor = Fparser2Reader()
    # Test kernel with two modules
    with pytest.raises(GenerationError) as error:
        _ = processor.generate_schedule("dummy_code", ast)
    assert ("Unexpected AST when generating 'dummy_code' kernel schedule."
            " Just one module definition per file supported.") \
        in str(error.value)


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
    assert schedule.symbol_table.lookup('f3').scope == 'global'
    assert schedule.symbol_table.lookup('f3').access is Symbol.Access.READWRITE

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
    '''Test that process_declarations method of fparse2reader
    converts the fparser2 declarations to symbols in the provided
    parent Kernel Schedule.
    '''
    from fparser.two.Fortran2003 import Specification_Part
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader("integer :: l1")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l1").name == 'l1'
    assert fake_parent.symbol_table.lookup("l1").datatype == 'integer'
    assert fake_parent.symbol_table.lookup("l1").shape == []
    assert fake_parent.symbol_table.lookup("l1").scope == 'local'
    assert not fake_parent.symbol_table.lookup("l1").access
    assert not fake_parent.symbol_table.lookup("l1").interface

    reader = FortranStringReader("Real      ::      l2")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l2").name == "l2"
    assert fake_parent.symbol_table.lookup("l2").datatype == 'real'

    reader = FortranStringReader("LOGICAL      ::      b")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("b").name == "b"
    assert fake_parent.symbol_table.lookup("b").datatype == 'boolean'

    # RHS array specifications
    reader = FortranStringReader("integer :: l3(l1)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l3").name == 'l3'
    assert fake_parent.symbol_table.lookup("l3").datatype == 'integer'
    assert len(fake_parent.symbol_table.lookup("l3").shape) == 1

    reader = FortranStringReader("integer :: l4(l1, 2)")
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert fake_parent.symbol_table.lookup("l4").name == 'l4'
    assert fake_parent.symbol_table.lookup("l4").datatype == 'integer'
    assert len(fake_parent.symbol_table.lookup("l4").shape) == 2

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

    # Initialisations are not supported
    reader = FortranStringReader("integer :: l1 = 1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert ("Initialisations on the declaration statements are not "
            "supported.") in str(error.value)

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


def test_process_not_supported_declarations(f2008_parser):
    '''Test that process_declarations method raises the proper errors when
    declarations contain unsupported attributes.
    '''
    from fparser.two.Fortran2003 import Specification_Part
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


def test_process_declarations_intent(f2008_parser):
    '''Test that process_declarations method handles various different
    specifications of variable attributes.
    '''
    from fparser.two.Fortran2003 import Specification_Part, Name
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()

    reader = FortranStringReader("integer, intent(in) :: arg1")
    fparser2spec = Specification_Part(reader).content[0]
    arg_list = [Name("arg1")]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg1").scope == 'global'
    assert fake_parent.symbol_table.lookup("arg1").access == Symbol.Access.READ

    reader = FortranStringReader("integer, intent( IN ) :: arg2")
    arg_list.append(Name("arg2"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg2").scope == 'global'
    assert fake_parent.symbol_table.lookup("arg2").access == Symbol.Access.READ

    reader = FortranStringReader("integer, intent( Out ) :: arg3")
    arg_list.append(Name("arg3"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg3").scope == 'global'
    assert fake_parent.symbol_table.lookup("arg3").access == \
        Symbol.Access.WRITE

    reader = FortranStringReader("integer, intent ( InOut ) :: arg4")
    arg_list.append(Name("arg4"))
    fparser2spec = Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], arg_list)
    assert fake_parent.symbol_table.lookup("arg4").scope == 'global'
    assert fake_parent.symbol_table.lookup("arg4").access is \
        Symbol.Access.READWRITE


def test_process_declarations_stmt_functions(f2008_parser):
    '''Test that process_declarations method handles statement functions
    appropriately.
    '''
    from fparser.two.Fortran2003 import Specification_Part
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

    # The code below checks that misclassified Statment_Functions are
    # recovered as arrays, this may become unecessary after fparser/#171
    # is fixed.

    # This Specification part is expected to contain a statment_function
    # with the current fparser, this may change depending on how
    # fparser/#171 is fixed.
    reader = FortranStringReader("a(x) = 1")
    fparser2spec = Specification_Part(reader).content[0]
    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(fake_parent, [fparser2spec], [])
    assert "Could not process '" in str(error.value)
    assert "'. Statement Function declarations are not supported." \
        in str(error.value)

    # If 'a' is declared in the symbol table as an array, it is an array
    # assignment which belongs in the execution part.
    fake_parent.symbol_table.add(Symbol('a', 'real', shape=[None]))
    fake_parent.symbol_table.add(Symbol('x', 'real', shape=[]))
    processor.process_declarations(fake_parent, [fparser2spec], [])
    assert len(fake_parent.children) == 1
    array = fake_parent.children[0].children[0]
    assert isinstance(array, Array)
    assert array.name == "a"

    # Test that it works with multi-dimensional arrays
    fake_parent = KernelSchedule("dummy_schedule")
    reader = FortranStringReader("b(x, y) = 1")
    fparser2spec = Stmt_Function_Stmt(reader)
    fake_parent.symbol_table.add(Symbol('b', 'real', shape=[None, None]))
    fake_parent.symbol_table.add(Symbol('x', 'integer', shape=[]))
    fake_parent.symbol_table.add(Symbol('y', 'integer', shape=[]))
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


def test_parse_array_dimensions_attributes(f2008_parser):
    '''Test that process_declarations method parses multiple specifications
    of array attributes.
    '''
    from fparser.two.Fortran2003 import Specification_Part, Name
    from fparser.two.Fortran2003 import Dimension_Attr_Spec

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

    sym_table.add(Symbol('var1', 'integer', []))
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
        sym_table.add(Symbol("var2", "real", []))
        _ = Fparser2Reader._parse_dimensions(fparser2spec, sym_table)
    assert "Could not process " in str(error.value)
    assert ("Only scalar integer literals or symbols are supported for "
            "explicit shape array declarations.") in str(error.value)

    # Explicit shape symbols can only be Literal or Symbol
    with pytest.raises(NotImplementedError) as error:
        class UnrecognizedType(object):
            '''Type guaranteed to not be part of the _parse_dimensions
            conditional type handler.'''
        fparser2spec.items[1].items[1].__class__ = UnrecognizedType
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
    assert fake_parent.symbol_table.lookup("array3").datatype == 'real'
    assert fake_parent.symbol_table.lookup("array3").shape == [None]
    assert fake_parent.symbol_table.lookup("array3").scope == "global"
    assert fake_parent.symbol_table.lookup("array3").access is \
        Symbol.Access.READ


def test_use_stmt(f2008_parser):
    ''' Check that SymbolTable entries are correctly created from
    module use statements. '''
    from fparser.two.Fortran2003 import Specification_Part
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod, only: some_var\n"
                                 "use this_mod\n"
                                 "use other_mod, only: var1, var2\n")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    for var in ["some_var", "var1", "var2"]:
        assert fake_parent.symbol_table.lookup(var).name == var
        assert fake_parent.symbol_table.lookup(var).scope == "global"
    assert fake_parent.symbol_table.lookup("some_var").interface.module_name \
        == "my_mod"
    assert fake_parent.symbol_table.lookup("var2").interface.module_name == \
        "other_mod"


def test_use_stmt_error(f2008_parser, monkeypatch):
    ''' Check that we raise the expected error if the parse tree representing
    a USE statement doesn't have the expected structure. '''
    from fparser.two.Fortran2003 import Specification_Part
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
            "but found 3 for 'hello'" in str(err))


def test_parse_array_dimensions_unhandled(f2008_parser, monkeypatch):
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
    assert "Reached end of loop body and" in str(error.value)
    assert " has not been handled." in str(error.value)


def test_handling_assignment_stmt(f2008_parser):
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


def test_handling_name(f2008_parser):
    ''' Test that fparser2 Name is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    from psyclone.psyGen import SymbolError
    reader = FortranStringReader("x=1")
    fparser2name = Execution_Part.match(reader)[0][0].items[0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2name], None)
    assert "Undeclared reference 'x' found." in str(error)

    fake_parent.symbol_table.add(Symbol('x', 'integer'))
    processor.process_nodes(fake_parent, [fparser2name], None)
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Reference)
    assert new_node._reference == "x"


def test_handling_parenthesis(f2008_parser):
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


def test_handling_part_ref(f2008_parser):
    ''' Test that fparser2 Part_Ref is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    from psyclone.psyGen import SymbolError
    reader = FortranStringReader("x(2)=1")
    fparser2part_ref = Execution_Part.match(reader)[0][0].items[0]

    fake_parent = KernelSchedule('kernel')
    processor = Fparser2Reader()

    # If one of the ancestors has a symbol table then process_nodes()
    # checks that the symbol is declared.
    with pytest.raises(SymbolError) as error:
        processor.process_nodes(fake_parent, [fparser2part_ref], None)
    assert "Undeclared reference 'x' found." in str(error)

    fake_parent.symbol_table.add(Symbol('x', 'integer'))
    processor.process_nodes(fake_parent, [fparser2part_ref], None)
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Array)
    assert new_node._reference == "x"
    assert len(new_node.children) == 1  # Array dimensions

    # Parse a complex array expression
    fake_parent = Node()
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


def test_handling_intrinsics(f2008_parser):
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


def test_intrinsic_no_args(f2008_parser):
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
    assert "SUM" in str(err)


def test_unary_op_handler_error(f2008_parser):
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
            "therefore not unary" in str(err))


def test_binary_op_handler_error(f2008_parser):
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
            "for 'SUM(a)'." in str(err))
    # Now break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._binary_op_handler(fp2node, fake_parent)
    assert ("binary intrinsic operation 'SUM(dummy)'. Expected second child "
            "to be Actual_Arg_Spec_List" in str(err))


def test_nary_op_handler_error(f2008_parser):
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
            "for 'SUM(a)'" in str(err))
    # Break the 'items' tuple of this fparser node
    fp2node.items = (fp2node.items[0], Name('dummy'))
    with pytest.raises(InternalError) as err:
        processor._nary_op_handler(fp2node, fake_parent)
    assert ("Expected second 'item' of N-ary intrinsic 'SUM(dummy)' in fparser"
            " parse tree to be an Actual_Arg_Spec_List" in str(err))


def test_handling_nested_intrinsic(f2008_parser):
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
    fake_parent.children[0].view()
    array_refs = fake_parent.walk(Reference)
    assert "sum" not in [str(ref.name) for ref in array_refs]


@pytest.mark.xfail(reason="#412 Fortran array notation not yet handled in "
                   "non-NEMO PSyIR")
def test_handling_array_product(f2008_parser):
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


def test_handling_if_stmt(f2008_parser):
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


def test_handling_if_construct(f2008_parser):
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


def test_handling_if_construct_errors(f2008_parser):
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


def test_handling_complex_if_construct(f2008_parser):
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


def test_handling_case_construct(f2008_parser):
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


def test_case_default(f2008_parser):
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


def test_handling_case_list(f2008_parser):
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


def test_handling_case_range(f2008_parser):
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


def test_handling_case_range_list(f2008_parser):
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


def test_handling_invalid_case_construct(f2008_parser):
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


def test_handling_numberbase(f2008_parser):
    ''' Test that fparser2 NumberBase is converted to the expected PSyIR
    tree structure.
    '''
    from fparser.two.Fortran2003 import Execution_Part
    reader = FortranStringReader("x=1")
    fparser2number = Execution_Part.match(reader)[0][0].items[2]

    fake_parent = Node()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2number], None)
    # Check a new node was generated and connected to parent
    assert len(fake_parent.children) == 1
    new_node = fake_parent.children[0]
    assert isinstance(new_node, Literal)
    assert new_node._value == "1"


def test_handling_binaryopbase(f2008_parser):
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


def test_handling_unaryopbase(f2008_parser):
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


def test_handling_return_stmt(f2008_parser):
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


def test_handling_end_do_stmt(f2008_parser):
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


def test_handling_end_subroutine_stmt(f2008_parser):
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


def test_do_construct(f2008_parser):
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


def test_do_construct_while(f2008_parser):
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
def test_nodes_to_code_block_3(f2008_parser):
    '''Check that a codeblock that contains an expression has the
    structure property set to expression.

    '''
    # The string "HELLO" is currently a code block in the PSyIR
    reader = FortranStringReader('''
        program test
        if (a == "HELLO") then
        end if
        end program test
        ''')
    prog = f2008_parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    code_block = schedule[0].condition.children[1]
    assert isinstance(code_block, CodeBlock)
    assert code_block.structure == CodeBlock.Structure.EXPRESSION


# (4/4) fparser2reader::nodes_to_code_block
def test_nodes_to_code_block_4(f2008_parser):
    '''Check that a codeblock that has a directive as a parent causes the
    expected exception.

    '''
    with pytest.raises(InternalError) as excinfo:
        _ = Fparser2Reader.nodes_to_code_block(Directive(), "hello")
    assert ("A CodeBlock with a Directive as parent is not yet supported."
            in str(excinfo.value))


def test_missing_loop_control(f2008_parser, monkeypatch):
    ''' Check that encountering a loop in the fparser parse tree that is
    missing a Loop_Control element raises an InternalError. '''
    from fparser.two.utils import walk_ast
    from fparser.two import Fortran2003
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
        "element in the node '<fparser2while>'." in str(err)
