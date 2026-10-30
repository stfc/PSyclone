# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the handling of select type
construction for the Fparser->PSyIR frontend.

'''
import pytest

from psyclone.psyir.nodes import (
    CodeBlock, Reference, Routine, SelectType, TypeGuard
)
from psyclone.tests.utilities import Compile


def test_type(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output with a basic select type
    construct. Also check that the appropriate annotation is added to
    the if nodes.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type_selector)\n"
        "  class(*), target :: type_selector\n"
        "  integer :: branch1, branch2\n"
        "  integer :: iinfo\n"
        "  real :: rinfo\n"
        "  SELECT TYPE (type_selector)\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      iinfo = type_selector\n"
        "    TYPE IS (REAL)\n"
        "      branch2 = 1\n"
        "      rinfo = type_selector\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type_selector)
    class(*), target :: type_selector
    integer :: branch1
    integer :: branch2
    integer :: iinfo
    real :: rinfo

    select type(type_selector)
      type is(integer)
        branch1 = 1
        branch2 = 0
        iinfo = type_selector
      type is(real)
        branch2 = 1
        rinfo = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    routine = psyir.walk(Routine)[0]
    select_type = psyir.walk(SelectType)[0]
    assert routine.children[0] is select_type
    assert len(select_type.children) == 3
    assert isinstance(select_type.children[0], Reference)
    assert select_type.children[0].symbol.name == "type_selector"
    assert (select_type.children[1].type_guard_type ==
            TypeGuard.TypeGuardType.TYPE_IS)
    assert (select_type.children[2].type_guard_type ==
            TypeGuard.TypeGuardType.TYPE_IS)
    assert Compile(tmpdir).string_compiles(result)


def test_default(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output when select type has a
    default clause. The output of the default clause should be output
    under the final else of the generated if hierarchy irrespective of
    where it appears in the select type clauses.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*), target :: type\n"
        "  integer :: branch1, branch2, branch3\n"
        "  integer :: iinfo\n"
        "  real :: rinfo\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (INTEGER)\n"
        "        branch1 = 1\n"
        "        branch2 = 0\n"
        "        iinfo = type\n"
        "    CLASS DEFAULT\n"
        "        branch3 = 1\n"
        "    TYPE IS (REAL)\n"
        "        branch2 = 1\n"
        "        rinfo = type\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type)
    class(*), target :: type
    integer :: branch1
    integer :: branch2
    integer :: branch3
    integer :: iinfo
    real :: rinfo

    select type(type)
      type is(integer)
        branch1 = 1
        branch2 = 0
        iinfo = type
      class default
        branch3 = 1
      type is(real)
        branch2 = 1
        rinfo = type
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    select_type = psyir.walk(SelectType)[0]
    # Check the class default type is as expected.
    assert (select_type.children[2].type_guard_type ==
            TypeGuard.TypeGuardType.CLASS_DEFAULT)
    assert select_type.children[2].comparison_type is None
    assert Compile(tmpdir).string_compiles(result)


def test_class(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output when select type has a 'class
    is' clause. Place one as the first clause and one later for
    coverage. Also check that the appropriate annotation is added to
    the if nodes related to the 'class is' clause.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*), pointer :: type\n"
        "  type type2\n"
        "    integer :: scalar\n"
        "  end type\n"
        "  type type3\n"
        "    integer :: field\n"
        "  end type\n"
        "  integer :: branch0, branch1, branch2, branch3\n"
        "  type(type2) :: my_type2\n"
        "  type(type3) :: my_type3\n"
        "  integer :: iinfo\n"
        "  SELECT TYPE (type)\n"
        "    CLASS IS(type2)\n"
        "      branch0 = 1\n"
        "      my_type2 = type\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      iinfo = type\n"
        "    CLASS IS(type3)\n"
        "      branch2 = 1\n"
        "      my_type3 = type\n"
        "    TYPE IS (REAL)\n"
        "      branch3 = 1\n"
        "      ! type not used here\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type)
    class(*), pointer :: type
    type :: type2
      integer :: scalar
    end type type2
    type :: type3
      integer :: field
    end type type3
    integer :: branch0
    integer :: branch1
    integer :: branch2
    integer :: branch3
    type(type2) :: my_type2
    type(type3) :: my_type3
    integer :: iinfo

    select type(type)
      class is(type2)
        branch0 = 1
        my_type2 = type
      type is(integer)
        branch1 = 1
        branch2 = 0
        iinfo = type
      class is(type3)
        branch2 = 1
        my_type3 = type
      type is(real)
        branch3 = 1
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    select_type = psyir.walk(SelectType)[0]
    # Check the class default type is as expected.
    assert (select_type.children[1].type_guard_type ==
            TypeGuard.TypeGuardType.CLASS_IS)
    assert select_type.children[1].comparison_type.name == "type2"
    assert Compile(tmpdir).string_compiles(result)


def test_class_with_codeblock(fortran_reader, fortran_writer, tmpdir):
    '''Check that the handler copes with the case where we have a CodeBlock
    that refers to the type-selector variable inside a 'class is' clause.'''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*) :: type\n"
        "  type type2\n"
        "    integer :: scalar\n"
        "  end type\n"
        "  type type3\n"
        "    integer :: field\n"
        "  end type\n"
        "  integer :: branch0, branch1, branch2, branch3\n"
        "  type(type2) :: my_type2\n"
        "  type(type3) :: my_type3\n"
        "  integer :: iinfo\n"
        "  SELECT TYPE (type)\n"
        "    CLASS IS(type2)\n"
        "      branch0 = 1\n"
        "      my_type2 = type\n"
        "      write(*,*) 'This is a CodeBlock', print_me(type)\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      iinfo = type\n"
        "  END SELECT\n"
        "end subroutine\n"
        "function print_me(var)\n"
        "  class(*) :: var\n"
        "  character(len=10) :: print_me\n"
        "  print_me = 'hello'\n"
        "end function print_me\n"
        "end module\n")
    correct = """  subroutine select_type(type)
    class(*) :: type
    type :: type2
      integer :: scalar
    end type type2
    type :: type3
      integer :: field
    end type type3
    integer :: branch0
    integer :: branch1
    integer :: branch2
    integer :: branch3
    type(type2) :: my_type2
    type(type3) :: my_type3
    integer :: iinfo

    select type(type)
      class is(type2)
        branch0 = 1
        my_type2 = type

        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Write_Stmt
        WRITE(*, *) 'This is a CodeBlock', print_me(type)
      type is(integer)
        branch1 = 1
        branch2 = 0
        iinfo = type
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    select_type = psyir.walk(SelectType)[0]
    assert isinstance(select_type.children[1].children[0].children[2],
                      CodeBlock)
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize("use_stmt", ["use some_mod",
                                      "use some_mod, only: my_ptr"])
def test_unresolved_selector_symbol(fortran_reader, fortran_writer, tmpdir,
                                    use_stmt):
    '''Test the case where the Symbol representing the type-selector variable
    is unresolved or of unknown type.'''
    code = f'''\
    module my_mod
      {use_stmt}
      implicit none
    contains
      subroutine my_sub()
        select type (my_ptr)
          type is (type1)
            write(*,*) "yes"
        end select
      end subroutine my_sub
    end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    correct = """subroutine my_sub()

    select type(my_ptr)
      type is(type1)
        ! PSyclone CodeBlock (unsupported code) reason:
        !  - Unsupported statement: Write_Stmt
        WRITE(*, *) "yes"
    end select

  end subroutine my_sub"""
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


def test_select_rename(fortran_reader, fortran_writer, tmpdir):
    '''Check that a CodeBlock is created when the type in select type is
    renamed (i.e. the code is not modified). This is done as we are
    not yet able to rename the variables inside the select type
    clause.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*) :: type\n"
        "  SELECT TYPE (newtype => type)\n"
        "    TYPE IS (INTEGER)\n"
        "        print *, newtype\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    SELECT TYPE(newtype=>type)\n"
        "      TYPE IS (INTEGER)\n"
        "      PRINT *, newtype\n"
        "    END SELECT\n")
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0].children[0], CodeBlock)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_kind(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output when the TYPE IS intrinsic
    content includes precision. REAL*16 (quad precision) is not
    supported by the nvidia compiler at the moment so we limit the
    test to 4 and 8 here

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*), target :: type\n"
        "  integer :: branch1, branch2\n"
        "  real(kind=4) :: rinfo1\n"
        "  real(kind=8) :: rinfo2\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (REAL(kind=4))\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      rinfo1 = type\n"
        "    TYPE IS (REAL(8))\n"
        "      branch2 = 1\n"
        "      rinfo2 = type\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type)
    class(*), target :: type
    integer :: branch1
    integer :: branch2
    real(kind=4) :: rinfo1
    real(kind=8) :: rinfo2

    select type(type)
      type is(real(kind=4))
        branch1 = 1
        branch2 = 0
        rinfo1 = type
      type is(real(kind=8))
        branch2 = 1
        rinfo2 = type
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


def test_derived(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is produced when 'TYPE IS type' is a
    derived type.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*), target :: type\n"
        "  type field_type\n"
        "    integer :: x\n"
        "  end type\n"
        "  type(field_type) :: field_type_info\n"
        "  integer :: branch1\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (field_type)\n"
        "      branch1 = 1\n"
        "      field_type_info = type\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type)
    class(*), target :: type
    type :: field_type
      integer :: x
    end type field_type
    type(field_type) :: field_type_info
    integer :: branch1

    select type(type)
      type is(field_type)
        branch1 = 1
        field_type_info = type
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


def test_datatype(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output with different intrinsic
    datatypes. REAL and INTEGER have already been tested so are not
    included here. CHARACTER must be assumed size in a 'TYPE IS'
    clause i.e. 'CHARACTER(*)' or 'CHARACTER(LEN=*)'.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type_selector)\n"
        "  class(*), target :: type_selector\n"
        "  integer :: branch1, branch2, branch3\n"
        "  logical :: logical_type\n"
        "  character(len = 256) :: character_type\n"
        "  complex :: complex_type\n"
        "  SELECT TYPE (type_selector)\n"
        "    TYPE IS (LOGICAL)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      logical_type = type_selector\n"
        "    TYPE IS (CHARACTER(len = *))\n"
        "      branch2 = 1\n"
        "      character_type = type_selector\n"
        "    TYPE IS (COMPLEX)\n"
        "      branch3 = 1\n"
        "      complex_type = type_selector\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    correct = """  subroutine select_type(type_selector)
    class(*), target :: type_selector
    integer :: branch1
    integer :: branch2
    integer :: branch3
    logical :: logical_type
    character(len=256) :: character_type
    complex :: complex_type

    select type(type_selector)
      type is(logical)
        branch1 = 1
        branch2 = 0
        logical_type = type_selector
      type is(character(len=*))
        branch2 = 1
        character_type = type_selector
      type is(complex)
        branch3 = 1
        complex_type = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out",
    (["*256", "(len=256)"], ["(256)", "(len=256)"],
     ["(LEN = 256)", "(len=256)"]))
def test_character(fortran_reader, fortran_writer, tmpdir, char_type_in,
                   char_type_out):
    '''Check that the correct code is output with literal and implicit
    character formats. An implicit format requires the character to be
    passed by argument or specified as a pointer. Here we pass it by
    argument.

    '''
    code = (
        f"module select_mod\n"
        f"contains\n"
        f"subroutine select_type(type_selector)\n"
        f"  class(*), target :: type_selector\n"
        f"  character{char_type_in} :: character_type\n"
        f"  SELECT TYPE (type_selector)\n"
        f"    TYPE IS (CHARACTER(len = *))\n"
        f"      character_type = type_selector\n"
        f"  END SELECT\n"
        f"end subroutine\n"
        f"end module\n")
    correct = f"""subroutine select_type(type_selector)
    class(*), target :: type_selector
    character{char_type_out} :: character_type

    select type(type_selector)
      type is(character(len=*))
        character_type = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out",
    (["(:)", "(len = :)"], ["(LEN = :)", "(len = :)"], ["*(:)", "*(:)"]))
def test_character_assumed_len(fortran_reader, fortran_writer, tmpdir,
                               char_type_in, char_type_out):
    '''Check that the correct code is output with different assumed-length
    specifications for intrinsic character formats.

    '''
    code = (
        f"module select_mod\n"
        f"contains\n"
        f"subroutine select_type(type_selector)\n"
        f"  class(*), target :: type_selector\n"
        f"  character{char_type_in}, pointer :: character_type => null()\n"
        f"  SELECT TYPE (type_selector)\n"
        f"    TYPE IS (CHARACTER(len = *))\n"
        f"      character_type = type_selector\n"
        f"  END SELECT\n"
        f"end subroutine\n"
        f"end module\n")
    correct = f"""subroutine select_type(type_selector)
    class(*), target :: type_selector
    character{char_type_out}, pointer :: character_type => null()

    select type(type_selector)
      type is(character(len=*))
        character_type = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out, pointer",
    (["(LEN=*, KIND=1)", "(kind=1, len=*)", ""],
     ["(LEN=*, KIND=1*1)", "(kind=1 * 1, len=*)", ""],
     ["(LEN=1*2, KIND=1*1)", "(kind=1 * 1, len=1 * 2)", ""],
     ["(*, KIND=1*1)", "(kind=1 * 1, len=*)", ""],
     ["(256*1, KIND=1*1)", "(kind=1 * 1, len=256 * 1)", ""],
     ["(*, 1*1)", "(kind=1 * 1, len=*)", ""],
     ["(256*1, 1*1)", "(kind=1 * 1, len=256 * 1)", ""],
     ["(KIND=1*1, LEN=*)", "(kind=1 * 1, len=*)", ""],
     ["(KIND=1*1, LEN=256*1)", "(kind=1 * 1, len=256 * 1)", ""],
     ["(KIND=1*1)", "(kind=1 * 1, len=1)", ""],
     ["(LEN=:, KIND=1*1)", "(len = :, kind = 1 * 1)", ", pointer"],
     ["(*, KIND=1*1)", "(len = *, kind = 1 * 1)", ", pointer"],
     ["(:, 1*1)", "(len = :, kind = 1 * 1)", ", pointer"],
     ["(KIND=1*1, LEN=:)", "(len = :, kind = 1 * 1)", ", pointer"]))
def test_character_kind(
        fortran_reader, fortran_writer, tmpdir, char_type_in, char_type_out,
        pointer):
    '''Test that characters with kind and len clauses in various formats are
    supported.

    '''
    code = (
        f"module select_mod\n"
        f"  contains\n"
        f"  subroutine select_type(type_selector, character_type)\n"
        f"    class(*), target :: type_selector\n"
        f"    character{char_type_in}{pointer} :: character_type\n"
        f"    select type(type_selector)\n"
        f"      type is (character(len = *))\n"
        f"        character_type = type_selector\n"
        f"    end select\n"
        f"  end subroutine select_type\n"
        f"end module select_mod\n")
    correct = f"""  subroutine select_type(type_selector, character_type)
    class(*), target :: type_selector
    character{char_type_out}{pointer} :: character_type

    select type(type_selector)
      type is(character(len=*))
        character_type = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "pre_attribute, post_attribute",
    (["TARGET", "target"],
     ["POINTER", "pointer"],
     ["ALLOCATABLE", "allocatable"]))
def test_class_target(
        fortran_reader, fortran_writer, tmpdir, pre_attribute, post_attribute):
    '''Test that the type_selector is still created for target, pointer or
    allocatable class attributes.

    '''
    code = (
        f"module select_mod\n"
        f"  contains\n"
        f"  subroutine select_type(type_selector, character_type)\n"
        f"    class(*), {pre_attribute} :: type_selector\n"
        f"    character(len=*) :: character_type\n"
        f"    select type(type_selector)\n"
        f"      type is (character(len = *))\n"
        f"        character_type = type_selector\n"
        f"    end select\n"
        f"  end subroutine select_type\n"
        f"end module select_mod\n")
    correct = f"""subroutine select_type(type_selector, character_type)
    class(*), {post_attribute} :: type_selector
    character(len=*) :: character_type

    select type(type_selector)
      type is(character(len=*))
        character_type = type_selector
    end select

  end subroutine select_type"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)


def test_imported_type_target(
        fortran_reader, fortran_writer, tmpdir
):
    '''Test that if we have an imported type we still correctly create the
    required DataTypeSymbol to not crash the creation of the TypeGuard.'''
    code = """
    subroutine test(input)
        use mod, only: some_type
        class(*) :: input

        select type(input)
          type is(some_type)
             print *, "test print"
        end select
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    correct = """
    subroutine test(input)
  use mod, only :
  class(*) :: input

  select type(input)
    type is(some_type)
      ! psyclone codeblock (unsupported code) reason:
      !  - unsupported statement: print_stmt
      print *, "test print"
  end select

end subroutine test"""
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)
