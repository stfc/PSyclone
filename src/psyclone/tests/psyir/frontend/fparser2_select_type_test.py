# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, STFC Daresbury Lab
#          A. R. Porter, STFC Daresbury Lab

'''Module containing pytest tests for the handling of select type
construction for the Fparser->PSyIR frontend.

'''
import pytest

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import CodeBlock, IfBlock, Routine
from psyclone.psyir.symbols import (AutomaticInterface, UnsupportedFortranType,
                                    DataSymbol)
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
    expected1 = "CLASS(*), TARGET :: type_selector"
    expected2 = (
        "    character(256) :: type_string\n"
        "    INTEGER, pointer :: ptr_INTEGER => null()\n"
        "    REAL, pointer :: ptr_REAL => null()\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "      TYPE IS (INTEGER)\n"
        "      type_string = \"integer\"\n"
        "      ptr_INTEGER => type_selector\n"
        "      TYPE IS (REAL)\n"
        "      type_string = \"real\"\n"
        "      ptr_REAL => type_selector\n"
        "    END SELECT\n"
        "    if (type_string == 'integer') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      iinfo = ptr_INTEGER\n"
        "    else\n"
        "      if (type_string == 'real') then\n"
        "        branch2 = 1\n"
        "        rinfo = ptr_REAL\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1.lower() in result
    assert expected2.lower() in result
    if_blocks = psyir.walk(IfBlock)
    assert "was_type_is" in if_blocks[0].annotations
    assert "was_type_is" in if_blocks[1].annotations
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
    expected = (
        "    character(256) :: type_string\n"
        "    INTEGER, pointer :: ptr_INTEGER => null()\n"
        "    REAL, pointer :: ptr_REAL => null()\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "      TYPE IS (INTEGER)\n"
        "      type_string = \"integer\"\n"
        "      ptr_INTEGER => type\n"
        "      TYPE IS (REAL)\n"
        "      type_string = \"real\"\n"
        "      ptr_REAL => type\n"
        "    END SELECT\n"
        "    if (type_string == 'integer') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      iinfo = ptr_INTEGER\n"
        "    else\n"
        "      if (type_string == 'real') then\n"
        "        branch2 = 1\n"
        "        rinfo = ptr_REAL\n"
        "      else\n"
        "        branch3 = 1\n"
        "      end if\n"
        "    end if\n\n"
        "  end subroutine select_type\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected in result
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
    expected1 = "class(*), pointer :: type"
    expected2 = (
        "    character(256) :: type_string\n"
        "    type(type2), pointer :: ptr_type2 => null()\n"
        "    INTEGER, pointer :: ptr_INTEGER => null()\n"
        "    type(type3), pointer :: ptr_type3 => null()\n"
        "    REAL, pointer :: ptr_REAL => null()\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "      CLASS IS (type2)\n"
        "      type_string = \"type2\"\n"
        "      ptr_type2 => type\n"
        "      TYPE IS (INTEGER)\n"
        "      type_string = \"integer\"\n"
        "      ptr_INTEGER => type\n"
        "      CLASS IS (type3)\n"
        "      type_string = \"type3\"\n"
        "      ptr_type3 => type\n"
        "      TYPE IS (REAL)\n"
        "      type_string = \"real\"\n"
        "      ptr_REAL => type\n"
        "    END SELECT\n"
        "    if (type_string == 'type2') then\n"
        "      branch0 = 1\n"
        "      my_type2 = ptr_type2\n"
        "    else\n"
        "      if (type_string == 'integer') then\n"
        "        branch1 = 1\n"
        "        branch2 = 0\n"
        "        iinfo = ptr_INTEGER\n"
        "      else\n"
        "        if (type_string == 'type3') then\n"
        "          branch2 = 1\n"
        "          my_type3 = ptr_type3\n"
        "        else\n"
        "          if (type_string == 'real') then\n"
        "            branch3 = 1\n"
        "          end if\n"
        "        end if\n"
        "      end if\n"
        "    end if\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1 in result
    assert expected2 in result
    if_blocks = psyir.walk(IfBlock)
    assert "was_class_is" in if_blocks[0].annotations
    assert "was_class_is" in if_blocks[2].annotations
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
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine.children[0], CodeBlock)
    result = fortran_writer(psyir)
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize("use_stmt", ["use some_mod",
                                      "use some_mod, only: my_ptr"])
def test_unresolved_selector_symbol(fortran_reader, use_stmt):
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
    routine = psyir.walk(Routine)[0]
    assert isinstance(routine.children[0], CodeBlock)


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
    expected1 = (
        "    CLASS(*), TARGET :: type\n"
        "    integer :: branch1\n"
        "    integer :: branch2\n"
        "    REAL(KIND=4) :: rinfo1\n"
        "    REAL(KIND=8) :: rinfo2\n"
        "    character(256) :: type_string\n"
        "    REAL(KIND = 4), pointer :: ptr_REAL_4 => null()\n"
        "    REAL(KIND = 8), pointer :: ptr_REAL_8 => null()\n").lower()
    expected2 = (
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "      TYPE IS (REAL(KIND = 4))\n"
        "      type_string = \"real_4\"\n"
        "      ptr_REAL_4 => type\n"
        "      TYPE IS (REAL(KIND = 8))\n"
        "      type_string = \"real_8\"\n"
        "      ptr_REAL_8 => type\n"
        "    END SELECT\n"
        "    if (type_string == 'real_4') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      rinfo1 = ptr_REAL_4\n"
        "    else\n"
        "      if (type_string == 'real_8') then\n"
        "        branch2 = 1\n"
        "        rinfo2 = ptr_REAL_8\n"
        "      end if\n"
        "    end if\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1 in result
    assert expected2 in result
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
    expected1 = (
        "    CLASS(*), TARGET :: type\n"
        "    type :: field_type\n"
        "      integer :: x\n"
        "    end type field_type\n"
        "    type(field_type) :: field_type_info\n"
        "    integer :: branch1\n"
        "    character(256) :: type_string\n"
        "    type(field_type), pointer :: ptr_field_type => null()\n")
    expected2 = (
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "      TYPE IS (field_type)\n"
        "      type_string = \"field_type\"\n"
        "      ptr_field_type => type\n"
        "    END SELECT\n"
        "    if (type_string == 'field_type') then\n"
        "      branch1 = 1\n"
        "      field_type_info = ptr_field_type\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected1 in result
    assert expected2 in result
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
    expected1 = (
        "    CLASS(*), TARGET :: type_selector\n"
        "    integer :: branch1\n"
        "    integer :: branch2\n"
        "    integer :: branch3\n"
        "    logical :: logical_type\n"
        "    CHARACTER(LEN = 256) :: character_type\n"
        "    COMPLEX :: complex_type\n"
        "    character(256) :: type_string\n"
        "    LOGICAL, pointer :: ptr_LOGICAL => null()\n"
        "    CHARACTER(LEN=256), pointer :: ptr_CHARACTER_star => null()\n"
        "    COMPLEX, pointer :: ptr_COMPLEX => null()\n").lower()
    expected2 = (
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "      TYPE IS (LOGICAL)\n"
        "      type_string = \"logical\"\n"
        "      ptr_LOGICAL => type_selector\n"
        "      TYPE IS (CHARACTER(LEN = *))\n"
        "      type_string = \"character_star\"\n"
        "      ptr_CHARACTER_star => type_selector\n"
        "      TYPE IS (COMPLEX)\n"
        "      type_string = \"complex\"\n"
        "      ptr_COMPLEX => type_selector\n"
        "    END SELECT\n"
        "    if (type_string == 'logical') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "      logical_type = ptr_LOGICAL\n"
        "    else\n"
        "      if (type_string == 'character_star') then\n"
        "        branch2 = 1\n"
        "        character_type = ptr_CHARACTER_star\n"
        "      else\n"
        "        if (type_string == 'complex') then\n"
        "          branch3 = 1\n"
        "          complex_type = ptr_COMPLEX\n"
        "        end if\n"
        "      end if\n"
        "    end if\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1 in result
    assert expected2 in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out",
    (["*256", "*256"], ["(256)", "(LEN = 256)"],
     ["(LEN = 256)", "(LEN = 256)"]))
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
    expected1 = (
        f"  subroutine select_type(type_selector)\n"
        f"    CLASS(*), TARGET :: type_selector\n"
        f"    CHARACTER{char_type_out} :: character_type\n"
        f"    character(256) :: type_string\n"
        f"    CHARACTER(LEN=256), pointer :: ptr_CHARACTER_star => "
        f"null()\n").lower()
    expected2 = (
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "      TYPE IS (CHARACTER(LEN = *))\n"
        "      type_string = \"character_star\"\n"
        "      ptr_CHARACTER_star => type_selector\n"
        "    END SELECT\n"
        "    if (type_string == 'character_star') then\n"
        "      character_type = ptr_CHARACTER_star\n"
        "    end if\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1 in result
    assert expected2 in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out",
    (["(:)", "(LEN = :)"], ["(LEN = :)", "(LEN = :)"], ["*(:)", "*(:)"]))
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
    expected1 = (
        f"  subroutine select_type(type_selector)\n"
        f"    CLASS(*), TARGET :: type_selector\n"
        f"    CHARACTER{char_type_out}, POINTER :: character_type => null()\n"
        f"    character(256) :: type_string\n"
        f"    CHARACTER(LEN=256), pointer :: ptr_CHARACTER_star => "
        f"null()\n").lower()
    expected2 = (
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "      TYPE IS (CHARACTER(LEN = *))\n"
        "      type_string = \"character_star\"\n"
        "      ptr_CHARACTER_star => type_selector\n"
        "    END SELECT\n"
        "    if (type_string == 'character_star') then\n"
        "      character_type = ptr_CHARACTER_star\n"
        "    end if\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert expected1 in result
    assert expected2 in result
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "char_type_in, char_type_out, pointer",
    (["(LEN=*, KIND=1)", "(LEN = *, KIND = 1)", ""],
     ["(LEN=*, KIND=1*1)", "(LEN = *, KIND = 1 * 1)", ""],
     ["(LEN=1*2, KIND=1*1)", "(LEN = 1 * 2, KIND = 1 * 1)", ""],
     ["(*, KIND=1*1)", "(LEN = *, KIND = 1 * 1)", ""],
     ["(256*1, KIND=1*1)", "(LEN = 256 * 1, KIND = 1 * 1)", ""],
     ["(*, 1*1)", "(LEN = *, KIND = 1 * 1)", ""],
     ["(256*1, 1*1)", "(LEN = 256 * 1, KIND = 1 * 1)", ""],
     ["(KIND=1*1, LEN=*)", "(LEN = *, KIND = 1 * 1)", ""],
     ["(KIND=1*1, LEN=256*1)", "(LEN = 256 * 1, KIND = 1 * 1)", ""],
     ["(KIND=1*1)", "(KIND = 1 * 1)", ""],
     ["(LEN=:, KIND=1*1)", "(LEN = :, KIND = 1 * 1)", ", POINTER"],
     ["(:, KIND=1*1)", "(LEN = :, KIND = 1 * 1)", ", POINTER"],
     ["(:, 1*1)", "(LEN = :, KIND = 1 * 1)", ", POINTER"],
     ["(KIND=1*1, LEN=:)", "(LEN = :, KIND = 1 * 1)", ", POINTER"]))
def test_character_kind(
        fortran_reader, fortran_writer, tmpdir, char_type_in, char_type_out,
        pointer):
    '''Test that characters with kind clauses in various formats are
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
    expected = (
        f"module select_mod\n"
        f"  implicit none\n"
        f"  public\n\n"
        f"  contains\n"
        f"  subroutine select_type(type_selector, character_type)\n"
        f"    class(*), target :: type_selector\n"
        f"    character{char_type_out}{pointer} :: character_type\n"
        f"    character(256) :: type_string\n"
        f"    character(len=256), pointer :: ptr_character_star => null()\n\n"
        f"    type_string = ''\n"
        f"    select type(type_selector)\n"
        f"      type is (character(len = *))\n"
        f"      type_string = \"character_star\"\n"
        f"      ptr_character_star => type_selector\n"
        f"    end select\n"
        f"    if (type_string == 'character_star') then\n"
        f"      character_type = ptr_character_star\n"
        f"    end if\n\n"
        f"  end subroutine select_type\n\n"
        f"end module select_mod\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir).lower()
    assert result == expected
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize(
    "pre_attribute, post_attribute",
    (["TARGET", "TARGET"],
     ["POINTER", "POINTER"],
     ["ALLOCATABLE", "ALLOCATABLE, TARGET"]))
def test_class_target(
        fortran_reader, fortran_writer, tmpdir, pre_attribute, post_attribute):
    '''Test that the type_selector remains unchanged if it already has the
    target or pointer attribute and that target is added if there is a
    pre-existing unrelated atttribute.

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
    expected = (
        f"module select_mod\n"
        f"  implicit none\n"
        f"  public\n\n"
        f"  contains\n"
        f"  subroutine select_type(type_selector, character_type)\n"
        f"    CLASS(*), {post_attribute} :: type_selector\n"
        f"    CHARACTER(LEN = *) :: character_type\n"
        f"    character(256) :: type_string\n"
        f"    CHARACTER(LEN=256), pointer :: ptr_CHARACTER_star => null()\n\n"
        f"    type_string = ''\n"
        f"    SELECT TYPE(type_selector)\n"
        f"      TYPE IS (CHARACTER(LEN = *))\n"
        f"      type_string = \"character_star\"\n"
        f"      ptr_CHARACTER_star => type_selector\n"
        f"    END SELECT\n"
        f"    if (type_string == 'character_star') then\n"
        f"      character_type = ptr_CHARACTER_star\n"
        f"    end if\n\n"
        f"  end subroutine select_type\n\n"
        f"end module select_mod\n").lower()
    psyir = fortran_reader.psyir_from_source(code)
    if pre_attribute == "ALLOCATABLE":
        # If the argument doesn't already have a POINTER/TARGET attribute
        # then we can't handle it and should get a CodeBlock.
        routine = psyir.walk(Routine)[0]
        assert len(routine.children) == 1
        assert isinstance(routine.children[0], CodeBlock)
        # The reason for the CodeBlock should be in a comment.
        output = fortran_writer(routine)
        assert ("Type-selector variable 'type_selector' is defined externally "
                "(has interface 'Argument(Access.UNKNOWN)') and thus cannot "
                "be given the TARGET attribute" in output)
    else:
        result = fortran_writer(psyir).lower()
        assert result == expected
        assert Compile(tmpdir).string_compiles(result)


def test_add_target_attribute(fortran_reader):
    '''Check that _add_target_attribute works when a declaration either has
    or has no existing attributes.

    This is complicated because it appears that the only way for
    something that is not an argument to have a dynamic type is for it
    to be class(*). The fparser2 frontend cannot handle this and we
    end up with an UnsupportedFortranType with an UnknownInterface. We
    therefore have to explicitly fix the interface before we can test the
    functionality.

    '''
    code = '''\
module my_mod
  implicit none
contains

  subroutine my_sub()
    ! class(*) must have the ALLOCATABLE or POINTER attribute if it is not
    ! a dummy argument.
    class(*), allocatable, dimension(:) :: something
  end subroutine my_sub
end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    table = psyir.children[0].children[0].symbol_table
    sym = table.lookup("something")
    # Fix the interface of the symbol.
    sym.interface = AutomaticInterface()
    fp2reader = Fparser2Reader()
    # Test that _add_target_attribute now works for this Symbol.
    fp2reader._add_target_attribute("something", table)
    assert ("CLASS(*), ALLOCATABLE, DIMENSION(:), TARGET :: something"
            in str(sym.datatype))
    # Test in the absence of any attributes on the original declaration.
    # This is probably not reachable in normal use (but we want to make sure
    # that the code handles it in case it happens) so create our own
    # UnsupportedFortranType using a simple declaration.
    new_type = UnsupportedFortranType("integer :: a_local")
    sym2 = DataSymbol("a_local", datatype=new_type)
    table.add(sym2)
    fp2reader._add_target_attribute("a_local", table)
    assert "INTEGER, TARGET :: a_local" in str(sym2.datatype)
