# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing pytest tests for the handling of select type
construction for the Fparser->PSyIR frontend.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import CodeBlock, IfBlock
from psyclone.tests.utilities import Compile


def test_invalid_node():
    '''Check that the expected exception is raised if the supplied node is
    not an fparser2 Select_Type_Construct.

    '''
    reader = Fparser2Reader()
    with pytest.raises(InternalError) as info:
        reader._type_construct_handler(None, None)
    assert ("Failed to find opening select type statement in: None"
            in str(info.value))


def test_no_end_select():
    '''Check that the expected exception is raised if the supplied fparser
    tree root node does not have End_Select_Type_Stmt as its last child.

    '''
    code = (
        "  SELECT TYPE (type_selector)\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    TYPE IS (REAL)\n"
        "      branch2 = 1\n"
        "  END SELECT\n")
    string_reader = FortranStringReader(code)
    ParserFactory().create(std="f2008")
    ast = Fortran2003.Select_Type_Construct(string_reader)
    # Remove the end_select instance to force the exception
    del ast.children[-1]
    reader = Fparser2Reader()
    with pytest.raises(InternalError) as info:
        reader._type_construct_handler(ast, None)
    assert("Failed to find closing select type statement in: "
           "SELECT TYPE(type_selector)" in str(info.value))


def test_type(fortran_reader, fortran_writer, tmpdir):

    '''Check that the correct code is output with a basic select type
    construct. Also check that the appropriate annotation is added to
    the if nodes.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type_selector)\n"
        "  class(*) :: type_selector\n"
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
    expected = (
        "    character(256) :: type_string\n\n"
        "    INTEGER, pointer :: ptr_INTEGER\n\n"
        "    REAL, pointer :: ptr_REAL\n\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "  TYPE IS (INTEGER)\n"
        "  type_string = \"integer\"\n"
        "  ptr_INTEGER => type_selector\n"
        "  TYPE IS (REAL)\n"
        "  type_string = \"real\"\n"
        "  ptr_REAL => type_selector\n"
        "END SELECT\n"
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
    result = fortran_writer(psyir)
    assert expected in result
    if_blocks = psyir.walk(IfBlock)
    assert "was_select_type" in if_blocks[0].annotations
    assert "was_select_type" in if_blocks[1].annotations
    assert Compile(tmpdir).string_compiles(result)


def test_default(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output when select type has a
    default clause. The output of the default clause should be output
    uder the final else of the generated if hierarchy irrespective of
    where it appears in the select type clauses.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*) :: type\n"
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
        "    character(256) :: type_string\n\n"
        "    INTEGER, pointer :: ptr_INTEGER\n\n"
        "    REAL, pointer :: ptr_REAL\n\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "  TYPE IS (INTEGER)\n"
        "  type_string = \"integer\"\n"
        "  ptr_INTEGER => type\n"
        "  TYPE IS (REAL)\n"
        "  type_string = \"real\"\n"
        "  ptr_REAL => type\n"
        "END SELECT\n"
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
        "    end if")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
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
    expected = (
        "    character(256) :: type_string\n\n"
        "    type(type2), pointer :: ptr_type2\n\n"
        "    INTEGER, pointer :: ptr_INTEGER\n\n"
        "    type(type3), pointer :: ptr_type3\n\n"
        "    REAL, pointer :: ptr_REAL\n\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "  CLASS IS (type2)\n"
        "  type_string = \"type2\"\n"
        "  ptr_type2 => type\n"
        "  TYPE IS (INTEGER)\n"
        "  type_string = \"integer\"\n"
        "  ptr_INTEGER => type\n"
        "  CLASS IS (type3)\n"
        "  type_string = \"type3\"\n"
        "  ptr_type3 => type\n"
        "  TYPE IS (REAL)\n"
        "  type_string = \"real\"\n"
        "  ptr_REAL => type\n"
        "END SELECT\n"
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
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    print(result)
    exit(1)
    assert expected in result
    if_blocks = psyir.walk(IfBlock)
    assert "was_class_type" in if_blocks[0].annotations
    assert "was_class_type" in if_blocks[2].annotations
    assert Compile(tmpdir).string_compiles(result)


def test_select_rename(fortran_reader, fortran_writer, tmpdir):
    '''Check that a code block is created when the type in select type is
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
        "  TYPE IS (INTEGER)\n"
        "  PRINT *, newtype\n"
        "END SELECT\n")
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0].children[0], CodeBlock)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_kind(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output when the TYPE IS intrinsic
    content includes precision.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*) :: type\n"
        "  integer :: branch1, branch2\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (REAL(kind=8))\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    TYPE IS (REAL(16))\n"
        "      branch2 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    character(256) :: type_string\n\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "  TYPE IS (REAL(KIND = 8))\n"
        "  type_string = \"real(kind=8)\"\n"
        "  TYPE IS (REAL(KIND = 16))\n"
        "  type_string = \"real(16)\"\n"
        "END SELECT\n"
        "    if (type_string == 'real(kind=8)') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (type_string == 'real(16)') then\n"
        "        branch2 = 1\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_derived(fortran_reader, fortran_writer, tmpdir):
    '''Check that the expected code is prodiced when 'TYPE IS type' is a
    derived type.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type(type)\n"
        "  class(*) :: type\n"
        "  type field_type\n"
        "    integer :: x\n"
        "  end type\n"
        "  integer :: branch1\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (field_type)\n"
        "      branch1 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    character(256) :: type_string\n\n\n"
        "    type_string = ''\n"
        "    SELECT TYPE(type)\n"
        "  TYPE IS (field_type)\n"
        "  type_string = \"field_type\"\n"
        "END SELECT\n"
        "    if (type_string == 'field_type') then\n"
        "      branch1 = 1\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result
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
        "  class(*) :: type_selector\n"
        "  integer :: branch1, branch2, branch3\n"
        "  SELECT TYPE (type_selector)\n"
        "    TYPE IS (LOGICAL)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    TYPE IS (CHARACTER(*))\n"
        "      branch2 = 1\n"
        "    TYPE IS (COMPLEX)\n"
        "      branch3 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    type_string = ''\n"
        "    SELECT TYPE(type_selector)\n"
        "  TYPE IS (LOGICAL)\n"
        "  type_string = \"logical\"\n"
        "  TYPE IS (CHARACTER(LEN = *))\n"
        "  type_string = \"character(*)\"\n"
        "  TYPE IS (COMPLEX)\n"
        "  type_string = \"complex\"\n"
        "END SELECT\n"
        "    if (type_string == 'logical') then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (type_string == 'character(*)') then\n"
        "        branch2 = 1\n"
        "      else\n"
        "        if (type_string == 'complex') then\n"
        "          branch3 = 1\n"
        "        end if\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)
