# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2023, Science and Technology Facilities Council.
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
# Author R. W. Ford

'''Module containing pytest tests for the handling of select type
construction for the Fparser->PSyIR frontend.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import (
    Assignment_Stmt, Execution_Part, Name)

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Schedule, CodeBlock
from psyclone.tests.utilities import Compile

def test_type(fortran_reader, fortran_writer, tmpdir):
    '''Check that the correct code is output with a basic select type
    construct. Also check that the appropriate annotation is added to
    the if nodes.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  class(*) :: type\n"
        "  integer :: branch1, branch2\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    TYPE IS (REAL)\n"
        "      branch2 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    if (psyclone_cmp_type(type, 'INTEGER')) then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (psyclone_cmp_type(type, 'REAL')) then\n"
        "        branch2 = 1\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result
    ifnode1 = psyir.children[0].children[7].children[0]
    assert "was_select_type" in ifnode1.annotations
    ifnode2 = ifnode1.children[2][0]
    assert "was_select_type" in ifnode1.annotations
    print(tmpdir)
    assert Compile(tmpdir).string_compiles(result)

def test_default(fortran_reader, fortran_writer):
    '''Check that the correct code is output when select type has a
    default clause. The output of the default clause should be output
    uder the final else of the generated if hierarchy irrespective of
    where it appears in the select type clauses.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  class(*) :: type\n"
        "  integer :: branch1, branch2, branch3\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (INTEGER)\n"
        "        branch1 = 1\n"
        "        branch2 = 0\n"
        "    CLASS DEFAULT\n"
        "        branch3 = 1\n"
        "    TYPE IS (REAL)\n"
        "        branch2 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    if (psyclone_cmp_type(type, 'INTEGER')) then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (psyclone_cmp_type(type, 'REAL')) then\n"
        "        branch2 = 1\n"
        "      else\n"
        "        branch3 = 1\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result


def test_class(fortran_reader, fortran_writer):
    '''Check that the correct code is output when select type has a
    class is clause.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  class(*) :: type, type2\n"
        "  integer :: branch1, branch2, branch3\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (INTEGER)\n"
        "        branch1 = 1\n"
        "        branch2 = 0\n"
        "    CLASS IS(type2)\n"
        "        branch3 = 1\n"
        "    TYPE IS (REAL)\n"
        "        branch2 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    if (psyclone_cmp_type(type, 'INTEGER')) then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (psyclone_internal_cmp(type, type2)) then\n"
        "        branch3 = 1\n"
        "      else\n"
        "        if (psyclone_cmp_type(type, 'REAL')) then\n"
        "          branch2 = 1\n"
        "        end if\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result


def test_select_rename(fortran_reader, fortran_writer):
    '''Check that a code block is created when the type in select type is
    renamed (i.e. the code is not modified). This is done as we are
    not yet able to rename the variables inside the select type
    clause.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
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


def test_select_expr(fortran_reader, fortran_writer):
    '''Check that a code block is created when the type in select type is
    an expression (i.e. the code is not modified). This is done as we
    are not yet able to simply convert an fparser2 expression into a
    PSyIR expression.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  class(*) :: type, type2\n"
        "  integer :: branch1\n"
        "  SELECT TYPE (type+type2)\n"
        "    TYPE IS (INTEGER)\n"
        "      branch1 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    if (psyclone_cmp_type(type + type2, 'INTEGER')) "
        "then\n"
        "      branch1 = 1\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result


def test_kind(fortran_reader, fortran_writer):
    '''Check that the correct code is output when the TYPE IS intrinsic
    content includes precision.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  class(*) :: type\n"
        "  integer :: branch1, branch2, branch3, branch4\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (REAL(kind=8))\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    TYPE IS (REAL(16))\n"
        "      branch2 = 1\n"
        "    TYPE IS (REAL*4)\n"
        "      branch3 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    if (psyclone_cmp_type_kind(type, 'REAL', 8)) then\n"
        "      branch1 = 1\n"
        "      branch2 = 0\n"
        "    else\n"
        "      if (psyclone_cmp_type_kind(type, 'REAL', 16)) then\n"
        "        branch2 = 1\n"
        "      else\n"
        "        if (psyclone_cmp_type_starred(type, 'REAL*4')) then\n"
        "          branch3 = 1\n"
        "        end if\n"
        "      end if\n"
        "    end if\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    assert expected in result


def test_derived(fortran_reader, fortran_writer):
    '''Check that the code is not modified when the TYPE IS type is a
    derived type. This is because there does not seem to be a way to
    pass the type of a derived type via the argument list so it is not
    possible to write a generic comparison function. Note, the generic
    comparison functions are output in this implementation even though
    they are not used.

    '''
    code = (
        "module select_mod\n"
        "contains\n"
        "subroutine select_type()\n"
        "  use field_mod, only : field_type\n"
        "  class(*) :: type\n"
        "  integer :: branch1\n"
        "  SELECT TYPE (type)\n"
        "    TYPE IS (field_type)\n"
        "      branch1 = 1\n"
        "  END SELECT\n"
        "end subroutine\n"
        "end module\n")
    expected = (
        "    SELECT TYPE(type)\n"
        "  TYPE IS (field_type)\n"
        "  branch1 = 1\n"
        "END SELECT\n")
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[7].children[0], CodeBlock)
    result = fortran_writer(psyir)
    assert expected in result


# _find_or_create_psyclone_internal_cmp Working with program and
# subroutine - separate PR I think - before this one

# support char selector options
