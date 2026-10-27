# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Contains the tests for the fortran backend that correspond to
select type and type guard nodes.'''

import pytest

from psyclone.psyir.nodes import TypeGuard
from psyclone.tests.utilities import Compile


@pytest.mark.parametrize(
    "input_type, output_type",
    (["integer(8)", "integer(kind=8)"],  # Integer kind
     ["integer(int32)", "integer(kind=int32)"],  # Reference kind
     ["integer(wp)", "integer(kind=wp)"],  # Reference kind
     ["character(*)", "character(len=*)"],  # Character of assumed length
     ["character(kind=1, len=*)", "character(kind=1, len=*)"],  # with kind
     ["character(kind=wp, len=*)", "character(kind=wp, len=*)"],
     ["some_kind", "some_kind"],  # User defined type
     ["double precision", "double precision"],  # double precision type
     ["double complex", "double complex"],  # double complex type
     ["real", "real"],  # Undefined precision real type.
     ))
def test_supported_typeguard_datatypes(fortran_reader, fortran_writer,
                                       input_type, output_type):
    '''Tests the non exception cases of _get_typeguard_datatype_str.'''

    code = f"""subroutine test(i)
    use some_mod
    use kind_mod, only: wp
    use iso_fortran_env, only: int32

    class(*), allocatable :: i

    select type(i)
        type is({input_type})
           call a()
    end select
    end subroutine"""

    psyir = fortran_reader.psyir_from_source(code)

    type_guard = psyir.walk(TypeGuard)[0]

    result = fortran_writer._get_typeguard_datatype_str(
        type_guard.comparison_type
    )
    assert result == output_type


# def test_unsupported_typeguard_datatypes(fortran_reader, fortran_writer):
#    assert False

def test_select_type_output(fortran_reader, fortran_writer, tmpdir):
    '''Tests that select type output is as expected.'''
    code = """subroutine test(i)
    use some_mod

    class(*), allocatable :: i

    select type(i)
        type is(integer)
           call a()
        class is(some_class)
           call b()
        class default
           call c()
    end select
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    correct = """subroutine test(i)
  use some_mod
  class(*), allocatable :: i

  select type(i)
    type is(integer)
      call a()
    class is(some_class)
      call b()
    class default
      call c()
  end select
end subroutine test"""
    assert correct in result
    assert Compile(tmpdir).string_compiles(result)
