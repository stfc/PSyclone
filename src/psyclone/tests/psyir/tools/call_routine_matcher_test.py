# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Author: Martin Schreiber, Univ. Grenoble Alpes / LJK / Inria
# -----------------------------------------------------------------------------


import pytest
from psyclone.psyir.tools.call_routine_matcher import (
    CallRoutineMatcher,
    CallMatchingArgumentsNotFoundError,
)
from psyclone.psyir.nodes import Call, Node, Routine
from psyclone.psyir.transformations import InlineTrans


def test_apply_optional_and_named_arg(fortran_reader):
    """Test that the validate method inlines a routine
    that has an optional argument."""
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real :: var = 0.0\n"
        "  call sub(var, named=1.0)\n"
        "  ! Result:\n"
        "  ! var = var + 1.0 + 1.0\n"
        "  call sub(var, 2.0, named=1.0)\n"
        "  ! Result:\n"
        "  ! var = var + 2.0\n"
        "  ! var = var + 1.0 + 1.0\n"
        "end subroutine main\n"
        "subroutine sub(x, opt, named)\n"
        "  real, intent(inout) :: x\n"
        "  real, optional :: opt\n"
        "  real :: named\n"
        "  if( present(opt) )then\n"
        "    x = x + opt\n"
        "  end if\n"
        "  x = x + 1.0 + named\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir: Node = fortran_reader.psyir_from_source(code)

    inline_trans = InlineTrans()

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"
    for call in psyir.walk(Call, stop_type=Call):
        call: Call
        if call.routine.name != "sub":
            continue

        inline_trans.apply(call)

    assert (
        """var = var + 1.0 + 1.0
  var = var + 2.0
  var = var + 1.0 + 1.0"""
        in routine_main.debug_string()
    )


def test_unresolved_types(fortran_reader):
    """Test that the validate method inlines a routine that has a named
    argument."""

    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real :: var = 0.0\n"
        "  call sub(var, opt=1.0)\n"
        "end subroutine main\n"
        "subroutine sub(x, opt)\n"
        "  real, intent(inout) :: x\n"
        "  real :: opt\n"
        "  x = x + 1.0\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )

    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]

    crm = CallRoutineMatcher(call)

    crm.set_option(ignore_unresolved_types=True)
    crm.get_callee_candidates()


def test_call_get_callee_1_simple_match(fortran_reader):
    """
    Check that the right routine has been found for a single routine
    implementation.
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod"""

    psyir = fortran_reader.psyir_from_source(code)

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]

    (result, _) = call_foo.get_callee()

    routine_match: Routine = psyir.walk(Routine)[1]
    assert result is routine_match


def test_call_get_callee_2_optional_args(fortran_reader):
    """
    Check that optional arguments have been correlated correctly.
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

end module some_mod"""

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_match


def test_call_get_callee_3a_trigger_error(fortran_reader):
    """
    Test which is supposed to trigger an error when no matching routine
    is found
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b)
    integer :: a, b
  end subroutine

end module some_mod"""

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    with pytest.raises(CallMatchingArgumentsNotFoundError) as err:
        call_foo.get_callee()

    assert (
        "Found routines, but no routine with matching arguments found"
        in str(err.value)
    )


def test_call_get_callee_3c_trigger_error(fortran_reader):
    """
    Test which is supposed to trigger an error when no matching routine
    is found, but we use the special option check_matching_arguments=False
    to find one.
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b)
    integer :: a, b
  end subroutine

end module some_mod"""

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    call_foo.get_callee(check_matching_arguments=False)


def test_call_get_callee_4_named_arguments(fortran_reader):
    """
    Check that named arguments have been correlated correctly
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(c=e, a=f, b=g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod"""

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 2
    assert arg_idx_list[1] == 0
    assert arg_idx_list[2] == 1

    assert result is routine_match


def test_call_get_callee_5_optional_and_named_arguments(fortran_reader):
    """
    Check that optional and named arguments have been correlated correctly
    when the call is to a generic interface.
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(b=e, a=f)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

end module some_mod"""

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0

    assert result is routine_match


_code_test_get_callee_6 = """
module some_mod
  implicit none

  interface foo
    procedure foo_a, foo_b, foo_c, foo_optional
  end interface
contains

  subroutine main()
    integer :: e_int, f_int, g_int
    real :: e_real, f_real, g_real

    ! Should match foo_a, test_call_get_callee_6_interfaces_0_0
    call foo(e_int, f_int)

    ! Should match foo_a, test_call_get_callee_6_interfaces_0_1
    call foo(e_int, f_int, g_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_0
    call foo(e_real, f_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_1
    call foo(e_real, f_int, g_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_2
    call foo(e_real, c=f_int, b=g_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_0
    call foo(e_int, f_real, g_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_1
    call foo(b=e_real, a=f_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_2
    call foo(b=e_real, a=f_int, g_int)

    ! Should not match foo_optional because of invalid type,
    ! test_call_get_callee_6_interfaces_3_0_mismatch
    call foo(f_int, e_real, g_int, g_int)
  end subroutine

  subroutine foo_a(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

  subroutine foo_b(a, b, c)
    real :: a
    integer :: b
    integer, optional :: c
  end subroutine

  subroutine foo_c(a, b, c)
    integer :: a
    real :: b
    integer, optional :: c
  end subroutine

  subroutine foo_optional(a, b, c, d)
    integer :: a
    real :: b
    integer :: c
    real, optional :: d ! real vs. int
  end subroutine


end module some_mod"""


def test_set_routine(fortran_reader):
    """Test the routine setter (not in the constructor)."""

    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real :: var = 0.0\n"
        "  call sub(var, opt=1.0)\n"
        "end subroutine main\n"
        "subroutine sub(x, opt)\n"
        "  real, intent(inout) :: x\n"
        "  real :: opt\n"
        "  x = x + 1.0\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )

    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    routine = psyir.walk(Routine)[0]

    crm = CallRoutineMatcher()
    crm.set_call_node(call)
    crm.set_routine_node(routine)
