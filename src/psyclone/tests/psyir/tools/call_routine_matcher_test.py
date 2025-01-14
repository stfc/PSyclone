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


from psyclone.psyir.symbols.datatypes import ArrayType, UnresolvedType
import os
import pytest
from psyclone.configuration import Config
from psyclone.parse import ModuleManager
from psyclone.psyir.tools.call_routine_matcher import (
    CallRoutineMatcher,
    CallMatchingArgumentsNotFoundError,
)
from psyclone.psyir.symbols import UnsupportedFortranType, SymbolError
from psyclone.psyir.nodes import Call, Node, Routine, Assignment, CodeBlock
from psyclone.psyir.transformations import InlineTrans
from psyclone.tests.utilities import Compile


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


def test_call_get_callee_6_interfaces_0_0(fortran_reader):
    """
    Check that a non-existing optional argument at the end of the list
    has been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_a: Routine = root_node.walk(Routine)[1]
    assert routine_foo_a.name == "foo_a"

    call_foo_a: Call = routine_main.walk(Call)[0]
    assert call_foo_a.routine.name == "foo"

    (result, arg_idx_list) = call_foo_a.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_foo_a


def test_call_get_callee_6_interfaces_0_1(fortran_reader):
    """
    Check that an existing optional argument at the end of the list
    has been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_a: Routine = root_node.walk(Routine)[1]
    assert routine_foo_a.name == "foo_a"

    call_foo_a: Call = routine_main.walk(Call)[1]
    assert call_foo_a.routine.name == "foo"

    (result, arg_idx_list) = call_foo_a.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_a


def test_call_get_callee_6_interfaces_1_0(fortran_reader):
    """
    Check that
    - different argument types and
    - non-existing optional argument at the end of the list
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[2]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_1_1(fortran_reader):
    """
    Check that
    - different argument types and
    - existing optional argument at the end of the list
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[3]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_1_2(fortran_reader):
    """
    Check that
    - different argument types and
    - naming arguments resulting in a different order
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[4]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 2
    assert arg_idx_list[2] == 1

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_2_0(fortran_reader):
    """
    Check that
    - different argument types (different order than in tests before)
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[5]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_2_1(fortran_reader):
    """
    Check that
    - different argument types (different order than in tests before) and
    - naming arguments resulting in a different order and
    - optional argument
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[6]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_2_2(fortran_reader):
    """
    Check that
    - different argument types (different order than in tests before) and
    - naming arguments resulting in a different order and
    - last call argument without naming
    have been correctly determined.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[7]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0
    assert arg_idx_list[2] == 2

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_3_0_mismatch(fortran_reader):
    """
    Check that matching a partial data type can also go wrong.
    """

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_optional: Routine = root_node.walk(Routine)[4]
    assert routine_foo_optional.name == "foo_optional"

    call_foo_optional: Call = routine_main.walk(Call)[8]
    assert call_foo_optional.routine.name == "foo"

    with pytest.raises(CallMatchingArgumentsNotFoundError) as einfo:
        call_foo_optional.get_callee()

    assert "Argument partial type mismatch of call argument" in (
        str(einfo.value)
    )


def test_call_get_callee_7_matching_arguments_not_found(fortran_reader):
    """
    Trigger error that matching arguments were not found
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    ! Use named argument 'd', which doesn't exist
    ! to trigger an error when searching for the matching routine.
    call foo(e, f, d=g)
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

    with pytest.raises(CallMatchingArgumentsNotFoundError) as err:
        call_foo.get_callee()

    assert (
        "Found routines, but no routine with matching arguments"
        " found for 'call foo(e, f, d=g)':" in str(err.value)
    )

    print(str(err.value))
    assert (
        "CallMatchingArgumentsNotFound: Named argument"
        " 'd' not found." in str(err.value)
    )


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


def test_fn_call_get_callees(fortran_reader):
    """
    Test that get_callees() works for a function call.
    """
    code = """
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    luggage = luggage + my_func(1)
  end subroutine top

  function my_func(val)
    integer, intent(in) :: val
    integer :: my_func
    my_func = 1 + val
  end function my_func
end module some_mod"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[1]]


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_get_callees_wildcard_import_container(
    fortran_reader, tmpdir, monkeypatch
):
    """
    Check that get_callees() works successfully for a routine accessed via
    a wildcard import from a module in another file.
    """
    code = """
module other_mod
  use some_mod
contains
  subroutine run_it()
    call just_do_it()
  end subroutine run_it
end module other_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    # This should fail as it can't find the module.
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find the source code of the unresolved routine "
        "'just_do_it' - looked at any routines in the same source file"
        in str(err.value)
    )
    # Create the module containing the subroutine definition,
    # write it to file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), "_include_paths", [path])

    with open(
        os.path.join(path, "some_mod.f90"), "w", encoding="utf-8"
    ) as mfile:
        mfile.write(
            """\
module some_mod
contains
  subroutine just_do_it()
    write(*,*) "hello"
  end subroutine just_do_it
end module some_mod"""
        )
    routines = call.get_callees()
    assert len(routines) == 1
    assert isinstance(routines[0], Routine)
    assert routines[0].name == "just_do_it"


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_get_callees_unresolved(fortran_reader, tmpdir, monkeypatch):
    """
    Test that get_callees() raises the expected error if the called routine
    is unresolved.
    """
    code = """
subroutine top()
  call bottom()
end subroutine top"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find the source code of the unresolved routine 'bottom'"
        " - looked at any routines in the same source file and there are "
        "no wildcard imports." in str(err.value)
    )
    # Repeat but in the presence of a wildcard import.
    code = """
subroutine top()
  use some_mod_somewhere
  call bottom()
end subroutine top"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find the source code of the unresolved routine 'bottom'"
        " - looked at any routines in the same source file and attempted "
        "to resolve the wildcard imports from ['some_mod_somewhere']. "
        "However, failed to find the source for ['some_mod_somewhere']. "
        "The module search path is set to []" in str(err.value)
    )
    # Repeat but when some_mod_somewhere *is* resolved but doesn't help us
    # find the routine we're looking for.
    mod_manager = ModuleManager.get()
    monkeypatch.setattr(mod_manager, "_instance", None)
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), "_include_paths", [path])
    with open(
        os.path.join(path, "some_mod_somewhere.f90"), "w", encoding="utf-8"
    ) as ofile:
        ofile.write(
            """\
module some_mod_somewhere
end module some_mod_somewhere
"""
        )
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find the source code of the unresolved routine 'bottom'"
        " - looked at any routines in the same source file and wildcard "
        "imports from ['some_mod_somewhere']." in str(err.value)
    )
    mod_manager = ModuleManager.get()
    monkeypatch.setattr(mod_manager, "_instance", None)
    code = """
subroutine top()
  use another_mod, only: this_one
  call this_one()
end subroutine top"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "RoutineSymbol 'this_one' is imported from Container 'another_mod'"
        " but the source defining that container could not be found. The "
        "module search path is set to [" in str(err.value)
    )


def test_call_get_callees_interface(fortran_reader):
    """
    Check that get_callees() works correctly when the target of a call is
    actually a generic interface.
    """
    code = """
module my_mod

    interface bottom
      module procedure :: rbottom, ibottom
    end interface bottom
contains
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  subroutine ibottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine ibottom

  subroutine rbottom(luggage)
    real :: luggage
    luggage = luggage + 1.0
  end subroutine rbottom
end module my_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    callees = call.get_callees()
    assert len(callees) == 2
    assert isinstance(callees[0], Routine)
    assert callees[0].name == "rbottom"
    assert isinstance(callees[1], Routine)
    assert callees[1].name == "ibottom"


def test_call_get_callees_unsupported_type(fortran_reader):
    """
    Check that get_callees() raises the expected error when the called routine
    is of UnsupportedFortranType. This is hard to achieve so we have to
    manually construct some aspects of the test case.

    """
    code = """
module my_mod
  integer, target :: value
contains
  subroutine top()
    integer :: luggage
    luggage = bottom()
  end subroutine top
  function bottom() result(fval)
    integer, pointer :: fval
    fval => value
  end function bottom
end module my_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    container = psyir.children[0]
    routine = container.find_routine_psyir("bottom")
    rsym = container.symbol_table.lookup(routine.name)
    # Ensure the type of this RoutineSymbol is UnsupportedFortranType.
    rsym.datatype = UnsupportedFortranType("integer, pointer :: fval")
    assign = container.walk(Assignment)[0]
    # Currently `bottom()` gets matched by fparser2 as a structure constructor
    # and the fparser2 frontend leaves this as a CodeBlock (TODO #2429) so
    # replace it with a Call. Once #2429 is fixed the next two lines can be
    # removed.
    assert isinstance(assign.rhs, CodeBlock)
    assign.rhs.replace_with(Call.create(rsym))
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "RoutineSymbol 'bottom' exists in Container 'my_mod' but is of "
        "UnsupportedFortranType" in str(err.value)
    )


def test_call_get_callees_local(fortran_reader):
    """
    Check that get_callees() works as expected when the target of the Call
    exists in the same Container as the call site.
    """
    code = """
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    call bottom()
  end subroutine top

  subroutine bottom()
    luggage = luggage + 1
  end subroutine bottom
end module some_mod"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[1]]


def test_call_get_callee_matching_arguments_not_found(fortran_reader):
    """
    Trigger error that matching arguments were not found.
    In this test, this is caused by omitting the required third non-optional
    argument.
    """
    code = """
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f
    ! Omit the 3rd required argument
    call foo(e, f)
  end subroutine

  ! Routine matching by 'name', but not by argument matching
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod"""

    psyir = fortran_reader.psyir_from_source(code)

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]

    with pytest.raises(CallMatchingArgumentsNotFoundError) as err:
        call_foo.get_callee()

    assert (
        "CallMatchingArgumentsNotFound: Found routines, but"
        " no routine with matching arguments found for 'call"
        " foo(e, f)':" in str(err.value)
    )

    assert (
        "CallMatchingArgumentsNotFound: Argument 'c' in"
        " subroutine 'foo' not handled." in str(err.value)
    )


def test_call_get_callees_file_container(fortran_reader):
    """
    Check that get_callees works if the called routine happens to be in file
    scope, even when there's no Container.
    """
    code = """
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  subroutine bottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine bottom
"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert len(result) == 1
    assert isinstance(result[0], Routine)
    assert result[0].name == "bottom"


def test_call_get_callees_no_container(fortran_reader):
    """
    Check that get_callees() raises the expected error when the Call is not
    within a Container and the target routine cannot be found.
    """
    # To avoid having the routine symbol immediately dismissed as
    # unresolved, the code that we initially process *does* have a Container.
    code = """
module my_mod

contains
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  subroutine bottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine bottom
end module my_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    top_routine = psyir.walk(Routine)[0]
    # Deliberately make the Routine node an orphan so there's no Container.
    top_routine.detach()
    call = top_routine.walk(Call)[0]
    with pytest.raises(SymbolError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find a Routine named 'bottom' in code:\n'subroutine "
        "top()" in str(err.value)
    )


def test_call_get_callees_wildcard_import_local_container(fortran_reader):
    """
    Check that get_callees() works successfully for a routine accessed via
    a wildcard import from another module in the same file.
    """
    code = """
module some_mod
contains
  subroutine just_do_it()
    write(*,*) "hello"
  end subroutine just_do_it
end module some_mod
module other_mod
  use some_mod
contains
  subroutine run_it()
    call just_do_it()
  end subroutine run_it
end module other_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    routines = call.get_callees()
    assert len(routines) == 1
    assert isinstance(routines[0], Routine)
    assert routines[0].name == "just_do_it"


def test_call_get_callees_import_local_container(fortran_reader):
    """
    Check that get_callees() works successfully for a routine accessed via
    a specific import from another module in the same file.
    """
    code = """
module some_mod
contains
  subroutine just_do_it()
    write(*,*) "hello"
  end subroutine just_do_it
end module some_mod
module other_mod
  use some_mod, only: just_do_it
contains
  subroutine run_it()
    call just_do_it()
  end subroutine run_it
end module other_mod
"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    routines = call.get_callees()
    assert len(routines) == 1
    assert isinstance(routines[0], Routine)
    assert routines[0].name == "just_do_it"


def test_get_callees_code_block(fortran_reader):
    """Test that get_callees() raises the expected error when the called
    routine is in a CodeBlock."""
    code = """
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    luggage = luggage + real(my_func(1))
  end subroutine top

  complex function my_func(val)
    integer, intent(in) :: val
    my_func = CMPLX(1 + val, 1.0)
  end function my_func
end module some_mod"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[1]
    with pytest.raises(SymbolError) as err:
        _ = call.get_callees()
    assert (
        "Failed to find a Routine named 'my_func' in Container "
        "'some_mod'" in str(err.value)
    )


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_get_callees_follow_imports(fortran_reader, tmpdir, monkeypatch):
    """
    Test that get_callees() follows imports to find the definition of the
    called routine.
    """
    code = """
module some_mod
  use other_mod, only: pack_it
  implicit none
contains
  subroutine top()
    integer :: luggage = 0
    call pack_it(luggage)
  end subroutine top
end module some_mod"""
    # Create the module containing an import of the subroutine definition,
    # write it to file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), "_include_paths", [path])

    with open(
        os.path.join(path, "other_mod.f90"), "w", encoding="utf-8"
    ) as mfile:
        mfile.write(
            """\
    module other_mod
        use another_mod, only: pack_it
    contains
    end module other_mod
    """
        )
    # Finally, create the module containing the routine definition.
    with open(
        os.path.join(path, "another_mod.f90"), "w", encoding="utf-8"
    ) as mfile:
        mfile.write(
            """\
    module another_mod
    contains
        subroutine pack_it(arg)
          integer, intent(inout) :: arg
          arg = arg + 2
        end subroutine pack_it
    end module another_mod
    """
        )
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert len(result) == 1
    assert isinstance(result[0], Routine)
    assert result[0].name == "pack_it"


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_get_callees_import_private_clash(fortran_reader, tmpdir, monkeypatch):
    """
    Test that get_callees() raises the expected error if a module from which
    a routine is imported has a private shadow of that routine (and thus we
    don't know where to look for the target routine).
    """
    code = """
module some_mod
  use other_mod, only: pack_it
  implicit none
contains
  subroutine top()
    integer :: luggage = 0
    call pack_it(luggage)
  end subroutine top
end module some_mod"""
    # Create the module containing a private routine with the name we are
    # searching for, write it to file and set the search path so that PSyclone
    # can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), "_include_paths", [path])

    with open(
        os.path.join(path, "other_mod.f90"), "w", encoding="utf-8"
    ) as mfile:
        mfile.write(
            """\
    module other_mod
        use another_mod
        private pack_it
    contains
        function pack_it(arg)
          integer :: arg
          integer :: pack_it
        end function pack_it
    end module other_mod
    """
        )
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert (
        "RoutineSymbol 'pack_it' is imported from Container 'other_mod' "
        "but that Container defines a private Symbol of the same name. "
        "Searching for the Container that defines a public Routine with "
        "that name is not yet supported - TODO #924" in str(err.value)
    )


def test_apply_empty_routine_coverage_option_check_strict_array_datatype(
    fortran_reader, fortran_writer, tmpdir
):
    """For coverage of particular branch in `inline_trans.py`."""
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer, dimension(6) :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer, dimension(:) :: idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.set_option(check_argument_strict_array_datatype=False)
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert "    i = 10\n\n" "  end subroutine run_it\n" in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_access_check_unresolved_symbols_error(
    fortran_reader, fortran_writer, tmpdir
):
    """
    This check solely exists for the coverage report to
    catch the simple case `if not check_unresolved_symbols:`
    in `symbol_table.py`

    """
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    real :: a(10)\n"
        "    do i=1,10\n"
        "      call sub(a, i)\n"
        "    end do\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x, ivar)\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    integer, intent(in) :: ivar\n"
        "    integer :: i\n"
        "    do i = 1, 10\n"
        "      x(i) = 2.0*ivar\n"
        "    end do\n"
        "  end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.set_option(check_unresolved_symbols=False)
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert (
        "    do i = 1, 10, 1\n"
        "      do i_1 = 1, 10, 1\n"
        "        a(i_1) = 2.0 * i\n"
        "      enddo\n" in output
    )
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_access_check_unresolved_override_option(
    fortran_reader, fortran_writer, tmpdir
):
    """
    This check solely exists for the coverage report to catch
    the case where the override option to ignore unresolved
    types is used.

    """
    code = (
        "module test_mod\n"
        "use does_not_exist\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    type(unknown_type) :: a\n"
        "    call sub(a%unresolved_type)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(a)\n"
        "    type(unresolved) :: a\n"
        "  end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.set_option(
        check_argument_ignore_unresolved_types=True
        )
    inline_trans.apply(call)
