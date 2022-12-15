# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab

'''This module tests the inlining transformation.
'''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Call, Routine
from psyclone.psyir.transformations import (InlineTrans,
                                            TransformationError)
from psyclone.tests.utilities import Compile

MY_TYPE = ("  type other_type\n"
           "    real, dimension(10) :: data\n"
           "  end type other_type\n"
           "  type my_type\n"
           "    integer :: idx\n"
           "    real, dimension(10) :: data\n"
           "    type(other_type) :: local\n"
           "  end type my_type\n")

# init

def test_init():
    '''Test an InlineTrans transformation can be successfully created.'''
    inline_trans = InlineTrans()
    assert isinstance(inline_trans, InlineTrans)


# apply

def test_apply_empty_routine(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to an empty routine is simply removed.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_single_return(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to a routine containing only a return statement
    is removed. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    return\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_return_then_cb(fortran_reader, fortran_writer, tmpdir):
    '''Check that a call to a routine containing a return statement followed
    by a CodeBlock is removed.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    return\n"
        "    write(*,*) idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_arg(fortran_reader, fortran_writer, tmpdir):
    ''' Check that the apply() method works correctly for a very simple
    call to a routine with an array reference as argument. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "    call sub(a(i))\n"
        "  end do\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    x = 2.0*x\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      a(i) = 1.0\n"
            "      a(i) = 2.0 * a(i)\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_access(fortran_reader, fortran_writer, tmpdir):
    '''
    Check that the apply method works correctly when an array is passed
    into the routine and then indexed within it.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    call sub(a, i)\n"
        "  end do\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x, ivar)\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    integer, intent(in) :: ivar\n"
        "    integer :: i\n"
        "    do i = 1, 10\n"
        "      x(i) = 2.0*ivar\n"
        "    end do\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      do i_1 = 1, 10, 1\n"
            "        a(i_1) = 2.0 * i\n"
            "      enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_arg(fortran_reader, fortran_writer, tmpdir):
    '''
    Check that the apply() method works correctly when the routine argument
    is a StructureReference containing an ArrayMember which is accessed inside
    the routine.

    '''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"  integer :: i\n"
        f"  type big_type\n"
        f"    type(my_type) :: local\n"
        f"  end type big_type\n"
        f"  type(my_type) :: var\n"
        f"  type(my_type) :: var_list(10)\n"
        f"  type(big_type) :: var2(5)\n"
        f"  do i=1,5\n"
        f"    call sub(var, i)\n"
        f"    call sub(var_list(i), i)\n"
        f"    call sub(var2(i)%local, i)\n"
        f"  end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x, ivar)\n"
        f"    type(my_type), intent(inout) :: x\n"
        f"    integer, intent(in) :: ivar\n"
        f"    integer :: i\n"
        f"    do i = 1, 10\n"
        f"      x%data(i) = 2.0*ivar\n"
        f"    end do\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routines = psyir.walk(Call)
    inline_trans = InlineTrans()
    inline_trans.apply(routines[0])
    inline_trans.apply(routines[1])
    inline_trans.apply(routines[2])
    output = fortran_writer(psyir)
    assert ("    do i = 1, 5, 1\n"
            "      do i_3 = 1, 10, 1\n"
            "        var%data(i_3) = 2.0 * i\n"
            "      enddo\n"
            "      do i_1 = 1, 10, 1\n"
            "        var_list(i)%data(i_1) = 2.0 * i\n"
            "      enddo\n"
            "      do i_2 = 1, 10, 1\n"
            "        var2(i)%local%data(i_2) = 2.0 * i\n"
            "      enddo\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_array_slice_arg(fortran_reader, fortran_writer, tmpdir):
    '''
    Check that the apply() method works correctly when an array slice is
    passed to a routine and then accessed within it.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  real :: a(5,10,10), b(10,10)\n"
        "  do i=1,10\n"
        "    call sub1(a(1,:,i))\n"
        "  end do\n"
        "  call sub1a(a(1,1,:))\n"
        "  call sub2(a(1,:,:))\n"
        "  call sub2(b)\n"
        "  end subroutine run_it\n"
        "  subroutine sub1(x)\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    integer :: i\n"
        "    do i = 1, 10\n"
        "      x(i) = 2.0*i\n"
        "    end do\n"
        "  end subroutine sub1\n"
        "  subroutine sub1a(x)\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    x(1:10) = 2.0 * x(1:10)\n"
        "  end subroutine sub1a\n"
        "  subroutine sub2(x)\n"
        "    real, intent(inout), dimension(10,10) :: x\n"
        "    integer :: i\n"
        "    x = 2.0 * x\n"
        "  end subroutine sub2\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for call in psyir.walk(Call):
        inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      do i_3 = 1, 10, 1\n"
            "        a(1,i_3,i) = 2.0 * i_3\n"
            "      enddo\n"
            "    enddo\n"
            "    a(1,:,:) = 2.0 * a(1,:,:)\n"
            "    b = 2.0 * b\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_array_arg(fortran_reader, fortran_writer, tmpdir):
    '''Check that apply works correctly when the actual argument is an
    array element within a structure.'''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"  integer :: i, ig\n"
        f"  real :: a(10)\n"
        f"  type(my_type) :: grid\n"
        f"  type(my_type), dimension(5) :: grid_list\n"
        f"  grid%data(:) = 1.0\n"
        f"  do i=1,10\n"
        f"    a(i) = 1.0\n"
        f"    call sub(grid%data(i))\n"
        f"  end do\n"
        f"  do i=1,10\n"
        f"    ig = min(i, 5)\n"
        f"    call sub(grid_list(ig)%data(i))\n"
        f"  end do\n"
        f"  do i=1,10\n"
        f"    ig = min(i, 5)\n"
        f"    call sub(grid_list(ig)%local%data(i))\n"
        f"  end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x)\n"
        f"    real, intent(inout) :: x\n"
        f"    x = 2.0*x\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routines = psyir.walk(Call)
    inline_trans = InlineTrans()
    inline_trans.apply(routines[0])
    inline_trans.apply(routines[1])
    inline_trans.apply(routines[2])
    output = fortran_writer(psyir).lower()
    assert ("    do i = 1, 10, 1\n"
            "      a(i) = 1.0\n"
            "      grid%data(i) = 2.0 * grid%data(i)\n"
            "    enddo\n" in output)
    assert ("    do i = 1, 10, 1\n"
            "      ig = min(i, 5)\n"
            "      grid_list(ig)%data(i) = 2.0 * grid_list(ig)%data(i)\n"
            "    enddo\n" in output)
    assert ("    do i = 1, 10, 1\n"
            "      ig = min(i, 5)\n"
            "      grid_list(ig)%local%data(i) = 2.0 * "
            "grid_list(ig)%local%data(i)\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_array_slice_arg(fortran_reader, fortran_writer, tmpdir):
    '''Check that apply works correctly when the actual argument is an
    array slice within a structure.'''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"  integer :: i\n"
        f"  real :: a(10)\n"
        f"  type(my_type) :: grid\n"
        f"  grid%data(:) = 1.0\n"
        f"  do i=1,10\n"
        f"    a(i) = 1.0\n"
        f"    call sub(grid%data(:))\n"
        f"  end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x)\n"
        f"    real, dimension(:), intent(inout) :: x\n"
        f"    integer ji\n"
        f"    do ji = 1, 5\n"
        f"      x(ji) = 2.0*x(ji)\n"
        f"    end do\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      a(i) = 1.0\n"
            "      do ji = 1, 5, 1\n"
            "        grid%data(ji) = 2.0 * grid%data(ji)\n"
            "      enddo\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_ptr_arg(fortran_reader, fortran_writer, tmpdir):
    '''Check that apply works correctly when the routine has a pointer
    argument (which is captured as an UnknownFortranType). '''
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real, target :: var = 0.0\n"
        "  real, pointer :: ptr => null()\n"
        "  ptr => var\n"
        "  call sub(ptr)\n"
        "end subroutine main\n"
        "subroutine sub(x)\n"
        "  real, pointer, intent(inout) :: x\n"
        "  x = x + 1.0\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    ptr => var\n"
            "    ptr = ptr + 1.0\n\n"
            "  end subroutine main\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_saved_var(fortran_reader, fortran_writer, tmpdir):
    '''
    Test that a subroutine with a 'save'd variable is inlined correctly.
    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  real :: a(10)\n"
        "  do i=1,10\n"
        "    a(i) = 1.0\n"
        "    call sub(a(i))\n"
        "  end do\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    real, save :: state = 0.0\n"
        "    state = state + x\n"
        "    x = 2.0*x + state\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir).lower()
    assert ("  subroutine run_it()\n"
            "    integer :: i\n"
            "    real, dimension(10) :: a\n"
            "    real, save :: state = 0.0\n" in output)
    assert ("      a(i) = 1.0\n"
            "      state = state + a(i)\n"
            "      a(i) = 2.0 * a(i) + state\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_name_clash(fortran_reader, fortran_writer, tmpdir):
    ''' Check that apply() correctly handles the case where a symbol
    in the routine to be in-lined clashes with an existing symbol. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    real :: y\n"
        "    i = 10\n"
        "    y = 1.0\n"
        "    call sub(y)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    real :: i\n"
        "    i = 3.0\n"
        "    x = 2.0*x + i\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    y = 1.0\n"
            "    i_1 = 3.0\n"
            "    y = 2.0 * y + i_1\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_imported_symbols(fortran_reader, fortran_writer):
    '''Test that the apply method correctly handles imported symbols in the
    routine being inlined. '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use some_mod, only: var2\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = 3*var2\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use some_mod, only : var2\n"
            "    integer :: i\n\n"
            "    i = 10\n"
            "    i = 3 * var2\n" in output)
    # We can't check this with compilation because of the import of some_mod.


def test_apply_last_stmt_is_return(fortran_reader, fortran_writer, tmpdir):
    '''Test that the apply method correctly omits any final 'return'
    statement that may be present in the routine to be inlined.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + 3\n"
        "    return\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    i = i + 3\n\n"
            "  end subroutine run_it\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_call_args(fortran_reader, fortran_writer):
    '''Check that apply works correctly if any of the actual
    arguments are not simple references.'''
    code = (
        "module test_mod\n"
        " use kinds_mod, only: i_def\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i, 2*i, 5_i_def)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx, incr1, incr2)\n"
        "    integer, intent(inout) :: idx\n"
        "    integer, intent(in) :: incr1\n"
        "    integer(kind=i_def), intent(in) :: incr2\n"
        "    idx = idx + incr1 * incr2\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    i = 10\n"
            "    i = i + 2 * i * 5_i_def\n\n"
            "  end subroutine run_it\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_duplicate_imports(fortran_reader, fortran_writer):
    '''Check that apply works correctly when the routine to be inlined
    imports symbols from a container that is also accessed in the
    calling routine.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: i_def\n"
        "  integer :: i\n"
        "  i = 10_i_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def\n"
            "    integer :: i\n\n" in output)
    assert ("    i = 10_i_def\n"
            "    i = i + 5_i_def\n\n"
            "  end subroutine run_it\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_wildcard_import(fortran_reader, fortran_writer):
    '''Check that apply works correctly when a wildcard import is present
    in the routine to be inlined.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: i_def\n"
        "  integer :: i\n"
        "  i = 10_i_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod\n"
            "    integer :: i\n\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_import_union(fortran_reader, fortran_writer):
    '''Test that the apply method works correctly when the set of symbols
    imported from a given container is not the same as that imported into
    the scope of the call site.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    integer :: i\n\n" in output)
    assert ("    i = 10.0_r_def\n"
            "    i = i + 5_i_def\n" in output)
    # Cannot test for compilation because of 'kinds_mod'.


def test_apply_callsite_rename(fortran_reader, fortran_writer):
    '''Check that a symbol import in the routine causes a
    rename of a symbol that is local to the *calling* scope.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i, a_clash\n"
        "  a_clash = 2\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  i = i * a_clash\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    use a_mod, only : a_clash\n"
            "    integer :: i\n"
            "    integer :: a_clash_1\n\n"
            "    a_clash_1 = 2\n"
            "    i = 10.0_r_def\n"
            "    i = i + 5_i_def + a_clash\n"
            "    i = i * a_clash_1\n" in output)


def test_apply_callsite_rename_container(fortran_reader, fortran_writer):
    '''Check that an import from a container in the routine causes a
    rename of a symbol that is local to the *calling* scope.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  use kinds_mod, only: r_def\n"
        "  integer :: i, a_mod\n"
        "  a_mod = 2\n"
        "  i = 10.0_r_def\n"
        "  call sub(i)\n"
        "  i = i * a_mod\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    use kinds_mod, only : i_def, r_def\n"
            "    use a_mod, only : a_clash\n"
            "    integer :: i\n"
            "    integer :: a_mod_1\n\n"
            "    a_mod_1 = 2\n"
            "    i = 10.0_r_def\n"
            "    i = i + 5_i_def + a_clash\n"
            "    i = i * a_mod_1\n" in output)


def test_inline_symbols_check(fortran_reader):
    '''Test the internal consistency check within _inline_symbols.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: r_def\n"
        "    integer :: i, a_var\n"
        "    a_var = a_clash\n"
        "    i = 10.0_r_def\n"
        "    call sub(i)\n"
        "    i = i * a_var\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use a_mod, only: a_clash\n"
        "    use kinds_mod, only: i_def\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def + a_clash\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routines = psyir.walk(Routine)
    caller = routines[0]
    callee = routines[1]
    inline_trans = InlineTrans()
    with pytest.raises(InternalError) as err:
        inline_trans._inline_symbols(caller.symbol_table,
                                     callee.symbol_table, {})
    assert ("Symbol 'a_clash' imported from 'a_mod' has not been updated to "
            "refer to that container at the call site." in str(err.value))


def test_inline_non_local_import(fortran_reader, fortran_writer):
    '''Test that we correctly handle the case where the routine to be
    inlined accesses a symbol from an import in its parent container.'''
    code = (
        "module test_mod\n"
        "  use some_mod, only: trouble\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("  subroutine run_it()\n"
            "    integer :: i\n\n"
            "    i = 10\n"
            "    i = i + trouble\n" in output)


def test_apply_function(fortran_reader, fortran_writer, tmpdir):
    '''Check that the apply() method works correctly for a simple call to
    a function.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  real :: a,b\n"
        "  a = func(b)\n"
        "  end subroutine run_it\n"
        "  real function func(b)\n"
        "    real :: b\n"
        "    func = 2.0\n"
        "  end function\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    expected = (
        "  subroutine run_it()\n"
        "    real :: a\n"
        "    real :: b\n"
        "    real :: inlined_func\n\n"
        "    inlined_func = 2.0\n"
        "    a = inlined_func")
    assert expected in output
    assert Compile(tmpdir).string_compiles(output)


# Try two different forms of function declaration.
@pytest.mark.parametrize("function_header", [
    "  function func(b) result(x)\n    real :: x\n",
    "  real function func(b) result(x)\n"])
def test_apply_function_declare_name(
        fortran_reader, fortran_writer, tmpdir, function_header):
    '''Check that the apply() method works correctly for a simple call to
    a function where the name of the return name differs from the
    function name.

    '''
    code = (
        f"module test_mod\n"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"  real :: a,b\n"
        f"  a = func(b)\n"
        f"  end subroutine run_it\n"
        f"{function_header}"
        f"    real :: b\n"
        f"    x = 2.0\n"
        f"  end function\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)

    expected = (
        "  subroutine run_it()\n"
        "    real :: a\n"
        "    real :: b\n"
        "    real :: inlined_x\n\n"
        "    inlined_x = 2.0\n"
        "    a = inlined_x")
    assert expected in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_function_expression(fortran_reader, fortran_writer, tmpdir):
    '''Check that the apply() method works correctly for a call to a
    function that is within an expression.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  real :: a,b\n"
        "  a = (a*func(b)+2.0)/a\n"
        "  end subroutine run_it\n"
        "  real function func(b) result(x)\n"
        "    real :: b\n"
        "    b = b + 3.0\n"
        "    x = b * 2.0\n"
        "  end function\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert (
        "    real :: b\n"
        "    real :: inlined_x\n\n"
        "    b = b + 3.0\n"
        "    inlined_x = b * 2.0\n"
        "    a = (a * inlined_x + 2.0) / a\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_multi_function(fortran_reader, fortran_writer, tmpdir):
    '''Check that the apply() method works correctly when a function is
    called twice but only one of these function calls is inlined.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  real :: a,b,c\n"
        "  a = func(b)\n"
        "  c = func(a)\n"
        "  end subroutine run_it\n"
        "  real function func(b)\n"
        "    real :: b\n"
        "    func = 2.0\n"
        "  end function\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    expected = (
        "  subroutine run_it()\n"
        "    real :: a\n"
        "    real :: b\n"
        "    real :: c\n"
        "    real :: inlined_func\n\n"
        "    inlined_func = 2.0\n"
        "    a = inlined_func\n"
        "    c = func(a)")
    assert expected in output
    assert Compile(tmpdir).string_compiles(output)

    # inline again
    routine = psyir.walk(Call)[0]
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    expected = (
        "    real :: inlined_func\n"
        "    real :: inlined_func_1\n\n"
        "    inlined_func = 2.0\n"
        "    a = inlined_func\n"
        "    inlined_func_1 = 2.0\n"
        "    c = inlined_func_1")
    assert expected in output


@pytest.mark.parametrize("start, end, indent", [
    ("", "", ""),
    ("module test_mod\ncontains\n", "end module test_mod\n", "  "),
    ("module test_mod\nuse dummy\ncontains\n", "end module test_mod\n", "  ")])
def test_apply_raw_subroutine(
        fortran_reader, fortran_writer, tmpdir, start, end, indent):
    '''Test the apply method works correctly when the routine to be
    inlined is a raw subroutine and is called directly from another
    raw subroutine, a subroutine within a module but without a use
    statement and a subroutine within a module with a wildcard use
    statement.

    '''
    code = (
        f"{start}"
        f"  subroutine run_it()\n"
        f"    real :: a\n"
        f"    call sub(a)\n"
        f"  end subroutine run_it\n"
        f"{end}"
        f"subroutine sub(x)\n"
        f"  real, intent(inout) :: x\n"
        f"  x = 2.0*x\n"
        f"end subroutine sub\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    expected = (
        f"{indent}subroutine run_it()\n"
        f"{indent}  real :: a\n\n"
        f"{indent}  a = 2.0 * a\n\n"
        f"{indent}end subroutine run_it\n")
    assert expected in output
    if "use dummy" not in output:
        # Compilation will not work with "use dummy" as there is no
        # mod file.
        assert Compile(tmpdir).string_compiles(output)


@pytest.mark.parametrize("use1, use2", [
    ("use inline_mod, only : sub\n", ""), ("use inline_mod\n", ""),
    ("", "use inline_mod, only : sub\n"), ("", "use inline_mod\n")])
def test_apply_container_subroutine(
        fortran_reader, fortran_writer, tmpdir, use1, use2):
    '''Test the apply method works correctly when the routine to be
    inlined is in a different container and is within a module (so
    a use statement is required).

    '''
    code = (
        f"module inline_mod\n"
        f"contains\n"
        f"  subroutine sub(x)\n"
        f"    real, intent(inout) :: x\n"
        f"    x = 2.0*x\n"
        f"  end subroutine sub\n"
        f"end module inline_mod\n"
        f"module test_mod\n"
        f"{use1}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    {use2}"
        f"    real :: a\n"
        f"    call sub(a)\n"
        f"  end subroutine run_it\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert (
        "    real :: a\n\n"
        "    a = 2.0 * a\n\n"
        "  end subroutine run_it" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_validate():
    '''Test the apply method calls the validate method.'''
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as info:
        inline_trans.apply(None)
    assert ("The target of the InlineTrans transformation should be "
            "a Call but found 'NoneType'." in str(info.value))


# validate

def test_validate_node():
    ''' Test the expected exception is raised if an invalid node is
    supplied to the transformation. '''
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as info:
        inline_trans.validate(None)
    assert ("The target of the InlineTrans transformation should be "
            "a Call but found 'NoneType'." in str(info.value))


def test_validate_calls_find_routine(fortran_reader):
    '''Test that validate() calls the _find_routine method. Use an example
    where an exception is raised as the source of the routine to be
    inlined cannot be found.

    '''
    code = (
        "module test_mod\n"
        "  use some_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Failed to find the source code of the unresolved routine 'sub' "
            "after trying wildcard imports from ['some_mod'] and all "
            "routines that are not in containers." in str(err.value))


def test_validate_return_stmt(fortran_reader):
    '''Test that validate() raises the expected error if the target routine
    contains one or more Returns which that aren't either the very first
    statement or very last statement.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = 3\n"
        "    return\n"
        "    idx = idx + 3\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' contains one or more Return statements and "
            "therefore cannot be inlined" in str(err.value))


def test_validate_codeblock(fortran_reader):
    '''Test that validate() raises the expected error for a routine that
    contains a CodeBlock.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "  integer :: i\n"
        "  i = 10\n"
        "  call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    write(*,*) idx\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' contains one or more CodeBlocks and therefore "
            "cannot be inlined" in str(err.value))


def test_validate_import_clash(fortran_reader):
    '''Test that validate() raises the expected error when two symbols of the
    same name are imported from different containers at the call site and
    within the routine.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use some_mod, only: trouble\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use other_mod, only: trouble\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' imports 'trouble' from Container 'other_mod' but "
            "the call site has an import of a symbol with the same name from "
            "Container 'some_mod'" in str(err.value))


def test_validate_non_local_symbol(fortran_reader):
    '''Test that validate() raises the expected error when the routine to be
    inlined accesses a symbol from its parent container.'''
    code = (
        "module test_mod\n"
        "  integer :: trouble\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'trouble' from its parent container" in str(err.value))


def test_validate_unresolved_import(fortran_reader):
    '''Test that validate rejects a routine that accesses a symbol which
    is unresolved.'''
    code = (
        "module test_mod\n"
        "  use some_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + trouble\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' cannot be inlined because it accesses an "
            "un-resolved variable 'trouble'" in str(err.value))


def test_validate_array_subsection(fortran_reader):
    '''Test that the validate method rejects an attempt to inline a routine
    if any of its arguments are array subsections.'''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"subroutine main\n"
        f"  real A(100, 100)\n"
        f"  type(my_type) :: var\n"
        f"  call S(A(26:,2:))\n"
        f"  call s(var%data(3:10,3:10))\n"
        f"  call s( A(var%data(3:10), var%data(2:9)) )\n"
        f"end subroutine\n"
        f"subroutine s(x)\n"
        f"  real :: x(:, :)\n"
        f"  x(:,:) = 0.0\n"
        f"end subroutine\n"
        f"end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    calls = psyir.walk(Call)
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(calls[0])
    assert ("Cannot inline routine 's' because argument 'a(26:,2:)' is an "
            "array subsection (TODO #924)" in str(err.value))
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(calls[1])
    assert ("Cannot inline routine 's' because argument 'var%data(3:10,3:10)' "
            "is an array subsection (TODO #924)" in str(err.value))
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(calls[2])
    assert ("Cannot inline routine 's' because argument 'a(var%data(3:10),"
            "var%data(2:9))' has an array range in an indirect access (TODO "
            "#924)" in str(err.value))


def test_validate_assumed_shape(fortran_reader):
    '''Test that the validate method rejects an attempt to inline a routine
    if any of its dummy arguments are declared to be a different shape from
    those at the call site.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  REAL A(100, 100)\n"
        "  CALL S(A(:,:),10)\n"
        "end subroutine\n"
        "subroutine s(x,m)\n"
        "  integer, intent(in) :: m\n"
        "  real :: x(m)\n"
        "  integer :: i\n"
        "  do i = 1, m\n"
        "     x(i) = x(i) + m\n"
        "  enddo\n"
        "end subroutine\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(call)
    assert ("Cannot inline routine 's' because it reshapes an argument: actual"
            " argument 'a(:,:)' has rank 2 but the corresponding dummy "
            "argument, 'x', has rank 1" in str(err.value))


def test_validate_named_arg(fortran_reader):
    '''Test that the validate method rejects an attempt to inline a routine
    that has a named argument.'''
    # In reality, the routine with a named argument would almost certainly
    # use the 'present' intrinsic but, since that gives a CodeBlock that itself
    # prevents inlining, out test example omits it.
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real :: var = 0.0\n"
        "  call sub(var, opt=1.0)\n"
        "end subroutine main\n"
        "subroutine sub(x, opt)\n"
        "  real, intent(inout) :: x\n"
        "  real, optional :: opt\n"
        "  !if( present(opt) )then\n"
        "  !  x = x + opt\n"
        "  !end if\n"
        "  x = x + 1.0\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(call)
    assert ("Routine 'sub' cannot be inlined because it has a named argument "
            "'opt' (TODO #924)" in str(err.value))


CALL_IN_SUB_USE = (
    "subroutine run_it()\n"
    "  use inline_mod, only : sub\n"
    "  real :: a\n"
    "  call sub(a)\n"
    "end subroutine run_it\n")
CALL_IN_SUB = CALL_IN_SUB_USE.replace(
    "  use inline_mod, only : sub\n", "")
SUB = (
    "subroutine sub(x)\n"
    "  real :: x\n"
    "  x = 1.0\n"
    "end subroutine sub\n")
SUB_IN_MODULE = (
    f"module inline_mod\n"
    f"contains\n"
    f"{SUB}"
    f"end module inline_mod\n")


# _find_routine

def test_find_routine_local(fortran_reader):
    '''Test that the PSyIR of the Routine is returned when it is local to
    the associated call.

    '''
    code = (
        f"module test_mod\n"
        f"contains\n"
        f"{CALL_IN_SUB}"
        f"{SUB}"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine(call)
    assert call.routine.is_local
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_missing_exception(fortran_reader):
    '''Test that the expected exception is raised if the Call's Routine
    symbol has a local interface but the Routine can't be found in the
    PSyIR.

    '''
    code = (
        f"module test_mod\n"
        f"contains\n"
        f"{CALL_IN_SUB}"
        f"{SUB}"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    # remove the subroutine 'sub' from the PSyIR tree.
    psyir.children[0].children[1].detach()
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_local
    with pytest.raises(InternalError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Failed to find the source code of the local routine 'sub'."
            in str(info.value))


def test_find_routine_unresolved_wildcard(fortran_reader):
    '''Test that the routine can be found via a wildcard use statement.'''

    wildcard_use = CALL_IN_SUB_USE.replace(", only : sub", "")
    code = f"{wildcard_use}{SUB_IN_MODULE}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_unresolved
    result = inline_trans._find_routine(call)
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_unresolved(fortran_reader):
    '''Test that the routine can be found when there are no use statements
    and there is a 'raw' subroutine.

    '''
    code = f"{CALL_IN_SUB}{SUB}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_unresolved
    result = inline_trans._find_routine(call)
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_raw_to_module_exception(fortran_reader):
    '''Test that the routine is not found and an exception is raised if
    there is no use statement and the code for another routine with
    the same name is specified within a module.

    '''
    code = f"{CALL_IN_SUB}{SUB_IN_MODULE}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_unresolved
    with pytest.raises(TransformationError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Failed to find the source code of the unresolved routine 'sub' "
            "after trying wildcard imports from [] and all routines that are "
            "not in containers." in str(info.value))


def test_find_routine_unresolved_exception(fortran_reader):
    '''Test that the expected exception is raised if the routine is
    unresolved and can't be found in any wildcard imports or any 'raw'
    subroutines.

    '''
    wildcard_use = CALL_IN_SUB_USE.replace(", only : sub", "")
    code = (
        f"{wildcard_use}"
        f"module inline_mod\n"
        f"end module inline_mod\n"
        f"subroutine sub2()\n"
        f"end subroutine sub2\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_unresolved
    with pytest.raises(TransformationError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Failed to find the source code of the unresolved routine 'sub' "
            "after trying wildcard imports from ['inline_mod'] and all "
            "routines that are not in containers." in str(info.value))


def test_find_routine_import(fortran_reader):
    '''Test that the routine can be found when there is a use statement
    and the subroutine can be found in the associated module.

    '''
    code = f"{CALL_IN_SUB_USE}{SUB_IN_MODULE}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_import
    result = inline_trans._find_routine(call)
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_import_exception(fortran_reader):
    '''Test that the routine raises the expected exception if the imported
    module can't be found.

    '''
    code = f"{CALL_IN_SUB_USE}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_import
    with pytest.raises(TransformationError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Failed to find the source for routine 'sub' imported from "
            "'inline_mod' and therefore cannot inline it." in str(info.value))


def test_find_routine_module_to_raw_exception(fortran_reader):
    '''Test that the routine raises the expected exception if the call
    imports the routine and the routine can't be found, even in the
    presence of a 'raw' subroutine with the same name.

    '''
    code = f"{CALL_IN_SUB_USE}{SUB}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    assert call.routine.is_import
    with pytest.raises(TransformationError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Failed to find the source for routine 'sub' imported from "
            "'inline_mod' and therefore cannot inline it." in str(info.value))


def test_find_routine_exception(fortran_reader, monkeypatch):
    '''Test that the routine raises the expected exception if the call's
    routine symbol is not local, unresolved or import. Need to
    monkeypatch as this exception is not something that should happen.

    '''
    code = f"{CALL_IN_SUB}"
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    # Set the interface to None so it is not local, unresolved or import.
    monkeypatch.setattr(call.routine, "_interface", None)
    inline_trans = InlineTrans()
    with pytest.raises(InternalError) as info:
        _ = inline_trans._find_routine(call)
    assert ("Routine Symbol 'sub' is not local, unresolved or imported."
            in str(info.value))


# _find_routine_in_container

def test_find_routine_in_container_no_container(fortran_reader):
    '''Test that None is returned when the Container associated with the
    supplied container symbol is not found in the PSyIR.

    '''
    psyir = fortran_reader.psyir_from_source(CALL_IN_SUB_USE)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert result is None


def test_find_routine_in_container_no_file_container(fortran_reader):
    '''Test that None is returned when the Container associated with the
    supplied container symbol is not found in the PSyIR and the root
    is not a FileContainer.

    '''
    psyir = fortran_reader.psyir_from_source(CALL_IN_SUB_USE)
    # Remove the FileContainer from the PSyIR tree
    psyir = psyir.children[0]
    psyir.detach()
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert result is None


def test_find_routine_in_container_routine_not_found(fortran_reader):
    '''Test that None is returned when the required Routine is not found
    in the Container associated with the supplied container symbol, as
    it does not exist.

    '''
    code = (
        f"module inline_mod\n"
        f"end module inline_mod\n"
        f"{CALL_IN_SUB_USE}")
    psyir = fortran_reader.psyir_from_source(code)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert result is None


def test_find_routine_in_container_recurse_named(fortran_reader):
    '''Test that when a container does not contain the required routine,
    any imported containers within this container are also
    searched. In this case the test is for a container within the
    original container that explicitly names the routine. The PSyIR of
    the routine is returned when it is found in the second container.

    '''
    code = (
        f"{CALL_IN_SUB_USE}"
        f"module inline_mod\n"
        f"use inline_mod2, only : sub\n"
        f"end module inline_mod\n"
        f"module inline_mod2\n"
        f"contains\n"
        f"{SUB}\n"
        f"end module inline_mod2\n")
    psyir = fortran_reader.psyir_from_source(code)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_in_container_recurse_wildcard(fortran_reader):
    '''Test that when a container does not contain the required routine,
    any imported containers within this container are also
    searched. In this case the test is for a wildcard container within
    the original container. The PSyIR of the routine is returned when
    it is found in the second container.

    '''
    code = (
        f"{CALL_IN_SUB_USE}"
        f"module inline_mod\n"
        f"use inline_mod2\n"
        f"end module inline_mod\n"
        f"module inline_mod2\n"
        f"contains\n"
        f"{SUB}\n"
        f"end module inline_mod2\n")
    psyir = fortran_reader.psyir_from_source(code)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_in_container_private_routine_not_found(fortran_reader):
    '''Test that None is returned when the required Routine is not found
    in the Container associated with the supplied container symbol, as
    it is private. This situation should not arise as it is invalid to
    try to import a private routine. However, there are currrently no
    checks for this when creating PSyIR.

    '''
    private_sub_in_module = SUB_IN_MODULE.replace(
        "contains\n", "  private :: sub\ncontains\n")
    code = f"{private_sub_in_module}{CALL_IN_SUB_USE}"
    psyir = fortran_reader.psyir_from_source(code)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert result is None


def test_find_routine_in_container(fortran_reader):
    '''Test that the PSyIR of the Routine is returned when it is found
    in the Container associated with the supplied container symbol.

    '''
    code = f"{SUB_IN_MODULE}{CALL_IN_SUB_USE}"
    psyir = fortran_reader.psyir_from_source(code)
    call_node = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    result = inline_trans._find_routine_in_container(
            call_node, call_node.routine.interface.container_symbol)
    assert isinstance(result, Routine)
    assert result.name == "sub"
