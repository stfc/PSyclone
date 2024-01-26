# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified: R. W. Ford and S. Siso, STFC Daresbury Lab

'''This module tests the inlining transformation.
'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Call, IntrinsicCall, Reference, Routine, Loop
from psyclone.psyir.symbols import (
    DataSymbol, UnresolvedType, AutomaticInterface)
from psyclone.psyir.transformations import (
    InlineTrans, TransformationError)
from psyclone.tests.utilities import Compile

MY_TYPE = ("  integer, parameter :: ngrids = 10\n"
           "  type other_type\n"
           "    real, dimension(10) :: data\n"
           "    integer :: nx\n"
           "  end type other_type\n"
           "  type my_type\n"
           "    integer :: idx\n"
           "    real, dimension(10) :: data\n"
           "    real, dimension(5,10) :: data2d\n"
           "    type(other_type) :: local\n"
           "  end type my_type\n"
           "  type big_type\n"
           "    type(my_type) :: region\n"
           "  end type big_type\n"
           "  type vbig_type\n"
           "    type(big_type), dimension(ngrids) :: grids\n"
           "  end type vbig_type\n")


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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    real :: a(10)\n"
        "    do i=1,10\n"
        "      a(i) = 1.0\n"
        "      call sub(a(i))\n"
        "    end do\n"
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


def test_apply_gocean_kern(fortran_reader, fortran_writer, monkeypatch):
    '''
    Test the apply method with a typical GOcean kernel.

    TODO #924 - currently this xfails because we don't resolve the type of
    the actual argument.

    '''
    code = (
        "module psy_single_invoke_test\n"
        "  use field_mod, only: r2d_field\n"
        "  use kind_params_mod\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine invoke_0_compute_cu(cu_fld, pf, u_fld)\n"
        "    type(r2d_field), intent(inout) :: cu_fld, pf, u_fld\n"
        "    integer j, i\n"
        "    do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
        "      do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
        "        call compute_cu_code(i, j, cu_fld%data, pf%data, "
        "u_fld%data)\n"
        "      end do\n"
        "    end do\n"
        "  end subroutine invoke_0_compute_cu\n"
        "  subroutine compute_cu_code(i, j, cu, p, u)\n"
        "    implicit none\n"
        "    integer,  intent(in) :: i, j\n"
        "    real(go_wp), intent(out), dimension(:,:) :: cu\n"
        "    real(go_wp), intent(in),  dimension(:,:) :: p, u\n"
        "    cu(i,j) = 0.5d0*(p(i,j)+p(i-1,j))*u(i,j)\n"
        "  end subroutine compute_cu_code\n"
        "end module psy_single_invoke_test\n"
    )
    # Set up include_path to import the proper module
    src_dir = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "../../../external/dl_esm_inf/finite_difference/src")
    monkeypatch.setattr(Config.get(), '_include_paths', [str(src_dir)])
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(psyir.walk(Call)[0])
    if ("actual argument 'cu_fld' corresponding to an array formal "
            "argument ('cu') is unknown" in str(err.value)):
        pytest.xfail(
            "TODO #924 - extend validation to attempt to resolve type of "
            "actual argument.")
    output = fortran_writer(psyir)
    assert ("    do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1\n"
            "      do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1\n"
            "        cu_fld%data(i,j) = 0.5d0 * (pf%data(i,j) + "
            "pf%data(i - 1,j)) * u_fld%data(i,j)\n"
            "      enddo\n"
            "    enddo\n" in output)


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
        f"    integer :: i\n"
        f"    type(my_type) :: var\n"
        f"    type(my_type) :: var_list(10)\n"
        f"    type(big_type) :: var2(5)\n"
        f"    do i=1,5\n"
        f"      call sub(var, i)\n"
        f"      call sub(var_list(i), i)\n"
        f"      call sub(var2(i)%region, i)\n"
        f"      call sub2(var2)\n"
        f"    end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x, ivar)\n"
        f"    type(my_type), intent(inout) :: x\n"
        f"    integer, intent(in) :: ivar\n"
        f"    integer :: i\n"
        f"    do i = 1, 10\n"
        f"      x%data(i) = 2.0*ivar\n"
        f"    end do\n"
        f"    x%data(:) = -1.0\n"
        f"    x%data = -5.0\n"
        f"    x%data(1:2) = 0.0\n"
        f"  end subroutine sub\n"
        f"  subroutine sub2(x)\n"
        f"    type(big_type), dimension(:), intent(inout) :: x\n"
        f"    x(:)%region%local%nx = 0\n"
        f"  end subroutine sub2\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(routine)

    output = fortran_writer(psyir)
    assert ("    do i = 1, 5, 1\n"
            "      do i_1 = 1, 10, 1\n"
            "        var%data(i_1) = 2.0 * i\n"
            "      enddo\n"
            "      var%data(:) = -1.0\n"
            "      var%data = -5.0\n"
            "      var%data(1:2) = 0.0\n"
            "      do i_2 = 1, 10, 1\n"
            "        var_list(i)%data(i_2) = 2.0 * i\n"
            "      enddo\n"
            "      var_list(i)%data(:) = -1.0\n"
            "      var_list(i)%data = -5.0\n"
            "      var_list(i)%data(1:2) = 0.0\n"
            "      do i_3 = 1, 10, 1\n"
            "        var2(i)%region%data(i_3) = 2.0 * i\n"
            "      enddo\n"
            "      var2(i)%region%data(:) = -1.0\n"
            "      var2(i)%region%data = -5.0\n"
            "      var2(i)%region%data(1:2) = 0.0\n"
            "      var2(1:5)%region%local%nx = 0\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_unresolved_struct_arg(fortran_reader, fortran_writer):
    '''
    Check that we handle acceptable cases of the type of an argument being
    unresolved but that we reject the case where we can't be sure of
    the array indexing.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use some_mod, only: mystery_type, mystery\n"
        "    integer :: i\n"
        "    type(mystery_type) :: var3, varr(5)\n"
        # Unresolved structure type but array dims are known.
        "    call sub3(varr)\n"
        # Unresolved actual argument corresponding to a formal array argument
        # so we can't be sure that it isn't being reshaped.
        "    call sub3(mystery)\n"
        # Unresolved actual argument corresponding to a formal scalar argument
        # so lack of type information isn't a problem.
        "    call sub3a(mystery)\n"
        # Formal arg specifies array bounds and we don't have them for
        # the actual argument.
        "    call sub4(mystery)\n"
        "  end subroutine run_it\n"
        "  subroutine sub3(x)\n"
        "    use some_mod, only: mystery_type\n"
        "    type(mystery_type), dimension(:), intent(inout) :: x\n"
        "    x(:)%region%local%nx = 0\n"
        "  end subroutine sub3\n"
        "  subroutine sub3a(x)\n"
        "    use some_mod, only: mystery_type\n"
        "    type(mystery_type) :: x\n"
        "    x%flag = 1\n"
        "  end subroutine sub3a\n"
        "  subroutine sub4(x)\n"
        "    use some_mod, only: mystery_type\n"
        "    type(mystery_type), dimension(3:5), intent(inout) :: x\n"
        "    x(:)%region%local%nx = 0\n"
        "  end subroutine sub4\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    calls = psyir.walk(Call)
    # First one should be fine.
    inline_trans.apply(calls[0])
    # Second one should fail.
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(calls[1])
    assert ("Routine 'sub3' cannot be inlined because the type of the actual "
            "argument 'mystery' corresponding to an array formal argument "
            "('x') is unknown" in str(err.value))
    # Third one should be fine because it is a scalar argument.
    inline_trans.apply(calls[2])
    # We can't do the fourth one.
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(calls[3])
    assert ("Routine 'sub4' cannot be inlined because the type of the actual "
            "argument 'mystery' corresponding to an array formal argument "
            "('x') is unknown." in str(err.value))
    output = fortran_writer(psyir)
    assert ("    varr(1:5)%region%local%nx = 0\n"
            "    call sub3(mystery)\n"
            "    mystery%flag = 1\n"
            "    call sub4(mystery)\n" in output)


def test_apply_struct_slice_arg(fortran_reader, fortran_writer, tmpdir):
    '''
    Check that the apply() method works correctly when there are slices in
    structure accesses in both the actual and formal arguments.

    '''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    integer :: i\n"
        f"    type(my_type) :: var_list(10)\n"
        f"    type(vbig_type), dimension(5) :: cvar\n"
        f"    call sub(var_list(:)%local%nx, i)\n"
        f"    call sub2(var_list(:), 1, 1)\n"
        f"    call sub2(var_list(:), i, i+2)\n"
        f"    call sub3(cvar)\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(ix, indx)\n"
        f"    integer, dimension(:) :: ix\n"
        f"    integer, intent(in) :: indx\n"
        f"    ix(:) = ix(:) + 1\n"
        f"  end subroutine sub\n"
        f"  subroutine sub2(x, start, stop)\n"
        f"    type(my_type), dimension(:) :: x\n"
        f"    integer :: start, stop\n"
        f"    x(:)%data(2) = 0.0\n"
        f"    x(:)%local%nx = 4\n"
        f"    x(start:stop+1)%local%nx = -2\n"
        f"  end subroutine sub2\n"
        f"  subroutine sub3(y)\n"
        f"    type(vbig_type), dimension(:) :: y\n"
        f"    y(2)%grids(2)%region%data(:) = 0.0\n"
        f"  end subroutine sub3\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert "var_list(:)%local%nx = var_list(:)%local%nx + 1" in output
    assert "var_list(:)%data(2) = 0.0" in output
    assert "var_list(:)%local%nx = 4" in output
    assert "var_list(1:1 + 1)%local%nx = -2" in output
    assert "cvar(2)%grids(2)%region%data(:) = 0.0" in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_local_limits_caller(fortran_reader, fortran_writer,
                                          tmpdir):
    '''
    Test the apply() method when there are array bounds specified in the
    caller.

    '''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    integer :: i\n"
        f"    type(my_type) :: var_list(10)\n"
        f"    call sub2(var_list(3:7), 5, 6)\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub2(x, start, stop)\n"
        f"    type(my_type), dimension(:) :: x\n"
        f"    integer :: start, stop\n"
        f"    x(:)%data(2) = 1.0\n"
        f"    x(:)%local%nx = 3\n"
        f"    x(start:stop+1)%local%nx = -2\n"
        f"  end subroutine sub2\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(routine)
    output = fortran_writer(psyir)
    assert "var_list(3:7)%data(2) = 1.0" in output
    assert "var_list(3:7)%local%nx = 3" in output
    # Element 1 in routine corresponds to element 3 in caller
    assert "var_list(5 - 1 + 3:6 + 1 - 1 + 3)%local%nx = -2" in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_local_limits_caller_decln(fortran_reader, fortran_writer,
                                                tmpdir):
    '''
    Test the apply() method when there are non-default array bounds specified
    in the declaration at the call site.

    '''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    integer :: i\n"
        f"    type(my_type), dimension(2:9) :: varat2\n"
        f"    real, dimension(4:8) :: varat3\n"
        f"    call sub2(varat2(:), 5, 6)\n"
        f"    call sub2(varat2(3:8), 5, 6)\n"
        f"    call sub3(varat3(5:6))\n"
        f"    call sub3(varat3)\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub2(x, start, stop)\n"
        f"    type(my_type), dimension(:) :: x\n"
        f"    integer :: start, stop\n"
        f"    x(:)%data(2) = 1.0\n"
        f"    x(:)%local%nx = 3\n"
        f"    x(start:stop+1)%local%nx = -2\n"
        f"  end subroutine sub2\n"
        f"  subroutine sub3(x)\n"
        f"    real, dimension(:) :: x\n"
        f"    x(1:2) = 4.0\n"
        f"  end subroutine sub3\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(routine)
    output = fortran_writer(psyir)
    # Actual declared range is non-default.
    assert "varat2(:)%data(2) = 1.0\n" in output
    assert "varat2(:)%local%nx = 3\n" in output
    # A local access of '1' corresponds to the start of the array which is
    # index '2' at the call site.
    assert "varat2(5 - 1 + 2:6 + 1 - 1 + 2)%local%nx = -2\n" in output
    # Actual arg. has non-default range - index 1 in the routine corresponds
    # to index 3 at the call site.
    assert "varat2(3:8)%data(2) = 1.0\n" in output
    assert "varat2(3:8)%local%nx = 3\n" in output
    assert "varat2(5 - 1 + 3:6 + 1 - 1 + 3)%local%nx = -2" in output
    assert "varat3(1 - 1 + 5:2 - 1 + 5) = 4.0\n" in output
    assert "varat3(:2 - 1 + 4) = 4.0\n" in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_local_limits_routine(fortran_reader, fortran_writer,
                                           tmpdir):
    '''
    Test the apply() method when there are non-default array bounds specified
    in the declaration within the called routine.

    '''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    real zarg(13)\n"
        f"    real ardvarkarg(4:10)\n"
        f"    type(my_type) :: var_list(10)\n"
        f"    type(my_type), dimension(2:8) :: varat2\n"
        f"    call sub3(var_list(:), 5, 6, zarg)\n"
        f"    call sub3(varat2(:), 5, 6, ardvarkarg)\n"
        f"    call sub3(varat2(3:7), 4, 5, zarg(2:))\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub3(y, start, stop, z)\n"
        f"    type(my_type), dimension(4:6) :: y\n"
        # TODO #2125 - if 'start' is used for the lower bound instead of a
        # literal then the inlined code is incorrect.
        f"    real, dimension(3:) :: z\n"
        f"    integer :: start, stop\n"
        f"    y(:)%data(2) = 2.0\n"
        f"    y(4:5)%local%nx = 4\n"
        f"    y(start:stop+1)%local%nx = -3\n"
        f"    z(start+1) = 8.0\n"
        f"  end subroutine sub3\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(routine)
    output = fortran_writer(psyir)
    # Access within routine is to full range but formal arg. is declared with
    # explicit bounds so these have to be taken into account.
    assert "var_list(4 - 4 + 1:6 - 4 + 1)%data(2) = 2.0" in output
    assert "var_list(4 - 4 + 1:5 - 4 + 1)%local%nx = 4" in output
    # Element 3 in routine corresponds to element 1 in caller
    assert "zarg(5 + 1 - 3 + 1) = 8.0" in output
    # Element 4 in routine corresponds to element 1 in caller
    assert "var_list(5 - 4 + 1:6 + 1 - 4 + 1)%local%nx = -3" in output
    # Custom limits  in declarations for both formal and actual.
    # Element 4 in routine corresponds to element 2 in caller.
    assert "varat2(4 - 4 + 2:6 - 4 + 2)%data(2) = 2.0\n" in output
    assert "varat2(4 - 4 + 2:5 - 4 + 2)%local%nx = 4\n" in output
    # Element 3 in routine corresponds to element 4 in caller.
    assert "ardvarkarg(5 + 1 - 3 + 4) = 8.0" in output
    # A local access of '1' corresponds to the start of the array which is
    # index '2' at the call site.
    assert "varat2(5 - 4 + 2:6 + 1 - 4 + 2)%local%nx = -3\n" in output
    # Actual arg. has non-default range in slice. Therefore index 3 at the
    # call site becomes index 4 in the routine.
    assert "varat2(4 - 4 + 3:6 - 4 + 3)%data(2) = 2.0\n" in output
    assert "varat2(4 - 4 + 3:5 - 4 + 3)%local%nx = 4\n" in output
    assert "varat2(4 - 4 + 3:5 + 1 - 4 + 3)%local%nx = -3" in output
    assert Compile(tmpdir).string_compiles(output)


def test_apply_allocatable_array_arg(fortran_reader, fortran_writer):
    '''
    Check that apply() works correctly when a formal argument is given the
    ALLOCATABLE attribute (meaning that the bounds of the formal argument
    are those of the actual argument).

    '''
    code = (
        "module test_mod\n"
        "  type my_type\n"
        # TODO #2053 - if the 'data' attribute is correctly given the
        # 'allocatable' attribute then the whole type ends up as an
        # UnsupportedFortranType. For now we therefore omit the 'allocatable'
        # attribute. This means that the Fortran is not strictly correct
        # and we can't compile the code.
        # "    real, allocatable, dimension(:,:) :: data\n"
        "    real, dimension(:,:) :: data\n"
        "  end type my_type\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    type(my_type) :: grid\n"
        "    integer :: jim1, jjp1, jim2, jjp2\n"
        "    real, allocatable, dimension(:,:) :: avar\n"
        "    allocate(grid%data(2:6,-1:8))\n"
        # TODO #1858 - ideally 'grid%data' would work below (instead of
        # 'grid%data(:,:)') but Reference2ArrayRangeTrans doesn't yet work for
        # members of structures.
        "    call sub1(grid%data(:,:), jim1, jjp1)\n"
        "    call sub1(grid%data(2:6,-1:8), jim2, jjp2)\n"
        "  end subroutine run_it\n"
        "  subroutine sub1(x, ji, jj)\n"
        "    integer, intent(in) :: ji, jj\n"
        "    real, dimension(:,:), allocatable :: x\n"
        "    x(2,-1) = 0.0\n"
        "    x(ji+2,jj+1) = -1.0\n"
        "  end subroutine sub1\n"
        "end module test_mod\n"
        )
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for routine in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        if not isinstance(routine, IntrinsicCall):
            inline_trans.apply(routine)
    output = fortran_writer(psyir)
    # Array index expressions should not be shifted when inlined as the
    # array bounds are the same.
    assert "grid%data(2,-1) = 0.0\n" in output
    assert "grid%data(jim1 + 2,jjp1 + 1) = -1.0\n" in output
    assert "grid%data(jim2 + 2,jjp2 + 1) = -1.0\n" in output
    # TODO #2053 - we can't compile this code because the *input* isn't
    # valid Fortran (see earlier).
    # assert Compile(tmpdir).string_compiles(output)


def test_apply_array_slice_arg(fortran_reader, fortran_writer, tmpdir):
    '''
    Check that the apply() method works correctly when an array slice is
    passed to a routine and then accessed within it.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    real :: a(5,10,10), b(10,10)\n"
        "    do i=1,10\n"
        "      call sub1(a(1,:,i))\n"
        "    end do\n"
        "    call sub1a(a(1,1,:))\n"
        "    call sub2(a(:,1,:))\n"
        "    call sub2(b)\n"
        "    call sub2a(b(:,1:5))\n"
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
        "    x(1:10) = 3.0 * x(1:10)\n"
        "  end subroutine sub1a\n"
        "  subroutine sub2(x)\n"
        "    real, intent(inout), dimension(5,10) :: x\n"
        "    integer :: i\n"
        "    x = 2.0 * x\n"
        "  end subroutine sub2\n"
        "  subroutine sub2a(x)\n"
        "    real, intent(inout), dimension(10,5) :: x\n"
        "    integer :: i\n"
        "    do i=1, 10\n"
        "      x(i,:) = 2.0 * x(i,:)\n"
        "    end do\n"
        "  end subroutine sub2a\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for call in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      do i_1 = 1, 10, 1\n"
            "        a(1,i_1,i) = 2.0 * i_1\n"
            "      enddo\n"
            "    enddo\n"
            "    a(1,1,:) = 3.0 * a(1,1,:)\n"
            "    a(:,1,:) = 2.0 * a(:,1,:)\n"
            "    b(:,:) = 2.0 * b(:,:)\n"
            "    do i_4 = 1, 10, 1\n"
            "      b(i_4,:5) = 2.0 * b(i_4,:5)\n" in output)
    assert Compile(tmpdir).string_compiles(output)


def test_apply_struct_array_arg(fortran_reader, fortran_writer, tmpdir):
    '''Check that apply works correctly when the actual argument is an
    array element within a structure.'''
    code = (
        f"module test_mod\n"
        f"{MY_TYPE}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    integer :: i, ig\n"
        f"    real :: a(10)\n"
        f"    type(my_type) :: grid\n"
        f"    type(my_type), dimension(5) :: grid_list\n"
        f"    grid%data(:) = 1.0\n"
        f"    do i=1,10\n"
        f"      a(i) = 1.0\n"
        f"      call sub(grid%data(i))\n"
        f"    end do\n"
        f"    do i=1,10\n"
        f"      ig = min(i, 5)\n"
        f"      call sub(grid_list(ig)%data(i))\n"
        f"    end do\n"
        f"    do i=1,10\n"
        f"      ig = min(i, 5)\n"
        f"      call sub(grid_list(ig)%local%data(i))\n"
        f"    end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x)\n"
        f"    real, intent(inout) :: x\n"
        f"    x = 2.0*x\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    inline_trans = InlineTrans()
    inline_trans.apply(loops[0].loop_body.children[1])
    inline_trans.apply(loops[1].loop_body.children[1])
    inline_trans.apply(loops[2].loop_body.children[1])
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
        f"    integer :: i\n"
        f"    real :: a(10)\n"
        f"    type(my_type) :: grid\n"
        f"    type(vbig_type) :: micah\n"
        f"    grid%data(:) = 1.0\n"
        f"    grid%data2d(:,:) = 1.0\n"
        f"    do i=1,10\n"
        f"      a(i) = 1.0\n"
        f"      call sub(grid%local%data)\n"
        f"      call sub(micah%grids(3)%region%data(:))\n"
        f"      call sub(grid%data2d(:,i))\n"
        f"      call sub(grid%data2d(1:5,i))\n"
        f"    end do\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x)\n"
        f"    real, dimension(:), intent(inout) :: x\n"
        f"    integer ji\n"
        f"    do ji = 1, 5\n"
        f"      x(ji) = 2.0*x(ji)\n"
        f"    end do\n"
        f"    x(1:2) = 0.0\n"
        f"    x(:) = 3.0\n"
        f"    x = 5.0\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for call in psyir.walk(Call):
        if not isinstance(call, IntrinsicCall):
            inline_trans.apply(call)
    output = fortran_writer(psyir)
    assert ("    do i = 1, 10, 1\n"
            "      a(i) = 1.0\n"
            "      do ji = 1, 5, 1\n"
            "        grid%local%data(ji) = 2.0 * grid%local%data(ji)\n"
            "      enddo\n"
            "      grid%local%data(1:2) = 0.0\n"
            "      grid%local%data(:) = 3.0\n"
            "      grid%local%data = 5.0\n"
            "      do ji_1 = 1, 5, 1\n"
            "        micah%grids(3)%region%data(ji_1) = 2.0 * "
            "micah%grids(3)%region%data(ji_1)\n"
            "      enddo\n"
            "      micah%grids(3)%region%data(1:2) = 0.0\n"
            "      micah%grids(3)%region%data(:) = 3.0\n"
            "      micah%grids(3)%region%data(:) = 5.0\n"
            "      do ji_2 = 1, 5, 1\n"
            "        grid%data2d(ji_2,i) = 2.0 * grid%data2d(ji_2,i)\n"
            "      enddo\n"
            "      grid%data2d(1:2,i) = 0.0\n"
            "      grid%data2d(:,i) = 3.0\n"
            "      grid%data2d(:,i) = 5.0\n"
            "      do ji_3 = 1, 5, 1\n"
            "        grid%data2d(ji_3,i) = 2.0 * grid%data2d(ji_3,i)\n"
            "      enddo\n"
            "      grid%data2d(1:2,i) = 0.0\n"
            "      grid%data2d(1:5,i) = 3.0\n"
            "      grid%data2d(1:5,i) = 5.0\n"
            "    enddo\n" in output)
    assert Compile(tmpdir).string_compiles(output)


@pytest.mark.parametrize("type_decln", [MY_TYPE, "  use some_mod\n"])
def test_apply_struct_array(fortran_reader, fortran_writer, tmpdir,
                            type_decln):
    '''Test that apply works correctly when the formal argument is an
    array of structures. We test both when the type of the structure is
    resolved and when it isn't. In the latter case we cannot perform
    inlining because we don't know the array bounds at the call site.

    '''
    code = (
        f"module test_mod\n"
        f"{type_decln}"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    integer :: i\n"
        f"    real :: a(10)\n"
        f"    type(my_type) :: grid\n"
        f"    type(vbig_type) :: micah\n"
        f"    call sub(micah%grids(:))\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(x)\n"
        f"    type(big_type), dimension(2:4) :: x\n"
        f"    integer ji\n"
        f"    ji = 2\n"
        f"    x(:)%region%idx = 3.0\n"
        f"    x(ji)%region%idx = 2.0\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    if "use some_mod" in type_decln:
        with pytest.raises(TransformationError) as err:
            inline_trans.apply(psyir.walk(Call)[0])
        assert ("Routine 'sub' cannot be inlined because the type of the "
                "actual argument 'micah' corresponding to an array formal "
                "argument ('x') is unknown." in str(err.value))
    else:
        inline_trans.apply(psyir.walk(Call)[0])
        output = fortran_writer(psyir)
        assert ("    ji = 2\n"
                "    micah%grids(2 - 2 + 1:4 - 2 + 1)%region%idx = 3.0\n"
                "    micah%grids(ji - 2 + 1)%region%idx = 2.0\n" in output)
        assert Compile(tmpdir).string_compiles(output)


def test_apply_repeated_module_use(fortran_reader, fortran_writer):
    '''
    Check that any module use statements are not duplicated when
    multiple calls are inlined.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    real :: a(10,10), b(10,10)\n"
        "    do i = 1, 10\n"
        "      call sub2(a(:,i))\n"
        "    end do\n"
        "    call sub1(b(:,2))\n"
        "  end subroutine run_it\n"
        "  subroutine sub1(x)\n"
        "    use model_mod, only: radius\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    x(:) = radius\n"
        "  end subroutine sub1\n"
        "  subroutine sub2(x)\n"
        "    use model_mod, only: radius\n"
        "    real, intent(inout), dimension(10) :: x\n"
        "    x(:) = 4*radius\n"
        "  end subroutine sub2\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    for call in psyir.walk(Routine)[0].walk(Call, stop_type=Call):
        inline_trans.apply(call)
    output = fortran_writer(psyir)
    # Check container symbol has not been renamed.
    assert "use model_mod_1" not in output
    assert ("  subroutine run_it()\n"
            "    use model_mod, only : radius\n"
            "    integer :: i\n" in output)
    assert ("    do i = 1, 10, 1\n"
            "      a(:,i) = 4 * radius\n"
            "    enddo\n"
            "    b(:,2) = radius\n" in output)


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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i, 2*i, 5_i_def)\n"
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
        "    use kinds_mod, only: i_def\n"
        "    integer :: i\n"
        "    i = 10_i_def\n"
        "    call sub(i)\n"
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
        "    use kinds_mod, only: i_def\n"
        "    integer :: i\n"
        "    i = 10_i_def\n"
        "    call sub(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    use kinds_mod\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5\n"
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
        "    use kinds_mod, only: r_def\n"
        "    integer :: i\n"
        "    i = 10.0_r_def\n"
        "    call sub(i)\n"
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
        "    use kinds_mod, only: r_def\n"
        "    integer :: i, a_clash\n"
        "    a_clash = 2\n"
        "    i = 10.0_r_def\n"
        "    call sub(i)\n"
        "    i = i * a_clash\n"
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
        "    use kinds_mod, only: r_def\n"
        "    integer :: i, a_mod\n"
        "    a_mod = 2\n"
        "    i = 10.0_r_def\n"
        "    call sub(i)\n"
        "    i = i * a_mod\n"
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


def test_validate_non_local_import(fortran_reader):
    '''Test that we reject the case where the routine to be
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
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'trouble' from its parent container." in str(err.value))


def test_apply_function(fortran_reader, fortran_writer, tmpdir):
    '''Check that the apply() method works correctly for a simple call to
    a function.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    real :: a,b\n"
        "    a = func(b)\n"
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
        f"    real :: a,b\n"
        f"    a = func(b)\n"
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
        "    real :: a,b\n"
        "    a = (a*func(b)+2.0)/a\n"
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
        "    real :: a,b,c\n"
        "    a = func(b)\n"
        "    c = func(a)\n"
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
    ("module test_mod\nuse formal\ncontains\n", "end module test_mod\n",
     "  ")])
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
    if "use formal" not in output:
        # Compilation will not work with "use formal" as there is no
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
    call = IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                                [Reference(DataSymbol("array",
                                                      UnresolvedType()))])
    with pytest.raises(TransformationError) as info:
        inline_trans.validate(call)
    assert "Cannot inline an IntrinsicCall ('ALLOCATE')" in str(info.value)


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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i)\n"
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


def test_validate_unsupportedtype_argument(fortran_reader):
    '''
    Test that validate rejects a subroutine with arguments of UnsupportedType.

    '''
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
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(routine)
    assert ("Routine 'sub' cannot be inlined because it contains a Symbol 'x' "
            "which is an Argument of UnsupportedType: 'REAL, POINTER, "
            "INTENT(INOUT) :: x'" in str(err.value))


def test_validate_unknowninterface(fortran_reader, fortran_writer, tmpdir):
    '''
    Test that validate rejects a subroutine containing variables with
    UnknownInterface.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  call sub()\n"
        "end subroutine main\n"
        "subroutine sub()\n"
        "  real, pointer :: x\n"
        "  x = x + 1.0\n"
        "end subroutine sub\n"
        "end module test_mod\n"
    )
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(routine)
    assert (" Routine 'sub' cannot be inlined because it contains a Symbol "
            "'x' with an UnknownInterface: 'REAL, POINTER :: x'"
            in str(err.value))

    # But if the interface is known, it has no problem inlining it
    xvar = psyir.walk(Routine)[1].symbol_table.lookup("x")
    xvar.interface = AutomaticInterface()
    inline_trans.apply(routine)
    assert fortran_writer(psyir.walk(Routine)[0]) == """\
subroutine main()
  REAL, POINTER :: x

  x = x + 1.0

end subroutine main
"""
    assert Compile(tmpdir).string_compiles(fortran_writer(psyir))


def test_validate_static_var(fortran_reader):
    '''
    Test that validate rejects a subroutine with StaticInterface variables.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    real :: a(10)\n"
        "    call sub(a(1))\n"
        "  end subroutine run_it\n"
        "  subroutine sub(x)\n"
        "    real, intent(inout) :: x\n"
        "    real, save :: state\n"
        "    state = state + x\n"
        "    x = 2.0*x + state\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(routine)
    assert ("Routine 'sub' cannot be inlined because it has a static (Fortran "
            "SAVE) interface for Symbol 'state'." in str(err.value))


@pytest.mark.parametrize("code_body", ["idx = idx + 5_i_def",
                                       "real, parameter :: pi = 3_wp\n"
                                       "idx = idx + 1\n"])
def test_validate_unresolved_precision_sym(fortran_reader, code_body):
    '''Test that a routine that uses an unresolved precision symbol is
    rejected. We test when the precision symbol appears in an executable
    statement and when it appears in a constant initialisation.'''
    code = (
        f"module test_mod\n"
        f"  use kinds_mod\n"
        f"contains\n"
        f"  subroutine run_it()\n"
        f"    use a_mod\n"
        f"    integer :: i\n"
        f"    i = 10_i_def\n"
        f"    call sub(i)\n"
        f"  end subroutine run_it\n"
        f"  subroutine sub(idx)\n"
        f"    integer, intent(inout) :: idx\n"
        f"    {code_body}\n"
        f"  end subroutine sub\n"
        f"end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    call = psyir.walk(Call)[0]
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    if "_wp" in code_body:
        var_name = "wp"
    else:
        var_name = "i_def"
    assert (f"Routine 'sub' cannot be inlined because it accesses variable "
            f"'{var_name}' and this cannot be found in any of the containers "
            f"directly imported into its symbol table" in str(err.value))


def test_validate_resolved_precision_sym(fortran_reader, monkeypatch,
                                         tmpdir):
    '''Test that a routine that uses a resolved precision symbol is
    rejected.'''
    code = (
        "module test_mod\n"
        "  use kinds_mod\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    use a_mod\n"
        "    integer :: i\n"
        "    a_var = a_clash\n"
        "    i = 10_i_def\n"
        "    call sub(i)\n"
        "    call sub2(i)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub\n"
        "  subroutine sub2(idx)\n"
        "    use kinds_mod\n"
        "    integer, intent(inout) :: idx\n"
        "    idx = idx + 5_i_def\n"
        "  end subroutine sub2\n"
        "end module test_mod\n")
    # Set up include_path to import the proper module
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "kinds_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module kinds_mod
          integer, parameter :: i_def = kind(1)
        end module kinds_mod
        ''')
    psyir = fortran_reader.psyir_from_source(code)
    inline_trans = InlineTrans()
    # First subroutine accesses i_def from parent Container.
    calls = psyir.walk(Call)
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(calls[0])
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'i_def' and this cannot be found in any of the containers "
            "directly imported into its symbol table." in str(err.value))
    # Second subroutine imports i_def directly into its own SymbolTable and
    # so is OK to inline.
    inline_trans.validate(calls[1])


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
    assert ("One or more symbols from routine 'sub' cannot be added to the "
            "table at the call site." in str(err.value))


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


def test_validate_wrong_number_args(fortran_reader):
    ''' Test that validate rejects inlining routines with different number
    of arguments.
    '''
    code = (
        "module test_mod\n"
        "  integer :: trouble\n"
        "contains\n"
        "  subroutine run_it()\n"
        "    integer :: i\n"
        "    i = 10\n"
        "    call sub(i, trouble)\n"
        "  end subroutine run_it\n"
        "  subroutine sub(idx)\n"
        "    integer :: idx\n"
        "    idx = idx + 1\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Cannot inline 'call sub(i, trouble)' because the number of "
            "arguments supplied to the call (2) does not match the number of "
            "arguments the routine is declared to have (1)" in str(err.value))


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
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'trouble' and this cannot be found in any of the containers "
            "directly imported into its symbol table." in str(err.value))


def test_validate_unresolved_array_dim(fortran_reader):
    '''
    Check that validate rejects a routine if it uses an unresolved Symbol
    when defining an array dimension.

    '''
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
        "    integer, dimension(some_size) :: var\n"
        "    idx = idx + 2\n"
        "  end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Routine 'sub' cannot be inlined because it accesses variable "
            "'some_size' and this cannot be found in any of the containers "
            "directly imported into its symbol table" in str(err.value))


def test_validate_array_reshape(fortran_reader):
    '''Test that the validate method rejects an attempt to inline a routine
    if any of its formal arguments are declared to be a different shape from
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
        inline_trans.validate(call)
    assert ("Cannot inline routine 's' because it reshapes an argument: actual"
            " argument 'a(:,:)' has rank 2 but the corresponding formal "
            "argument, 'x', has rank 1" in str(err.value))


def test_validate_array_arg_expression(fortran_reader):
    '''
    Check that validate rejects a call if an argument corresponding to
    a formal array argument is not a simple Reference or Literal.

    '''
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  use some_mod, only: a, b\n"
        "  CALL S(a+b,10)\n"
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
        inline_trans.validate(call)
    assert ("The call 'call s(a + b, 10)\n' cannot be inlined because actual "
            "argument 'a + b' corresponds to a formal argument with array "
            "type but is not a Reference or a Literal" in str(err.value))


def test_validate_indirect_range(fortran_reader):
    '''Test that validate rejects an attempt to inline a call to a routine
    with an argument constructed using an indirect slice.'''
    code = (
        "module test_mod\n"
        "  integer, dimension(10) :: indices\n"
        "contains\n"
        "subroutine main\n"
        "  real, dimension(10,10) :: var\n"
        "  call sub(var(indices(:)))\n"
        "end subroutine main\n"
        "subroutine sub(x)\n"
        "  real, dimension(:), intent(inout) :: x\n"
        "  x(:) = 0.0\n"
        "end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Cannot inline routine 'sub' because argument 'var(indices(:))' "
            "has an array range in an indirect access" in str(err.value))


def test_validate_non_unit_stride_slice(fortran_reader):
    '''Test that validate rejects an attempt to inline a call to a routine
    with an argument constructed using an array slice with non-unit stride.'''
    code = (
        "module test_mod\n"
        "contains\n"
        "subroutine main\n"
        "  real, dimension(10,10) :: var\n"
        "  call sub(var(1:10:2))\n"
        "end subroutine main\n"
        "subroutine sub(x)\n"
        "  real, dimension(:), intent(inout) :: x\n"
        "  x(:) = 0.0\n"
        "end subroutine sub\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    with pytest.raises(TransformationError) as err:
        inline_trans.validate(call)
    assert ("Cannot inline routine 'sub' because one of its arguments is an "
            "array slice with a non-unit stride: 'var(::2)' (TODO #1646)" in
            str(err.value))


def test_validate_named_arg(fortran_reader):
    '''Test that the validate method rejects an attempt to inline a routine
    that has a named argument.'''
    # In reality, the routine with a named argument would almost certainly
    # use the 'present' intrinsic but, since that gives a CodeBlock that itself
    # prevents inlining, our test example omits it.
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
        inline_trans.validate(call)
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
    the module associated with the call.

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
    assert call.routine.is_modulevar
    assert isinstance(result, Routine)
    assert result.name == "sub"


def test_find_routine_missing_exception(fortran_reader):
    '''Test that the expected exception is raised if the Call's Routine
    symbol has a module interface but the Routine can't be found in the
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
    assert call.routine.is_modulevar
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


def test_apply_merges_symbol_table_with_routine(fortran_reader):
    '''
    Check that the apply method merges the inlined function's symbol table to
    the containing Routine when the call node is inside a child ScopingNode.
    '''
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
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Call)[0]
    inline_trans = InlineTrans()
    inline_trans.apply(routine)
    # The i_1 symbol is the renamed i from the inlined call.
    assert psyir.walk(Routine)[0].symbol_table.get_symbols()['i_1'] is not None
