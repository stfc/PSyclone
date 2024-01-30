# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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
# Authors: S. Siso and A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for the PSyclone
GOMoveIterationBoundariesInsideKernelTrans transformation.
'''

import pytest
from psyclone.tests.utilities import get_invoke
from psyclone.domain.gocean.transformations import \
    GOMoveIterationBoundariesInsideKernelTrans
from psyclone.psyir.nodes import Assignment, Container, IfBlock, Return
from psyclone.psyir.symbols import ArgumentInterface
from psyclone.gocean1p0 import GOLoop
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.backend.fortran import FortranWriter

API = "gocean1.0"


def test_description():
    ''' Check that the transformation returns the expected strings '''
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    assert trans.name == "GOMoveIterationBoundariesInsideKernelTrans"
    assert str(trans) == \
        "Move kernel iteration boundaries inside the kernel code."


def test_validation():
    ''' Check that the transformation can only be applied to routine nodes '''
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in GOMoveIterationBoundariesInsideKernelTrans "
            "transformation. This transformation can only be applied to "
            "'GOKern' nodes, but found 'NoneType'." in str(info.value))


def test_go_move_iteration_boundaries_inside_kernel_trans():
    ''' Tests that the GOMoveIterationBoundariesInsideKernelTrans
    transformation for the GOcean API adds the 4 boundary values as kernel
    arguments and adds a masking statement at the beginning of the code.
    '''
    psy, _ = get_invoke("single_invoke.f90", API, idx=0, dist_mem=False)
    sched = psy.invokes.invoke_list[0].schedule
    kernel = sched.children[0].loop_body[0].loop_body[0]  # compute_cu kernel
    num_args = len(kernel.arguments.args)

    # Add some name conflicting symbols in the Invoke and the Kernel
    kernel.ancestor(Container).symbol_table.new_symbol("xstop")
    kernel.get_kernel_schedule().symbol_table.new_symbol("ystart")

    # Apply the transformation
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    trans.apply(kernel)

    # Check that the kernel call have been transformed:
    # - Immediately before the loop there are the boundaries assignments
    assert isinstance(sched.children[0], Assignment)
    assert sched.children[0].lhs.symbol.name == "xstart"
    assert isinstance(sched.children[1], Assignment)
    assert sched.children[1].lhs.symbol.name == "xstop_1"
    assert isinstance(sched.children[2], Assignment)
    assert sched.children[2].lhs.symbol.name == "ystart"
    assert isinstance(sched.children[3], Assignment)
    assert sched.children[3].lhs.symbol.name == "ystop"

    # - The loops have been transformed
    assert isinstance(sched.children[4], GOLoop)
    assert sched.children[4].field_space == "go_every"
    assert sched.children[4].iteration_space == "go_all_pts"
    assert isinstance(sched.children[4].loop_body[0], GOLoop)
    assert sched.children[4].loop_body[0].field_space == "go_every"
    assert sched.children[4].loop_body[0].iteration_space == "go_all_pts"

    # - And the appropriate arguments have been added to the kernel call
    assert len(kernel.arguments.args) == num_args + 4
    assert kernel.arguments.args[-4].name == "xstart"
    assert kernel.arguments.args[-4].argument_type == "scalar"
    assert kernel.arguments.args[-3].name == "xstop_1"
    assert kernel.arguments.args[-3].argument_type == "scalar"
    assert kernel.arguments.args[-2].name == "ystart"
    assert kernel.arguments.args[-1].argument_type == "scalar"
    assert kernel.arguments.args[-1].name == "ystop"
    assert kernel.arguments.args[-1].argument_type == "scalar"

    # Check that the kernel subroutine has been transformed:
    kschedule = kernel.get_kernel_schedule()

    # - It has the boundary conditions mask
    assert isinstance(kschedule.children[0], IfBlock)
    assert str(kschedule.children[0].condition) == (
        "BinaryOperation[operator:'OR']\n"
        "BinaryOperation[operator:'OR']\n"
        "BinaryOperation[operator:'LT']\n"
        "Reference[name:'i']\n"
        "Reference[name:'xstart']\n"
        "BinaryOperation[operator:'GT']\n"
        "Reference[name:'i']\n"
        "Reference[name:'xstop']\n"
        "BinaryOperation[operator:'OR']\n"
        "BinaryOperation[operator:'LT']\n"
        "Reference[name:'j']\n"
        "Reference[name:'ystart_1']\n"
        "BinaryOperation[operator:'GT']\n"
        "Reference[name:'j']\n"
        "Reference[name:'ystop']")
    assert isinstance(kschedule.children[0].if_body[0], Return)

    # - It has the boundary symbol as kernel arguments
    assert isinstance(kschedule.symbol_table.lookup("xstart").interface,
                      ArgumentInterface)
    assert isinstance(kschedule.symbol_table.lookup("xstop").interface,
                      ArgumentInterface)
    assert isinstance(kschedule.symbol_table.lookup("ystart_1").interface,
                      ArgumentInterface)
    assert isinstance(kschedule.symbol_table.lookup("ystop").interface,
                      ArgumentInterface)


def test_go_move_iteration_boundaries_inside_kernel_two_kernels_apply_twice():
    ''' Tests that the GOMoveIterationBoundariesInsideKernelTrans
    transformation for the GOcean API produces the expected code when the
    invoke has two kernels and the transformation is applied twice.
    We check that the kernels don't use the same boundary values (some are
    postfixed with a number) and that kernels don't duplicate boundary
    arguments themself when applying the transformation twice.
    '''
    psy, _ = get_invoke("single_invoke_two_kernels.f90", API, idx=0,
                        dist_mem=False)
    sched = psy.invokes.invoke_list[0].schedule

    # Apply the transformation twice
    trans = GOMoveIterationBoundariesInsideKernelTrans()
    for kernel in sched.coded_kernels():
        trans.apply(kernel)
        trans.apply(kernel)

    expected = '''subroutine invoke_0(cu_fld, p_fld, u_fld, unew_fld, uold_fld)
  use compute_cu_mod, only : compute_cu_code
  use time_smooth_mod, only : time_smooth_code
  type(r2d_field), intent(inout) :: cu_fld
  type(r2d_field), intent(inout) :: p_fld
  type(r2d_field), intent(inout) :: u_fld
  type(r2d_field), intent(inout) :: unew_fld
  type(r2d_field), intent(inout) :: uold_fld
  integer :: j
  integer :: i
  integer :: xstart
  integer :: xstop
  integer :: ystart
  integer :: ystop
  integer :: xstart_1
  integer :: xstop_1
  integer :: ystart_1
  integer :: ystop_1

  xstart = cu_fld%internal%xstart
  xstop = cu_fld%internal%xstop
  ystart = cu_fld%internal%ystart
  ystop = cu_fld%internal%ystop
  do j = 1, SIZE(cu_fld%data, 2), 1
    do i = 1, SIZE(cu_fld%data, 1), 1
      call compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data, xstart, \
xstop, ystart, ystop)
    enddo
  enddo
  xstart_1 = 1
  xstop_1 = SIZE(uold_fld%data, 1)
  ystart_1 = 1
  ystop_1 = SIZE(uold_fld%data, 2)
  do j = 1, SIZE(uold_fld%data, 2), 1
    do i = 1, SIZE(uold_fld%data, 1), 1
      call time_smooth_code(i, j, u_fld%data, unew_fld%data, uold_fld%data, \
xstart_1, xstop_1, ystart_1, ystop_1)
    enddo
  enddo

end subroutine invoke_0
'''

    writer = FortranWriter()
    assert writer(sched) == expected
