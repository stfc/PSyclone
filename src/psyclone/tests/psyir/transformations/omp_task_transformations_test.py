# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab
'''
API-agnostic tests for OpenMP task transformation class.
'''
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.errors import GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import Kern, PSyFactory
from psyclone.psyir.nodes import Call, CodeBlock, Container, Loop
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import OMPParallelTrans, \
    OMPSingleTrans
from psyclone.psyir.transformations import OMPTaskTrans

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_omptask_trans_str():
    '''Test the __str__ and name method of the OMPTaskTrans'''
    trans = OMPTaskTrans()
    assert str(trans) == "Adds an 'OMP TASK' directive to a statement"
    assert trans.name == "OMPTaskTrans"


def test_omptask_directive_fails():
    ''' Test the _directive method of the OMPTaskTrans fails if a collapse
    value is supplied. '''
    trans = OMPTaskTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans._directive([], True)
    assert ("Collapse attribute should not be set for OMPTaskTrans" in
            str(excinfo.value))


def test_omptask_validate(fortran_reader):
    '''Test the validate method of the OMPTaskTrans fails when supplied
    a loop containing a codeblock'''
    code = '''
    subroutine sub()
        integer :: ji, jj, n
        integer, dimension(10, 10) :: t
        integer, dimension(10, 10) :: s
        do jj = 1, 10
            do ji = 1, 10
                t(ji, jj) = s(ji, jj)
            end do
        end do
    end subroutine sub
    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = OMPTaskTrans()
    loops = psyir.walk(Loop)
    loops[1].children[3].children.append(CodeBlock([], None))
    with pytest.raises(GenerationError) as excinfo:
        trans.apply(loops[1])
    assert ("OMPTaskTransformation cannot be applied to a region containing "
            "a code block" in str(excinfo.value))


def test_omptask_apply(fortran_reader, fortran_writer):
    ''' Test the apply method of the OMPTaskTrans. We contain
    an IntrinsicCall inside the region to be transformed to ensure that
    the application doesn't attempt to inline it. '''
    code = '''
    subroutine sub()
        integer :: ji, jj, n
        integer, dimension(10, 10) :: t
        integer, dimension(10, 10) :: s
        do jj = 1, 10
            do ji = 1, SIZE(ji,2)
                t(ji, jj) = INT(s(ji, jj))
            end do
        end do
    end subroutine sub
    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = OMPTaskTrans()
    master = OMPSingleTrans()
    parallel = OMPParallelTrans()
    loops = psyir.walk(Loop)
    trans.apply(loops[1])
    master.apply(loops[0])
    parallel.apply(psyir.children[0].children[:])
    out = fortran_writer(psyir)
    correct = '''subroutine sub()
  integer :: ji
  integer :: jj
  integer :: n
  integer, dimension(10,10) :: t
  integer, dimension(10,10) :: s

  !$omp parallel default(shared), private(ji,jj)
  !$omp single
  do jj = 1, 10, 1
    !$omp task private(ji), firstprivate(jj), shared(t,s), \
depend(in: s(:,jj)), depend(out: t(:,jj))
    do ji = 1, SIZE(ji, 2), 1
      t(ji,jj) = INT(s(ji,jj))
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine sub
'''
    assert out == correct


def test_omptask_apply_kern(fortran_reader, fortran_writer):
    code = '''
    module test_kernel_mod
    contains
      subroutine test_kernel(i, j, array)
        integer :: i, j
        integer, dimension(:,:), intent(out) :: array

        array(i, j) = 1
      end subroutine test_kernel
    end module test_kernel_mod

    subroutine my_test()
    use test_kernel_mod, only: test_kernel
    integer :: i, j
    integer, dimension(100, 100) :: array

    do i = 1, 100
      do j = 1, 100
        call test_kernel(i, j, array)
      end do
    end do

    end subroutine my_test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    new_container = Container("test_container")
    test_kernel_mod = psyir.children[0].detach()
    my_test = psyir.children[0].detach()
    new_container.addchild(test_kernel_mod)
    new_container.addchild(my_test)
    sym = my_test.symbol_table.lookup("test_kernel")
    sym.interface.container_symbol._reference = test_kernel_mod
    trans = OMPTaskTrans()
    master = OMPSingleTrans()
    parallel = OMPParallelTrans()
    calls = my_test.walk(Call)
    calls[0].routine.is_pure = True
    loops = my_test.walk(Loop)
    trans.apply(loops[1])
    master.apply(my_test.children[:])
    parallel.apply(my_test.children[:])
    assert len(my_test.walk(Call, Kern)) == 0


# This test relies on inline functionality not yet supported
@pytest.mark.xfail()
def test_omptask_inline_kernels():
    '''Test the _inline_kernels functionality up to inlining of Call nodes.'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskt = OMPTaskTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # Cover the _inline_kernels code
    taskt._inline_kernels(schedule.children[0])


# This test relies on inline functionality not yet supported
@pytest.mark.xfail
def test_omptask_apply_gocean():
    ''' Test the apply method of the OMPTaskTrans. '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskt = OMPTaskTrans()
    master = OMPSingleTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    taskt.apply(schedule.children[0])
    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])

    code = str(psy.gen)
    assert (
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp single\n" +
        "      !$omp task private(j,i), shared(cu_fld,p_fld,u_fld), depend(" +
        "in: cu_fld,p_fld%data(:,:),u_fld%data(:,:)), depend(" +
        "out: cu_fld%data(:,:))" +
        "\n" + "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end task\n" +
        "      !$omp end single\n" +
        "      !$omp end parallel" in code)
