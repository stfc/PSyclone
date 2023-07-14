# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

import os
import pytest
from psyclone.psyir.nodes import Schedule, \
    Loop, OMPTaskDirective, OMPPrivateClause, OMPFirstprivateClause, \
    OMPSharedClause, OMPDependClause, DynamicOMPTaskDirective, \
    OMPSingleDirective
from psyclone.errors import GenerationError
from psyclone.transformations import OMPSingleTrans, \
    OMPParallelTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_omp_task_directive_validate_global_constraints():
    ''' Test the validate_global_constraints method of the
    OMPTaskDirective'''
    node = OMPTaskDirective()
    with pytest.raises(GenerationError) as excinfo:
        node.validate_global_constraints()
    assert ("OMPTaskDirective must be inside an OMP Single region but could"
            " not find an ancestor node.") in str(excinfo.value)
    parent = OMPSingleDirective(nowait=True)
    parent.children[0].addchild(node)
    with pytest.raises(GenerationError) as excinfo:
        node.validate_global_constraints()
    assert ("OMPTaskDirective found inside an OMP Single region with nowait "
            "attached. This means we can't guarantee correctness with other "
            "potential Single regions so is forbidden with PSyclone."
            in str(excinfo.value))


def test_omp_task_validate_child():
    ''' Test the validate_child method of the OMPTaskDirective'''
    assert OMPTaskDirective._validate_child(0, Schedule()) is True
    assert OMPTaskDirective._validate_child(1, OMPPrivateClause()) is True
    assert OMPTaskDirective._validate_child(2, OMPFirstprivateClause()) is True
    assert OMPTaskDirective._validate_child(3, OMPSharedClause()) is True
    assert OMPTaskDirective._validate_child(4, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(5, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(6, OMPDependClause()) is False
    assert OMPTaskDirective._validate_child(0, "string") is False
    assert OMPTaskDirective._validate_child(1, "string") is False
    assert OMPTaskDirective._validate_child(2, "string") is False
    assert OMPTaskDirective._validate_child(3, "string") is False
    assert OMPTaskDirective._validate_child(4, "string") is False
    assert OMPTaskDirective._validate_child(5, "string") is False


def test_omp_task_directive_clause_accesors(fortran_reader):
    ''' Test the input_depend_clause and output_depend_clause methods.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)

    copy = tree.lower_to_language_level()
    task_dir = copy.walk(OMPTaskDirective)[0]
    assert isinstance(task_dir.input_depend_clause, OMPDependClause)
    assert (task_dir.input_depend_clause._operand ==
            OMPDependClause.DependClauseTypes.IN)
    assert isinstance(task_dir.output_depend_clause, OMPDependClause)
    assert (task_dir.output_depend_clause._operand ==
            OMPDependClause.DependClauseTypes.OUT)


def test_omp_task_directive_1(fortran_reader, fortran_writer):
    ''' Test a basic code generation with the task directive applied to a
    loop which accesses the full arrays.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(10,10) :: b
  integer :: i
  integer :: j

  !$omp parallel default(shared), private(i,j)
  !$omp single
  !$omp task private(i,j), shared(a,b), depend(in: b(:,:)), depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = b(i,j) + 1
    enddo
  enddo
  !$omp end task
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = 0
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_2(fortran_reader):
    ''' Test the code generation fails when attempting to access an array
    when using an array element as an index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(B(1,2), j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("ArrayReference object is not allowed to appear in an Array Index "
            "expression inside an OMPTaskDirective.") in str(excinfo.value)


def test_omp_task_directive_3(fortran_reader, fortran_writer):
    '''Test the code generation correctly captures if a variable should be
    declared as firstprivate.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 10
            k = i
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = k
                A(i, j) = B(i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(parent.children[1])
    ptrans.apply(parent.children)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(10,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j,k)
  do i = 1, 10, 1
    k = i
  enddo
  !$omp single
  !$omp task private(i,j), firstprivate(k), shared(a,b), depend(in: b(:,:)), \
depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = k
      a(i,j) = b(i,j) + k
    enddo
  enddo
  !$omp end task
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_4(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an input array shifted by the step size of the outer loop.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(11, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 10
            do j = 1, 10
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(11,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(i + 1,:)), depend(out: a(i,:))
    do j = 1, 10, 1
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_5(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is not quite a real use-case, however its a first check for this
    idea.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(i + 32,:),b(i,:)), depend(out: a(i,:))
    do j = 1, 32, 1
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_6(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(i + 32,:),b(i,:),k), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_7(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 321) :: A
        integer, dimension(321, 321) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii,jj) = B(ii+1,jj+1) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,321) :: a
  integer, dimension(321,321) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i + 32,j + 32),b(i + 32,j),b(i,j + 32),b(i,j),k), \
depend(out: a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii,jj) = b(ii + 1,jj + 1) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_8(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by a mixture of steps of the chunked loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii,jj) = B(ii+1,jj+33) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i + 32,j + 2 * 32),b(i + 32,j + 32),b(i,j + 2 * 32),\
b(i,j + 32),k), depend(out: a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii,jj) = b(ii + 1,jj + 33) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_9(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an output array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 32) :: A
        integer, dimension(321, 32) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii+1, j) = B(ii, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,32) :: a
  integer, dimension(321,32) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(i,:),k), depend(out: a(i + 32,:),a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii + 1,j) = b(ii,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_10(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an output array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 321) :: A
        integer, dimension(321, 321) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii+1,jj+1) = B(ii,jj) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,321) :: a
  integer, dimension(321,321) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i,j),k), depend(out: a(i + 32,j + 32),a(i + 32,j),\
a(i,j + 32),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 1) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_11(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by a mixture of steps of the chunked loop.
    This is expected to be similar to a real use-case. In this case, we have
    multiple loops to handle. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        A(ii+1,jj+33) = B(ii,jj) * k
                        A(ii+1,jj+65) = B(ii,jj) * k
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i,j),k), depend(out: a(i + 32,j + 2 * 32),a(i + 32,j + 32),\
a(i,j + 2 * 32),a(i,j + 32),a(i + 32,j + 3 * 32),a(i,j + 3 * 32))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 33) = b(ii,jj) * k
          a(ii + 1,jj + 65) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_12(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an if statement is present. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+1,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: a(i,j),b(i,j),k), depend(out: a(i + 32,j),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          if (a(ii,jj) > 0.0) then
            a(ii + 1,jj) = b(ii,jj) * k
          end if
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_mul_index_fail(fortran_reader):
    ''' Test the code generation throws an Error when a multiplication is
    inside an index Binop. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii*3,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Binary Operator of type Operator.MUL used as in index inside an "
            "OMPTaskDirective which is not supported" in str(excinfo.value))


def test_omp_task_directive_refref_index_fail(fortran_reader):
    ''' Test the code generation throws an Error when an index Binop is on two
    references. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+ii,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Children of BinaryOperation are of types Reference and Reference,"
            " expected one Reference and one Literal when used as an index "
            "inside an OMPTaskDirective." in str(excinfo.value))


def test_omp_task_directive_13(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have Literal+Reference.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                A(i, j) = k
                A(i, j) = B(1+i, j) + k
                A(i, j) = B(33+i, j) + k
                A(i, j) = B(32+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(32 + i,:),b(i,:),b(2 * 32 + i,:)), depend(out: a(i,:))
    do j = 1, 32, 1
      a(i,j) = k
      a(i,j) = b(1 + i,j) + k
      a(i,j) = b(33 + i,j) + k
      a(i,j) = b(32 + i,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_write_index_shared(fortran_reader):
    ''' Test the code generation correctly generates an error if an array index
    of a written array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                A(i, k) = k
                A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)

    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_read_index_shared(fortran_reader):
    ''' Test the code generation correctly generates an error if an array index
    of a read array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                A(i, j) = A(i, k)
                A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)

    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_14(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    firstprivate clause when the first access to a private variable is a
    read.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 9
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, k) + k
                    A(ii, j) = B(ii+1, k) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    k = 9
    !$omp task private(ii,j), firstprivate(i,k), shared(a,b), \
depend(in: b(i + 32,k),b(i,k)), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,k) + k
        a(ii,j) = b(ii + 1,k) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_15(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a non-array shared variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        k = 0
        do i = 1, 320, 32
            k = k + i
            do ii=i, i+32
                do j = 1, 32
                    k = k + B(ii+1, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=1)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j)
  k = 0
  !$omp single
  do i = 1, 320, 32
    k = k + i
    !$omp task private(ii,j), firstprivate(i), shared(k,b), \
depend(in: k,b(i + 32,:),b(i,:)), depend(out: k)
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        k = k + b(ii + 1,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_16(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an else statement is present. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 354) :: A
        integer, dimension(321, 354) :: B
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        if (A(ii, jj) > 0.0) then
                            A(ii+1,jj) = B(ii,jj) * k
                        else
                            A(ii-1,jj) = B(ii,jj) * k
                        end if
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(321,354) :: a
  integer, dimension(321,354) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: a(i,j),b(i,j),k), depend(out: a(i + 32,j),a(i,j),a(i - 32,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          if (a(ii,jj) > 0.0) then
            a(ii + 1,jj) = b(ii,jj) * k
          else
            a(ii - 1,jj) = b(ii,jj) * k
          end if
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_17(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct clauses
    when an output variable is just a shared vairable '''
    code = '''
    subroutine my_subroutine(k)
        integer :: i, ii
        integer :: j, jj
        integer :: k
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ii
                        k = k * jj
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine(k)
  integer :: k
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(k), \
depend(in: k), depend(out: k)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ii
          k = k * jj
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_18(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    stepval is not yet declared firstprivate '''
    code = '''
    subroutine my_subroutine()
        integer :: i, ii
        integer :: j, jj
        integer :: k, kk
        kk = 2
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32, kk
                    do jj = j,j+32
                        k = k + ii
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  integer :: kk

  kk = 2
  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,kk,j), shared(k), \
depend(in: k), depend(out: k)
      do ii = i, i + 32, kk
        do jj = j, j + 32, 1
          k = k + ii
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_19(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    stepval is not yet declared firstprivate '''
    code = '''
    subroutine my_subroutine()
      type :: x
        integer :: jp
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ty%jp + ii
                        ty%jp = ty%jp - (1 - ty%jp)
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  type :: x
    integer :: jp
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(k,ty), \
depend(in: k,ty), depend(out: k,ty)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%jp + ii
          ty%jp = ty%jp - (1 - ty%jp)
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_20(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a literal read-only array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(1, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(1,:)), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(1,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_21(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct
    code for a literal written to array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(i,:)), depend(out: a(i,1))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,1) = b(ii,j) + 1
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_shared_index(fortran_reader):
    ''' Test the code generation correctly throws an error if a shared variable
    is used as an array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
            do ii=i, i+32
                do j = 1, 32
                    k = k + 1
                    A(ii, k) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_non_loop(fortran_reader):
    ''' Test the code generation correctly throws an error if an
    OMPTaskDirective's child is a non Loop node.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("OMPTaskDirective must have exactly one Loop child. Found <class "
            "'psyclone.psyir.nodes.assignment.Assignment'>" in
            str(excinfo.value))


def test_omp_task_directive_multichild(fortran_reader):
    ''' Test the code generation correctly throws an error if it an
    OMPTaskDirective has multiple children.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer :: k

        do i = 1, 320, 32
            k = 1
            do ii=i, i+32
                do j = 1, 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    assign = loops[0].children[3].children[0]
    assign.detach()
    tdir.children[0].addchild(assign)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("OMPTaskDirective must have exactly one Loop child. Found 2 "
            "children." in str(excinfo.value))


def test_omp_task_directive_loop_start_array(fortran_reader):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array start value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = B(3,2), 32
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("ArrayReference not supported in the start variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_loop_stop_array(fortran_reader):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array stop value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, B(i,2)
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("ArrayReference not supported in the stop variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_loop_step_array(fortran_reader):
    ''' Test the code generation correctly throws an error if it a
    Loop inside an OMPTaskDirective has an array step value.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 22, B(i,2)
                    A(ii, 1) = B(ii, j) + 1
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("ArrayReference not supported in the step variable of a Loop "
            "in a OMPTaskDirective node." in str(excinfo.value))


def test_omp_task_directive_22(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have Literal+Reference for a proxy loop variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii = i, i + 32
                do j = 1, 32
                    A(i, j) = k
                    A(i, j) = B(1+ii, j) + k
                    A(i, j) = B(33+ii, j) + k
                    A(i, j) = B(32+ii, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: k,b(32 + i,:),b(i,:),b(2 * 32 + i,:)), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(i,j) = k
        a(i,j) = b(1 + ii,j) + k
        a(i,j) = b(33 + ii,j) + k
        a(i,j) = b(32 + ii,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_23(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have a private variable in an array reference, either as a child
    loop member or not.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do ii = i, i + 32
                k = 3
                do j = 1, 32
                    A(i, j+1) = k
                end do
                A(i,k + 2) = 3
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,k,j), firstprivate(i), shared(a), depend(out: a(i,:))
    do ii = i, i + 32, 1
      k = 3
      do j = 1, 32, 1
        a(i,j + 1) = k
      enddo
      a(i,k + 2) = 3
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_24(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when we have access to a non-proxy parent loop variable in an array index
    binary operation.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i,ii
        integer :: j
        integer :: k
        do j = 1, 320, 32
          do i = 1, 320, 32
            do ii = i, i + 32
                    A(i, j+65) = k
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: ii
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do j = 1, 320, 32
    do i = 1, 320, 32
      !$omp task private(ii), firstprivate(i,j), shared(a), \
depend(in: k), depend(out: a(i,j + 3 * 32),a(i,j + 2 * 32))
      do ii = i, i + 32, 1
        a(i,j + 65) = k
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_25(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct clauses
    when we have access to a first private constant in the routine'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer :: i,ii
        integer :: k
        logical :: statement
        k = 32
        do i = 1, 320, 32
          do ii = i, i + 32
            if(statement) then
              k = 30
            end if
            A(ii,k+1) = 20
          end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[1:])
    ptrans.apply(tree.children[0].children[1:])
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer :: i
  integer :: ii
  integer :: k
  logical :: statement

  k = 32
  !$omp parallel default(shared), private(i,ii), firstprivate(k)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii), firstprivate(i,k), shared(a), \
depend(in: statement), depend(out: a(i,:))
    do ii = i, i + 32, 1
      if (statement) then
        k = 30
      end if
      a(ii,k + 1) = 20
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    print(fortran_writer(tree))
    assert fortran_writer(tree) == correct


def test_omp_task_directive_26(fortran_reader):
    ''' Test the code generation correctly throws an error if an
    index is a shared non-array variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        j = 32
        do i = 1, 320, 32
            do ii=i, i+32
                A(ii, 1) = B(ii, j+1) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported." in str(excinfo.value))


def test_omp_task_directive_27(fortran_reader):
    ''' Test the code generation correctly throws an error if an
    index is a shared non-array variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        j = 32
        do i = 1, 320, 32
            do j=i, i+32
                A(ii, 1) = B(ii, 1) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    print(str(excinfo.value))
    assert ("Found shared loop variable which is not allowed in OpenMP Task "
           "directive. Variable name is j" in str(excinfo.value))


def test_omp_task_directive_28(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an input array shifted by the step size of the outer loop,
    but the shift is hidden in a temporary variable.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(11, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        integer :: iu
        do i = 1, 10
            do j = 1, 10
                iu = i + 1
                A(i, j) = k
                A(i, j) = B(iu, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(11,10) :: b
  integer :: i
  integer :: j
  integer :: k
  integer :: iu

  !$omp parallel default(shared), private(i,iu,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j,iu), firstprivate(i), shared(a,b), \
depend(in: k,b(i + 1,:)), depend(out: a(i,:))
    do j = 1, 10, 1
      iu = i + 1
      a(i,j) = k
      a(i,j) = b(iu,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_29(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(3) :: jp
      end type
      type :: x
        type(y) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty
        integer :: index

        do i = 1, 10
            index = i
        end do
        index = 1
        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ty%y%jp(index) + ii
                        ty%y%jp(index+1) = ty%y%jp(index+1) - (1 - \
ty%y%jp(index+1))
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  type :: y
    integer, dimension(3) :: jp
  end type y
  type :: x
    type(y) :: y
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty
  integer :: index

  !$omp parallel default(shared), private(i,ii,index,j,jj)
  !$omp single
  do i = 1, 10, 1
    index = i
  enddo
  index = 1
  do i = 1, 320, 32
    !$omp task private(j,ii,jj), firstprivate(i,index), shared(k,ty), depend(\
in: k,ty%y%jp(index),ty%y%jp(index + 1)), depend(out: k,ty%y%jp(index + 1))
    do j = 1, 320, 32
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%y%jp(index) + ii
          ty%y%jp(index + 1) = ty%y%jp(index + 1) - (1 - ty%y%jp(index + 1))
        enddo
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_30(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(321) :: jp
      end type
      type :: x
        type(y) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty

        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ty%y%jp(j) + ii
                        ty%y%jp(j+1) = ty%y%jp(j+1) - (1 - ty%y%jp(j+1))
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  type :: y
    integer, dimension(321) :: jp
  end type y
  type :: x
    type(y) :: y
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    do j = 1, 320, 32
      !$omp task private(ii,jj), firstprivate(i,j), shared(k,ty), depend(in: \
k,ty%y%jp(j),ty%y%jp(j + 32)), depend(out: k,ty%y%jp(j + 32),ty%y%jp(j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%y%jp(j) + ii
          ty%y%jp(j + 1) = ty%y%jp(j + 1) - (1 - ty%y%jp(j + 1))
        enddo
      enddo
      !$omp end task
    enddo
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_31(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(321) :: jp
      end type
      type :: x
        type(y) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty

        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ty%y%jp(ii) + ii
                        ty%y%jp(ii+1) = ty%y%jp(ii+1) - (1 - ty%y%jp(ii+1))
                        ty%y%jp(1) = ty%y%jp(1) + 1
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  type :: y
    integer, dimension(321) :: jp
  end type y
  type :: x
    type(y) :: y
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j,ii,jj), firstprivate(i), shared(k,ty), depend(in: \
k,ty%y%jp(i),ty%y%jp(i + 32),ty%y%jp(1)), depend(out: k,ty%y%jp(i + 32),\
ty%y%jp(i),ty%y%jp(1))
    do j = 1, 320, 32
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%y%jp(ii) + ii
          ty%y%jp(ii + 1) = ty%y%jp(ii + 1) - (1 - ty%y%jp(ii + 1))
          ty%y%jp(1) = ty%y%jp(1) + 1
        enddo
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_32(fortran_reader):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(321) :: jp
      end type
      type :: x
        type(y), dimension(5) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty

        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = ty%y%jp(ii) + ii
                        ty%y(2)%jp(ii+1) = ty%y(1)%jp(ii+1) - (1 - \
ty%y(3)%jp(ii+1))
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert("Doesn't support a StructureReference child with multiple "
           "array accessing members." in str(excinfo.value))


def test_omp_task_directive_write_index_shared_type(fortran_reader):
    ''' Test the code generation correctly generates an error if an array index
    of a written array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                AA%A(i, k) = k
                AA%A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)

    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_read_index_shared_type(fortran_reader):
    ''' Test the code generation correctly generates an error if an array index
    of a read array is a shared variable.'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            k = 4
            k = -2
            k = k + 3
            do j = 1, 32
                AA%A(i, j) = AA%A(i, k)
                AA%A(i, j) = B(1+i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[3]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Shared variable access used as an index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "Reference[name:'k']" in str(excinfo.value))


def test_omp_task_directive_33(fortran_reader):
    ''' Test the code generation fails when attempting to access an array
    when using an array element as an index.'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                B(i, j) = AA%A(B(1,2), j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                AA%A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("ArrayReference object is not allowed to appear in an Array Index "
            "expression inside an OMPTaskDirective.") in str(excinfo.value)


def test_omp_task_directive_34(fortran_reader):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(321) :: jp
      end type
      type :: x
        type(y), dimension(5) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty

        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = ty%y%jp(ii) + ii
                        k = ty%y(1)%jp(ii+1) - (1 - ty%y(3)%jp(ii+1))
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert("Doesn't support a StructureReference child with multiple "
           "array accessing members." in str(excinfo.value))


def test_omp_task_directive_35(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct code if
    code contains a type within a type .'''
    code = '''
    subroutine my_subroutine()
      type :: y
        integer, dimension(321) :: jp
      end type
      type :: x
        type(y) :: y
      end type
        integer :: i, ii
        integer :: j, jj
        integer :: k
        type(x) :: ty

        do i = 1, 320, 32
            do j = 1, 320, 32
                do ii=i, i+32
                    do jj = j,j+32
                        k = k + ty%y%jp(ii) + ii
                        ty%y%jp(ii) = ty%y%jp(ii) - (1 - ty%y%jp(ii))
                        ty%y%jp(1) = ty%y%jp(1) + 1
                    end do
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    correct = '''subroutine my_subroutine()
  type :: y
    integer, dimension(321) :: jp
  end type y
  type :: x
    type(y) :: y
  end type x
  integer :: i
  integer :: ii
  integer :: j
  integer :: jj
  integer :: k
  type(x) :: ty

  !$omp parallel default(shared), private(i,ii,j,jj)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j,ii,jj), firstprivate(i), shared(k,ty), depend(in: \
k,ty%y%jp(i),ty%y%jp(1)), depend(out: k,ty%y%jp(i),ty%y%jp(1))
    do j = 1, 320, 32
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%y%jp(ii) + ii
          ty%y%jp(ii) = ty%y%jp(ii) - (1 - ty%y%jp(ii))
          ty%y%jp(1) = ty%y%jp(1) + 1
        enddo
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_36(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an input array shifted by the step size of the outer loop.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(11, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        integer :: l
        integer :: m
        integer :: n

        do i = 1, 10, 1
            do j = l, m, n
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer, dimension(11,10) :: b
  integer :: i
  integer :: j
  integer :: k
  integer :: l
  integer :: m
  integer :: n

  !$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j), firstprivate(l,m,n,i), shared(a,b), \
depend(in: k,b(i + 1,:)), depend(out: a(i,:))
    do j = l, m, n
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_37(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an input array shifted by the step size of the outer loop.'''
    code = '''
    subroutine my_subroutine()
        type :: x
           integer :: k
        end type
        integer, dimension(10, 10) :: A
        integer, dimension(11, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        type(x) :: l
        type(x) :: m
        type(x) :: n

        do i = 1, 10, 1
            do j = l%k, m%k, n%k
                A(i, j) = k
                A(i, j) = B(i+1, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  type :: x
    integer :: k
  end type x
  integer, dimension(10,10) :: a
  integer, dimension(11,10) :: b
  integer :: i
  integer :: j
  integer :: k
  type(x) :: l
  type(x) :: m
  type(x) :: n

  !$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j), firstprivate(i), shared(l,m,n,a,b), \
depend(in: l,m,n,k,b(i + 1,:)), depend(out: a(i,:))
    do j = l%k, m%k, n%k
      a(i,j) = k
      a(i,j) = b(i + 1,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_38(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an array access child of a type'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                B(i, j) = AA%A(i,j) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)
    correct = '''subroutine my_subroutine()
  type :: x
    integer, dimension(320,10) :: a
  end type x
  type(x) :: aa
  integer, dimension(10,10) :: b
  integer :: i
  integer :: j

  !$omp parallel default(shared), private(i,j)
  !$omp single
  !$omp task private(i,j), shared(b,aa), depend(in: aa%A(:,:)), \
depend(out: b(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      b(i,j) = aa%A(i,j) + 1
    enddo
  enddo
  !$omp end task
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine
'''
    assert correct == fortran_writer(tree)


def test_omp_task_directive_39(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection.
    This is not quite a real use-case, however its a first check for this
    idea.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: iplusone
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                iplusone = i + 1
                A(i, j) = k
                A(i, j) = B(iplusone, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer :: i
  integer :: iplusone
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,iplusone,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j,iplusone), firstprivate(i), shared(a,b), \
depend(in: k,b(i + 32,:),b(i,:)), depend(out: a(i,:))
    do j = 1, 32, 1
      iplusone = i + 1
      a(i,j) = k
      a(i,j) = b(iplusone,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_40(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case
    we have indirection from an if statement for +1 or -1
    '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer, dimension(320, 10) :: boundary
        integer :: i
        integer :: iplusone
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                if(boundary(i,j) > 1) then
                    iplusone = i + 1
                else
                    iplusone = i - 1
                endif
                A(iplusone, j) = B(i, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer, dimension(320,10) :: boundary
  integer :: i
  integer :: iplusone
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,a,b), \
depend(in: boundary(i,:),b(i,:),k), depend(out: a(i + 32,:),a(i,:),a(i - 32,:))
    do j = 1, 32, 1
      if (boundary(i,j) > 1) then
        iplusone = i + 1
      else
        iplusone = i - 1
      end if
      a(iplusone,j) = b(i,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_41(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case
    we have indirection from an if statement for +1 or -1
    '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer, dimension(320, 10) :: boundary
        integer :: i
        integer :: iplusone
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                if(boundary(i,j) > 1) then
                    iplusone = i + 1
                else
                    iplusone = i - 1
                endif
                A(i, j) = k
                A(i, j) = B(iplusone, j) + k
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer, dimension(320,10) :: boundary
  integer :: i
  integer :: iplusone
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,a,b), \
depend(in: boundary(i,:),k,b(i + 32,:),b(i,:),b(i - 32,:)), depend(out: a(i,:))
    do j = 1, 32, 1
      if (boundary(i,j) > 1) then
        iplusone = i + 1
      else
        iplusone = i - 1
      end if
      a(i,j) = k
      a(i,j) = b(iplusone,j) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


def test_omp_task_directive_42(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an array access child of a type through an indirection'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(320, 10) :: B
        integer, dimension(320, 10) :: boundary
        integer :: iplusone
        integer :: i
        integer :: j
        do i = 1, 320, 32
            do j = 1, 10
                if(boundary(i,j) > 1) then
                    iplusone = i+32
                else
                    iplusone = i-1
                end if
                B(i, j) = AA%A(iplusone,j) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  type :: x
    integer, dimension(320,10) :: a
  end type x
  type(x) :: aa
  integer, dimension(320,10) :: b
  integer, dimension(320,10) :: boundary
  integer :: iplusone
  integer :: i
  integer :: j

  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,b,aa), \
depend(in: boundary(i,:),aa%A(i + 32,:),aa%A(i - 32,:),aa%A(i,:)), \
depend(out: b(i,:))
    do j = 1, 10, 1
      if (boundary(i,j) > 1) then
        iplusone = i + 32
      else
        iplusone = i - 1
      end if
      b(i,j) = aa%A(iplusone,j) + 1
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine
'''
    assert correct == fortran_writer(tree)


def test_omp_task_directive_43(fortran_reader, fortran_writer):
    ''' Test the code generation correctly makes the depend clause when
    accessing an array access child of a type through an indirection'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(320, 10) :: A
        end type
        type(x) :: AA
        integer, dimension(320, 10) :: B
        integer, dimension(320, 10) :: boundary
        integer :: iplusone
        integer :: i
        integer :: j
        do i = 1, 320, 32
            do j = 1, 10
                if(boundary(i,j) > 1) then
                    iplusone = i+32
                else
                    iplusone = i-1
                end if
                AA%A(iplusone, j) = B(i, j) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  type :: x
    integer, dimension(320,10) :: a
  end type x
  type(x) :: aa
  integer, dimension(320,10) :: b
  integer, dimension(320,10) :: boundary
  integer :: iplusone
  integer :: i
  integer :: j

  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,aa,b), \
depend(in: boundary(i,:),b(i,:)), \
depend(out: aa%A(i + 32,:),aa%A(i - 32,:),aa%A(i,:))
    do j = 1, 10, 1
      if (boundary(i,j) > 1) then
        iplusone = i + 32
      else
        iplusone = i - 1
      end if
      aa%A(iplusone,j) = b(i,j) + 1
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine
'''
    assert correct == fortran_writer(tree)


def test_omp_task_directive_44(fortran_reader, fortran_writer):
    '''Test the code generation generates the correct depend clause for a case
    like the boundary condition for NemoLite2D. here we have a normal structure
    for task + chunk loop code, and jiv = j + 1 needs to be correctly resolved
    as related to j_out_var (and thus a proxy loop variable).'''
    code = '''
    subroutine my_subroutine()
    integer :: j_out_var, j, i, j_el_inner, ystart, ystop, xstart, xstop
    integer :: jiv
    integer, dimension(100, 100) :: boundary
    real, dimension(100, 100) :: va, hv, sshn_v
    real :: g

      DO j_out_var = ystart, ystop, 32
        j_el_inner = MIN(j_out_var + (32 - 1), ystop)
        DO j = j_out_var, j_el_inner, 1
          DO i = xstart, xstop, 1
            IF (.NOT.boundary(i,j) + boundary(i,j + 1) <= (-1)) THEN
              IF (boundary(i,j) < 0) THEN
                jiv = j + 1
                va(i,j) = va(i,jiv) + SQRT(g / hv(i,j)) * (sshn_v(i,j) - \
sshn_v(i,jiv))
              ELSE
                IF (boundary(i,j + 1) < 0) THEN
                  jiv = j - 1
                  va(i,j) = va(i,jiv) + SQRT(g / hv(i,j)) * (sshn_v(i,j) - \
sshn_v(i,jiv))
                END IF
              END IF
            END IF
          END DO
        END DO
      END DO

      DO j_out_var = ystart, ystop, 32
        j_el_inner = MIN(j_out_var + (32-1), ystop)
        Do j = j_out_var, j_el_inner, 1
          Do i = xstart, xstop, 1
            va(i, j) = sshn_v(i,j)
          end do
        end do
    end do
    end subroutine
      '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[1]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir)
    tdir = DynamicOMPTaskDirective()
    loop = loops[4]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir)

    strans.apply(loops[0].parent.children[:])
    ptrans.apply(loops[0].parent.parent)

    correct = '''subroutine my_subroutine()
  integer :: j_out_var
  integer :: j
  integer :: i
  integer :: j_el_inner
  integer :: ystart
  integer :: ystop
  integer :: xstart
  integer :: xstop
  integer :: jiv
  integer, dimension(100,100) :: boundary
  real, dimension(100,100) :: va
  real, dimension(100,100) :: hv
  real, dimension(100,100) :: sshn_v
  real :: g

  !$omp parallel default(shared), private(i,j,j_el_inner,j_out_var), \
firstprivate(jiv)
  !$omp single
  do j_out_var = ystart, ystop, 32
    j_el_inner = MIN(j_out_var + (32 - 1), ystop)
    !$omp task private(j,i), firstprivate(j_out_var,j_el_inner,xstart,\
xstop,jiv), shared(boundary,va,hv,sshn_v), depend(in: boundary(:,j_out_var),\
boundary(:,j_out_var + 32),va(:,j_out_var + 32),va(:,j_out_var),g,\
hv(:,j_out_var),sshn_v(:,j_out_var),sshn_v(:,j_out_var + 32),\
va(:,j_out_var - 32),sshn_v(:,j_out_var - 32)), depend(out: va(:,j_out_var))
    do j = j_out_var, j_el_inner, 1
      do i = xstart, xstop, 1
        if (.NOT.boundary(i,j) + boundary(i,j + 1) <= (-1)) then
          if (boundary(i,j) < 0) then
            jiv = j + 1
            va(i,j) = va(i,jiv) + SQRT(g / hv(i,j)) * (sshn_v(i,j) - \
sshn_v(i,jiv))
          else
            if (boundary(i,j + 1) < 0) then
              jiv = j - 1
              va(i,j) = va(i,jiv) + SQRT(g / hv(i,j)) * (sshn_v(i,j) - \
sshn_v(i,jiv))
            end if
          end if
        end if
      enddo
    enddo
    !$omp end task
  enddo
  do j_out_var = ystart, ystop, 32
    j_el_inner = MIN(j_out_var + (32 - 1), ystop)
    !$omp task private(j,i), firstprivate(j_out_var,j_el_inner,xstart,xstop), \
shared(va,sshn_v), depend(in: sshn_v(:,j_out_var)), depend(out: \
va(:,j_out_var))
    do j = j_out_var, j_el_inner, 1
      do i = xstart, xstop, 1
        va(i,j) = sshn_v(i,j)
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine
'''
    assert correct in fortran_writer(tree)


def test_omp_task_directive_45(fortran_reader, fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop.
    This is expected to be similar to a real use-case.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        integer, parameter :: k = 1
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, j) + k
                end do
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, parameter :: k = 1
  integer, dimension(321,10) :: a
  integer, dimension(32,10) :: b
  integer :: i
  integer :: ii
  integer :: j

  !$omp parallel default(shared), private(i,ii,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(i + 32,:),b(i,:)), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,j) + k
      enddo
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct


# TODO #2052 This test is expected to fail as we can't yet handle
# multiple indirections on either side of a statement and will over
# generate code for this dependency.
@pytest.mark.xfail
def test_omp_task_directive_xfail_indirection_test(fortran_reader,
                                                   fortran_writer):
    ''' Test the code generation correctly generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case,
    the indirection value is set before and after the access, and produces
    code based on both accesses, instead of only the one before.
    '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer, dimension(320, 10) :: boundary
        integer :: i
        integer :: iplusone
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                iplusone = i + 1
                A(i, j) = k
                A(i, j) = B(iplusone, j) + k
                iplusone = i - 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0].children[3].children[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(loops[0])
    ptrans.apply(loops[0].parent.parent)
    correct = '''subroutine my_subroutine()
  integer, dimension(320,10) :: a
  integer, dimension(321,10) :: b
  integer, dimension(320,10) :: boundary
  integer :: i
  integer :: iplusone
  integer :: j
  integer :: k

  !$omp parallel default(shared), private(i,iplusone,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j,iplusone), firstprivate(i), shared(boundary,a,b), \
depend(in: boundary(i,:),k,b(i + 32,:),b(i,:)), depend(out: a(i,:))
    do j = 1, 32, 1
      iplusone = i + 1
      a(i,j) = k
      a(i,j) = b(iplusone,j) + k
      iplusone = i - 1
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel

end subroutine my_subroutine\n'''
    assert fortran_writer(tree) == correct
