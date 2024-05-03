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
# -----------------------------------------------------------------------------
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Task Directive nodes. '''

import os
import pytest
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Assignment, BinaryOperation, \
        DynamicOMPTaskDirective, Literal, Loop, Reference
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.tests.utilities import Compile
from psyclone.transformations import OMPSingleTrans, \
    OMPParallelTrans

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_omp_task_directive_basic_full_array_test(
        fortran_reader, fortran_writer, tmpdir
        ):
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
    correct = '''\
!$omp task private(i,j), shared(a,b), depend(in: b(:,:)), depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = b(i,j) + 1
    enddo
  enddo
  !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_array_index_fail(fortran_reader):
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
    assert ("'ArrayReference' object is not allowed to appear in an array "
            "index expression inside an OMPTaskDirective. The index was "
            "'b(1,2)'."
            in str(excinfo.value))


def test_omp_task_directive_firstprivate_clause(
        fortran_reader, fortran_writer, tmpdir
        ):
    '''Test the code generation captures if a variable should be
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
    correct = '''\
!$omp task private(i,j), firstprivate(k), shared(a,b), depend(in: b(:,:)), \
depend(out: a(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = k
      a(i,j) = b(i,j) + k
    enddo
  enddo
  !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_full_step_input_access(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
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
                A(j, i) = k
                A(j, i) = B(j, i+1) + k
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
    correct = '''!$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(:,i + 1)), depend(out: a(:,i))
    do j = 1, 10, 1
      a(j,i) = k
      a(j,i) = b(j,i + 1) + k
    enddo
    !$omp end task
  '''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_step_input_access(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
                A(j, i) = k
                A(j, i) = B(j, i+1) + k
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
    correct = '''!$omp parallel default(shared), private(i,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(:,i + 32),b(:,i)), depend(out: a(:,i))
    do j = 1, 32, 1
      a(j,i) = k
      a(j,i) = b(j,i + 1) + k
    enddo
    !$omp end task
  enddo
  !$omp end single
  !$omp end parallel'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_step_access_chunked_loop(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
                    A(j, ii) = B(j, ii+1) + k
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
    correct = '''\
    !$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,i + 32),b(:,i),k), depend(out: a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii) = b(j,ii + 1) + k
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_step_access_double_chunked_loop(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp parallel default(shared), private(i,ii,j,jj)
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
  !$omp end parallel'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_multi_step_access_chunked_loops(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''
      !$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i + 32,j + 2 * 32),b(i + 32,j + 32),b(i,j + 2 * 32),\
b(i,j + 32),k), depend(out: a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii,jj) = b(ii + 1,jj + 33) * k
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_output_sub_step_access_chunked_loop(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
                    A(j, ii+1) = B(j, ii) + k
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,i),k), depend(out: a(:,i + 32),a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii + 1) = b(j,ii) + k
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_output_sub_step_access_double_chunked_loop(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i,j),k), depend(out: a(i + 32,j + 32),a(i + 32,j),\
a(i,j + 32),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 1) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_output_multi_step_access_double_chunked_loop(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: b(i,j),k), depend(out: a(i + 32,j + 2 * 32),a(i + 32,j + 32),\
a(i,j + 2 * 32),a(i,j + 32),a(i + 32,j + 3 * 32),a(i,j + 3 * 32))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          a(ii + 1,jj + 33) = b(ii,jj) * k
          a(ii + 1,jj + 65) = b(ii,jj) * k
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_input_shift_loop_reference_expressions(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
    accessing an input array shifted by the step size of the outer loop,
    and the inner loop uses Reference values for start, stop and step.'''
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
                A(j, i) = k
                A(j, i) = B(j, i+1) + k
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
    correct = '''!$omp task private(j), firstprivate(l,m,n,i), shared(a,b), \
depend(in: k,b(:,i + 1)), depend(out: a(:,i))
    do j = l, m, n
      a(j,i) = k
      a(j,i) = b(j,i + 1) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_input_shift_loop_strucutre_reference_expressions(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
    accessing an input array shifted by the step size of the outer loop,
    and the inner loop contains start, stop and step values that are elements
    of a structure.'''
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
                A(j, i) = k
                A(j, i) = B(j, i+1) + k
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
    correct = '''!$omp task private(j), firstprivate(i), shared(l,m,n,a,b), \
depend(in: l,m,n,k,b(:,i + 1)), depend(out: a(:,i))
    do j = l%k, m%k, n%k
      a(j,i) = k
      a(j,i) = b(j,i + 1) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_input_sub_step_access_parameter_ignored(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause (ignoring
    the parameter) when an input array is shifted by less than a full step
    of the outer loop.
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
                    A(j, ii) = B(j, ii+1) + k
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,i + 32),b(:,i)), depend(out: a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii) = b(j,ii + 1) + k
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_if_statement(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
depend(in: a(i,j),b(i,j),k), depend(out: a(i + 32,j),a(i,j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          if (a(ii,jj) > 0.0) then
            a(ii + 1,jj) = b(ii,jj) * k
          end if
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


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
    assert ("Binary Operator of type Operator.MUL used as an array index "
            "'ii * 3' inside an OMPTaskDirective which is not supported"
            in str(excinfo.value))


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
    assert ("Children of BinaryOperation are of types 'Reference' and "
            "'Reference', expected one Reference and one Literal when used "
            "as an array index inside an OMPTaskDirective. The containing "
            "ArrayReference is 'a(ii + ii,jj)'."
            in str(excinfo.value))


def test_omp_task_directive_lit_sum_ref_array_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
    when we have Literal+Reference as an array index.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(320, 10) :: A
        integer, dimension(321, 10) :: B
        integer :: i
        integer :: j
        integer :: k
        do i = 1, 320, 32
            do j = 1, 32
                A(j,i) = k
                A(j,i) = B(j, 1+i) + k
                A(j,i) = B(j, 33+i) + k
                A(j,i) = B(j, 32+i) + k
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
    correct = '''!$omp task private(j), firstprivate(i), shared(a,b), \
depend(in: k,b(:,32 + i),b(:,i),b(:,2 * 32 + i)), depend(out: a(:,i))
    do j = 1, 32, 1
      a(j,i) = k
      a(j,i) = b(j,1 + i) + k
      a(j,i) = b(j,33 + i) + k
      a(j,i) = b(j,32 + i) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_write_index_shared(fortran_reader):
    ''' Test the code generation generates an error if an array index
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
    assert ("Shared variable access used as an array index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "'k'. The full access is 'a(i,k)'." in str(excinfo.value))


def test_omp_task_directive_read_index_shared(fortran_reader):
    ''' Test the code generation generates an error if an array index
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
    assert ("Shared variable access used as an array index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "'k'. The full access is 'a(i,k)'" in str(excinfo.value))


def test_omp_task_directive_read_index_shared_type(fortran_reader):
    ''' Test the code generation generates an error if an array index
    of a read array member of structure is a shared variable.'''
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
    assert ("Shared variable access used as an array index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "'k'. The full access is 'aa%A(i,k)'" in str(excinfo.value))


def test_omp_task_directive_write_index_shared_type(fortran_reader):
    ''' Test the code generation generates an error if an array index
    of a written array member of a structure is a shared variable.'''
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
    assert ("Shared variable access used as an array index inside an "
            "OMPTaskDirective which is not supported. Variable name is "
            "'k'. The full access is 'aa%A(i,k)'" in str(excinfo.value))


def test_omp_task_directive_first_access_read_firstprivate(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct
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
                    A(j, ii) = B(k, ii+1) + k
                    A(j, ii) = B(k, ii+1) + 1
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
    correct = '''!$omp parallel default(shared), private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    k = 9
    !$omp task private(ii,j), firstprivate(i,k), shared(a,b), \
depend(in: b(k,i + 32),b(k,i)), depend(out: a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii) = b(k,ii + 1) + k
        a(j,ii) = b(k,ii + 1) + 1
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_shared_nonarray(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct
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
                    k = k + B(j, ii+1) + 1
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(k,b), \
depend(in: k,b(:,i + 32),b(:,i)), depend(out: k)
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        k = k + b(j,ii + 1) + 1
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_else_statement(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(a,b), \
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
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_output_nonarray_shared_var(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct clauses
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(k), \
depend(in: k), depend(out: k)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ii
          k = k * jj
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_stepval_not_yet_firstprivate(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,kk,j), shared(k), \
depend(in: k), depend(out: k)
      do ii = i, i + 32, kk
        do jj = j, j + 32, 1
          k = k + ii
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_steval_not_yet_firsprivate_structureref(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    stepval is not yet declared firstprivate and the loop contains a structure
    access.'''
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
    correct = '''!$omp task private(ii,jj), firstprivate(i,j), shared(k,ty), \
depend(in: k,ty), depend(out: k,ty)
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%jp + ii
          ty%jp = ty%jp - (1 - ty%jp)
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_literal_index_to_read_array(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct
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
                    A(j, ii) = B(j, 1) + 1
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,1)), depend(out: a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii) = b(j,1) + 1
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_literal_index_to_write_array(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct
    code for a literal index to a written array.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j

        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(1, ii) = B(j, ii) + 1
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,i)), depend(out: a(1,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(1,ii) = b(j,ii) + 1
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_non_loop(fortran_reader):
    ''' Test the code generation throws an error if an
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
    assert ("OMPTaskDirective must have exactly one Loop child. Found '<class "
            "'psyclone.psyir.nodes.assignment.Assignment'>'" in
            str(excinfo.value))


def test_omp_task_directive_multichild(fortran_reader):
    ''' Test the code generation throws an error if it an
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
    ''' Test the code generation throws an error if a
    Loop inside an OMPTaskDirective has an array access as a start value.'''
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
    assert ("'ArrayReference' not supported in the start variable of a Loop "
            "in a OMPTaskDirective node. The start expression is 'b(3,2)'."
            in str(excinfo.value))


def test_omp_task_directive_loop_stop_array(fortran_reader):
    ''' Test the code generation throws an error if a
    Loop inside an OMPTaskDirective has an array access in its stop value.'''
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
    assert ("'ArrayReference' not supported in the stop variable of a Loop "
            "in a OMPTaskDirective node. The stop expression is "
            "'b(i,2)'." in str(excinfo.value))


def test_omp_task_directive_loop_step_array(fortran_reader):
    ''' Test the code generation throws an error if a
    Loop inside an OMPTaskDirective has an array access in its step value.'''
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
    assert ("'ArrayReference' not supported in the step variable of a Loop "
            "in a OMPTaskDirective node. The step expression is "
            "'b(i,2)'." in str(excinfo.value))


def test_omp_task_directive_literal_add_reference_proxy_var(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
                    A(j, i) = k
                    A(j, i) = B(j, 1+ii) + k
                    A(j, i) = B(j, 33+ii) + k
                    A(j, i) = B(j, 32+ii) + k
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: k,b(:,32 + i),b(:,i),b(:,2 * 32 + i)), depend(out: a(:,i))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,i) = k
        a(j,i) = b(j,1 + ii) + k
        a(j,i) = b(j,33 + ii) + k
        a(j,i) = b(j,32 + ii) + k
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_private_variable_in_array_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
                    A(j+1,i) = k
                end do
                A(k+2,i) = 3
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
    correct = '''!$omp parallel default(shared), private(i,ii,j,k)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii,k,j), firstprivate(i), shared(a), depend(out: a(:,i))
    do ii = i, i + 32, 1
      k = 3
      do j = 1, 32, 1
        a(j + 1,i) = k
      enddo
      a(k + 2,i) = 3
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_parent_loop_array_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp task private(ii), firstprivate(i,j), shared(a), \
depend(in: k), depend(out: a(i,j + 3 * 32),a(i,j + 2 * 32))
      do ii = i, i + 32, 1
        a(i,j + 65) = k
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_firstprivate_constant(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct clauses
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
            A(k+1,ii) = 20
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
    correct = '''k = 32
  !$omp parallel default(shared), private(i,ii), firstprivate(k)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(ii), firstprivate(i,k), shared(a), \
depend(in: statement), depend(out: a(:,i))
    do ii = i, i + 32, 1
      if (statement) then
        k = 30
      end if
      a(k + 1,ii) = 20
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_error_shared_index_nonarray(
        fortran_reader
        ):
    ''' Test the code generation throws an error if an
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
    assert ("Shared variable 'j' used as an array index inside an "
            "OMPTaskDirective which is not supported. The full access is "
            "'j + 1'" in str(excinfo.value))


def test_omp_task_directive_shared_loop_var(
        fortran_reader
        ):
    ''' Test the code generation throws an error if a loop
    variable is declared as shared.'''
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
    assert ("Found shared loop variable which is not allowed in OpenMP Task "
           "directive. Variable name is j" in str(excinfo.value))


def test_omp_task_directive_temporary_variable_shift_input(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
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
                A(j, i) = k
                A(j, i) = B(j, iu) + k
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
    correct = '''!$omp parallel default(shared), private(i,iu,j)
  !$omp single
  do i = 1, 10, 1
    !$omp task private(j,iu), firstprivate(i), shared(a,b), \
depend(in: k,b(:,i + 1)), depend(out: a(:,i))
    do j = 1, 10, 1
      iu = i + 1
      a(j,i) = k
      a(j,i) = b(j,iu) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_subtype_nonloop_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    code contains a type within a type, and is indexed using a non-loop
    variable.'''
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
    correct = '''!$omp parallel default(shared), private(i,ii,index,j,jj)
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
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_subtype_loop_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    code contains a type within a type and is indexed using a loop
    variable.'''
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
    correct = '''\
      !$omp task private(ii,jj), firstprivate(i,j), shared(k,ty), depend(in: \
k,ty%y%jp(j),ty%y%jp(j + 32)), depend(out: k,ty%y%jp(j + 32),ty%y%jp(j))
      do ii = i, i + 32, 1
        do jj = j, j + 32, 1
          k = k + ty%y%jp(j) + ii
          ty%y%jp(j + 1) = ty%y%jp(j + 1) - (1 - ty%y%jp(j + 1))
        enddo
      enddo
      !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_subtype_literal_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    code contains a type within a type and is indexed with a Literal
    value.'''
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
    correct = '''\
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
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_error_multiarray_structure(
        fortran_reader
        ):
    ''' Test the code generation throws an error if the
    code contains a type within a type which contains multiple
    ArrayMember children.'''
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
    assert ("PSyclone doesn't support an OMPTaskDirective containing a "
            "StructureReference with multiple "
            "array accessing members. Found 'ty%y(2)%jp(ii + 1)'"
            in str(excinfo.value))


def test_omp_task_directive_error_array_member_array_index(
        fortran_reader
        ):
    ''' Test the code generation fails when attempting to access an array
    member of a structure when using an array element as an index.'''
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
    assert ("'ArrayReference' object is not allowed to appear in an array "
            "index expression inside an OMPTaskDirective."
            in str(excinfo.value))


def test_omp_task_directive_multi_array_structure_rhs(fortran_reader):
    ''' Test the code generation throws an error if
    code contains an array member within an array member that is only accessed
    on the right hand side of an assignment.'''
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
    assert ("PSyclone doesn't support an OMPTaskDirective containing a "
            "StructureReference with multiple array accessing members. "
            "Found 'ty%y(1)%jp(ii + 1)'." in str(excinfo.value))


def test_omp_task_directive_array_member_ref_and_literal_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    code contains a type within a type. In this case the array member
    is accessed with both a Reference and a Literal index, from both
    sides of an assignment.'''
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
    correct = '''\
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
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_array_member_child_loop_index(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
    accessing an array access child of a type, when the indexes are
    not proxy loop variables but loop variables of children of the task.'''
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
    correct = '''\
!$omp task private(i,j), shared(b,aa), depend(in: aa%A(1:320,1:10)), \
depend(out: b(:,:))
  do i = 1, 10, 1
    do j = 1, 10, 1
      b(i,j) = aa%A(i,j) + 1
    enddo
  enddo
  !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_shift_indirection_input(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection.'''
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
                A(j, i) = k
                A(j, i) = B(j, iplusone) + k
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
    correct = '''!$omp parallel default(shared), private(i,iplusone,j)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j,iplusone), firstprivate(i), shared(a,b), \
depend(in: k,b(:,i + 32),b(:,i)), depend(out: a(:,i))
    do j = 1, 32, 1
      iplusone = i + 1
      a(j,i) = k
      a(j,i) = b(j,iplusone) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_shift_indirection_if(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case
    we have indirection from an if statement for +1 or -1, and the indirecting
    variable is used as an index to a written array.
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
                if(boundary(j,i) > 1) then
                    iplusone = i + 1
                else
                    iplusone = i - 1
                endif
                A(j,iplusone) = B(j, i) + k
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
    correct = '''\
  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,a,b), \
depend(in: boundary(:,i),b(:,i),k), depend(out: a(:,i + 32),a(:,i),a(:,i - 32))
    do j = 1, 32, 1
      if (boundary(j,i) > 1) then
        iplusone = i + 1
      else
        iplusone = i - 1
      end if
      a(j,iplusone) = b(j,i) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_sub_shift_indirection_if_readonly(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case
    we have indirection from an if statement for +1 or -1, and the indirecting
    variable is used as an index to a read-only array.
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
                if(boundary(j,i) > 1) then
                    iplusone = i + 1
                else
                    iplusone = i - 1
                endif
                A(j,i) = k
                A(j,i) = B(j,iplusone) + k
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
    correct = '''\
  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,a,b), \
depend(in: boundary(:,i),k,b(:,i + 32),b(:,i),b(:,i - 32)), depend(out: a(:,i))
    do j = 1, 32, 1
      if (boundary(j,i) > 1) then
        iplusone = i + 1
      else
        iplusone = i - 1
      end if
      a(j,i) = k
      a(j,i) = b(j,iplusone) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_array_member_if_indirection_readonly(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
    accessing an array access child of a type through an indirection,
    and the array access is read-only.'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(10,320) :: A
        end type
        type(x) :: AA
        integer, dimension(10,320) :: B
        integer, dimension(10,320) :: boundary
        integer :: iplusone
        integer :: i
        integer :: j
        do i = 1, 320, 32
            do j = 1, 10
                if(boundary(j,i) > 1) then
                    iplusone = i+32
                else
                    iplusone = i-1
                end if
                B(j,i) = AA%A(j,iplusone) + 1
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
    correct = '''\
  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,b,aa), \
depend(in: boundary(:,i),aa%A(1:10,i + 32),aa%A(1:10,i - 32),aa%A(1:10,i))\
, depend(out: b(:,i))
    do j = 1, 10, 1
      if (boundary(j,i) > 1) then
        iplusone = i + 32
      else
        iplusone = i - 1
      end if
      b(j,i) = aa%A(j,iplusone) + 1
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_array_member_if_indirection_write(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation makes the depend clause when
    accessing an array access child of a type through an indirection,
    and the array access is written to.'''
    code = '''
    subroutine my_subroutine()
        type :: x
          integer, dimension(10,320) :: A
        end type
        type(x) :: AA
        integer, dimension(10,320) :: B
        integer, dimension(10,320) :: boundary
        integer :: iplusone
        integer :: i
        integer :: j
        do i = 1, 320, 32
            do j = 1, 10
                if(boundary(j,i) > 1) then
                    iplusone = i+32
                else
                    iplusone = i-1
                end if
                AA%A(j,iplusone) = B(j,i) + 1
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
    correct = '''\
  !$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,aa,b), \
depend(in: boundary(:,i),b(:,i)), \
depend(out: aa%A(1:10,i + 32),aa%A(1:10,i - 32),aa%A(1:10,i))
    do j = 1, 10, 1
      if (boundary(j,i) > 1) then
        iplusone = i + 32
      else
        iplusone = i - 1
      end if
      aa%A(j,iplusone) = b(j,i) + 1
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_nemolite_boundary(
        fortran_reader, fortran_writer, tmpdir
        ):
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

    correct = '''\
  !$omp parallel default(shared), private(i,j,j_el_inner,j_out_var), \
firstprivate(jiv)
  !$omp single
  do j_out_var = ystart, ystop, 32
    j_el_inner = MIN(j_out_var + (32 - 1), ystop)
    !$omp task private(j,i), firstprivate(j_out_var,j_el_inner,xstart,\
xstop,jiv), shared(boundary,va,hv,sshn_v), depend(in: boundary(:,j_out_var),\
boundary(:,j_out_var + 32),g,hv(:,j_out_var),va(:,j_out_var + 32),\
va(:,j_out_var),sshn_v(:,j_out_var),sshn_v(:,j_out_var + 32),\
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
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_multi_step_if_indirection(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct depend clause
    when an input array is shifted by less than a full step of the outer loop,
    but this is done using an extra variable for indirection. In this case
    we have indirection from an if statement for +32 (a full step) or +1
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
                if(boundary(j,i) > 1) then
                    iplusone = i + 32
                else
                    iplusone = i + 1
                endif
                A(j,iplusone) = B(j,i) + k
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
    correct = '''\
!$omp parallel default(shared), private(i,j), firstprivate(iplusone)
  !$omp single
  do i = 1, 320, 32
    !$omp task private(j), firstprivate(i,iplusone), shared(boundary,a,b), \
depend(in: boundary(:,i),b(:,i),k), depend(out: a(:,i + 32),a(:,i))
    do j = 1, 32, 1
      if (boundary(j,i) > 1) then
        iplusone = i + 32
      else
        iplusone = i + 1
      end if
      a(j,iplusone) = b(j,i) + k
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_binop_index_array_member(
        fortran_reader, fortran_writer, tmpdir
        ):
    ''' Test the code generation generates the correct code if
    code contains a type within a type and the array index is a binary
    operation.'''
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
                k = k + ty%y%jp(i) + i
                ty%y%jp(i+1) = ty%y%jp(i+1) - (1 - ty%y%jp(i+1))
                ty%y%jp(1) = ty%y%jp(1) + 1
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    correct = '''\
  !$omp task private(i,j), shared(k,ty), depend(in: k,ty%y%jp(1:321),\
ty%y%jp(1)), depend(out: k,ty%y%jp(1:321),ty%y%jp(1))
  do i = 1, 320, 32
    do j = 1, 320, 32
      k = k + ty%y%jp(i) + i
      ty%y%jp(i + 1) = ty%y%jp(i + 1) - (1 - ty%y%jp(i + 1))
      ty%y%jp(1) = ty%y%jp(1) + 1
    enddo
  enddo
  !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_call_failure(fortran_reader):
    ''' Test that lowering fails when we have a non-inlined call.'''
    code = '''
    subroutine my_subroutine()
        use temp_mod, only: external_sub
        integer :: i, ii
        integer :: j, jj

        do i = 1, 320, 32
            do j = 1, 320, 32
                call external_sub(i, j)
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(tree.children[0].children[:])
    ptrans.apply(tree.children[0].children[:])
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Attempted to lower to OMPTaskDirective node, but the node "
            "contains a Call which must be inlined first."
            in str(excinfo.value))


def test_omp_task_external_constant(fortran_reader, fortran_writer):
    ''' Test the an external constant is ignored in clauses.'''
    code = '''
    module mymod
        integer, parameter :: constant = 1
    end module
    subroutine my_subroutine()
        use mymod, only: constant
        integer, dimension(321, 10) :: A
        integer, dimension(32, 10) :: B
        integer :: i, ii
        integer :: j
        do i = 1, 320, 32
            do ii=i, i+32
                do j = 1, 32
                    A(ii, j) = B(ii+1, j) + constant
                end do
            end do
        end do
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    tree = psyir.children[1]
    # Setup the module import for the constant symbol
    # Once 2201 is fixed we won't need this.
    sym = tree.symbol_table.lookup("constant")
    sym.interface.container_symbol._reference = psyir.children[0]
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
    # Lower it explicitly now to avoid import issues
    # For some reason this locks it in, test fails
    # without it.
    psyir.lower_to_language_level()
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(i + 32,:),b(i,:)), depend(out: a(i,:))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(ii,j) = b(ii + 1,j) + constant
      enddo
    enddo
    !$omp end task'''
    assert correct in fortran_writer(psyir)
    # Cannot test compilation of this test as it uses an external module.


@pytest.mark.xfail(reason="TODO #2052 This test is expected to fail as "
                          "we can't yet handle multiple indirections on "
                          "either side of a statement and will over "
                          "generate code for this dependency.")
def test_omp_task_directive_xfail_indirection_test(fortran_reader,
                                                   fortran_writer,
                                                   tmpdir):
    ''' Test the code generation generates the correct depend clause
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
    correct = '''!$omp parallel default(shared), private(i,iplusone,j)
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
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_48(fortran_reader, fortran_writer, tmpdir):
    ''' Test the code generation generates the correct depend clause
    when a loop variable is then used as a value outside of the loop to access
    an array. Worst case is assumed (use full-range)
    '''
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
                    A(j,ii) = B(j,ii) + k
                end do
                j = 3
                a(j,3) = 1
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a,b), \
depend(in: b(:,i)), depend(out: a(:,i),a(:,3))
    do ii = i, i + 32, 1
      do j = 1, 32, 1
        a(j,ii) = b(j,ii) + k
      enddo
      j = 3
      a(j,3) = 1
    enddo
    !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_inquiry_intrinsic(fortran_reader, fortran_writer):
    ''' Test the code generation generates the correct depend clause when
    an inquiry intrinsic is used inside the task. '''

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
                    A(j,ii) = LBOUND(B, 1) + k
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
    correct = '''!$omp task private(ii,j), firstprivate(i), shared(a),\
 depend(out: a(:,i))
    do ii = i, i + 32, 1'''
    assert correct in fortran_writer(tree)


def test_omp_task_directive_disallowed_intrinsic(fortran_reader):
    '''Test that the code generation throws an error if an intrinsic is used
    inside a task region that is not enabled in PSyclone.'''
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
                    A(j,ii) = SUM(B) + k
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
    with pytest.raises(GenerationError) as excinfo:
        tree.lower_to_language_level()
    assert ("Attempted to lower to OMPTaskDirective node, but the "
            "node contains a 'SUM(b)' intrinsic call, which is not "
            "supported." in str(excinfo.value))


def test_omp_task_directive_intrinsic_loop_bound(fortran_reader,
                                                 fortran_writer, tmpdir):
    ''' Test code generation with the task directive applied to a
    loop which accesses the full arrays with the ubound and lbound
    intrinsics.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = LBOUND(A,2), UBOUND(A, 2)
            do j = LBOUND(A,1), UBOUND(A,1)
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
    correct = '''\
!$omp task private(i,j), shared(a,b), depend(in: b(:,:)), depend(out: a(:,:))
  do i = LBOUND(a, 2), UBOUND(a, 2), 1
    do j = LBOUND(a, 1), UBOUND(a, 1), 1
      a(i,j) = b(i,j) + 1
    enddo
  enddo
  !$omp end task'''
    assert correct in fortran_writer(tree)
    assert Compile(tmpdir).string_compiles(fortran_writer(tree))


def test_omp_task_directive_intrinsic_loop_step(fortran_reader):
    ''' Test lowering fails when the step of a loop contains an
    intrinsic call.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = LBOUND(A,2), UBOUND(A, 2), LBOUND(A,2)
            do j = LBOUND(A,1), UBOUND(A,1), LBOUND(A,1)
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

    with pytest.raises(GenerationError) as excinfo:
        tdir.lower_to_language_level()
    assert ("IntrinsicCall not supported in the step variable of a Loop"
            " in an OMPTaskDirective node. The step expression is "
            "'LBOUND(a, 2)'." in str(excinfo.value))


def test_evaluate_write_reference_failcase():
    ''' Tests that the _evaluate_write_reference function throws an
    InternalError if provided a non-reference value for the ref argument.
    '''
    tdir = DynamicOMPTaskDirective()
    one = Literal("1", INTEGER_TYPE)
    clause_lists = DynamicOMPTaskDirective._clause_lists(
            [], [], [], [], []
    )
    with pytest.raises(InternalError) as excinfo:
        tdir._evaluate_write_reference(one, clause_lists)
    assert ("PSyclone can't handle an OMPTaskDirective containing an "
            "assignment with a LHS that is not a Reference. Found '1'"
            in str(excinfo.value))


def test_create_binops_from_step_and_divisors():
    ''' Tests the _create_binops_from_step_and_divisors function.
    '''
    tdir = DynamicOMPTaskDirective()
    tmp = DataSymbol("tmp", INTEGER_TYPE)
    ref = Reference(tmp)
    one = Literal("1", INTEGER_TYPE)
    binop1 = BinaryOperation.create(BinaryOperation.Operator.ADD, ref.copy(),
                                    one.copy())

    res, res2 = tdir._create_binops_from_step_and_divisors(
            binop1, ref, 32, 1, 1, 0
    )
    assert isinstance(res, BinaryOperation)
    assert isinstance(res.children[0], Reference)
    assert isinstance(res.children[1], Literal)
    assert res.children[1].value == "32"
    assert isinstance(res2, Reference)

    # Test case where literal is > step but literal % step != 0
    val = Literal("33", INTEGER_TYPE)
    binop2 = BinaryOperation.create(BinaryOperation.Operator.ADD, ref.copy(),
                                    val.copy())
    res, res2 = tdir._create_binops_from_step_and_divisors(
            binop2, ref, 32, 2, 1, 0
    )
    assert isinstance(res, BinaryOperation)
    assert isinstance(res.children[0], Reference)
    assert isinstance(res.children[1], BinaryOperation)
    assert isinstance(res.children[1].children[0], Literal)
    assert isinstance(res.children[1].children[1], Literal)
    assert res.children[1].operator == BinaryOperation.Operator.MUL
    assert res.children[1].children[0].value == "2"
    assert res.children[1].children[1].value == "32"
    assert isinstance(res2, BinaryOperation)
    assert isinstance(res2.children[0], Reference)
    assert isinstance(res2.children[1], Literal)
    assert res2.children[1].value == "32"

    # Test case where x + lit is an exact multiple of the step and the
    # multiple is > 1
    val = Literal("64", INTEGER_TYPE)
    binop2 = BinaryOperation.create(BinaryOperation.Operator.ADD, ref.copy(),
                                    val.copy())
    res, res2 = tdir._create_binops_from_step_and_divisors(
            binop2, ref, 32, 2, 0, 0
    )

    assert res2 is None
    assert isinstance(res, BinaryOperation)
    assert isinstance(res.children[0], Reference)
    assert isinstance(res.children[1], BinaryOperation)
    assert isinstance(res.children[1].children[0], Literal)
    assert isinstance(res.children[1].children[1], Literal)
    assert res.children[1].operator == BinaryOperation.Operator.MUL
    assert res.children[1].children[0].value == "2"
    assert res.children[1].children[1].value == "32"


def test_find_parent_loop_vars_fail():
    '''Tests the _find_parent_loop_vars throws an exception
    if there is no parent OMPParallelDirective.'''
    tdir = DynamicOMPTaskDirective()
    with pytest.raises(GenerationError) as excinfo:
        tdir._find_parent_loop_vars()
    assert ("Failed to find an ancestor OMPParallelDirective "
            "which is required to compute dependencies of a "
            "(Dynamic)OMPTaskDirective" in str(excinfo.value))


def test_evaluate_readonly_ref_failcase(fortran_reader):
    '''Tests that the _evaluate_readonly_reference function fails
    when it is passed an ArrayOfStructureReference with an ArrayMember
    child.'''

    code = '''subroutine test
    type c
        integer, dimension(5) :: b
    end type
    type(c), dimension(5) :: a
    integer :: x
    x = a(3)%b(1)
    end subroutine test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    # Find the AoS
    assign = psyir.walk(Assignment)[0]
    rhs = assign.rhs

    tdir = DynamicOMPTaskDirective()
    clause_lists = DynamicOMPTaskDirective._clause_lists(
            [], [], [], [], []
    )
    with pytest.raises(GenerationError) as excinfo:
        tdir._evaluate_readonly_reference(rhs, clause_lists)
    assert ("PSyclone doesn't support an OMPTaskDirective containing "
            "an ArrayOfStructuresReference with an array accessing member. "
            "Found 'a(3)%b(1)'." in str(excinfo.value))


def test_evaluate_write_ref_failcase(fortran_reader):
    '''Tests that the _evaluate_write_reference function fails
    when it is passed an ArrayOfStructureReference with an ArrayMember
    child.'''

    code = '''subroutine test
    type c
        integer, dimension(5) :: b
    end type
    type(c), dimension(5) :: a
    integer :: x
    x = a(3)%b(1)
    end subroutine test
    '''
    psyir = fortran_reader.psyir_from_source(code)
    # Find the AoS
    assign = psyir.walk(Assignment)[0]
    rhs = assign.rhs

    tdir = DynamicOMPTaskDirective()
    clause_lists = DynamicOMPTaskDirective._clause_lists(
            [], [], [], [], []
    )
    with pytest.raises(GenerationError) as excinfo:
        tdir._evaluate_write_reference(rhs, clause_lists)
    assert ("PSyclone doesn't support an OMPTaskDirective containing "
            "an ArrayOfStructuresReference with an array accessing member. "
            "Found 'a(3)%b(1)'." in str(excinfo.value))


def test_lowering_containing_kern_error():
    ''' Test that lowering throws an error when the task region
    contains a Kern child.'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loops = schedule.walk(Loop)
    tdir = DynamicOMPTaskDirective()
    children = loops[0].children[3].pop_all_children()
    loops[0].children[3].addchild(tdir)
    tdir.children[0].children = children
    with pytest.raises(GenerationError) as excinfo:
        tdir.lower_to_language_level()
    assert ("Attempted to lower to OMPTaskDirective node, but the "
            "node contains a Kern which must be inlined first."
            in str(excinfo.value))
