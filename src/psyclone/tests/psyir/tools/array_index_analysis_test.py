# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, University of Cambridge, UK
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
# Author M. Naylor, University of Cambridge, UK

''' Module containing tests for the SMT-based array index analysis.'''

import pytest
from psyclone.psyir.nodes import (Loop, Assignment, Reference)
from psyclone.psyir.symbols import Symbol
from psyclone.psyir.tools import (
    ArrayIndexAnalysis, ArrayIndexAnalysisOptions)
from psyclone.psyir.tools.array_index_analysis import translate_logical_expr
import z3


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("use_bv", [True, False])
def test_reverse(use_bv, fortran_reader, fortran_writer):
    '''Test that an array reversal routine has no array conflicts
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine reverse(arr)
          real, intent(inout) :: arr(:)
          real :: tmp
          integer :: i, n
          n = size(arr)
          do i = 1, n/2
            tmp = arr(i)
            arr(i) = arr(n+1-i)
            arr(n+1-i) = tmp
          end do
        end subroutine''')
    opts = ArrayIndexAnalysisOptions(use_bv=use_bv, prohibit_overflow=True)
    results = []
    for loop in psyir.walk(Loop):
        results.append(ArrayIndexAnalysis(opts).is_loop_conflict_free(loop))
    assert results == [True]


# -----------------------------------------------------------------------------
def test_odd_even_trans(fortran_reader, fortran_writer):
    '''Test that Knuth's odd-even transposition has no array conflicts
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine odd_even_transposition(arr, start)
          real, intent(inout) :: arr(:)
          integer, intent(in) :: start
          real :: tmp
          integer :: i
          do i = start, size(arr), 2
            if (arr(i) > arr(i+1)) then
              tmp = arr(i+1)
              arr(i+1) = arr(i)
              arr(i) = tmp
            end if
          end do
        end subroutine''')
    results = []
    opts = ArrayIndexAnalysisOptions(prohibit_overflow=True)
    for loop in psyir.walk(Loop):
        results.append(ArrayIndexAnalysis(opts).is_loop_conflict_free(loop))
    assert results == [True]


# -----------------------------------------------------------------------------
def test_tiled_matmul(fortran_reader, fortran_writer):
    '''Test that tiled matmul has no array conflicts in 4/6 loops
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_matmul(a, b, c)
          integer, dimension(:,:), intent(in) :: a
          integer, dimension(:,:), intent(in) :: b
          integer, dimension(:,:), intent(out) :: c
          integer :: x, y, k, k_out_var, x_out_var, y_out_var, a1_n, a2_n, b1_n

          a2_n = SIZE(a, 2)
          b1_n = SIZE(b, 1)
          a1_n = SIZE(a, 1)

          c(:,:) = 0
          do y_out_var = 1, a2_n, 8
            do x_out_var = 1, b1_n, 8
              do k_out_var = 1, a1_n, 8
                do y = y_out_var, MIN(y_out_var + (8 - 1), a2_n), 1
                  do x = x_out_var, MIN(x_out_var + (8 - 1), b1_n), 1
                    do k = k_out_var, MIN(k_out_var + (8 - 1), a1_n), 1
                      c(x,y) = c(x,y) + a(k,y) * b(x,k)
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        end subroutine my_matmul''')
    results = []
    for loop in psyir.walk(Loop):
        results.append(ArrayIndexAnalysis().is_loop_conflict_free(loop))
    assert results == [True, True, False, True, True, False]


# -----------------------------------------------------------------------------
def test_flatten(fortran_reader, fortran_writer):
    '''Test that an array flattening routine has no array conflicts in
    either loop.
    '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine flatten1(mat, arr)
          real, intent(in) :: mat(0:,0:)
          real, intent(out) :: arr(0:)
          integer :: x, y
          integer :: nx, ny
          nx = size(mat, 1)
          ny = size(mat, 2)
          do y = 0, ny-1
            do x = 0, nx-1
              arr(nx * y + x) = mat(x, y)
            end do
          end do
        end subroutine''')
    results = []
    for loop in psyir.walk(Loop):
        results.append(ArrayIndexAnalysis().is_loop_conflict_free(loop))
    assert results == [True, True]


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("use_bv", [True, False])
def test_translate_expr(use_bv,
                        fortran_reader,
                        fortran_writer):
    '''Test that Fortran expressions are being correctly translated to SMT.
    '''
    opts = ArrayIndexAnalysisOptions(
               use_bv=use_bv,
               prohibit_overflow=True)

    def test(expr):
        psyir = fortran_reader.psyir_from_source(f'''
                  subroutine sub(x)
                    integer :: arr(10)
                    logical, intent(out) :: x
                    integer :: i
                    x = {expr}
                  end subroutine''')
        for assign in psyir.walk(Assignment):
            (rhs_smt, prohibit_overflow) = translate_logical_expr(
                assign.rhs, opts)
            solver = z3.Solver()
            assert solver.check(rhs_smt) == z3.sat

    test("+1 == 1")
    test("abs(-1) == 1")
    # test("shiftl(2,1) == 4")
    # test("shiftr(2,1) == 1")
    # test("shifta(-2,1) == -1")
    test("iand(5,1) == 1")
    test("ior(1,2) == 3")
    test("ieor(3,1) == 2")
    test("max(3,1) == 3")
    test("i == 3")
    test(".true.")
    test(".not. .false.")
    test(".true. .and. .true.")
    test(".true. .or. .false.")
    test(".false. .eqv. .false.")
    test(".false. .neqv. .true.")
    test("1 /= 2")
    test("1 < 2")
    test("10 > 2")
    test("1 <= 1 .and. 0 <= 1")
    test("1 >= 1 .and. 2 >= 1")
    test("1 * 1 == 1")
    test("mod(3, 2) == 1")
    test("foo(1)")
    test("foo(1) == 1")
    test("size(arr,tmp) == 1")
    test("size(arr(1:2)) == 2")


# -----------------------------------------------------------------------------
def check_conflict_free(fortran_reader, loop_str, yesno):
    '''Helper function to check that given loop for conflicts.
       The loop may refer to array "arr", integer variables "i" and "n",
       and logical variable "ok".
    '''
    psyir = fortran_reader.psyir_from_source(f'''
              subroutine sub(arr, n)
                integer, intent(inout) :: arr(:)
                integer, intent(in) :: n, i, tmp, tmp2
                logical :: ok
                {loop_str}
              end subroutine''')
    results = []
    opts = ArrayIndexAnalysisOptions(prohibit_overflow=True)
    for loop in psyir.walk(Loop):
        analysis = ArrayIndexAnalysis(opts)
        results.append(analysis.is_loop_conflict_free(loop))
    assert results == [yesno]


# -----------------------------------------------------------------------------
def test_ifblock_with_else(fortran_reader, fortran_writer):
    '''Test that an IfBlock with an "else" is correctly handled'''
    check_conflict_free(fortran_reader,
                        '''do i = 1, n
                             ok = i == 1
                             if (ok) then
                               arr(ior(1, 1)) = 0
                             else
                               tmp = i
                               arr(tmp) = i
                             end if
                           end do
                           arr(2) = 0
                        ''',
                        True)


# -----------------------------------------------------------------------------
def test_array_reference(fortran_reader, fortran_writer):
    '''Test an array Reference with no indices is correctly handled'''
    check_conflict_free(fortran_reader,
                        '''do i = 1, n
                             arr = arr + i
                           end do
                        ''',
                        False)


# -----------------------------------------------------------------------------
def test_singleton_slice(fortran_reader, fortran_writer):
    '''Test that an array slice with a single element is correctly handled'''
    check_conflict_free(fortran_reader,
                        '''do i = 1, n
                             arr(i:i:) = 0
                           end do
                        ''',
                        True)


# -----------------------------------------------------------------------------
def test_while_loop(fortran_reader, fortran_writer):
    '''Test a do loop nested within a while loop'''
    check_conflict_free(fortran_reader,
                        '''do while (tmp > 0)
                             do i = 1, n
                               tmp2 = arr(i)
                               arr(i) = 0
                               do while (tmp2 > 0)
                                 tmp2 = tmp2 - 1
                               end do
                             end do
                             tmp = tmp - 1
                           end do
                        ''',
                        True)


# -----------------------------------------------------------------------------
def test_injective_index(fortran_reader, fortran_writer):
    '''Test a do loop with an injective index mapping'''
    check_conflict_free(fortran_reader,
                        '''do i = 1, n
                             tmp = i+1
                             arr(tmp) = 0
                           end do
                        ''',
                        True)


# -----------------------------------------------------------------------------
def test_errors(fortran_reader, fortran_writer):
    '''Test that ArrayIndexAnalysis raises appropriate exceptions in
       error cases
    '''
    with pytest.raises(TypeError) as err:
        ArrayIndexAnalysis().is_loop_conflict_free(Reference(Symbol("foo")))
    assert ("ArrayIndexAnalysis: Loop argument expected"
            in str(err.value))

    psyir = fortran_reader.psyir_from_source('''
                subroutine sub(arr, n)
                  integer, intent(inout) :: arr(:)
                  integer, intent(in) :: n, i
                  do i = 1, n
                    arr(i) = i
                  end do
                end subroutine''')
    loop = psyir.walk(Loop)[0]
    loop.detach()
    with pytest.raises(ValueError) as err:
        ArrayIndexAnalysis().is_loop_conflict_free(loop)
    assert ("ArrayIndexAnalysis: loop has no enclosing routine"
            in str(err.value))
