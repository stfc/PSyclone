# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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
# Author: M. Naylor, University of Cambridge, UK
# Based on code by: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module contains the unit tests for the LoopTilingTrans module'''

import pytest

from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import TransformationError, \
    LoopTilingTrans


def test_loop_tiling_trans():
    '''Test the base methods of LoopTilingTrans'''
    trans = LoopTilingTrans()
    assert str(trans) == "Tile the loop construct"


def test_loop_tiling_trans_validation1(fortran_reader):
    ''' Validation passes when found a 3D nested loop construct. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j, k
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                do k=1, 100
                  tmp(i,j) = 2 * tmp(i,j) + k
                end do
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    LoopTilingTrans().validate(outer_loop, tiledims=[8, 8, 8])


def test_loop_tiling_trans_validation2(fortran_reader):
    ''' Validation fails if the are more tile dimensions than loops. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                tmp(i,j) = 2 * tmp(i,j)
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        LoopTilingTrans().validate(outer_loop, tiledims=[8, 8, 8])
    assert ("must be a sub-class of Loop but got 'Assignment'."
            in str(err.value))


def test_loop_tiling_trans_validation3(fortran_reader):
    ''' Validation fails if the loop body may have side effects. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            use other_mod
            integer:: i, j
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                call my_sub(i, j)
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        LoopTilingTrans().validate(outer_loop, tiledims=[8, 8])
    assert ("Nodes of type 'Call' cannot be enclosed by a LoopSwapTrans "
            "unless they can be guaranteed to be pure, but found: "
            "['call my_sub(i, j)\\n']." in str(err.value))


def test_loop_tiling_trans_validation4(fortran_reader):
    ''' Validation fails if loops are not perfectly nested. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j, a
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              a = 4
              do j=1, 100
                tmp(i,j) = 2 * tmp(i,j)
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        LoopTilingTrans().validate(outer_loop, tiledims=[8, 8])
    assert ("must be a sub-class of Loop but got 'Assignment'."
            in str(err.value))


def test_loop_tiling_trans_validation_args(fortran_reader):
    ''' Validation fails if invalid arguments are provided '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                tmp(i,j) = 2 * tmp(i,j)
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    with pytest.raises(ValueError) as err:
        LoopTilingTrans().apply(outer_loop, tiledims=[8, 8], unsupported=None)
    assert ("'LoopTilingTrans' received invalid options "
            "['unsupported']" in str(err.value))

    with pytest.raises(TransformationError) as err:
        LoopTilingTrans().apply(outer_loop, tiledims=32)
    assert ("The LoopTilingTrans tiledims argument must be a list of positive "
            "integers but found '32'." in str(err.value))

    with pytest.raises(TransformationError) as err:
        LoopTilingTrans().apply(outer_loop, tiledims=[-32])
    assert ("The LoopTilingTrans tiledims argument must be a list of positive "
            "integers but found '[-32]'." in str(err.value))


def test_loop_tiling_trans_apply1(fortran_reader, fortran_writer):
    ''' Transformation works on a 3D nested loop construct. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j, k
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                do k=1, 100
                  tmp(i,j) = 2 * tmp(i,j) + k
                enddo
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    LoopTilingTrans().apply(outer_loop, tiledims=[8, 16, 32])

    outer_loop = psyir.walk(Loop)[0]
    result = fortran_writer(outer_loop)
    expected = '''\
do i_out_var = 1, 100, 8
  do j_out_var = 1, 100, 16
    do k_out_var = 1, 100, 32
      do i = i_out_var, MIN(i_out_var + (8 - 1), 100), 1
        do j = j_out_var, MIN(j_out_var + (16 - 1), 100), 1
          do k = k_out_var, MIN(k_out_var + (32 - 1), 100), 1
            tmp(i,j) = 2 * tmp(i,j) + k
          enddo
        enddo
      enddo
    enddo
  enddo
enddo'''
    assert expected in result


def test_loop_tiling_trans_apply2(fortran_reader, fortran_writer):
    ''' Transformation works for a 2D tile in a 3D nested loop construct. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j, k
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                do k=1, 100
                  tmp(i,j) = 2 * tmp(i,j) + k
                enddo
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    LoopTilingTrans().apply(outer_loop, tiledims=[8, 16])

    outer_loop = psyir.walk(Loop)[0]
    result = fortran_writer(outer_loop)
    expected = '''\
do i_out_var = 1, 100, 8
  do j_out_var = 1, 100, 16
    do i = i_out_var, MIN(i_out_var + (8 - 1), 100), 1
      do j = j_out_var, MIN(j_out_var + (16 - 1), 100), 1
        do k = 1, 100, 1
          tmp(i,j) = 2 * tmp(i,j) + k
        enddo
      enddo
    enddo
  enddo
enddo'''
    assert expected in result


def test_loop_tiling_trans_apply3(fortran_reader, fortran_writer):
    ''' Transformation works for a 0D tile. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j, k
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
              do j=1, 100
                do k=1, 100
                  tmp(i,j) = 2 * tmp(i,j) + k
                enddo
              enddo
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    LoopTilingTrans().apply(outer_loop, tiledims=[])

    outer_loop = psyir.walk(Loop)[0]
    result = fortran_writer(outer_loop)
    print(result)
    expected = '''\
do i = 1, 100, 1
  do j = 1, 100, 1
    do k = 1, 100, 1
      tmp(i,j) = 2 * tmp(i,j) + k
    enddo
  enddo
enddo'''
    assert expected in result
