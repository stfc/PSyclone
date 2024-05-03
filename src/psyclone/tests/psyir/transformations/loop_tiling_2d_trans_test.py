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
# -----------------------------------------------------------------------------
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module contains the unit tests for the LoopTiling2DTrans module'''

import pytest

from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import TransformationError, \
    LoopTiling2DTrans


def test_loop_tiling_2d_trans():
    '''Test the base methods of LoopTiling2DTrans'''
    trans = LoopTiling2DTrans()
    assert str(trans) == "Tile the loop construct using 2D blocks"


def test_loop_tiling_2d_trans_validation(fortran_reader):
    ''' Validation passes when found a 2D nested loop construct. '''
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
    LoopTiling2DTrans().validate(outer_loop)


def test_loop_tiling_2d_trans_validation1(fortran_reader):
    ''' Validation fails if the outer loop has more than one child. '''
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
        LoopTiling2DTrans().validate(outer_loop)
    assert ("must be a sub-class of Loop but got 'Assignment'."
            in str(err.value))


def test_loop_tiling_2d_trans_validation2(fortran_reader):
    ''' Validation fails if the loop body is not another loop. '''
    psyir = fortran_reader.psyir_from_source('''
        subroutine test(tmp)
            integer:: i, j
            integer, intent(inout), dimension(100,100) :: tmp

            do i=1, 100
                tmp(i,j) = 2 * tmp(i,j)
            enddo
        end subroutine test
     ''')
    outer_loop = psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        LoopTiling2DTrans().validate(outer_loop)
    assert ("must be a sub-class of Loop but got 'Assignment'."
            in str(err.value))


def test_loop_tiling_2d_trans_validation3(fortran_reader):
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
        LoopTiling2DTrans().validate(outer_loop)
    assert ("Nodes of type 'Call' cannot be enclosed by a LoopSwapTrans "
            "unless they can be guaranteed to be pure, but found: "
            "['call my_sub(i, j)\\n']." in str(err.value))


def test_loop_tiling_2d_trans_validation_options(fortran_reader):
    ''' Validation fails if an invalid option map is provided '''
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
        LoopTiling2DTrans().apply(outer_loop, {'unsupported': None})
    assert ("The LoopTiling2DTrans does not support the transformation option"
            " 'unsupported', the supported options are: ['tilesize']."
            in str(err.value))

    with pytest.raises(TransformationError) as err:
        LoopTiling2DTrans().apply(outer_loop, {'tilesize': '32'})
    assert ("The LoopTiling2DTrans tilesize option must be a positive integer "
            "but found a 'str'." in str(err.value))

    with pytest.raises(TransformationError) as err:
        LoopTiling2DTrans().apply(outer_loop, {'tilesize': -32})
    assert ("The LoopTiling2DTrans tilesize option must be a positive integer "
            "but found '-32'." in str(err.value))


def test_loop_tiling_2d_trans_apply(fortran_reader, fortran_writer):
    ''' Validation passes when found a 2D nested loop construct. '''
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
    LoopTiling2DTrans().apply(outer_loop)

    outer_loop = psyir.walk(Loop)[0]
    result = fortran_writer(outer_loop)
    expected = '''\
do i_out_var = 1, 100, 32
  i_el_inner = MIN(i_out_var + (32 - 1), 100)
  do j_out_var = 1, 100, 32
    do i = i_out_var, i_el_inner, 1
      j_el_inner = MIN(j_out_var + (32 - 1), 100)
      do j = j_out_var, j_el_inner, 1
        tmp(i,j) = 2 * tmp(i,j)
      enddo
    enddo
  enddo
enddo'''
    assert expected in result


def test_loop_tiling_2d_trans_apply_options(fortran_reader, fortran_writer):
    ''' Check that a non-default tilesize option is used correctly. '''
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
    LoopTiling2DTrans().apply(outer_loop, {"tilesize": 64})

    outer_loop = psyir.walk(Loop)[0]
    result = fortran_writer(outer_loop)
    expected = '''\
do i_out_var = 1, 100, 64
  i_el_inner = MIN(i_out_var + (64 - 1), 100)
  do j_out_var = 1, 100, 64
    do i = i_out_var, i_el_inner, 1
      j_el_inner = MIN(j_out_var + (64 - 1), 100)
      do j = j_out_var, j_el_inner, 1
        tmp(i,j) = 2 * tmp(i,j)
      enddo
    enddo
  enddo
enddo'''
    assert expected in result
